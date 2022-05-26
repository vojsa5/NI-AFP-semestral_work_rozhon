module C.MyC
    where


import Language.C
import Language.C.System.GCC
import Data.Maybe


import Data.Result



run file = parseMyFile file >>= (printMyAST file)

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast




parse :: CTranslUnit -> String ->  Result
parse (CTranslUnit tUnit info) sourceFile = case foldl (+) 0 (map (\unit -> parseExternalDeclaration unit sourceFile) tUnit) of
  (Result classCnt branchesCnt varCnt fncCnt) -> Result classCnt branchesCnt (varCnt - fncCnt) fncCnt -- don't count functions as variable declarations




parseExternalDeclaration :: CExternalDeclaration NodeInfo -> String -> Result
parseExternalDeclaration (CDeclExt decl) sourceFile = parseTopLevelDeclaration decl sourceFile
parseExternalDeclaration (CFDefExt fce) sourceFile = parseTopLevelFunction fce sourceFile
parseExternalDeclaration _ _ = emptyResult


parseTopLevelDeclaration :: CDeclaration NodeInfo -> String -> Result
parseTopLevelDeclaration (CDecl declSpec declList info) sourceFile = if checkSourceCode info sourceFile
  then 
    (foldl (+) 0 (map parseDeclarationSpecifier declSpec)) + (parseDeclaration' declList emptyResult)
  else
    emptyResult
parseTopLevelDeclaration (CStaticAssert expr _ info) sourceFile = if checkSourceCode info sourceFile
  then
    parseExpression expr
  else
    emptyResult



parseDeclaration :: CDeclaration NodeInfo -> Result
parseDeclaration (CDecl declSpec declList info) = 
  (foldl (+) 0 (map parseDeclarationSpecifier declSpec)) + (parseDeclaration' declList emptyResult)

parseDeclaration (CStaticAssert expr _ info) = parseExpression expr




parseDeclarationSpecifier :: CDeclarationSpecifier NodeInfo -> Result
parseDeclarationSpecifier (CStorageSpec _) = emptyResult
parseDeclarationSpecifier (CTypeSpec spec) = parseTypeSpecification spec
parseDeclarationSpecifier _ = emptyResult


parseTypeSpecification :: CTypeSpecifier NodeInfo -> Result
parseTypeSpecification (CSUType (CStruct _ _ decl _ _) _) = case decl of
  (Just j) -> (foldl (+) 0 (map parseDeclaration j)) + newClass
  Nothing -> newClass
parseTypeSpecification (CEnumType _ _) = newClass
parseTypeSpecification (CTypeOfExpr expr _) = parseExpression expr
parseTypeSpecification (CTypeOfType decl _) = parseDeclaration decl
parseTypeSpecification (CAtomicType decl _) = parseDeclaration decl
parseTypeSpecification _ = emptyResult


checkSourceCode :: NodeInfo -> String -> Bool
checkSourceCode (OnlyPos pos _) sourceFile = (posFile pos) == sourceFile
checkSourceCode (NodeInfo pos _ _) sourceFile = (posFile pos) == sourceFile


parseDeclaration' :: [(Maybe (CDeclarator NodeInfo), Maybe (CInitializer NodeInfo), Maybe (CExpression NodeInfo))] -> Result -> Result
parseDeclaration' [] res = res
parseDeclaration' ((decl, init, expr):tail) res = 
  case decl of
    (Just j) -> newVar
    Nothing -> emptyResult
  +
  case expr of
    (Just j) -> parseExpression j
    Nothing -> emptyResult
  +
  parseDeclaration' tail res



parseTopLevelFunction :: CFunctionDef NodeInfo -> String -> Result
parseTopLevelFunction (CFunDef _ cDecl decls statement info) sourceFile = if checkSourceCode info sourceFile
  then
    parseStatement statement + (foldl (+) 0 (map parseDeclaration decls)) + newFunction + parseCDeclarator cDecl

  else
    emptyResult



parseFunction :: CFunctionDef NodeInfo -> Result
parseFunction (CFunDef _ _ decls statement info) = parseStatement statement + (foldl (+) 0 (map parseDeclaration decls))



parseStatement :: CStatement NodeInfo -> Result
parseStatement (CLabel _ statement _ _) = parseStatement statement
parseStatement (CCase expr statement _) = (parseExpression expr) + (parseStatement statement) + newBranch
parseStatement (CCases expr1 expr2 statement _) = (parseExpression expr1) + (parseExpression expr2) + (parseStatement statement) + newBranch
parseStatement (CDefault statement _) = parseStatement statement + newBranch
parseStatement (CExpr (Just expr) _) = parseExpression expr
parseStatement (CCompound _ blockItems _ ) = foldl (+) 0 (map parseBlockItems blockItems)
parseStatement (CIf expr statement maybeStatement _) = case maybeStatement of
  (Just statement2) -> (parseExpression expr) + (parseStatement statement) + (parseStatement statement2) + newBranch
  Nothing -> (parseExpression expr) + (parseStatement statement) + newBranch
parseStatement (CSwitch expr statement _) = parseExpression expr + parseStatement statement
parseStatement (CWhile expr statement _ _) = (parseExpression expr) + (parseStatement statement)
parseStatement (CFor _ _ _ _ _) = undefined
parseStatement (CGotoPtr expr _) = parseExpression expr
parseStatement (CReturn (Just expr) _) = parseExpression expr


parseStatement _ = emptyResult

parseExpressions :: [CExpression NodeInfo] -> Result
parseExpressions exprs = foldl (+) 0 (map parseExpression exprs)

parseExpression :: CExpression NodeInfo -> Result
parseExpression (CCond expr1 expr2 expr3 _) = case expr2 of
  (Just j) -> (parseExpression expr1) + (parseExpression expr3) + (parseExpression j)
  Nothing -> (parseExpression expr1) + (parseExpression expr3)
parseExpression (CAssign _ expr1 expr2 _) = (parseExpression expr1) + (parseExpression expr2)
parseExpression (CBinary _ expr1 expr2 _) = (parseExpression expr1) + (parseExpression expr2)
parseExpression (CCast decl expr _) = (parseDeclaration decl) + (parseExpression expr)
parseExpression (CUnary _ expr _) = parseExpression expr
parseExpression (CSizeofExpr expr _) = parseExpression expr
parseExpression (CSizeofType decl _) = parseDeclaration decl
parseExpression (CAlignofExpr expr _) = parseExpression expr
parseExpression (CAlignofType decl _) = parseDeclaration decl
parseExpression (CComplexReal expr _) = parseExpression expr
parseExpression (CComplexImag expr _) = parseExpression expr
parseExpression (CIndex expr1 expr2 _) = (parseExpression expr1) + (parseExpression expr2)
parseExpression (CCall expr exprs _) = (parseExpression expr) + (foldl (+) 0 (map parseExpression exprs))
parseExpression (CMember expr iden _ _) = parseExpression expr
parseExpression (CCompoundLit decl _ _) = parseDeclaration decl
parseExpression (CGenericSelection expr list _) = (parseExpression expr) + (parseGenericSelection list emptyResult)
parseExpression (CStatExpr statement _) = parseStatement statement
parseExpression _ = emptyResult


parseGenericSelection :: [(Maybe (CDeclaration NodeInfo), CExpression NodeInfo)] -> Result -> Result
parseGenericSelection [] res = res
parseGenericSelection ((decl, expr):tail) res = 
  (case decl of
      (Just j) -> parseDeclaration j
      Nothing -> emptyResult)
    + parseExpression expr + parseGenericSelection tail res



parseCDeclarator :: CDeclarator NodeInfo -> Result
parseCDeclarator (CDeclr _ decls _ _ _) = foldl (+) 0 (map parseCDeclarator' decls)


parseCDeclarator' :: CDerivedDeclarator NodeInfo -> Result
parseCDeclarator' (CFunDeclr (Right declsTuple) _ _) = case declsTuple of
  (decls, _) -> foldl (+) 0 (map parseDeclaration decls) + newVar
parseCDeclarator' _ = emptyResult


parseBlockItems :: CCompoundBlockItem NodeInfo -> Result
parseBlockItems (CBlockStmt statement) = parseStatement statement
parseBlockItems (CBlockDecl decl) = parseDeclaration decl
parseBlockItems (CNestedFunDef fce) = parseFunction fce


printMyAST :: String -> CTranslUnit -> IO (Result)
printMyAST file ctu = do
    return (parse ctu file)