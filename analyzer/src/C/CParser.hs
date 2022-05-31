{-|
Module      : C.CParser
Description : Counts classes, functions and methods, variables and branch statements in a c source code
Copyright   : (c) Vojtech Rozhon, 2022
License     : MIT
Stability   : experimental
-}


module C.CParser
    where


import Language.C
import Language.C.System.GCC
import Data.Maybe
import Data.ParserResult


-- | Checks the Nodeinfo to see if the node comes from the source code file (library files are also present in the translation unit)

checkSourceCode :: NodeInfo -> String -> Bool
checkSourceCode (OnlyPos pos _) sourceFile = (posFile pos) == sourceFile
checkSourceCode (NodeInfo pos _ _) sourceFile = (posFile pos) == sourceFile


-- | Runs the c parser

run file = parseMyFile file >>= (parse file)

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast



-- | Parses the source code

parse :: String -> CTranslUnit -> IO (ParserResult)
parse file ctu = do
    return (parseCTU ctu file)


-- | Parses the translation unit


parseCTU :: CTranslUnit -> String ->  ParserResult
parseCTU (CTranslUnit tUnit info) sourceFile = case foldl (+) 0 (map (\unit -> parseExternalDeclaration unit sourceFile) tUnit) of
  (ParserResult classCnt branchesCnt varCnt fncCnt) -> 
    ParserResult classCnt branchesCnt (varCnt - fncCnt) fncCnt -- don't count functions as variable declarations



-- | Parses a top-level declaration

parseExternalDeclaration :: CExternalDeclaration NodeInfo -> String -> ParserResult
parseExternalDeclaration (CDeclExt decl) sourceFile = parseTopLevelDeclaration decl sourceFile
parseExternalDeclaration (CFDefExt fce) sourceFile = parseTopLevelFunction fce sourceFile
parseExternalDeclaration _ _ = emptyResult


-- | Parses a top-level declaration #2

parseTopLevelDeclaration :: CDeclaration NodeInfo -> String -> ParserResult
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


-- | parses a declaration

parseDeclaration :: CDeclaration NodeInfo -> ParserResult
parseDeclaration (CDecl declSpec declList info) = 
  (foldl (+) 0 (map parseDeclarationSpecifier declSpec)) + (parseDeclaration' declList emptyResult)

parseDeclaration (CStaticAssert expr _ info) = parseExpression expr


parseDeclaration' :: [(Maybe (CDeclarator NodeInfo), Maybe (CInitializer NodeInfo), Maybe (CExpression NodeInfo))] -> ParserResult -> ParserResult
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


-- | Parses a specifier of a declaration

parseDeclarationSpecifier :: CDeclarationSpecifier NodeInfo -> ParserResult
parseDeclarationSpecifier (CStorageSpec _) = emptyResult
parseDeclarationSpecifier (CTypeSpec spec) = parseTypeSpecification spec
parseDeclarationSpecifier _ = emptyResult


-- | Parses type of a specifier

parseTypeSpecification :: CTypeSpecifier NodeInfo -> ParserResult
parseTypeSpecification (CSUType (CStruct _ _ decl _ _) _) = case decl of
  (Just j) -> (foldl (+) 0 (map parseDeclaration j)) + newClass
  Nothing -> newClass
parseTypeSpecification (CEnumType _ _) = newClass
parseTypeSpecification (CTypeOfExpr expr _) = parseExpression expr
parseTypeSpecification (CTypeOfType decl _) = parseDeclaration decl
parseTypeSpecification (CAtomicType decl _) = parseDeclaration decl
parseTypeSpecification _ = emptyResult


-- | Parses a top level function

parseTopLevelFunction :: CFunctionDef NodeInfo -> String -> ParserResult
parseTopLevelFunction (CFunDef _ cDecl decls statement info) sourceFile = if checkSourceCode info sourceFile
  then
    parseStatement statement + (foldl (+) 0 (map parseDeclaration decls)) + newFunction + parseCDeclarator cDecl

  else
    emptyResult


-- | Parses a funciton

parseFunction :: CFunctionDef NodeInfo -> ParserResult
parseFunction (CFunDef _ _ decls statement info) = parseStatement statement + (foldl (+) 0 (map parseDeclaration decls))


-- | Parses a statement

parseStatement :: CStatement NodeInfo -> ParserResult
parseStatement (CLabel _ statement _ _) = parseStatement statement
parseStatement (CCase expr statement _) = (parseExpression expr) + (parseStatement statement) + newBranch
parseStatement (CCases expr1 expr2 statement _) = (parseExpression expr1) + (parseExpression expr2) + (parseStatement statement) + newBranch
parseStatement (CDefault statement _) = parseStatement statement
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


-- | Parses a list of expressions

parseExpressions :: [CExpression NodeInfo] -> ParserResult
parseExpressions exprs = foldl (+) 0 (map parseExpression exprs)


-- | Parses an expression

parseExpression :: CExpression NodeInfo -> ParserResult
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


-- | Parses C11 generic selection

parseGenericSelection :: [(Maybe (CDeclaration NodeInfo), CExpression NodeInfo)] -> ParserResult -> ParserResult
parseGenericSelection [] res = res
parseGenericSelection ((decl, expr):tail) res = 
  (case decl of
      (Just j) -> parseDeclaration j
      Nothing -> emptyResult)
    + parseExpression expr + parseGenericSelection tail res


-- | Parses declarations

parseCDeclarator :: CDeclarator NodeInfo -> ParserResult
parseCDeclarator (CDeclr _ decls _ _ _) = foldl (+) 0 (map parseCDeclarator' decls)


parseCDeclarator' :: CDerivedDeclarator NodeInfo -> ParserResult
parseCDeclarator' (CFunDeclr (Right declsTuple) _ _) = case declsTuple of
  (decls, _) -> foldl (+) 0 (map parseDeclaration decls) + newVar
parseCDeclarator' _ = emptyResult


-- | Parses a block

parseBlockItems :: CCompoundBlockItem NodeInfo -> ParserResult
parseBlockItems (CBlockStmt statement) = parseStatement statement
parseBlockItems (CBlockDecl decl) = parseDeclaration decl
parseBlockItems (CNestedFunDef fce) = parseFunction fce


