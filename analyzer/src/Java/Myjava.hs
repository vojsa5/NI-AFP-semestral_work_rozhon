module Java.Myjava
    where


import Language.Java.Parser
import Language.Java.Syntax
import Text.Parsec.Error (ParseError)
import Data.Result
import Data.LinesCnt
import Debug.Trace

run file = parseMyFile file


parseSourceCode :: String -> Result
parseSourceCode source = parse (parser compilationUnit source)


parse :: Either ParseError CompilationUnit -> Result
parse (Right (CompilationUnit _ _ classes)) = foldl (+) 0 (map (\cls -> parseType cls) classes)
parse _ = emptyResult



parseType :: TypeDecl -> Result
parseType (ClassTypeDecl decl) = parseClass decl
parseType (InterfaceTypeDecl decl) = parseInterface decl


parseClass :: ClassDecl -> Result
parseClass (ClassDecl _ _ _ _ _ body) = newClass + (parseClassBody body)
parseClass (EnumDecl _ _ _ _) = newClass


parseInterface :: InterfaceDecl -> Result
parseInterface (InterfaceDecl _ _ _ _ _ body) = parseInterfaceBody body


parseInterfaceBody :: InterfaceBody -> Result
parseInterfaceBody (InterfaceBody decls) = foldl (+) 0 (map (\decl -> parseMember decl) decls)


parseConstructorBody :: ConstructorBody -> Result
parseConstructorBody (ConstructorBody _ block) = (foldl (+) 0 (map (\decl -> parseStatements decl) block)) + newFunction

parseClassBody :: ClassBody -> Result
parseClassBody (ClassBody decls) = foldl (+) 0 (map (\decl -> parseDecl decl) decls)


parseDecl :: Decl -> Result
parseDecl (MemberDecl decl) = parseMember decl



parseMember :: MemberDecl -> Result
parseMember (FieldDecl _ _ vars) = foldl (+) 0 (map (\var -> parseVar var) vars)
parseMember (MethodDecl _ _ _ _ params _ _ body) = (parseMethodBody body) + (Result 0 0 (length params) 1)
parseMember (ConstructorDecl _ _ _ params _ body) = (parseConstructorBody body) + (Result 0 0 (length params) 0)
parseMember (MemberClassDecl cls) = parseClass cls
parseMember (MemberInterfaceDecl interface) = parseInterface interface


parseMethodBody :: MethodBody -> Result
parseMethodBody (MethodBody Nothing) = emptyResult
parseMethodBody (MethodBody (Just body)) = parseBlock body



parseBlock :: Block -> Result
parseBlock (Block statements) = foldl (+) 0 (map (\statement -> parseStatements statement) statements)



parseStatements :: BlockStmt -> Result
parseStatements (LocalClass cls) = parseClass cls
parseStatements (BlockStmt stmt) = parseStatement stmt
parseStatements (LocalVars _ _ vars) = foldl (+) 0 (map (\var -> parseVar var) vars)


parseStatement :: Stmt -> Result
parseStatement (StmtBlock blck) = parseBlock blck
parseStatement (IfThen _ stmt) = newBranch + (parseStatement stmt)
parseStatement (IfThenElse _ stmt1 stmt2) = newBranch + (parseStatement stmt1) + (parseStatement stmt2)
parseStatement (While _ stmt) = parseStatement stmt
parseStatement (BasicFor _ _ _ stmt) = parseStatement stmt
parseStatement (EnhancedFor _ _ _ _ stmt) = parseStatement stmt
parseStatement (Do stmt _) = newBranch + (parseStatement stmt)
parseStatement (Try blck _ Nothing) = parseBlock blck
parseStatement (Try blck1 _ (Just blck2)) = (parseBlock blck1) + (parseBlock blck2)
parseStatement (Synchronized _ blck) = parseBlock blck
parseStatement (Labeled _ stmt) = parseStatement stmt
parseStatement (Switch expr switch) = foldl (+) 0 (map parseSwitch switch)
parseStatement _ = emptyResult



parseSwitch :: SwitchBlock -> Result
parseSwitch (SwitchBlock _ block) = (Result 0 ((length block) `div` 2) 0 0) + (foldl (+) 0 (map parseStatements block))


parseVar :: VarDecl -> Result
parseVar _ = newVar




parseMyFile :: String -> IO (Result)
parseMyFile input_file = do
    source <- (readFile input_file)
    return (parseSourceCode source)
