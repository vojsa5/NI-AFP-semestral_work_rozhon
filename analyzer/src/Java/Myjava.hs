module Java.Myjava
    where


import Language.Java.Parser
import Language.Java.Syntax
import Text.Parsec.Error (ParseError)
import Data.Result
import Data.LinesCnt
import Debug.Trace


-- filename -> result (occurances of classes, branches, variables and methods)
-- run the java parser


run :: String -> IO (Result)
run input_file = do
    sourceCode <- readFile input_file
    return (parse sourceCode)


-- source code -> result
-- parse the source code

parse :: String -> Result
parse source = parse' (parser compilationUnit source)


parse' :: Either ParseError CompilationUnit -> Result
parse' (Right (CompilationUnit _ _ classes)) = foldl (+) 0 (map (\cls -> parseType cls) classes)
parse' _ = emptyResult


-- parse java top level type


parseType :: TypeDecl -> Result
parseType (ClassTypeDecl decl) = parseClass decl
parseType (InterfaceTypeDecl decl) = parseInterface decl


-- parse java class


parseClass :: ClassDecl -> Result
parseClass (ClassDecl _ _ _ _ _ body) = newClass + (parseClassBody body)
parseClass (EnumDecl _ _ _ _) = newClass


-- parse java interface


parseInterface :: InterfaceDecl -> Result
parseInterface (InterfaceDecl _ _ _ _ _ body) = parseInterfaceBody body


-- parse body of a java interface


parseInterfaceBody :: InterfaceBody -> Result
parseInterfaceBody (InterfaceBody decls) = foldl (+) 0 (map (\decl -> parseMember decl) decls)


-- parse body of a java class constructor


parseConstructorBody :: ConstructorBody -> Result
parseConstructorBody (ConstructorBody _ block) = (foldl (+) 0 (map (\decl -> parseStatements decl) block)) + newFunction


-- parse body of a java class

parseClassBody :: ClassBody -> Result
parseClassBody (ClassBody decls) = foldl (+) 0 (map (\decl -> parseDecl decl) decls)


-- parse declaration of java method, inner class or member variable


parseDecl :: Decl -> Result
parseDecl (MemberDecl decl) = parseMember decl


-- parse declaration of java method, inner class or member variable #2


parseMember :: MemberDecl -> Result
parseMember (FieldDecl _ _ vars) = foldl (+) 0 (map (\var -> parseVar var) vars)
parseMember (MethodDecl _ _ _ _ params _ _ body) = (parseMethodBody body) + (Result 0 0 (length params) 1)
parseMember (ConstructorDecl _ _ _ params _ body) = (parseConstructorBody body) + (Result 0 0 (length params) 0)
parseMember (MemberClassDecl cls) = parseClass cls
parseMember (MemberInterfaceDecl interface) = parseInterface interface


-- parse a body of a method

parseMethodBody :: MethodBody -> Result
parseMethodBody (MethodBody Nothing) = emptyResult
parseMethodBody (MethodBody (Just body)) = parseBlock body


-- parse a block of statements


parseBlock :: Block -> Result
parseBlock (Block statements) = foldl (+) 0 (map (\statement -> parseStatements statement) statements)


-- parse a block of statements #2


parseStatements :: BlockStmt -> Result
parseStatements (LocalClass cls) = parseClass cls
parseStatements (BlockStmt stmt) = parseStatement stmt
parseStatements (LocalVars _ _ vars) = foldl (+) 0 (map (\var -> parseVar var) vars)


-- parse java statement


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


-- parse switch statement

parseSwitch :: SwitchBlock -> Result
parseSwitch (SwitchBlock _ block) = (Result 0 ((length block) `div` 2) 0 0) + (foldl (+) 0 (map parseStatements block))


-- parse variable

parseVar :: VarDecl -> Result
parseVar _ = newVar

