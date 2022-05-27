{-|
Module      : Java Parser
Description : Counts classes, functions and methods, variables and branch statements in a java source code
Copyright   : (c) Vojtech Rozhon, 2022
License     : MIT
Stability   : experimental
-}


module Java.JavaParser
    where


import Language.Java.Parser
import Language.Java.Syntax
import Text.Parsec.Error (ParseError)
import Data.ParserResult
import Data.LinesCnt
import Debug.Trace



-- | Runs the java parser
-- filename -> result (occurances of classes, branches, variables and methods)

run :: String -> IO (ParserResult)
run input_file = do
    sourceCode <- readFile input_file
    return (parse sourceCode)


-- | Parses the source code
-- source code -> result

parse :: String -> ParserResult
parse source = parse' (parser compilationUnit source)


parse' :: Either ParseError CompilationUnit -> ParserResult
parse' (Right (CompilationUnit _ _ classes)) = foldl (+) 0 (map (\cls -> parseType cls) classes)
parse' _ = emptyResult


-- | Parses a java top level type


parseType :: TypeDecl -> ParserResult
parseType (ClassTypeDecl decl) = parseClass decl
parseType (InterfaceTypeDecl decl) = parseInterface decl


-- | Parses a java class


parseClass :: ClassDecl -> ParserResult
parseClass (ClassDecl _ _ _ _ _ body) = newClass + (parseClassBody body)
parseClass (EnumDecl _ _ _ _) = newClass


-- | Parses a java interface


parseInterface :: InterfaceDecl -> ParserResult
parseInterface (InterfaceDecl _ _ _ _ _ body) = parseInterfaceBody body


-- | Parses body of a java interface


parseInterfaceBody :: InterfaceBody -> ParserResult
parseInterfaceBody (InterfaceBody decls) = foldl (+) 0 (map (\decl -> parseMember decl) decls)


-- | Parses body of a java class constructor


parseConstructorBody :: ConstructorBody -> ParserResult
parseConstructorBody (ConstructorBody _ block) = (foldl (+) 0 (map (\decl -> parseStatements decl) block)) + newFunction


-- | Parses body of a java class

parseClassBody :: ClassBody -> ParserResult
parseClassBody (ClassBody decls) = foldl (+) 0 (map (\decl -> parseDecl decl) decls)


-- | Parses declaration of java method, inner class or member variable


parseDecl :: Decl -> ParserResult
parseDecl (MemberDecl decl) = parseMember decl


-- | Parses declaration of java method, inner class or member variable #2


parseMember :: MemberDecl -> ParserResult
parseMember (FieldDecl _ _ vars) = foldl (+) 0 (map (\var -> parseVar var) vars)
parseMember (MethodDecl _ _ _ _ params _ _ body) = (parseMethodBody body) + (ParserResult 0 0 (length params) 1)
parseMember (ConstructorDecl _ _ _ params _ body) = (parseConstructorBody body) + (ParserResult 0 0 (length params) 0)
parseMember (MemberClassDecl cls) = parseClass cls
parseMember (MemberInterfaceDecl interface) = parseInterface interface


-- | Parses a body of a method

parseMethodBody :: MethodBody -> ParserResult
parseMethodBody (MethodBody Nothing) = emptyResult
parseMethodBody (MethodBody (Just body)) = parseBlock body


-- | Parses a block of statements


parseBlock :: Block -> ParserResult
parseBlock (Block statements) = foldl (+) 0 (map (\statement -> parseStatements statement) statements)


-- | Parses a block of statements #2


parseStatements :: BlockStmt -> ParserResult
parseStatements (LocalClass cls) = parseClass cls
parseStatements (BlockStmt stmt) = parseStatement stmt
parseStatements (LocalVars _ _ vars) = foldl (+) 0 (map (\var -> parseVar var) vars)


-- | Parses java statement


parseStatement :: Stmt -> ParserResult
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


-- | Parses switch statement

parseSwitch :: SwitchBlock -> ParserResult
parseSwitch (SwitchBlock _ block) = (ParserResult 0 ((length block) `div` 2) 0 0) + (foldl (+) 0 (map parseStatements block))


-- | Parses variable

parseVar :: VarDecl -> ParserResult
parseVar _ = newVar

