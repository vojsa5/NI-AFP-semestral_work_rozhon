{-|
Module      : Python.PythonParser
Description : Counts classes, functions and methods, variables and branch statements in a python source code
Copyright   : (c) Vojtech Rozhon, 2022
License     : MIT
Stability   : experimental
-}


module Python.PythonParser
    where


import Language.Python.Version3 (parseModule)
import Language.Python.Common.AST
import Language.Python.Common.ParseError
import Language.Python.Common.Token (Token)
import Language.Python.Common.SrcLocation
import qualified Data.Set 
import Data.ParserResult
import Debug.Trace




-- Python AST does not include declarations of variables, so we have to look at all varibles defined in a function or on top level and sum them
-- Set of variables is being passed together with the final result

data PythonResult = PythonResult {
    -- | numbers of searched constructs in the language
    result :: ParserResult
    -- | variables defined either in current function of at top level if we are not in a function
    , vars :: Data.Set.Set String
} deriving (Eq, Show, Read)


-- | Sums multiple PythonResults into one

combinePythonResults :: [PythonResult] -> PythonResult
combinePythonResults [] = emptyPythonResult
combinePythonResults ((PythonResult res1 vars1):(PythonResult res2 vars2):tail) =
    combinePythonResults ([PythonResult (res1 + res2) (Data.Set.union vars1 vars2)] ++ tail)
combinePythonResults (res:tail) = res


-- | Creates an empty PythonResult


emptyPythonResult :: PythonResult
emptyPythonResult = PythonResult emptyResult Data.Set.empty


-- | Runs python parser
-- source code -> result


run :: String -> IO (ParserResult)
run input_file = do
    source <- (readFile input_file)
    return (parseSourceCode source)


-- | Parses the source code
-- source code -> result

parseSourceCode :: String -> ParserResult
parseSourceCode source = parse (parseModule source "")


-- | Parses S module

parse :: Either ParseError (ModuleSpan, [Token]) -> ParserResult
parse (Right ((Module span), tokens)) = case parseSuite span of
    (PythonResult (ParserResult classCnt branchesCnt varsCnt functionsCnt) idents) ->
        (ParserResult classCnt branchesCnt ((length idents) + varsCnt) functionsCnt)
parse _ = emptyResult


-- | Parses a suite

parseSuite :: Suite Language.Python.Common.SrcLocation.SrcSpan -> PythonResult
parseSuite suite = parseStatements suite


-- | Parses a statement

parseStatement :: Statement Language.Python.Common.SrcLocation.SrcSpan -> PythonResult
parseStatement (While cond suite suite2 _) = case (parseExpr cond, parseSuite suite, parseSuite suite2) of
    (res1, res2, res3) -> combinePythonResults [res1, res2, res3, PythonResult newBranch Data.Set.empty]
parseStatement (For targets generator suite suite2 _) = case (parseExprs targets, parseExpr generator, parseSuite suite, parseSuite suite2) of
    (res1, res2, res3, res4) -> combinePythonResults [res1, res2, res3, res4]
parseStatement (AsyncFor stmt _) = parseStatement stmt
parseStatement (Fun _ args _ body _) = case parseSuite body of
    (PythonResult res vars) -> case combinePythonResults[combinePythonResults (map parseParams args), PythonResult (res + newFunction) vars] of
        (PythonResult (ParserResult classCnt branchesCnt _ fncCnt) vars) -> PythonResult 
            (ParserResult classCnt branchesCnt (if Data.Set.member "self" vars --do not count self as a varaible
                then (length vars) - 1
                else length vars) fncCnt) Data.Set.empty
parseStatement (AsyncFun stmt _) = parseStatement stmt
parseStatement (Class _ _ suite _) = case parseSuite suite of
    (PythonResult res vars) -> PythonResult (res + newClass) vars
parseStatement (Conditional elifs suite _) = case (parseSuite suite, parseElifs elifs) of
    ((PythonResult res vars), pythRes) -> combinePythonResults [PythonResult (res + ParserResult 0 (length elifs) 0 0) vars, pythRes]
parseStatement (Assign to expr _) = case (parseExprs to, parseExpr expr) of
    (res1, res2) -> combinePythonResults [res1, res2]
parseStatement (AugmentedAssign to _ expr _) = case (parseExpr to, parseExpr expr) of
    (res1, res2) -> combinePythonResults [res1, res2]
parseStatement (AnnotatedAssign to expr _ _) = case (parseExpr to, parseExpr expr) of
    (res1, res2) -> combinePythonResults [res1, res2]
parseStatement _ = emptyPythonResult


-- | Parses elifs statements


parseElifs :: [(Expr Language.Python.Common.SrcLocation.SrcSpan, Suite Language.Python.Common.SrcLocation.SrcSpan)] -> PythonResult
parseElifs [] = emptyPythonResult
parseElifs ((expr, suite):tail) = combinePythonResults [parseExpr expr, parseSuite suite, parseElifs tail]


-- | Parses params

parseParams :: Parameter Language.Python.Common.SrcLocation.SrcSpan -> PythonResult
parseParams (Param (Ident iden _) _ _ _) = PythonResult emptyResult (Data.Set.fromList [iden])
parseParams (VarArgsPos (Ident iden _) _ _) = PythonResult emptyResult (Data.Set.fromList [iden])
parseParams (VarArgsKeyword (Ident iden _) _ _) = PythonResult emptyResult (Data.Set.fromList [iden])


-- | Parses statements

parseStatements :: [Statement Language.Python.Common.SrcLocation.SrcSpan] -> PythonResult
parseStatements [] = emptyPythonResult
parseStatements (head:tail) = combinePythonResults [parseStatements tail, parseStatement head] 


-- | Parses expressions

parseExprs :: [Expr Language.Python.Common.SrcLocation.SrcSpan] -> PythonResult
parseExprs [] = emptyPythonResult
parseExprs (head:tail) = combinePythonResults [parseExprs tail, parseExpr head] 


-- | parses an expression

parseExpr :: Expr Language.Python.Common.SrcLocation.SrcSpan -> PythonResult
parseExpr (Var iden _) = parseIden iden 
parseExpr (CondExpr _ _ _ _) = PythonResult newBranch Data.Set.empty
parseExpr _ = emptyPythonResult


-- | parses an identifier

parseIden :: Ident Language.Python.Common.SrcLocation.SrcSpan -> PythonResult
parseIden (Ident iden _) = PythonResult emptyResult (Data.Set.fromList [iden])
