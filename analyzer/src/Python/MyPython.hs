module Python.MyPython
    where


import Language.Python.Version3 (parseModule)
import Language.Python.Common.AST
import Language.Python.Common.ParseError
import Language.Python.Common.Token (Token)
import Language.Python.Common.SrcLocation
import qualified Data.Set 
import Data.Result
import Debug.Trace


-- Python AST does not include declarations of variables, so we have look at all varibles defined in a function or on top level and sum them
-- Set of variables is being passed together with the final result

data PythonResult = PythonResult {
    result :: Result
    , vars :: Data.Set.Set String
} deriving (Eq, Show, Read)


combinePythonResults :: [PythonResult] -> PythonResult
combinePythonResults [] = emptyPythonResult
combinePythonResults ((PythonResult res1 vars1):(PythonResult res2 vars2):tail) =
    combinePythonResults ([PythonResult (res1 + res2) (Data.Set.union vars1 vars2)] ++ tail)
combinePythonResults (res:tail) = res


emptyPythonResult :: PythonResult
emptyPythonResult = PythonResult emptyResult Data.Set.empty


-- source code -> result
-- run python parser


run :: String -> IO (Result)
run input_file = do
    source <- (readFile input_file)
    return (parseSourceCode source)


-- source code -> result
-- parse the source code


parseSourceCode :: String -> Result
parseSourceCode source = parse (parseModule source "")


-- parse the module

parse :: Either ParseError (ModuleSpan, [Token]) -> Result
parse (Right ((Module span), tokens)) = case parseSuite span of
    (PythonResult (Result classCnt branchesCnt varsCnt functionsCnt) idents) ->
        (Result classCnt branchesCnt ((length idents) + varsCnt) functionsCnt)
parse _ = emptyResult


-- parse suite

parseSuite :: Suite Language.Python.Common.SrcLocation.SrcSpan -> PythonResult
parseSuite suite = parseStatements suite


-- parse statement

parseStatement :: Statement Language.Python.Common.SrcLocation.SrcSpan -> PythonResult
parseStatement (While cond suite suite2 _) = case (parseExpr cond, parseSuite suite, parseSuite suite2) of
    (res1, res2, res3) -> combinePythonResults [res1, res2, res3, PythonResult newBranch Data.Set.empty]
parseStatement (For targets generator suite suite2 _) = case (parseExprs targets, parseExpr generator, parseSuite suite, parseSuite suite2) of
    (res1, res2, res3, res4) -> combinePythonResults [res1, res2, res3, res4]
parseStatement (AsyncFor stmt _) = parseStatement stmt
parseStatement (Fun _ args _ body _) = case parseSuite body of
    (PythonResult res vars) -> case combinePythonResults[combinePythonResults (map parseParams args), PythonResult (res + newFunction) vars] of
        (PythonResult (Result classCnt branchesCnt _ fncCnt) vars) -> PythonResult 
            (Result classCnt branchesCnt (if Data.Set.member "self" vars --do not count self as a varaible
                then (length vars) - 1
                else length vars) fncCnt) Data.Set.empty
parseStatement (AsyncFun stmt _) = parseStatement stmt
parseStatement (Class _ _ suite _) = case parseSuite suite of
    (PythonResult res vars) -> PythonResult (res + newClass) vars
parseStatement (Conditional elifs suite _) = case (parseSuite suite, parseElifs elifs) of
    ((PythonResult res vars), pythRes) -> combinePythonResults [PythonResult (res + Result 0 (length elifs) 0 0) vars, pythRes]
parseStatement (Assign to expr _) = case (parseExprs to, parseExpr expr) of
    (res1, res2) -> combinePythonResults [res1, res2]
parseStatement (AugmentedAssign to _ expr _) = case (parseExpr to, parseExpr expr) of
    (res1, res2) -> combinePythonResults [res1, res2]
parseStatement (AnnotatedAssign to expr _ _) = case (parseExpr to, parseExpr expr) of
    (res1, res2) -> combinePythonResults [res1, res2]
parseStatement _ = emptyPythonResult


-- parse elifs statements


parseElifs :: [(Expr Language.Python.Common.SrcLocation.SrcSpan, Suite Language.Python.Common.SrcLocation.SrcSpan)] -> PythonResult
parseElifs [] = emptyPythonResult
parseElifs ((expr, suite):tail) = combinePythonResults [parseExpr expr, parseSuite suite, parseElifs tail]


-- parse params

parseParams :: Parameter Language.Python.Common.SrcLocation.SrcSpan -> PythonResult
parseParams (Param (Ident iden _) _ _ _) = PythonResult emptyResult (Data.Set.fromList [iden])
parseParams (VarArgsPos (Ident iden _) _ _) = PythonResult emptyResult (Data.Set.fromList [iden])
parseParams (VarArgsKeyword (Ident iden _) _ _) = PythonResult emptyResult (Data.Set.fromList [iden])


-- parse statements

parseStatements :: [Statement Language.Python.Common.SrcLocation.SrcSpan] -> PythonResult
parseStatements [] = emptyPythonResult
parseStatements (head:tail) = combinePythonResults [parseStatements tail, parseStatement head] 


-- parse expressions

parseExprs :: [Expr Language.Python.Common.SrcLocation.SrcSpan] -> PythonResult
parseExprs [] = emptyPythonResult
parseExprs (head:tail) = combinePythonResults [parseExprs tail, parseExpr head] 


-- parse an expression

parseExpr :: Expr Language.Python.Common.SrcLocation.SrcSpan -> PythonResult
parseExpr (Var iden _) = parseIden iden 
parseExpr (CondExpr _ _ _ _) = PythonResult newBranch Data.Set.empty
parseExpr _ = emptyPythonResult


-- parse an identifier

parseIden :: Ident Language.Python.Common.SrcLocation.SrcSpan -> PythonResult
parseIden (Ident iden _) = PythonResult emptyResult (Data.Set.fromList [iden])
parseIden _ = emptyPythonResult
