{-|
Module      : Parser Result
Description : Stores number classes, branches, variables and functions in a source code
Copyright   : (c) Vojtech Rozhon, 2022
License     : MIT
Stability   : experimental
-}





module Data.ParserResult
    where

import Data.LinesCnt

type ClassCnt = Int

type BranchesCnt = Int

type VarDeclCnt = Int

type FunctionCnt = Int

data ParserResult = ParserResult {
    classCnt :: ClassCnt -- ^ Number of found classes
    , branchesCnt :: BranchesCnt -- ^ Number of found branches
    , varDecls :: VarDeclCnt -- ^ Number of found variables
    , functionsCnt :: FunctionCnt -- ^ Number of found functions
} deriving (Eq, Show, Read)


instance Num ParserResult where
    (+) (ParserResult classCnt branchesCnt varCnt fncCnt) (ParserResult classCnt2 branchesCnt2 varCnt2 fncCnt2) = 
        ParserResult (classCnt + classCnt2) (branchesCnt + branchesCnt2) (varCnt + varCnt2) (fncCnt + fncCnt2)
    (*) (ParserResult classCnt branchesCnt varCnt fncCnt) (ParserResult classCnt2 branchesCnt2 varCnt2 fncCnt2) = 
        ParserResult (classCnt * classCnt2) (branchesCnt * branchesCnt2) (varCnt * varCnt2) (fncCnt * fncCnt2)
    negate (ParserResult classCnt branchesCnt varCnt fncCnt) = (ParserResult (-classCnt) (-branchesCnt) (-varCnt) (-fncCnt))
    abs (ParserResult classCnt branchesCnt varCnt fncCnt) = (ParserResult (abs classCnt) (abs branchesCnt) (abs varCnt) (abs fncCnt))
    signum a = 0
    fromInteger i = emptyResult




emptyResult :: ParserResult
emptyResult = (ParserResult 0 0 0 0)


newFunction :: ParserResult
newFunction = (ParserResult 0 0 0 1)


newVar :: ParserResult
newVar = (ParserResult 0 0 1 0)


newBranch :: ParserResult
newBranch = (ParserResult 0 1 0 0)


newClass :: ParserResult
newClass = (ParserResult 1 0 0 0)


-- | Prints number result of a parser and line counter


printResult :: ParserResult -> LinesCnt -> IO ()
printResult (ParserResult classCnt branchesCnt varsCnt functionsCnt) (LinesCnt code blank comment) = do
    putStrLn ("\nCode lines: " ++ show code ++ "\n")
    putStrLn ("Blank lines: " ++ show blank ++ "\n")
    putStrLn ("Comment lines: " ++ show comment ++ "\n")
    putStrLn ("Number of classes: " ++ show classCnt ++ "\n")
    putStrLn ("Number of branches: " ++ show branchesCnt ++ "\n")
    putStrLn ("Number of variables: " ++ show varsCnt ++ "\n")
    putStrLn ("Number of functions/methods: " ++ show functionsCnt ++ "\n")

