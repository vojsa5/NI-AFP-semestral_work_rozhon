module Data.Result
    where

import Data.LinesCnt

type ClassCnt = Int

type BranchesCnt = Int

type VarDeclCnt = Int

type FunctionCnt = Int

data Result = Result {
    classCnt :: ClassCnt
    , branchesCnt :: BranchesCnt
    , varDecls :: VarDeclCnt
    , functionsCnt :: FunctionCnt
} deriving (Eq, Show, Read)


instance Num Result where
    (+) (Result classCnt branchesCnt varCnt fncCnt) (Result classCnt2 branchesCnt2 varCnt2 fncCnt2) = 
        Result (classCnt + classCnt2) (branchesCnt + branchesCnt2) (varCnt + varCnt2) (fncCnt + fncCnt2)
    (*) (Result classCnt branchesCnt varCnt fncCnt) (Result classCnt2 branchesCnt2 varCnt2 fncCnt2) = 
        Result (classCnt * classCnt2) (branchesCnt * branchesCnt2) (varCnt * varCnt2) (fncCnt * fncCnt2)
    negate (Result classCnt branchesCnt varCnt fncCnt) = (Result (-classCnt) (-branchesCnt) (-varCnt) (-fncCnt))
    abs (Result classCnt branchesCnt varCnt fncCnt) = (Result (abs classCnt) (abs branchesCnt) (abs varCnt) (abs fncCnt))
    signum a = 0
    fromInteger i = emptyResult




emptyResult :: Result
emptyResult = (Result 0 0 0 0)


newFunction :: Result
newFunction = (Result 0 0 0 1)


newVar :: Result
newVar = (Result 0 0 1 0)


newBranch :: Result
newBranch = (Result 0 1 0 0)


newClass :: Result
newClass = (Result 1 0 0 0)



printResult :: Result -> LinesCnt -> IO ()
printResult (Result classCnt branchesCnt varsCnt functionsCnt) (LinesCnt code blank comment) = do
    putStrLn ("Code lines: " ++ show code ++ "\n")
    putStrLn ("Blank lines: " ++ show blank ++ "\n")
    putStrLn ("Comment lines: " ++ show comment ++ "\n")
    putStrLn ("Number of classes: " ++ show classCnt ++ "\n")
    putStrLn ("Number of branches: " ++ show branchesCnt ++ "\n")
    putStrLn ("Number of variables: " ++ show varsCnt ++ "\n")
    putStrLn ("Number of functions/methods: " ++ show functionsCnt ++ "\n")

