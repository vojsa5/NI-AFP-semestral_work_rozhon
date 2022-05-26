module Main where

import Lib
import C.MyC
import Java.Myjava
import Python.MyPython
import Data.Result
import System.Environment 
import Data.Settings



-- run the app
-- filename is expected as an argument


main :: IO ()
main = do
    args <- getArgs
    run' (head args) (getLanguageName (head args))





run' :: String -> String -> IO ()
run' fileName "java" = do
    res <- Java.Myjava.run fileName
    code <- readFile fileName
    printResult res (countLines javaSettings (createLines javaSettings code))
run' fileName "c" = do
    res <- C.MyC.run fileName
    code <- readFile fileName
    printResult res (countLines cSettings (createLines cSettings code))
run' fileName "python" = do
    res <- Python.MyPython.run fileName
    code <- readFile fileName
    printResult res (countLines pythonSettings (createLines pythonSettings code))
run' fileName "" = do
    return ()