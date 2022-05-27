module Main where

import LineCounter
import C.CParser
import Java.JavaParser
import Python.PythonParser
import Data.ParserResult
import System.Environment 
import Data.Settings



-- | Runs the app
-- | Filename is expected as an argument


main :: IO ()
main = do
    args <- getArgs
    run' (head args) (getLanguageName (head args))





run' :: String -> String -> IO ()
run' fileName "java" = do
    res <- Java.JavaParser.run fileName
    code <- readFile fileName
    printResult res (countLines javaSettings (createLines javaSettings code))
run' fileName "c" = do
    res <- C.CParser.run fileName
    code <- readFile fileName
    printResult res (countLines cSettings (createLines cSettings code))
run' fileName "python" = do
    res <- Python.PythonParser.run fileName
    code <- readFile fileName
    printResult res (countLines pythonSettings (createLines pythonSettings code))
run' fileName "" = do
    return ()