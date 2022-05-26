module Lib
    where

import Debug.Trace
import Data.List
import Data.List.Split
import Lexer
import Data.Char (isSpace)
import Data.LinesCnt
import System.Environment 
import C.MyC
import Java.Myjava
import Python.MyPython
import Data.Result


data CommentType = DoubleSlash | Hash
                    deriving (Show, Read, Eq)


data Settings = Settings {
    commentType :: CommentType,
    hasBlockComments :: Bool
}



javaSettings = Settings DoubleSlash True
cSettings = javaSettings
pythonSettings = Settings Hash False



run :: IO ()
run = do
    args <- getArgs
    runLanguage (head args) (getLanguageName (head args))
    putStrLn "Byeee!"



runLanguage :: String -> String -> IO ()
runLanguage fileName "java" = do
    res <- Java.Myjava.run fileName
    code <- readLanguageFile fileName
    printResult res (countLines javaSettings (createLines code))
runLanguage fileName "c" = do
    res <- C.MyC.run fileName
    code <- readLanguageFile fileName
    printResult res (countLines cSettings (createLines code))
runLanguage fileName "python" = do
    res <- Python.MyPython.run fileName
    code <- readLanguageFile fileName
    printResult res (countLines pythonSettings (createLines code))
runLanguage fileName "" = do
    res <- C.MyC.run fileName
    code <- readLanguageFile fileName
    printResult res (countLines cSettings (createLines code))


getLanguageName :: String -> String
getLanguageName fileName = do
    if isSuffixOf ".java" fileName
        then 
            "java"
        else
            if isSuffixOf ".c" fileName
                then 
                    "c"
                else
                    if isSuffixOf ".py" fileName
                        then 
                            "python"
                        else
                            ""


readLanguageFile :: String -> IO (String)
readLanguageFile fileName = do
    code <- readFile fileName
    return code

getKeywordsAndVariables :: String -> [String]

getKeywordsAndVariables line = getKeywords line


createLines :: String -> [String]
createLines code = concat (map (\lst -> appendCommentSymbols "*/" lst) 
    (map (splitOn "*/") (concat (map (\lst -> appendCommentSymbols "/*" lst) 
        (map (splitOn "/*") (splitOn "\n" code))))))


appendCommentSymbols :: String -> [String] -> [String]
appendCommentSymbols _ [single] = [single]
appendCommentSymbols delim lst = if delim == "/*"
    then tail (appendCommentSymbols' delim lst)
    else appendCommentSymbols' delim lst


appendCommentSymbols' :: String -> [String] -> [String]
appendCommentSymbols' _ [] = []
appendCommentSymbols' delim (head:second:tail) = [head] ++ [delim ++ second] ++ (appendCommentSymbols' delim tail)
appendCommentSymbols' delim (head:tail) = if delim == "*/"
    then []
    else [head]


isCommentLine :: Settings -> String -> Bool
isCommentLine (Settings Hash _) line = case filter (== '#') line of
    [] -> False
    _ -> True
isCommentLine (Settings DoubleSlash _) line = isInfixOf "//" line


startsWithCommentSymbol :: Settings -> String -> Bool
startsWithCommentSymbol (Settings Hash _) (head:tail) = head == '#'
startsWithCommentSymbol (Settings DoubleSlash _) (head:second:tail) = head == '/' && second == '/'
startsWithCommentSymbol _ _ = False

checkComment :: Settings -> String -> LinesCnt -> LinesCnt
checkComment settings line (LinesCnt code blank comment) = 
    if startsWithCommentSymbol settings line
        then LinesCnt code blank comment
        else if isCommentLine settings line
            then checkIfCode settings line (LinesCnt code blank (comment + 1))
            else checkIfCode settings line (LinesCnt code blank comment)


checkIfCode :: Settings -> String -> LinesCnt -> LinesCnt
checkIfCode settings line (LinesCnt code blank comment) = 
    if all isSpace line 
        then LinesCnt code (blank + 1) comment
        else LinesCnt (code + 1) blank comment


analyzeLine :: Settings -> String -> LinesCnt -> LinesCnt
analyzeLine = checkComment


countLines :: Settings -> [String] -> LinesCnt
countLines settings lines = countLines' settings False lines emptyLines


checkBlockComment :: Bool -> String -> Bool
checkBlockComment False (head:second:tail) = head == '/' && second == '*'
checkBlockComment True (head:second:tail) = not (head == '*' && second == '/')
checkBlockComment isInBlockComment _ = isInBlockComment



countLines' :: Settings -> Bool -> [String] -> LinesCnt -> LinesCnt
countLines' settings@(Settings _ hasBlockComments) _ [] linesCnt = linesCnt
countLines' settings@(Settings _ hasBlockComments) isInBlockComment (head:tail) linesCnt = 
    if hasBlockComments && checkBlockComment isInBlockComment head
        then
            countLines' settings True tail (linesCnt + newCommentLine)
        else
            if hasBlockComments && isInBlockComment
                then
                    countLines' settings False tail linesCnt
                else
                    countLines' settings False tail (analyzeLine settings head linesCnt)


