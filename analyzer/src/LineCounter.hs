{-|
Module      : Line counter
Description : Counter of code, blank and comment lines of a source code.
Copyright   : (c) Vojtech Rozhon, 2022
License     : MIT
Stability   : experimental
-}




module LineCounter
    where

import Debug.Trace
import Data.List
import Data.List.Split
import Data.Char (isSpace)
import Data.LinesCnt
import Data.Settings





-- | Returns name of a language of a source file based on its suffix
-- filename -> language name
-- empty string means the language is not supported
-- get name of the language of the code based on the suffix of the file.

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


-- | Splits source code into lines. Also split a line, if it contains a block comment symbol
-- settings -> source code -> lines 
-- splits source code by new line symbols and block comment symbols
-- line where block comment starts 


createLines :: Settings -> String -> [String]
createLines (Settings _ True) code = concat (map (\lst -> appendCommentSymbols "*/" lst) 
    (map (splitOn "*/") (concat (map (\lst -> appendCommentSymbols "/*" lst) 
        (map (splitOn "/*") (splitOn "\n" code))))))
createLines (Settings _ False) code = splitOn "\n" code



-- | Appends back comment symbol removed by the splitOn function
-- comment symbol -> lines -> lines with comment symbol possibly appended
-- takes result of splitOn function called on a line with delimiter as a block comment symbol.
-- The function wants to add back the deleted delimiter to the code
-- If line was not splited (The list has one element), no element is added
-- If opening comment symbol is at the end of a line, this line is not counted as a comment line
-- If closing comment symbol is at the beginning of a line, this line is not counted as a comment line

appendCommentSymbols :: String -> [String] -> [String]
appendCommentSymbols _ [single] = [single]
appendCommentSymbols delim lst = 
    let 
        appendedSymbols = appendCommentSymbols' delim lst
    in
        if delim == "/*" && head appendedSymbols == "" -- if block comment was at the beginning of the line, ensure that the line will not be counted as blank line
            then
                 tail appendedSymbols
            else
                 appendedSymbols


appendCommentSymbols' :: String -> [String] -> [String]
appendCommentSymbols' _ [] = []
appendCommentSymbols' delim (head:second:tail) = [head] ++ (if delim == "/*" && second /= ""
    then
        [delim] ++ [second]
    else
        [delim ++ second]) ++ (appendCommentSymbols' delim tail)
appendCommentSymbols' delim (head:tail) = if delim == "*/"
    then 
        []
    else 
        [head]


-- | Checks whether the line contains a comment symbol
-- settings -> line -> wheather there is a comment symbol in the line
-- True if a line contains a comment symbol

isCommentLine :: Settings -> String -> Bool
isCommentLine (Settings Hash _) line = case filter (== '#') line of
    [] -> False
    _ -> True
isCommentLine (Settings DoubleSlash _) line = isInfixOf "//" line


-- | Checks wheather a line starts with a comment symbol
-- settings -> line -> wheather the line starts with a comment symbol
-- True if a line starts with a comment symbol


startsWithCommentSymbol :: Settings -> String -> Bool
startsWithCommentSymbol (Settings Hash _) (head:tail) = head == '#'
startsWithCommentSymbol (Settings DoubleSlash _) (head:second:tail) = head == '/' && second == '/'
startsWithCommentSymbol _ _ = False


-- | Checks wheather the line is blank or a code line
-- settings -> line -> result counted so far -> allow increasing blank lines -> type of the line appended to the result
-- Checks whether the line is code line or a black line

checkIfCode :: Settings -> String -> LinesCnt -> Bool -> LinesCnt
checkIfCode settings line (LinesCnt code blank comment) allowIncreasingBlankLines = 
    if all isSpace line 
        then 
            if allowIncreasingBlankLines
                then
                    LinesCnt code (blank + 1) comment
                else
                    LinesCnt code blank comment
        else 
            LinesCnt (code + 1) blank comment

-- | Checks wheather the line is blank, code or comment line
-- settings -> line -> result counted so far -> type of the line appended to the result
-- Analyzes type of the line

analyzeLine :: Settings -> String -> LinesCnt -> LinesCnt
analyzeLine settings line (LinesCnt code blank comment) = 
    if startsWithCommentSymbol settings line -- if line starts with a comment, it is a comment line
        then 
            LinesCnt code blank (comment + 1)
        else 
            if isCommentLine settings line -- if lines has a comment inside but does not start with it, it is both a comment line and code/blank line
                then 
                    checkIfCode settings (getPreceedingCommentSymbol settings line) (LinesCnt code blank (comment + 1)) False
                else
                    checkIfCode settings line (LinesCnt code blank comment) True


-- | Gets a part o line preceeding a comment symbol

getPreceedingCommentSymbol :: Settings -> String -> String
getPreceedingCommentSymbol (Settings Hash _) line = head (splitOn "#" line)
getPreceedingCommentSymbol (Settings DoubleSlash _) line = head (splitOn "//" line)
getPreceedingCommentSymbol _ line = line


-- | Checks wheather a line starts with a block comment
-- isAlreadyInBlockComment -> line -> is line in block comment
-- checks whether the line is in a block comment

checkBlockComment :: Bool -> String -> Bool
checkBlockComment False (head:second:tail) = head == '/' && second == '*'
checkBlockComment True (head:second:tail) = not (head == '*' && second == '/')
checkBlockComment isInBlockComment _ = isInBlockComment



-- | Counts code, comment and blank lines
-- settings -> lines -> counted lines
-- Counts code, blank and comment lines

countLines :: Settings -> [String] -> LinesCnt
countLines settings lines = countLines' settings False lines emptyLines


countLines' :: Settings -> Bool -> [String] -> LinesCnt -> LinesCnt
countLines' settings@(Settings _ hasBlockComments) _ [] linesCnt = linesCnt
countLines' settings@(Settings _ hasBlockComments) isInBlockComment (head:tail) linesCnt = 
    let 
        blockCommentDetected = checkBlockComment isInBlockComment head
    in
        if hasBlockComments
            then
                if isInBlockComment && blockCommentDetected -- lines inside block comment
                    then
                        countLines' settings blockCommentDetected tail (linesCnt + newCommentLine)
                    else
                        if isInBlockComment || blockCommentDetected -- do not count beginning and end of a block comment as a line
                            then
                                countLines' settings blockCommentDetected tail linesCnt
                            else
                                countLines' settings False tail (analyzeLine settings head linesCnt) -- code lines
        else
            countLines' settings False tail (analyzeLine settings head linesCnt) -- code lines


