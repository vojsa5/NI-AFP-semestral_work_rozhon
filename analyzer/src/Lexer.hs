module Lexer
    (getKeywords)
    where


import Data.Char (isDigit, isLetter)

data LexerToken = Char | UsableInVar | Other




getKeywords :: String -> [String]
getKeywords (head:tail) = getKeywords' (head:tail) "" (getLexerToken head)



getCharsUsableInVarName :: Char -> Bool
getCharsUsableInVarName ch = isLetter ch || isDigit ch || ch == '_' || ch == '-'


getLexerToken :: Char -> LexerToken
getLexerToken ch = if isLetter ch then Char else if getCharsUsableInVarName ch then UsableInVar else Other



getKeywords' :: String -> String -> LexerToken -> [String]
getKeywords' (head:second:tail) (varHead:varTail) Char = 
    getKeywords' (second:tail) (varHead:varTail ++ [head]) (getLexerToken second)
getKeywords' (head:second:tail) (varHead:varTail) UsableInVar = 
    getKeywords' (second:tail) (varHead:varTail ++ [head]) (getLexerToken second)
getKeywords' (head:second:tail) (varHead:varTail) Other = 
    getKeywords' (second:tail) [] (getLexerToken second) ++ [varHead:varTail]
getKeywords' (head:second:tail) [] Char = getKeywords' (second:tail) [head] (getLexerToken second)
getKeywords' (head:second:tail) [] _ = getKeywords' (second:tail) [] (getLexerToken second)
getKeywords' (head:tail) [] Char = [[head]]
getKeywords' (head:tail) [] _ = []
getKeywords' (head:tail) var _ = [var]