{-|
Module      : Data.LinesCnt
Description : Stores number of code, blank and comment lines.
Copyright   : (c) Vojtech Rozhon, 2022
License     : MIT
Stability   : experimental
-}




module Data.LinesCnt
    where




data LinesCnt = LinesCnt {
    code :: Int -- ^ Number of code lines
    , blank :: Int -- ^ Number of blank lines
    , comment :: Int  -- ^ Number of comment lines
} deriving (Eq, Show, Read)



instance Num LinesCnt where
    (+) (LinesCnt code blank comment) (LinesCnt code2 blank2 comment2) = 
        LinesCnt (code + code2) (blank + blank2) (comment + comment2)
    (*) (LinesCnt code blank comment) (LinesCnt code2 blank2 comment2) = 
        LinesCnt (code * code2) (blank * blank2) (comment * comment2)
    negate (LinesCnt code blank comment) = (LinesCnt (-code) (-blank) (-comment))
    abs (LinesCnt code blank comment) = (LinesCnt (abs code) (abs blank) (abs comment))
    signum a = 0
    fromInteger i = emptyLines



emptyLines :: LinesCnt
emptyLines = LinesCnt 0 0 0


newCodeLine :: LinesCnt
newCodeLine = LinesCnt 1 0 0


newBlankLine :: LinesCnt
newBlankLine = LinesCnt 0 1 0


newCommentLine :: LinesCnt
newCommentLine = LinesCnt 0 0 1