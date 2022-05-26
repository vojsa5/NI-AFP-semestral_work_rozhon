module Data.Settings
    where



-- Determines what single line comment symbol is used in the language

data CommentType = DoubleSlash | Hash
                    deriving (Show, Read, Eq)



-- Determines type of single line comments and presence of multiline comments.
-- Since only multiline comments used are /* */, it is handled with just Bool value


data Settings = Settings {
    commentType :: CommentType,
    hasBlockComments :: Bool
}


-- settings of supported languages

javaSettings = Settings DoubleSlash True
cSettings = javaSettings
pythonSettings = Settings Hash False


