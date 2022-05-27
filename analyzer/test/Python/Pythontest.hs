

module Python.Pythontest where



import Test.Hspec
import Data.ParserResult
import Data.Settings
import Python.PythonParser
import System.Directory


main :: IO ()
main = hspec spec




spec :: Spec
spec = do
  describe "getStatement" $ do
      it "solves python correctly" $ do
          currDir <- getCurrentDirectory
          sourceCode <- (readFile (currDir ++ "/test/Python/test.py")) 
          Python.PythonParser.parseSourceCode sourceCode `shouldBe` (ParserResult 0 3 6 1)
          sourceCode2 <- (readFile (currDir ++ "/test/Python/test2.py")) 
          Python.PythonParser.parseSourceCode sourceCode2 `shouldBe` (ParserResult 1 0 4 2)
          sourceCode3 <- (readFile (currDir ++ "/test/Python/test3.py")) 
          Python.PythonParser.parseSourceCode sourceCode3 `shouldBe` (ParserResult 0 4 11 4)
          sourceCode4 <- (readFile (currDir ++ "/test/Python/test4.py")) 
          Python.PythonParser.parseSourceCode sourceCode4 `shouldBe` (ParserResult 3 5 11 5)
          sourceCode5 <- (readFile (currDir ++ "/test/Python/test5.py")) 
          Python.PythonParser.parseSourceCode sourceCode5 `shouldBe` (ParserResult 1 1 10 2)