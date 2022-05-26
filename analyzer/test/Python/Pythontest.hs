

module Python.Pythontest where



import Test.Hspec
import Data.Result
import Data.Settings
import Python.MyPython
import System.Directory


main :: IO ()
main = hspec spec




spec :: Spec
spec = do
  describe "getStatement" $ do
      it "solves python correctly" $ do
          currDir <- getCurrentDirectory
          sourceCode <- (readFile (currDir ++ "/test/Python/test.py")) 
          Python.MyPython.parseSourceCode sourceCode `shouldBe` (Result 0 3 6 1)
          sourceCode2 <- (readFile (currDir ++ "/test/Python/test2.py")) 
          Python.MyPython.parseSourceCode sourceCode2 `shouldBe` (Result 1 0 4 2)
          sourceCode3 <- (readFile (currDir ++ "/test/Python/test3.py")) 
          Python.MyPython.parseSourceCode sourceCode3 `shouldBe` (Result 0 4 11 4)
          sourceCode4 <- (readFile (currDir ++ "/test/Python/test4.py")) 
          Python.MyPython.parseSourceCode sourceCode4 `shouldBe` (Result 3 5 11 5)
          sourceCode5 <- (readFile (currDir ++ "/test/Python/test5.py")) 
          Python.MyPython.parseSourceCode sourceCode5 `shouldBe` (Result 1 1 10 2)