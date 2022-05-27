

module Java.Javatest where



import Test.Hspec
import Data.ParserResult
import Data.Settings
import Java.JavaParser
import System.Directory


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "getStatement" $ do
      it "solves java correctly" $ do
          currDir <- getCurrentDirectory
          sourceCode <- (readFile (currDir ++ "/test/Java/test.java")) 
          Java.JavaParser.parse sourceCode `shouldBe` (ParserResult 1 0 6 1)
          sourceCode2 <- (readFile (currDir ++ "/test/Java/test2.java"))
          Java.JavaParser.parse sourceCode2 `shouldBe` (ParserResult 2 7 24 7)
          sourceCode3 <- (readFile (currDir ++ "/test/Java/test3.java"))  
          Java.JavaParser.parse sourceCode3 `shouldBe` (ParserResult 2 5 13 4)
          sourceCode3 <- (readFile (currDir ++ "/test/Java/Switch.java"))  
          Java.JavaParser.parse sourceCode3 `shouldBe` (ParserResult 1 13 3 1)
          sourceCode4 <- (readFile (currDir ++ "/test/Java/IfElse.java"))
          Java.JavaParser.parse sourceCode4 `shouldBe` (ParserResult 1 6 2 1)