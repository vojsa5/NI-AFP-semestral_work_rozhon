

module Java.Javatest where



import Test.Hspec
import Data.Result
import Data.Settings
import Java.Myjava
import System.Directory


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "getStatement" $ do
      it "solves java correctly" $ do
          currDir <- getCurrentDirectory
          sourceCode <- (readFile (currDir ++ "/test/Java/test.java")) 
          Java.Myjava.parse sourceCode `shouldBe` (Result 1 0 6 1)
          sourceCode2 <- (readFile (currDir ++ "/test/Java/test2.java"))
          Java.Myjava.parse sourceCode2 `shouldBe` (Result 2 7 24 7)
          sourceCode3 <- (readFile (currDir ++ "/test/Java/test3.java"))  
          Java.Myjava.parse sourceCode3 `shouldBe` (Result 2 5 13 4)
          sourceCode3 <- (readFile (currDir ++ "/test/Java/Switch.java"))  
          Java.Myjava.parse sourceCode3 `shouldBe` (Result 1 13 3 1)
          sourceCode4 <- (readFile (currDir ++ "/test/Java/IfElse.java"))
          Java.Myjava.parse sourceCode4 `shouldBe` (Result 1 6 2 1)