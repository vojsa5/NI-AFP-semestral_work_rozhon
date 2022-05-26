

module Java.Javatest where



import Test.Hspec
import Data.Result
import Data.Settings
import Lib
import Java.Myjava


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "getStatement" $ do
      it "solves java correctly" $ do
          sourceCode <- (readFile "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/Java/test.java")  
          Java.Myjava.parse sourceCode `shouldBe` (Result 1 0 6 1)
          sourceCode2 <- (readFile "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/Java/test2.java")  
          Java.Myjava.parse sourceCode2 `shouldBe` (Result 2 7 24 7)
          sourceCode3 <- (readFile "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/Java/test3.java")  
          Java.Myjava.parse sourceCode3 `shouldBe` (Result 2 5 13 4)
          sourceCode3 <- (readFile "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/Java/Switch.java")  
          Java.Myjava.parse sourceCode3 `shouldBe` (Result 1 13 3 1)
          sourceCode4 <- (readFile "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/Java/IfElse.java")  
          Java.Myjava.parse sourceCode4 `shouldBe` (Result 1 6 2 1)