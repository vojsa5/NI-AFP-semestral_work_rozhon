

module Python.Pythontest where



import Test.Hspec
import Data.Result
import Lib
import Python.MyPython

main :: IO ()
main = hspec spec




spec :: Spec
spec = do
  describe "getStatement" $ do
      it "solves python correctly" $ do
          sourceCode <- (readFile "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/Python/test.py") 
          Python.MyPython.parseSourceCode sourceCode `shouldBe` (Result 0 3 6 1)
          sourceCode2 <- (readFile "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/Python/test2.py") 
          Python.MyPython.parseSourceCode sourceCode2 `shouldBe` (Result 1 0 4 2)
          sourceCode3 <- (readFile "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/Python/test3.py") 
          Python.MyPython.parseSourceCode sourceCode3 `shouldBe` (Result 0 4 11 4)
          sourceCode4 <- (readFile "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/Python/test4.py") 
          Python.MyPython.parseSourceCode sourceCode4 `shouldBe` (Result 3 5 11 5)
          sourceCode5 <- (readFile "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/Python/test5.py") 
          Python.MyPython.parseSourceCode sourceCode5 `shouldBe` (Result 1 1 10 2)