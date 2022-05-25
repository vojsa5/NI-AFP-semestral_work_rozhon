import Test.Hspec
import Data.Result
import Data.LinesCnt
import Lib
import qualified C.Ctest
import qualified Java.Javatest
import qualified Python.Pythontest




main :: IO ()
main = hspec spec




javaSrc1 = "int i = 5;int j = 6;"
javaSrc2 = "int i = 5;//int j = 6;"
javaSrc3 = "class Foo{\n" ++
           "int i = 5;\n" ++
           "int j = 6;}"
javaSrc4 = "if(x==y){\n" ++
                "return 5;\n" ++
            "}\n" ++
            "else{\n" ++
                "return 6;\n" ++
            "}"

javaSrc5 = "if(x==y){\n" ++
                "return 5;\n" ++
            "}\n" ++
            "\n\n\n" ++
            "/*else{\n" ++
                "return 6;\n" ++
            "}*/"
javaSrc6 = "/**/"

javaSettings = Settings DoubleSlash True
pythonSettings = Settings Hash False


pythonSrc1 = "i = 5\nj = 6"
pythonSrc2 = "i = 5\n\n\n\nj = 6"

spec :: Spec
spec = do
  describe "C" C.Ctest.spec
  describe "Java" Java.Javatest.spec
  describe "Python" Python.Pythontest.spec
  describe "getStatement" $ do
      it "solves java correctly" $ do
          createLines javaSrc1 `shouldBe` ["int i = 5;int j = 6;"]
          createLines javaSrc2 `shouldBe` ["int i = 5;//int j = 6;"]
          createLines javaSrc3 `shouldBe` ["class Foo{", "int i = 5;", "int j = 6;}"]
          createLines javaSrc4 `shouldBe` ["if(x==y){", "return 5;", "}", "else{", "return 6;", "}"]
          createLines javaSrc5 `shouldBe` ["if(x==y){", "return 5;", "}", "", "", "", "/*else{", "return 6;", "}", "*/"]
          createLines javaSrc6 `shouldBe` ["/*", "*/"]
      it "solves python correctly" $ do
          createLines pythonSrc1 `shouldBe` ["i = 5", "j = 6"]
          createLines pythonSrc2 `shouldBe` ["i = 5", "", "", "", "j = 6"]
      it "count lines java" $ do
          countLines javaSettings (createLines javaSrc1) `shouldBe` LinesCnt 1 0 0
          countLines javaSettings (createLines javaSrc2) `shouldBe` LinesCnt 1 0 1
          countLines javaSettings (createLines javaSrc3) `shouldBe` LinesCnt 3 0 0
          countLines javaSettings (createLines javaSrc4) `shouldBe` LinesCnt 6 0 0
          countLines javaSettings (createLines javaSrc5) `shouldBe` LinesCnt 3 3 3
          countLines javaSettings (createLines javaSrc6) `shouldBe` LinesCnt 0 0 1
      it "count lines python" $ do
          countLines pythonSettings (createLines pythonSrc1) `shouldBe` LinesCnt 2 0 0
          countLines pythonSettings (createLines pythonSrc2) `shouldBe` LinesCnt 2 3 0