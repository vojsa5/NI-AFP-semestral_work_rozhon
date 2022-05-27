import Test.Hspec
import Data.ParserResult
import Data.LinesCnt
import Data.Settings
import LineCounter
import qualified C.Ctest
import qualified Java.Javatest
import qualified Python.Pythontest
import System.Directory


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
javaSrc7 = "int i = 5;/*\nint j = 6;*/"

pythonSrc1 = "i = 5\nj = 6"
pythonSrc2 = "i = 5\n\n\n\nj = 6"
pythonSrc3 = "i = 5\n#j = 6"


spec :: Spec
spec = do
  describe "C" C.Ctest.spec
  describe "Java" Java.Javatest.spec
  describe "Python" Python.Pythontest.spec
  describe "getStatement" $ do
      it "creates java lines correctly" $ do
          createLines javaSettings javaSrc1 `shouldBe` ["int i = 5;int j = 6;"]
          createLines javaSettings javaSrc2 `shouldBe` ["int i = 5;//int j = 6;"]
          createLines javaSettings javaSrc3 `shouldBe` ["class Foo{", "int i = 5;", "int j = 6;}"]
          createLines javaSettings javaSrc4 `shouldBe` ["if(x==y){", "return 5;", "}", "else{", "return 6;", "}"]
          createLines javaSettings javaSrc5 `shouldBe` ["if(x==y){", "return 5;", "}", "", "", "", "/*", "else{", "return 6;", "}", "*/"]
          createLines javaSettings javaSrc6 `shouldBe` ["/*", "", "*/"]
          createLines javaSettings javaSrc7 `shouldBe` ["int i = 5;", "/*", "int j = 6;", "*/"]
      it "creates python lines correctly" $ do
          createLines pythonSettings pythonSrc1 `shouldBe` ["i = 5", "j = 6"]
          createLines pythonSettings pythonSrc2 `shouldBe` ["i = 5", "", "", "", "j = 6"]
          createLines pythonSettings pythonSrc3 `shouldBe` ["i = 5", "#j = 6"]
      it "counts lines in java correctly" $ do
          countLines javaSettings (createLines javaSettings javaSrc1) `shouldBe` LinesCnt 1 0 0
          countLines javaSettings (createLines javaSettings javaSrc2) `shouldBe` LinesCnt 1 0 1
          countLines javaSettings (createLines javaSettings javaSrc3) `shouldBe` LinesCnt 3 0 0
          countLines javaSettings (createLines javaSettings javaSrc4) `shouldBe` LinesCnt 6 0 0
          countLines javaSettings (createLines javaSettings javaSrc5) `shouldBe` LinesCnt 3 3 3
          countLines javaSettings (createLines javaSettings javaSrc6) `shouldBe` LinesCnt 0 0 1
          countLines javaSettings (createLines javaSettings javaSrc7) `shouldBe` LinesCnt 1 0 1
      it "counts lines in python correctly" $ do
          countLines pythonSettings (createLines pythonSettings pythonSrc1) `shouldBe` LinesCnt 2 0 0
          countLines pythonSettings (createLines pythonSettings pythonSrc2) `shouldBe` LinesCnt 2 3 0
          countLines pythonSettings (createLines pythonSettings pythonSrc3) `shouldBe` LinesCnt 1 0 1
      it "counts real programs correctly" $ do
          currDir <- getCurrentDirectory
          sourceCodeC <- readFile (currDir ++ "/test/C/test.c")
          countLines cSettings (createLines cSettings sourceCodeC) `shouldBe` LinesCnt 22 9 1
          sourceCodeJava <- readFile (currDir ++ "/test/Java/test.java")
          countLines cSettings (createLines javaSettings sourceCodeJava) `shouldBe` LinesCnt 13 1 2
          sourceCodePython <- readFile (currDir ++ "/test/Python/test.py")
          countLines pythonSettings (createLines pythonSettings sourceCodePython) `shouldBe` LinesCnt 14 6 2