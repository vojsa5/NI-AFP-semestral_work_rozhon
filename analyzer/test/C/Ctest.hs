

module C.Ctest where



import Test.Hspec
import Data.ParserResult
import Data.Settings
import C.CParser
import Language.C
import Language.C.System.GCC
import System.Directory


main :: IO ()
main = hspec spec






spec :: Spec
spec = do
  describe "getStatement" $ do
      it "solves C correctly" $ do
          currDir <- getCurrentDirectory
          ctu <- C.CParser.parseMyFile (currDir ++ "/test/C/test.c")
          res <- C.CParser.parse (currDir ++ "/test/C/test.c") ctu
          res `shouldBe` (ParserResult 1 0 9 2)
          ctu2 <- C.CParser.parseMyFile (currDir ++ "/test/C/test2.c")
          res2 <- C.CParser.parse (currDir ++ "/test/C/test2.c") ctu2
          res2 `shouldBe` (ParserResult 10 0 34 1)
          ctu3 <- C.CParser.parseMyFile (currDir ++ "/test/C/test3.c")
          res3 <- C.CParser.parse (currDir ++ "/test/C/test3.c") ctu3
          res3 `shouldBe` (ParserResult 0 2 1 1)
          ctu4 <- C.CParser.parseMyFile (currDir ++ "/test/C/test4.c")
          res4 <- C.CParser.parse (currDir ++ "/test/C/test4.c") ctu4
          res4 `shouldBe` (ParserResult 1 0 4 1)
          ctu5 <- C.CParser.parseMyFile (currDir ++ "/test/C/switch.c")
          res5 <- C.CParser.parse (currDir ++ "/test/C/switch.c") ctu5
          res5 `shouldBe` (ParserResult 0 4 3 1)