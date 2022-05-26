

module C.Ctest where



import Test.Hspec
import Data.Result
import Data.Settings
import Lib
import C.MyC
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
          ctu <- C.MyC.parseMyFile (currDir ++ "/test/C/test.c")
          res <- C.MyC.parse (currDir ++ "/test/C/test.c") ctu
          res `shouldBe` (Result 1 0 9 2)
          ctu2 <- C.MyC.parseMyFile (currDir ++ "/test/C/test2.c")
          res2 <- C.MyC.parse (currDir ++ "/test/C/test2.c") ctu2
          res2 `shouldBe` (Result 10 0 34 1)
          ctu3 <- C.MyC.parseMyFile (currDir ++ "/test/C/test3.c")
          res3 <- C.MyC.parse (currDir ++ "/test/C/test3.c") ctu3
          res3 `shouldBe` (Result 0 2 1 1)
          ctu4 <- C.MyC.parseMyFile (currDir ++ "/test/C/test4.c")
          res4 <- C.MyC.parse (currDir ++ "/test/C/test4.c") ctu4
          res4 `shouldBe` (Result 1 0 4 1)
          ctu5 <- C.MyC.parseMyFile (currDir ++ "/test/C/switch.c")
          res5 <- C.MyC.parse (currDir ++ "/test/C/switch.c") ctu5
          res5 `shouldBe` (Result 0 5 3 1)