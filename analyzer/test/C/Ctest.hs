

module C.Ctest where



import Test.Hspec
import Data.Result
import Lib
import C.MyC
import Language.C
import Language.C.System.GCC

main :: IO ()
main = hspec spec




cFile = "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/C/test.c"
cFile2 = "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/C/test2.c"
cFile3 = "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/C/test3.c"
cFile4 = "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/C/test4.c"
cFile5 = "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/C/switch.c"

spec :: Spec
spec = do
  describe "getStatement" $ do
      it "solves C correctly" $ do
          ctu <- C.MyC.parseMyFile cFile
          C.MyC.parse ctu cFile `shouldBe` (Result 1 0 9 2)
          ctu2 <- C.MyC.parseMyFile cFile2
          C.MyC.parse ctu2 cFile2 `shouldBe` (Result 10 0 34 1)
          ctu3 <- C.MyC.parseMyFile cFile3
          C.MyC.parse ctu3 cFile3 `shouldBe` (Result 0 2 1 1)
          ctu4 <- C.MyC.parseMyFile cFile4
          C.MyC.parse ctu4 cFile4 `shouldBe` (Result 1 0 4 1)
          ctu5 <- C.MyC.parseMyFile cFile5
          C.MyC.parse ctu5 cFile5 `shouldBe` (Result 0 5 3 1)