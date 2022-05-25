module Main where

import Lib
import C.MyC

main :: IO ()
main = C.MyC.run "/home/vojta/Documents/skola/AFP/NI-AFP-semestral_work_rozhon/analyzer/test/C/test2.c"
