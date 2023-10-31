module Main where

import Linear
import MyLib

main :: IO ()
main = do
  let x1 = richardson (V3 (V3 (2.0 :: Double) 1.0 0.0) (V3 0.0 2.0 1.0) (V3 1.0 0.0 3.0)) (V3 2.0 1.0 4.0) (V3 0.0 0.0 0.0) 0.2
  let x2 = richardson (V3 (V3 (2.0 :: Double) 1.0 0.0) (V3 0.0 2.0 1.0) (V3 1.0 0.0 3.0)) (V3 2.0 1.0 4.0) (V3 0.0 0.0 0.0) 0.5
  print x1
  print x2
