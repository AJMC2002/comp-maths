module Main where

import Linear
import MyLib

main :: IO ()
main = do
  let a = V3 (V3 (2.0 :: Double) 1.0 0.0) (V3 0.0 2.0 1.0) (V3 1.0 0.0 3.0)
  let b = V3 2.0 1.0 4.0
  let x0 = zero V3
  let omega = 0.2
  let x = richardson a b x0 omega
  print x
