module Main (main) where

import Data.List (reverse, sort)
import MyLib (eigenvaluesWithStepCount)
import Numeric.LinearAlgebra (Matrix, toList)
import Numeric.LinearAlgebra.Data (fromLists)

main :: IO ()
main =
  -- e_1 = 2585.25381092892231
  let a :: Matrix Double =
        fromLists
          [ [4, -30, 60, -35],
            [-30, 300, -675, 420],
            [60, -675, 1620, -1050],
            [-35, 420, -1050, 700]
          ]
      (k, eigenvals) = eigenvaluesWithStepCount a
   in do
        putStrLn ("Eigenvalues " ++ show (reverse $ sort $ toList eigenvals))
        putStrLn ("Done in " ++ show k ++ " steps")
