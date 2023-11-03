module MyLib (givensRot, eigenvalues, eigenvaluesWithStepCount) where

import Numeric.LinearAlgebra (Matrix, Vector, atIndex, cmap, diag, ident, maxIndex, rows, takeDiag, tr)
import Numeric.LinearAlgebra.Devel (mapMatrixWithIndex)
import Numeric.LinearAlgebra.HMatrix (mul)

givensRot :: Int -> (Int, Int) -> Double -> Matrix Double
givensRot n (i, j) theta = mapMatrixWithIndex placeTrigs $ ident n
  where
    placeTrigs :: (Int, Int) -> Double -> Double
    placeTrigs (i', j') val
      | (i' == i && j' == i) || (i' == j && j' == j) = cos theta
      | i' == i && j' == j = sin theta
      | i' == j && j' == i = -(sin theta)
      | otherwise = val

-- | Uses Jacobi's method (rotation method)
eigenvalues :: Matrix Double -> Vector Double
eigenvalues a_mat = eigenvals
  where
    (_, eigenvals) = eigenvaluesWithStepCount a_mat

-- | Uses Jacobi's method (rotation method)
eigenvaluesWithStepCount :: Matrix Double -> (Int, Vector Double)
eigenvaluesWithStepCount = eigenvaluesRecur 0
  where
    eigenvaluesRecur :: Int -> Matrix Double -> (Int, Vector Double)
    eigenvaluesRecur k d_mat
      -- \| trace (show d_mat) False = undefined -- Debugging (import Debug.Trace (trace))
      | abs dij < (10e-10 :: Double) || k > 1000000 = (k, takeDiag d_mat)
      | otherwise = eigenvaluesRecur (k + 1) (s_mat' `mul` d_mat `mul` tr s_mat')
      where
        (i, j) = maxIndex $ cmap abs (d_mat - diag (takeDiag d_mat)) -- (i, j) is the index of the pivot (max abs off-diagonal element)
        dij = d_mat `atIndex` (i, j)
        dii = d_mat `atIndex` (i, i)
        djj = d_mat `atIndex` (j, j)
        theta =
          if dii == djj
            then (if dij > 0 then 1 else -1) * pi / 4
            else atan (2 * dij / (dii - djj)) / 2
        s_mat' = givensRot (rows d_mat) (i, j) theta