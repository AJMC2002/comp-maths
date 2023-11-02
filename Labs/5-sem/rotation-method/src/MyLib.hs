module MyLib (eigenvalues, eigenvaluesWithStepCount) where

import Numeric.LinearAlgebra (Matrix, Vector, atIndex, cmap, diag, ident, maxIndex, rows, takeDiag, tr)
import Numeric.LinearAlgebra.Devel (mapMatrixWithIndex)

givensRot :: Int -> Int -> Int -> Double -> Matrix Double
givensRot n i j theta = mapMatrixWithIndex placeTrigs $ ident n
  where
    placeTrigs :: (Int, Int) -> Double -> Double
    placeTrigs (i', j') val
      | (i' == i && j' == i) || (i' == j && j' == j) = cos theta
      | i' == i && j' == j = sin theta
      | i' == j && j' == i = -sin theta
      | otherwise = val

-- | Uses Jacobi's method (rotation method)
eigenvalues :: Matrix Double -> Vector Double
eigenvalues a_mat = eigenvals
  where
    (_, eigenvals) = eigenvaluesWithStepCount a_mat

-- | Uses Jacobi's method (rotation method)
eigenvaluesWithStepCount :: Matrix Double -> (Int, Vector Double)
eigenvaluesWithStepCount a_mat = eigenvaluesRecur 0 a_mat $ ident n
  where
    eigenvaluesRecur :: Int -> Matrix Double -> Matrix Double -> (Int, Vector Double)
    eigenvaluesRecur k d_mat s_mat
      | abs dij < (10e-5 :: Double) || k > 1000 = (k, takeDiag d_mat)
      | otherwise = eigenvaluesRecur (k + 1) (tr s_mat' * d_mat * s_mat') (s_mat * s_mat')
    n = rows a_mat
    (i, j) = maxIndex $ cmap abs (a_mat - diag (takeDiag a_mat)) -- (i, j) is the index of the pivot (max abs off-diagonal element)
    dij = a_mat `atIndex` (i, j)
    dii = a_mat `atIndex` (i, i)
    djj = a_mat `atIndex` (j, j)
    theta =
      if dii == djj
        then (if dij > 0 then 1 else -1) * pi / 4
        else 1 / 2 * atan (2 * dij / (dii - djj))
    s_mat' = givensRot n i j theta