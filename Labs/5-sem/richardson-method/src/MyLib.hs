module MyLib where

import Linear (Metric, norm, (!*), (*^), (^+^), (^-^))

-- | If || I-omega*A || < 1 then the function converges
richardson :: (Foldable n, Metric n, Floating a, Ord a) => n (n a) -> n a -> n a -> a -> Int -> (n a, Int)
richardson a b x0 omega k
  | k > 500 = (x0, k)
  | norm (x0 ^-^ x_next) < epsilon = (x0, k)
  | otherwise = richardson a b x_next omega (k + 1)
  where
    x_next = x0 ^+^ omega *^ (b ^-^ a !* x0)
    epsilon = 0.001
