module MyLib (richardson) where

import Linear (Metric, norm, (!*), (*^), (^+^), (^-^))

-- | If || I-omega*A || < 1 then the function converges
richardson :: (Foldable n, Metric n, Floating a, Ord a) => n (n a) -> n a -> n a -> a -> n a
richardson a b x0 omega
  | norm (x0 ^-^ x_next) < epsilon = x0
  | otherwise = richardson a b x_next omega
  where
    x_next = x0 ^+^ omega *^ (b ^-^ a !* x0)
    epsilon = 0.001
