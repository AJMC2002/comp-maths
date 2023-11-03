# [Лаб No6] Метод Ричардсона

Using a modified [Richardson's method](https://en.wikipedia.org/wiki/Modified_Richardson_iteration) one can solve any system of linear equations

Given the system

> A x = b,

Richardson's method is the following:

> x<sup>(k+1)</sup> = x<sup>(k)</sup> + omega * ( b - A x<sup>(k)</sup> )

It's convergence depends on if

> || I - omega * A || < 1

## Results

From the following test:

```haskell
main :: IO ()
main = do
  let a = V3 (V3 (2.0 :: Double) 1.0 0.0) (V3 0.0 2.0 1.0) (V3 1.0 0.0 3.0)
  let b = V3 2.0 1.0 4.0
  let x0 = zero V3
  let omega = 0.2
  let (x, k) = richardson a b x0 omega 0
  putStrLn ("Solution vector " ++ show x)
  putStrLn ("Obtained in " ++ show k ++ " steps.")
```

This result was obtained:

```haskell
Solution vector V3 1.001774599176192 (-1.1595019386879883e-3) 0.999328938459136
Obtained in 15 steps.
```
