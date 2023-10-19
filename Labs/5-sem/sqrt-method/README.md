# [Лаб No3] Метод квадратного корня

For the system

> Ax = b

we use the representation

> A = U<sup>T</sup> * U

where *U* is an upper triangular matrix.

This gives us the next series of expressions:

> Ax = b  
> U<sup>T</sup>Ux = b  
> U<sup>T</sup>(Ux) = b  
> U<sup>T</sup>y = b, y = Ux  

We obtain a system of equations in which
it's computationally easier to calculate each solution.

From there we get that for each matrix we get the following:

- u<sub>11</sub> = sqrt( a<sub>11</sub> )

- u<sub>1j</sub> = a<sub>1j</sub> / u<sub>11</sub> for all j > 1

- u<sub>ii</sub> = sqrt( a<sub>ii</sub> - Σ<sub>k=1</sub><sup>i-1</sup> u<sub>ki</sub><sup>2</sup> ) *[i = j]*

- u<sub>ij</sub> = 1 / u<sub>ii</sub> ( a<sub>ij</sub> - Σ<sub>k=1</sub><sup>i-1</sup> u<sub>ki</sub>u<sub>kk</sub> ) *[i < j]*

## Results

### Tests

```rust
x = 
  ┌    ┐
  │  6 │
  │ -5 │
  │ -4 │
  └    ┘


x_calc = 
  ┌                    ┐
  │  5.042735042735044 │
  │ -5.492307692307692 │
  │ -2.769230769230771 │
  └                    ┘


||x - x_calc|| = 1.635088976698541

* * * * * *

x = 
  ┌     ┐
  │ 0.8 │
  │  -2 │
  │   1 │
  └     ┘


x_calc = 
  ┌                     ┐
  │  0.8429009193054138 │
  │ -1.8723186925434117 │
  │  0.7191011235955056 │
  └                     ┘


||x - x_calc|| = 0.31152397005107146

* * * * * *

x = 
  ┌         ┐
  │ -6.0978 │
  │ -2.2016 │
  │ -6.8011 │
  │ -8.8996 │
  │  0.1998 │
  └         ┘


x_calc = 
  ┌     ┐
  │ NaN │
  │ NaN │
  │ NaN │
  │ NaN │
  │ NaN │
  └     ┘


||x - x_calc|| = NaN

* * * * * *

x = 
  ┌      ┐
  │ 2.22 │
  │ 0.55 │
  │ 1.67 │
  └      ┘


x_calc = 
  ┌                    ┐
  │  2.677595628415302 │
  │ 1.6939890710382504 │
  │ 0.9836065573770494 │
  └                    ┘


||x - x_calc|| = 1.410404449750333
```

### Solutions

```rust
a =
  ┌                   ┐
  │ 13.14 -2.12  1.17 │
  │ -2.12   6.3 -2.45 │
  │  1.17 -2.45   4.6 │
  └                   ┘

b =
  ┌      ┐
  │ 1.27 │
  │ 2.13 │
  │ 3.14 │
  └      ┘

x =
  ┌                     ┐
  │ 0.12996614930823797 │
  │  0.8001689444483464 │
  │  1.0757290259147416 │
  └                     ┘

* * * * * *

a =
  ┌                     ┐
  │ 4.31 0.26 0.61 0.27 │
  │ 0.26 2.32 0.18 0.34 │
  │ 0.61 0.18  3.2 0.31 │
  │ 0.27 0.34 0.31 5.17 │
  └                     ┘

b =
  ┌      ┐
  │ 1.02 │
  │    1 │
  │ 1.34 │
  │ 1.27 │
  └      ┘

x =
  ┌                     ┐
  │ 0.15331773408300753 │
  │  0.3583540606446823 │
  │ 0.35066487427095105 │
  │  0.1930479149234819 │
  └                     ┘
```
