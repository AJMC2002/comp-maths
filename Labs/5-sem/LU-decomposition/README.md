# [Лаб No2] LU-разложение

For the system

> Ax = b

we use the representation

> A = L * U

where *L* and *U* are triangular matrices. Lower and upper triangular matrices respectively.

This gives us the next series of expressions:

> Ax = b  
> LUx = b  
> L(Ux) = b  
> Ly = b, y = Ux  

We obtain a system of equations in which
it's computationally easier to calculate each solution.

From there we get that for each matrix we get the following:

- u<sub>1j</sub> = a<sub>1j</sub> for all j

- l<sub>i1</sub> = a<sub>i1</sub> / u<sub>11</sub> for all i

- u<sub>ij</sub> = a<sub>ij</sub> - Σ<sub>k=1</sub><sup>i-1</sup> l<sub>ik</sub> u<sub>kj</sub>

- l<sub>ij</sub> = 1 / u<sub>jj</sub> ( a<sub>ij</sub> - Σ<sub>k=1</sub><sup>j-1</sup> l<sub>ik</sub> u<sub>kj</sub> )

## Results

### Tests

```rust
x =
  ┌          ┐
  │  1.34025 │
  │ -4.75798 │
  │   2.5771 │
  └          ┘

x_calc =
  ┌                    ┐
  │ 1.3402264526564633 │
  │ -4.758006718924972 │
  │ 2.5777528928704743 │
  └                    ┘

||x - x_calc|| = 0.0006538635015469217

* * * * * *

x =
  ┌        ┐
  │ 0.7297 │
  │ 1.2138 │
  │ 0.1531 │
  └        ┘

x_calc =
  ┌                     ┐
  │  0.7296551724137932 │
  │  1.2137931034482758 │
  │ 0.15310344827586211 │
  └                     ┘

||x - x_calc|| = 0.000045485882614672384

* * * * * *

x =
  ┌    ┐
  │  2 │
  │  1 │
  │ -3 │
  └    ┘

x_calc =
  ┌                     ┐
  │  1.9999999999999996 │
  │                   1 │
  │ -3.0000000000000004 │
  └                     ┘

||x - x_calc|| = 0.0000000000000006280369834735101

* * * * * *

x =
  ┌    ┐
  │  3 │
  │ -1 │
  │  4 │
  │  2 │
  └    ┘

x_calc =
  ┌                     ┐
  │   3.000000000000001 │
  │ -1.0000000000000018 │
  │   3.999999999999999 │
  │   2.000000000000001 │
  └                     ┘

||x - x_calc|| = 0.0000000000000023498992183808826

* * * * * *

x =
  ┌    ┐
  │ -1 │
  │  0 │
  │  1 │
  └    ┘

x_calc =
  ┌    ┐
  │ -1 │
  │  0 │
  │  1 │
  └    ┘

||x - x_calc|| = 0
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
