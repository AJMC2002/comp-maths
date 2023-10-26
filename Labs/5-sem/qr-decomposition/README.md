# [Лаб No4] QR-разложение

For the system

> Ax = b

we use the representation

> A = Q * R

where *Q* and *R* are an orthogonal matrix and an upper triangular matrix respectively.

This gives us the next series of expressions:

> Ax = b  
> QRx = b  
> Q(Rx) = b  
> Qy = b, y = Rx  

We obtain a system of equations in which
it's computationally easier to calculate each solution.

Let's define what an inner product <u, v> is, and what a projection of a over u is:

- <u, v> = u<sup>T</sup> v

- proj<sub>u</sub> a = <u, a>/<u, u> u

From there we get that for each matrix we get the following:

- u<sub>1</sub> = a<sub>1</sub>

- u<sub>2</sub> = a<sub>2</sub> - proj<sub>u<sub>1</sub></sub> a<sub>2</sub>

- ...

- u<sub>k</sub> = a<sub>k</sub> - Σ<sub>j=1</sub><sup>k-1</sup> proj<sub>u<sub>j</sub></sub> a<sub>k</sub>

- e<sub>k</sub> = u<sub>k</sub> / ||u<sub>k</sub>||

- Q = [ e<sub>1</sub> | e<sub>2</sub> | ... | e<sub>k</sub> ]

- R = Q<sup>T</sup> A -> a<sub>ij</sub> = <e<sub>i</sub>, a<sub>j</sub>>, i<=j

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
  │  1.340226452656466 │
  │ -4.758006718924967 │
  │  2.577752892870468 │
  └                    ┘


||x - x_calc|| = 0.0006538635015404

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
  │   1.213793103448276 │
  │ 0.15310344827586217 │
  └                     ┘


||x - x_calc|| = 0.00004548588261464293

* * * * * *

x =
  ┌    ┐
  │  2 │
  │  1 │
  │ -3 │
  └    ┘


x_calc =
  ┌                    ┐
  │                  2 │
  │ 1.0000000000000002 │
  │                 -3 │
  └                    ┘


||x - x_calc|| = 0.0000000000000002220446049250313

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
  │  2.9999999999999996 │
  │ -1.0000000000000029 │
  │   3.999999999999999 │
  │   2.000000000000002 │
  └                     ┘


||x - x_calc|| = 0.000000000000003774758283725532

* * * * * *

x =
  ┌    ┐
  │ -1 │
  │  0 │
  │  1 │
  └    ┘


x_calc =
  ┌                                   ┐
  │               -1.0000000000000004 │
  │ 0.0000000000000018318679906315083 │
  │                0.9999999999999989 │
  └                                   ┘


||x - x_calc|| = 0.0000000000000021875901640420228
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
  ┌                    ┐
  │  0.129966149308238 │
  │ 0.8001689444483465 │
  │ 1.0757290259147418 │
  └                    ┘


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
  │ 0.15331773408300756 │
  │ 0.35835406064468234 │
  │ 0.35066487427095094 │
  │ 0.19304791492348186 │
  └                     ┘
```
