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
