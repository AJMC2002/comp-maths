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

> u_{1j} = a_{1j} for all j
> l_{i1} = a_{i1} / u_{11} for all i
> u_{ij} = a_{ij} - sum from k=1 to i-1 of l_{ik} u{kj}
> l*{ij} = 1 / u_{jj} ( a_{ij} - sum from k=1 to j-1 of l_{ik} u_{kj} )
