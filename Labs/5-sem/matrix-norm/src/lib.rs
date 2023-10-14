extern crate nalgebra as na;

use na::{allocator::Allocator, DMatrix, DefaultAllocator, Dim, Matrix, SquareMatrix, Storage};

pub type FloatingType = f64;

pub trait Norm {
    fn custom_norm(&self) -> FloatingType;
}

impl<R, C, S> Norm for Matrix<FloatingType, R, C, S>
where
    R: Dim,
    C: Dim,
    S: Storage<FloatingType, R, C>,
{
    /// Matrix:
    ///     Variant 2 of the table of functions for matrix
    ///         ||A|| = max 1<=j<=n sum from i=1 to n of |a_{ij}|
    /// Vector:
    ///     Since a vector is a matrix of 1 column, this same formula gets
    ///     the result of the norm formula 1 of a vector
    ///         ||x||_1 = sum from i=1 to n of |x_i|
    fn custom_norm(&self) -> FloatingType {
        let (_, r) = self.shape();
        (0..r)
            .map(|j| self.column(j).iter().map(|x| x.abs()).sum::<FloatingType>())
            .max_by(|a, b| a.partial_cmp(b).expect("Comparison failed"))
            .expect("Max not found.")
    }
}

pub trait Cond {
    fn cond(self) -> FloatingType;
}

impl<D, S> Cond for SquareMatrix<FloatingType, D, S>
where
    D: Dim,
    S: Storage<FloatingType, D, D>,
    DefaultAllocator: Allocator<FloatingType, D, D>,
{
    fn cond(self) -> FloatingType {
        self.norm() * self.try_inverse().expect("Could not invert matrix").norm()
    }
}

pub fn vandermonde_matrix(x0: FloatingType, xn: FloatingType, n: usize) -> DMatrix<FloatingType> {
    let step = (xn - x0) / n as FloatingType;
    DMatrix::from_fn(n, n, |i, j| (x0 + i as FloatingType * step).powi(j as _))
}
