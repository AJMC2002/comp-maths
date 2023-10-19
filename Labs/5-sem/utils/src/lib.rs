extern crate nalgebra as na;

pub type FloatingType = f64;

pub mod lab1 {
    use crate::FloatingType;
    use na::{allocator::Allocator, DMatrix, DefaultAllocator, Dim, Matrix, SquareMatrix, Storage};

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

    pub fn vandermonde_matrix(
        x0: FloatingType,
        xn: FloatingType,
        n: usize,
    ) -> DMatrix<FloatingType> {
        let step = (xn - x0) / n as FloatingType;
        DMatrix::from_fn(n, n, |i, j| (x0 + i as FloatingType * step).powi(j as _))
    }
}

pub mod lab2 {
    use nalgebra::{
        allocator::Allocator, DefaultAllocator, DimName, Owned, SquareMatrix, Vector, U1,
    };

    use crate::FloatingType;

    type SqMatType<D> = SquareMatrix<FloatingType, D, Owned<FloatingType, D, D>>;

    pub trait LUDescomposition<D>
    where
        D: DimName,
        DefaultAllocator: Allocator<FloatingType, D, D>,
    {
        fn lu_decompose(&self) -> (SqMatType<D>, SqMatType<D>);
    }

    impl<D> LUDescomposition<D> for SqMatType<D>
    where
        D: DimName,
        DefaultAllocator: Allocator<FloatingType, D, D>,
    {
        fn lu_decompose(&self) -> (SqMatType<D>, SqMatType<D>) {
            let (nrows, ncols) = self.shape();
            let mut u = SqMatType::<D>::zeros_generic(D::name(), D::name());
            let mut l = SqMatType::<D>::identity_generic(D::name(), D::name());
            (0..ncols).for_each(|j| u[(0, j)] = self[(0, j)]);
            (0..nrows).for_each(|i| l[(i, 0)] = self[(i, 0)] / u[(0, 0)]);
            (1..nrows).for_each(|i| {
                (1..ncols).for_each(|j| match i <= j {
                    true => {
                        u[(i, j)] = self[(i, j)]
                            - (0..=i - 1)
                                .map(|k| l[(i, k)] * u[(k, j)])
                                .sum::<FloatingType>()
                    }
                    false => {
                        l[(i, j)] = 1 as FloatingType / u[(j, j)]
                            * (self[(i, j)]
                                - (0..=j - 1)
                                    .map(|k| l[(i, k)] * u[(k, j)])
                                    .sum::<FloatingType>())
                    }
                })
            });
            (l, u)
        }
    }

    ///Uses the LU Decomposition method
    pub fn solve_linear_system<D>(
        a: &SqMatType<D>,
        b: &Vector<FloatingType, D, Owned<FloatingType, D, U1>>,
    ) -> Vector<FloatingType, D, Owned<FloatingType, D, U1>>
    where
        D: DimName,
        DefaultAllocator: Allocator<FloatingType, D, D>,
        DefaultAllocator: Allocator<FloatingType, D, U1>,
    {
        let (l, u) = a.lu_decompose();
        let y = l.try_inverse().expect("Could not invert matrix") * b;
        let x = u.try_inverse().expect("Could not invert matrix") * y;
        x
    }

    pub fn test_accuracy<D>(
        a: SqMatType<D>,
        b: Vector<FloatingType, D, Owned<FloatingType, D, U1>>,
        x: Vector<FloatingType, D, Owned<FloatingType, D, U1>>,
    ) where
        D: DimName,
        DefaultAllocator: Allocator<FloatingType, D, D> + Allocator<FloatingType, D, U1>,
    {
        let x_calc = solve_linear_system(&a, &b);
        let diff = (&x - &x_calc).norm();

        println!("\n* * * * * *\n");
        println!("x = {}\nx_calc = {}\n||x - x_calc|| = {}", x, x_calc, diff);
    }
}

pub mod lab3 {}
