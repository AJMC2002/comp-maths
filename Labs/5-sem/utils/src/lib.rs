use nalgebra::{Const, Owned, SquareMatrix, Vector};

pub type FloatingType = f64;

type __GenericSquareMatrix<D, S = Owned<FloatingType, D, D>> = SquareMatrix<FloatingType, D, S>;
type __GenericVector<D, S = Owned<FloatingType, D>> = Vector<FloatingType, D, S>;

type __SquareMatrix<const N: usize, S = Owned<FloatingType, Const<N>, Const<N>>> =
    SquareMatrix<FloatingType, Const<N>, S>;
type __Vector<const N: usize, S = Owned<FloatingType, Const<N>>> =
    Vector<FloatingType, Const<N>, S>;

pub mod lab1 {
    use crate::FloatingType;
    use nalgebra::{
        allocator::Allocator, DMatrix, DefaultAllocator, Dim, Matrix, SquareMatrix, Storage,
    };

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
    use crate::{FloatingType, __GenericSquareMatrix};
    use nalgebra::{allocator::Allocator, DefaultAllocator, DimName, Owned, Vector, U1};

    pub trait LUDescomposition<D>
    where
        D: DimName,
        DefaultAllocator: Allocator<FloatingType, D, D>,
    {
        fn lu_decompose(&self) -> (__GenericSquareMatrix<D>, __GenericSquareMatrix<D>);
    }

    impl<D> LUDescomposition<D> for __GenericSquareMatrix<D>
    where
        D: DimName,
        DefaultAllocator: Allocator<FloatingType, D, D>,
    {
        fn lu_decompose(&self) -> (__GenericSquareMatrix<D>, __GenericSquareMatrix<D>) {
            let (nrows, ncols) = self.shape();
            let mut u = __GenericSquareMatrix::<D>::zeros_generic(D::name(), D::name());
            let mut l = __GenericSquareMatrix::<D>::identity_generic(D::name(), D::name());
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
        a: &__GenericSquareMatrix<D>,
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
        x_calc: Vector<FloatingType, D, Owned<FloatingType, D, U1>>,
        x: Vector<FloatingType, D, Owned<FloatingType, D, U1>>,
    ) where
        D: DimName,
        DefaultAllocator: Allocator<FloatingType, D, D> + Allocator<FloatingType, D, U1>,
    {
        let diff = (&x - &x_calc).norm();
        println!("\n* * * * * *\n");
        println!("x = {}\nx_calc = {}\n||x - x_calc|| = {}", x, x_calc, diff);
    }
}

pub mod lab3 {
    use crate::{FloatingType, __GenericSquareMatrix, __GenericVector};
    use nalgebra::{allocator::Allocator, DefaultAllocator, DimName};

    pub trait UDescomposition {
        fn u_decompose(&self) -> Self;
    }

    impl<D> UDescomposition for __GenericSquareMatrix<D>
    where
        D: DimName,
        DefaultAllocator: Allocator<FloatingType, D, D>,
    {
        fn u_decompose(&self) -> __GenericSquareMatrix<D> {
            let (nrows, ncols) = self.shape();
            let mut u = __GenericSquareMatrix::<D>::zeros_generic(D::name(), D::name());
            u[(0, 0)] = self[(0, 0)].sqrt();
            (1..ncols).for_each(|j| u[(0, j)] = self[(0, j)] / u[(0, 0)]);
            (1..nrows).for_each(|i| {
                u[(i, i)] = (self[(i, i)]
                    - (0..=i - 1).map(|k| u[(k, i)].powi(2)).sum::<FloatingType>())
                .sqrt()
            });
            (1..nrows).for_each(|i| {
                (1..ncols).for_each(|j| {
                    if i < j {
                        u[(i, j)] = (self[(i, j)]
                            - (0..=i - 1)
                                .map(|k| u[(k, i)] * u[(k, j)])
                                .sum::<FloatingType>())
                            / u[(i, i)]
                    }
                })
            });
            u
        }
    }
    ///Uses the U Decomposition method
    pub fn solve_linear_system<D>(
        a: &__GenericSquareMatrix<D>,
        b: &__GenericVector<D>,
    ) -> __GenericVector<D>
    where
        D: DimName,
        DefaultAllocator: Allocator<FloatingType, D, D>,
        DefaultAllocator: Allocator<FloatingType, D>,
    {
        let u = a.u_decompose();
        let y = u
            .transpose()
            .try_inverse()
            .expect("Could not invert matrix")
            * b;
        let x = u.try_inverse().expect("Could not invert matrix") * y;
        x
    }
}

pub mod lab4 {

    use crate::{FloatingType, __SquareMatrix, __Vector};
    use nalgebra::{allocator::Allocator, Const, DefaultAllocator, DimMin, Storage, U1};

    trait InnerProduct {
        fn inner_product(&self, other: &Self) -> FloatingType;
    }

    impl<const N: usize, S> InnerProduct for __Vector<N, S>
    where
        S: Storage<FloatingType, Const<N>, U1>,
        DefaultAllocator: Allocator<FloatingType, Const<N>>, // For <D, U1> vector
        DefaultAllocator: Allocator<FloatingType, U1, Const<N>>, // For <U1, D> vector^T
    {
        fn inner_product(&self, other: &Self) -> FloatingType {
            (self.transpose() * other)[(0, 0)]
        }
    }

    trait Projection {
        type Output;
        fn project_on(&self, v: &Self) -> Self::Output;
    }

    impl<const N: usize, S> Projection for __Vector<N, S>
    where
        Self: InnerProduct,
        S: Storage<FloatingType, Const<N>, U1>,
        DefaultAllocator: Allocator<FloatingType, Const<N>>,
    {
        type Output = __Vector<N>;

        fn project_on(&self, u: &Self) -> Self::Output {
            u.inner_product(self) * u / u.inner_product(u)
        }
    }

    pub trait QRDescomposition
    where
        Self: Sized,
    {
        fn qr_decompose(&self) -> (Self, Self);
    }

    impl<const N: usize> QRDescomposition for __SquareMatrix<N>
    where
        Const<N>: DimMin<Const<N>, Output = Const<N>>,
        __Vector<N>: Projection + InnerProduct,
        DefaultAllocator: Allocator<FloatingType, Const<N>, Const<N>>, // For <N, N> matrix
        DefaultAllocator: Allocator<FloatingType, Const<N>>,           // For <N, 1> vector
        DefaultAllocator: Allocator<(usize, usize), Const<N>>,         // For determinant
    {
        /// Uses Gram-Schmidt process to calculate the QR decomposition
        fn qr_decompose(&self) -> (Self, Self) {
            assert_ne!(
                self.determinant(),
                0 as FloatingType,
                "Matrix provided has a rank smaller than it's dimension"
            );

            let mut q = __SquareMatrix::<N>::zeros();
            (0..N).for_each(|i| {
                q.set_column(
                    i,
                    &(self.column(i)
                        - match i {
                            0 => __Vector::<N>::zeros(),
                            _ => (0..=i - 1)
                                .map(|j| self.column(i).project_on(&q.column(j)))
                                .sum::<__Vector<N>>(),
                        }),
                )
            });
            q.column_iter_mut().for_each(|mut c| {
                c.normalize_mut();
            });

            let mut r = __SquareMatrix::<N>::zeros();
            (0..N).for_each(|i| {
                (0..N).for_each(|j| {
                    if i <= j {
                        r[(i, j)] = q.column(i).inner_product(&self.column(j))
                    }
                })
            });
            (q, r)
        }
    }

    ///Uses the QR Decomposition method
    pub fn solve_linear_system<const N: usize>(
        a: &__SquareMatrix<N>,
        b: &__Vector<N>,
    ) -> __Vector<N>
    where
        __SquareMatrix<N>: QRDescomposition,
        DefaultAllocator: Allocator<FloatingType, Const<N>, Const<N>>,
        DefaultAllocator: Allocator<FloatingType, Const<N>>,
    {
        let (q, r) = a.qr_decompose();
        let y = q.transpose() * b;
        let x = r.try_inverse().expect("Could not invert matrix") * y;
        x
    }
}

pub mod lab5 {
    use crate::{FloatingType, __SquareMatrix, __Vector};

    pub fn jacobi_method<const N: usize>(
        a: &__SquareMatrix<N>,
        b: &__Vector<N>,
        x0: &__Vector<N>,
        epsilon: FloatingType,
    ) -> __Vector<N> {
        let mut x_last = *x0;
        let mut x = __Vector::<N>::zeros();
        let mut k = 0;
        loop {
            (0..N).for_each(|i| {
                let sigma = (0..N)
                    .map(|j| if i != j { a[(i, j)] * x_last[j] } else { 0.0 })
                    .sum::<FloatingType>();
                x[i] = (b[i] - sigma) / a[(i, i)];
            });
            k += 1;
            println!(
                "x: {} diff to 0: {} | iter no.: {}",
                x,
                (a * x - b).norm(),
                k
            );
            if (a * x - b).norm() <= epsilon {
                break;
            } else {
                x_last = x
            }
            // if k == 4 {
            //     break;
            // }
        }
        x
    }
}
