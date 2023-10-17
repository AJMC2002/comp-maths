use nalgebra::{
    allocator::Allocator, matrix, vector, DefaultAllocator, DimName, Owned, SquareMatrix, Vector,
    U1,
};
use utils::{lab2::solve_linear_system, FloatingType};

#[allow(clippy::approx_constant)]
fn main() {
    do_tests();

    let a = matrix![
    13.14,-2.12,1.17;
    -2.12,6.3,-2.45;
    1.17,-2.45,4.6
    ];
    let b = vector![1.27, 2.13, 3.14];
    let x = solve_linear_system(&a, &b);
    println!("a = {}\nb = {}\nx = {}", a, b, x);

    let a = matrix![
    4.31,0.26,0.61,0.27;
    0.26,2.32,0.18,0.34;
    0.61,0.18,3.20,0.31;
    0.27,0.34,0.31,5.17
    ];
    let b = vector![1.02, 1.00, 1.34, 1.27];
    let x = solve_linear_system(&a, &b);
    println!("a = {}\nb = {}\nx = {}", a, b, x);
}

fn do_tests() {
    test_accuracy(
        matrix![
            2.1,-4.5,-2.0;
            3.0,2.5,4.3;
            -6.0,3.5,2.5
        ],
        vector![19.07, 3.21, -18.25],
        vector![1.34025, -4.75798, 2.5771],
    );
    test_accuracy(
        matrix![
            5.,-1.,5.;
            -3.,6.,2.;
            10.,-7.,0.
        ],
        vector![3.2, 5.4, -1.2],
        vector![0.7297, 1.2138, 0.1531],
    );
    test_accuracy(
        matrix![
            5.,2.,3.;
            1.,6.,1.;
            3.,-4.,-2.
        ],
        vector![3., 5., 8.],
        vector![2., 1., -3.],
    );
    test_accuracy(
        matrix![
            1.,2.,1.,4.;
            2.,0.,4.,3.;
            4.,2.,2.,1.;
            -3.,1.,3.,2.
        ],
        vector![13., 28., 20., 6.],
        vector![3., -1., 4., 2.],
    );
    test_accuracy(
        matrix![
            2.,1.,3.;
            11.,7.,5.;
            9.,8.,4.
        ],
        vector![1., -6., -5.],
        vector![-1., 0., 1.],
    );
    println!("\n* * * * * *\n");
}

fn test_accuracy<D>(
    a: SquareMatrix<FloatingType, D, Owned<FloatingType, D, D>>,
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
