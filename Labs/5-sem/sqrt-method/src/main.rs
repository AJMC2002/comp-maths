use nalgebra::{matrix, vector};
use utils::lab2::test_accuracy;
use utils::lab3::solve_linear_system;

fn main() {
    do_tests();
    solve_systems();
}

//Part 1
fn do_tests() {
    test_accuracy(
        solve_linear_system(
            &matrix![
                81.,-45.,45.;
                -45.,50.,-15.;
                45.,-15.,38.
            ],
            &vector![531., -460., 193.],
        ),
        vector![6., -5., -4.],
    );
    test_accuracy(
        solve_linear_system(
            &matrix![
                6.25,-1.,0.5;
                -1.,5.,2.12;
                0.5,2.12,3.6
            ],
            &vector![7.5, -8.68, -0.24],
        ),
        vector![0.8, -2., 1.],
    );
    test_accuracy(
        solve_linear_system(
            &matrix![
                1.,3.,-2.,0.,-2.;
                3.,4.,-5.,1.,-3.;
                -2.,-5.,3.,-2.,2.;
                0.,1.,-2.,5.,3.;
                -2.,-3.,2.,3.,4.
            ],
            &vector![0.5, 5.4, 5.0, 7.5, 3.3],
        ),
        vector![-6.0978, -2.2016, -6.8011, -8.8996, 0.1998],
    );
    test_accuracy(
        solve_linear_system(
            &matrix![
                1.,2.,4.;
                2.,13.,23.;
                4.,23.,77.
            ],
            &vector![10., 50., 150.],
        ),
        vector![2.22, 0.55, 1.67],
    );
    println!("\n* * * * * *\n");
}

//Part 2
fn solve_systems() {
    let a = matrix![
        5.8,0.3,-0.2;
        0.3,4.0,-0.7;
        -0.2,-0.7,6.7
    ];
    let b = vector![3.1, -1.7, 1.1];
    let x = solve_linear_system(&a, &b);
    println!("a = {}\nb = {}\nx = {}", a, b, x);

    let a = matrix![
        4.12,0.42,1.34,0.88;
        0.42,3.95,1.87,0.43;
        1.34,1.87,3.20,0.31;
        0.88,0.43,0.31,5.17
    ];
    let b = vector![11.17, 0.1115, 9.909, 9.349];
    let x = solve_linear_system(&a, &b);
    println!("a = {}\nb = {}\nx = {}", a, b, x);
}
