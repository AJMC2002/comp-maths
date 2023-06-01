use tridiagonal_matrix::tridiagonal_matrix_algorithm;

use ndarray::{arr2, Array2};

fn main() {
    let matrix: Array2<f64> = arr2(&[
        [2., 2., 0., 0., 0., 0., 0., 0., 0., 4.],
        [-5., 1., 4., 0., 0., 0., 0., 0., 0., 0.],
        [0., 3., 7., -10., 0., 0., 0., 0., 0., 0.],
        [0., 0., -2., 2., 4., 0., 0., 0., 0., 4.],
        [0., 0., 0., -4., 10., -6., 0., 0., 0., 0.],
        [0., 0., 0., 0., -7., 0., 7., 0., 0., 0.],
        [0., 0., 0., 0., 0., -9., 12., -3., 0., 0.],
        [0., 0., 0., 0., 0., 0., -4., 5., 9., 10.],
        [0., 0., 0., 0., 0., 0., 0., -10., 1., -9.],
    ]);
    let xs = tridiagonal_matrix_algorithm(&matrix);
    print!("{:?}", xs);
}