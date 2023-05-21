use num_traits::{Num, Zero};
use std::collections::HashSet;

use ndarray::{Array1, Array2};

fn is_valid<T>(matrix: &ndarray::Array2<T>) -> bool
where
    T: Num + Zero + Copy,
{
    if matrix.shape()[0] != matrix.shape()[1] - 1 {
        return false;
    }
    for (i, row) in matrix.rows().into_iter().enumerate() {
        let js = match i {
            0 => HashSet::from([i, i + 1]),
            x if x == matrix.shape()[0] - 1 => HashSet::from([i - 1, i]),
            _ => HashSet::from([i - 1, i, i + 1]),
        };
        for (j, x_ij) in row.iter().enumerate() {
            if j < matrix.shape()[0] && !js.contains(&j) && !x_ij.is_zero() {
                return false;
            }
        }
    }
    true
}

#[derive(Debug)]
#[allow(non_snake_case)]
struct Coefficients<T>
where
    T: Num + Zero + Copy,
{
    A: Array1<T>,
    B: Array1<T>,
    C: Array1<T>,
    D: Array1<T>,
}

#[allow(non_snake_case)]
fn get_coeffs<T>(matrix: &ndarray::Array2<T>) -> Coefficients<T>
where
    T: Num + Zero + Copy,
{
    let mut coeffs: Array2<T> = Array2::zeros((matrix.shape()[0], 4));

    for (i, row) in matrix.rows().into_iter().enumerate() {
        coeffs[(i, 1)] = row[i]; // b
        if i > 0 {
            coeffs[(i, 0)] = row[i - 1]; // a
        }
        if i < matrix.shape()[0] - 1 {
            coeffs[(i, 2)] = row[i + 1]; // c
        }
        coeffs[(i, 3)] = row[matrix.shape()[1] - 1]; // d
    }

    let mut coeffs_cols = coeffs
        .columns()
        .into_iter()
        .map(|c| c.to_owned())
        .collect::<Vec<_>>();

    let D: Array1<T> = coeffs_cols.pop().unwrap();
    let C: Array1<T> = coeffs_cols.pop().unwrap();
    let B: Array1<T> = coeffs_cols.pop().unwrap();
    let A: Array1<T> = coeffs_cols.pop().unwrap();

    Coefficients { A, B, C, D }
}

#[allow(non_snake_case)]
pub fn tridiagonal_matrix_algorithm<T>(matrix: &ndarray::Array2<T>) -> Array1<T>
where
    T: Num + Zero + Copy,
{
    assert!(is_valid(matrix));
    let coeffs = get_coeffs(matrix);
    let n = matrix.shape()[0];

    let mut C_prime: Array1<T> = Array1::zeros(n);
    let mut D_prime: Array1<T> = Array1::zeros(n);

    C_prime[0] = coeffs.C[0] / coeffs.B[0];
    D_prime[0] = coeffs.D[0] / coeffs.B[0];
    for i in 1..n {
        C_prime[i] = coeffs.C[i] / (coeffs.B[i] - coeffs.A[i] * C_prime[i - 1]);
        D_prime[i] = (coeffs.D[i] - coeffs.A[i] * D_prime[i - 1])
            / (coeffs.B[i] - coeffs.A[i] * C_prime[i - 1]);
    }

    let mut X: Array1<T> = Array1::zeros(n);

    X[n - 1] = D_prime[n - 1];
    for i in (0..n - 1).rev() {
        X[i] = D_prime[i] - C_prime[i] * X[i + 1]
    }

    X
}
