use cli_table::{Style, Table, WithTitle};
use matrix_norm::{vandermonde_matrix, Cond, FloatingType, Norm};
use nalgebra::{Matrix2, SMatrix, SVector};

const N: usize = 25;

fn main() {
    //Part 1
    {
        let matrices = (0..5)
            .map(|k| {
                let a = SMatrix::<FloatingType, N, N>::new_random();
                NormData {
                    k: k + 1,
                    norm: a.custom_norm(),
                    cond: a.cond(),
                }
            })
            .collect::<Vec<NormData>>()
            .with_title()
            .bold(false);
        println!("{}", matrices.display().unwrap());
    }

    //Part 2
    {
        let (x0, xn) = (0.4, 1.3);

        // vandermonde * coeffs = y
        let vandermonde_matrix = vandermonde_matrix(x0, xn, N);
        let real_coeffs = SVector::<FloatingType, N>::from_element(1.);
        let y = &vandermonde_matrix * real_coeffs;
        let calc_coeffs = vandermonde_matrix
            .clone()
            .try_inverse()
            .expect("Couldn't get inverse matrix.")
            * y;
        println!("cond(Vandermonde) = {}", vandermonde_matrix.cond());
        println!("||calc coeffs|| = {}", calc_coeffs.custom_norm());
        println!("||real coeffs|| = {}", real_coeffs.custom_norm())
    }
}

#[derive(Table)]
struct NormData {
    k: usize,
    #[table(title = "Норма матрицы ||A||")]
    norm: FloatingType,
    #[table(title = "cond(A)")]
    cond: FloatingType,
}
