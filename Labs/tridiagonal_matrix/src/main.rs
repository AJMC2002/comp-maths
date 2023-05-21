use ndarray::arr2;
use tridiagonal_matrix::tridiagonal_matrix_algorithm;

fn main() {
    let matrix = arr2(&[[1, 2, 0, 3], [1, 1, 1, 4], [0, 2, 3, 13]]);
    println!("Augmented Matrix:");
    println!("{:?}", matrix);
    println!("\n");
    println!("Solution Vector:");
    println!("{:?}", tridiagonal_matrix_algorithm(&matrix));
}
