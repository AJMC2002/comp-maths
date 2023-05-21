#[cfg(test)]
pub mod tests {
    pub fn system3x3() {
        use ndarray::arr2;
        use tridiagonal_matrix::tridiagonal_matrix_algorithm;

        let matrix = arr2(&[[1, 2, 0, 3], [1, 1, 1, 4], [0, 2, 3, 13]]);
        let solution = tridiagonal_matrix_algorithm(&matrix);
        assert_eq!(solution[0], -1);
        assert_eq!(solution[1], 2);
        assert_eq!(solution[2], 3);
    }

    pub fn system4x4() {
        use ndarray::arr2;
        use tridiagonal_matrix::tridiagonal_matrix_algorithm;

        let matrix = arr2(&[
            [20, 2, 0, 0, 26],
            [1, 1, 1, 0, 9],
            [0, 2, 3, 1, 28],
            [0, 0, 1, 1, 12],
        ]);
        let solution = tridiagonal_matrix_algorithm(&matrix);
        assert_eq!(solution[0], 1);
        assert_eq!(solution[1], 3);
        assert_eq!(solution[2], 5);
        assert_eq!(solution[3], 7);
    }
}
