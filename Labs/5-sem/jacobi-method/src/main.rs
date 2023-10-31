use nalgebra::{matrix, vector};
use utils::{lab5::jacobi_method, FloatingType};

const EPSILON: FloatingType = 0.0001;

fn main() {
    jacobi_method(
        &matrix![10.,2.,1.; 1.,10.,2.; 1.,1.,10.],
        &vector![10., 12., 8.],
        &vector![0., 0., 0.],
        EPSILON,
    );
}
