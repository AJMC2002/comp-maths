pub fn factorial(n: usize) -> f64 {
    (1..=n).product::<usize>() as f64
}
