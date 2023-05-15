pub fn factorial(n: isize) -> f64 {
    (1..=n).product::<isize>() as f64
}
