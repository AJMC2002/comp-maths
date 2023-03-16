// ___  ________ _____ _   _______
// |  \/  |  _  /  ___| | / /_   _|
// | .  . | | | \ `--.| |/ /  | |
// | |\/| | | | |`--. \    \  | |
// | |  | \ \_/ /\__/ / |\  \_| |_
// \_|  |_/\___/\____/\_| \_/\___/

use std::fs::File;

/// This generates the CSV data of the Lagrange interpolation for Python to use ;)
fn main() {
    let file = File::open("../../data/input/data.json").unwrap();
    let json: serde_json::Value = serde_json::from_reader(file).unwrap();
    println!("{:?}", json["x_values"].as_array())
}

/// Evaluates f(x) = x^2+log(x) (log in base 10) at x
fn f(x: &f64) -> f64 {
    (*x) * (*x) + (*x).log10()
}

/// Evaluates the n-th lagrange polynomial at x
fn L(n: &u32, [a, b]: &[f64; 2], x: &f64) -> Result<f64, String> {
    if *a >= *b {
        return Err("B O I, a is strictly less than b.".to_string());
    }

    let x_0 = *a;
    let dx = (*b - *a) / (*n as f64);

    let mut l = 0.0_f64;
    for i in 0..*n {
        l += f(&(x_0 + (i as f64) * dx)) * Li(i, n, &x_0, &dx, x);
    }
    Ok(l)
}

/// Evaluates the i-th term for the n-th Lagrange polynomial at x
fn Li(i: u32, n: &u32, x_0: &f64, dx: &f64, x: &f64) -> f64 {
    let mut li = 1.0_f64;
    for j in 0..(*n) {
        if j != i {
            li *= *x - (*x_0 + (j as f64) * *dx);
            li /= (i - j) as f64 * *dx;
        }
    }
    li
}

/// Obtain absolute error between f and L
fn abs_err(f: &f64, L_n: &f64) -> f64 {
    (*f - *L_n).abs()
}

/// Obtain relative error between f and L
fn rel_err(f: &f64, L_n: &f64) -> f64 {
    abs_err(f, L_n) / f.abs() * 100.0_f64
}

/// Obtain the upper bound of the error
fn R(n: &u32, [a, b]: &[f64; 2], x: &f64) -> Result<f64, String> {
    if *a >= *b {
        return Err("B O I, a is strictly less than b.".to_string());
    }

    let mut r = match n_th_derivative(n, x) {
        Ok(it) => it,
        Err(err) => return Err(err),
    };
    for k in 1..=*n + 1 {
        r *= *b - *a;
        r /= k as f64;
    }
    Ok(r)
}

/// Gets the value of the n-th derivative of y = x^2+log(x) (log in base 10)
fn n_th_derivative(n: &u32, x: &f64) -> Result<f64, String> {
    if *n < 1 {
        return Err("Are you stupid? Only n >= 1.".to_string());
    }

    // Consider log(x) = ln(x)/ln(10)
    let mut result = 1.0_f64 / 10.0_f64.ln();
    // n-th derivative of ln(x)=(n-1)!(-1)^(n-1)/x^n
    // We get the absolute value
    if *n == 1 {
        result /= *x;
    } else {
        for k in 1..(*n) {
            result *= k as f64 / *x;
        }
    }
    // Add sign
    if *n % 2 == 0 {
        result *= -1.0_f64;
    }
    // Add n-th derivative of x^2
    match *n {
        1 => Ok(2.0_f64 * *x + result),
        2 => Ok(2.0_f64 + result),
        _ => Ok(result),
    }
}
