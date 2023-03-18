// ___  ________ _____ _   _______
// |  \/  |  _  /  ___| | / /_   _|
// | .  . | | | \ `--.| |/ /  | |
// | |\/| | | | |`--. \    \  | |
// | |  | \ \_/ /\__/ / |\  \_| |_
// \_|  |_/\___/\____/\_| \_/\___/

use csv::Writer;
use std::{
    error::Error,
    fs::{self, File},
};

const DATA_DIR: &str = "data"; // if running from ./rust then use "../data"

/// This generates the CSV data of the Lagrange interpolation for Python to use ;)
fn main() -> Result<(), Box<dyn Error>> {
    let dirin_path = format!("{}/input", DATA_DIR);
    let fin_path = format!("{}/data.json", dirin_path);
    let fin = File::open(fin_path).expect("File not found.");

    let json: serde_json::Value = serde_json::from_reader(fin)?;

    let n_values_json = json.get("n_values").unwrap();
    let x_interval_json = json.get("x_interval").unwrap();
    let x_values_json = json.get("x_values").unwrap();

    let n_values: Vec<u32> = serde_json::from_value(n_values_json.clone())?;
    let x_interval: [f64; 2] = serde_json::from_value(x_interval_json.clone())?;
    let x_values: Vec<f64> = serde_json::from_value(x_values_json.clone())?;

    for x in x_values.iter() {
        let dirout_path = format!("{}/output", DATA_DIR);
        fs::create_dir_all(&dirout_path)?;
        let fout_path = format!("{}/x_{}.csv", dirout_path, *x);
        let fout = File::create(fout_path).expect("Failed to create file.");

        let mut writer = Writer::from_writer(fout);

        // CSV header
        writer.write_record(&["n", "AbsErr", "RelErr", "R"])?;

        for n in n_values.iter() {
            let f = f(x);
            let l_n = L(n, &x_interval, x)?;
            let abs_err_n = abs_err(&f, &l_n);
            let rel_err_n = rel_err(&f, &l_n);
            let r_n = R(n, &x_interval, x)?;

            writer.write_record(&[
                (*n).to_string(),
                abs_err_n.to_string(),
                rel_err_n.to_string(),
                r_n.to_string(),
            ])?;
        }

        // Make sure this iteration is written :3
        writer.flush()?;
    }

    Ok(())
}

/// Evaluates f(x) = x^2+log(x) (log in base 10) at x
fn f(x: &f64) -> f64 {
    (*x) * (*x) + (*x).log10()
}

/// Evaluates the n-th lagrange polynomial at x
#[allow(non_snake_case)]
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
#[allow(non_snake_case)]
fn Li(i: u32, n: &u32, x_0: &f64, dx: &f64, x: &f64) -> f64 {
    let mut li = 1.0_f64;
    for j in 0..(*n) {
        if j != i {
            li *= *x - (*x_0 + (j as f64) * *dx);
            li /= (i as i32 - j as i32) as f64 * *dx;
        }
    }
    li
}

/// Obtain absolute error between f and L
fn abs_err(f: &f64, l_n: &f64) -> f64 {
    (*f - *l_n).abs()
}

/// Obtain relative error between f and L
fn rel_err(f: &f64, l_n: &f64) -> f64 {
    abs_err(f, l_n) / f.abs() * 100.0_f64
}

/// Obtain the upper bound of the error
#[allow(non_snake_case)]
fn R(n: &u32, [a, b]: &[f64; 2], x: &f64) -> Result<f64, String> {
    if *a >= *b {
        return Err("B O I, 'a' is strictly less than 'b'.".to_string());
    }

    let mut r = n_th_derivative(*n + 1, x)?.abs();

    for k in 1..=*n + 1 {
        r *= *b - *a;
        r /= k as f64;
    }

    Ok(r)
}

/// Gets the value of the n-th derivative of y = x^2+log(x) (log in base 10)
fn n_th_derivative(n: u32, x: &f64) -> Result<f64, String> {
    if n < 1 {
        return Err("Are you stupid? Only n >= 1.".to_string());
    }

    // Consider log(x) = ln(x)/ln(10)
    let mut result = 1.0_f64 / 10.0_f64.ln();
    // n-th derivative of ln(x)=(n-1)!(-1)^(n-1)/x^n
    result /= *x;
    for k in 1..n {
        result *= k as f64 / *x;
    }
    // Add sign
    if n % 2 == 0 {
        result *= -1.0_f64;
    }
    // Add n-th derivative of x^2
    match n {
        1 => Ok(2.0_f64 * *x + result),
        2 => Ok(2.0_f64 + result),
        _ => Ok(result),
    }
}
