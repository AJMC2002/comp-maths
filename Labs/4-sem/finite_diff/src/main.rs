use std::error::Error;
use std::fs::File;
use std::io::Write;

/// Using forward diff
fn finite_diffs(f: fn(f64) -> f64, a: f64, n: usize, h: f64) -> Vec<Vec<f64>> {
    let mut table = vec![vec![0.0; n + 1]; n + 1];

    for i in 0..=n {
        table[i][0] = f(a + i as f64 * h);
    }

    for j in 1..=n {
        for i in 0..n - j + 1 {
            table[i][j] = table[i + 1][j - 1] - table[i][j - 1];
        }
    }

    table
}

fn factorial(n: usize) -> usize {
    match n {
        0 | 1 => 1,
        _ => (1..=n).product(),
    }
}

fn newton_1st(table: &Vec<Vec<f64>>, t: f64) -> f64 {
    let n = table.len();
    let mut l_n_x = table[0][n - 1] / factorial(n - 1) as f64;
    for i in (0..n - 1).rev() {
        l_n_x = l_n_x * (t - i as f64) + table[0][i] / factorial(i) as f64
    }
    l_n_x
}

fn newton_1st_err(a: f64, b: f64, n: usize, h: f64, t: f64) -> [f64; 2] {
    let mut r = (-1_f64).powi(n as i32) / (10_f64.ln() * (n + 1) as f64);
    for i in 0..=n {
        r *= (t - i as f64) * h;
    }
    let mut res = [r / a.powi(n as i32 + 1), r / b.powi(n as i32 + 1)];
    res = res.map(|e| e.abs());
    res.sort_by(|e1, e2| e1.partial_cmp(e2).unwrap());
    res
}

fn newton_2nd(table: &Vec<Vec<f64>>, t: f64) -> f64 {
    let n = table.len();
    let mut l_n_x = table[0][n - 1] / factorial(n - 1) as f64;
    for i in (0..n - 1).rev() {
        l_n_x = l_n_x * (t + i as f64) + table[n - 1 - i][i] / factorial(i) as f64
    }
    l_n_x
}

fn newton_2nd_err(a: f64, b: f64, n: usize, h: f64, t: f64) -> [f64; 2] {
    let mut r = (-1_f64).powi(n as i32) / (10_f64.ln() * (n + 1) as f64);
    for i in 0..=n {
        r *= (t + i as f64) * h;
    }
    let mut res = [r / a.powi(n as i32 + 1), r / b.powi(n as i32 + 1)];
    res = res.map(|e| e.abs());
    res.sort_by(|e1, e2| e1.partial_cmp(e2).unwrap());
    res
}

fn gauss_1st(table: &Vec<Vec<f64>>, t: f64, idx: usize) -> f64 {
    let n = table.len() - 1;
    let mut l_n_x = table[idx][0] + t * table[idx][1];
    let mut coeff = t;
    for i in 2..=n {
        coeff *= (t + (-1_f64).powi(i as i32 + 1) * (i / 2) as f64) / i as f64;
        l_n_x += coeff * table[idx - i / 2][i];
    }
    l_n_x
}

fn gauss_1st_err(a: f64, b: f64, n: usize, h: f64, t: f64) -> [f64; 2] {
    let mut r = (-1_f64).powi(n as i32) / (10_f64.ln() * (n + 1) as f64);
    for i in 0..=n {
        r *= (t + (-1_f64).powi(i as i32 + 1) * i as f64) * h;
    }

    let mut res = [r / a.powi(n as i32 + 1), r / b.powi(n as i32 + 1)];
    res = res.map(|e| e.abs());
    res.sort_by(|e1, e2| e1.partial_cmp(e2).unwrap());
    res
}

fn print_result(x: f64, f_x: f64, l_n: f64, err: f64, r: [f64; 2]) {
    println!("x = {}", x);
    println!("f(x) = {:.5} and L(x) = {:.5}", f_x, l_n);
    println!("Δ = {:.5e} is between {:.5e} and {:.5e}?", err, r[0], r[1]);
    if err > r[0] && err < r[1] {
        println!("Yes")
    } else {
        println!("No")
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let f = |x: f64| -> f64 { x.powi(2) + x.log10() };
    let a = 0.4;
    let b = 0.9;
    let n = 12;
    let h = (b - a) / n as f64;
    let table = finite_diffs(f, a, n, h);

    // Make table
    {
        let mut file = File::create("table.txt")?;
        for row in &table {
            for col in row {
                write!(file, "{:.1e}\t", col)?;
            }
            writeln!(file)?;
        }
    }

    // x**
    {
        let x = 0.43;
        let t = (x - a) / h;
        let f_x = f(x);
        let l_n = newton_1st(&table, t);
        let err = (f_x - l_n).abs();
        let r = newton_1st_err(a, b, n, h, t);
        print_result(x, f_x, l_n, err, r);
        println!("");
    }
    // x***
    {
        let x = 0.86;
        let t = (x - b) / h;
        let f_x = f(x);
        let l_n = newton_2nd(&table, t);
        let err = (f_x - l_n).abs();
        let r = newton_2nd_err(a, b, n, h, t);
        print_result(x, f_x, l_n, err, r);
        println!("");
    }
    // x****
    {
        let x = 0.67;
        let t = (x - (a + 6.0 * h)) / h;
        let f_x = f(x);
        let l_n = gauss_1st(&table, t, 6);
        let err = (f_x - l_n).abs();
        let r = gauss_1st_err(a, b, n, h, t);
        print_result(x, f_x, l_n, err, r)
    }

    Ok(())
}
