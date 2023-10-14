static K: isize = 2;
static N: isize = 3;
static M: isize = 3;

static A: f64 = 0.4;
static B: f64 = 0.9;
static H: f64 = (B - A) / N as f64;

fn factorial(n: isize) -> f64 {
    (1..=n).product::<isize>() as f64
}

fn f(i: isize) -> f64 {
    let x = A + i as f64 * H;
    x.powi(2) + x.log10()
}

fn f_nth_der(n: isize, i: isize) -> f64 {
    let x = A + i as f64 * H;
    (2 - n + 1..=2).product::<isize>() as f64 * x.powi(2 - n as i32)
        + (-1_f64).powi(n as i32 - 1) * factorial(n - 1) * x.powi(-n as i32) / 10_f64.ln()
}

/// Only for nodes
///
/// `m` is the node's index
/// for which we're calculating ω'
fn omega_1st_der(n: isize, i: isize) -> f64 {
    (0..=n)
        .filter_map(|j| match j != i {
            true => Some((i - j) as f64 * H),
            false => None,
        })
        .product()
}

/// Only for nodes  
///
/// `m` is the node's index
/// for which we're calculating ω''
fn omega_2nd_der(n: isize, i: isize) -> f64 {
    (0..=n)
        .map(|j| -> f64 {
            (0..=n)
                .filter_map(|k| match k != j && k != i {
                    true => Some((i - k) as f64 * H),
                    false => None,
                })
                .product()
        })
        .sum()
}

fn lagrange_2nd_der(n: isize, i: isize) -> f64 {
    (0..=n)
        .map(|j| -> f64 {
            f(j) / omega_1st_der(n, j)
                * (0..=n)
                    .filter_map(|k| -> Option<f64> {
                        match k != j {
                            true => Some(
                                (0..=n)
                                    .filter_map(|l| -> Option<f64> {
                                        match l != j && l != k {
                                            true => Some(
                                                (0..=n)
                                                    .filter_map(|m| -> Option<f64> {
                                                        match m != l && m != k && m != j {
                                                            true => Some((i - m) as f64 * H),
                                                            false => None,
                                                        }
                                                    })
                                                    .product(),
                                            ),
                                            false => None,
                                        }
                                    })
                                    .sum(),
                            ),
                            false => None,
                        }
                    })
                    .sum::<f64>()
        })
        .sum()
}

#[derive(Debug)]
struct RBounds {
    min: f64,
    max: f64,
}

fn err_2nd_der(n: isize, i: isize) -> RBounds {
    let base1 = omega_2nd_der(n, i) / factorial(n + 1);
    let base2 = 2.0 * omega_1st_der(n, i) / factorial(n + 2);

    let terms1 = [base1 * f_nth_der(n + 1, 0), base1 * f_nth_der(n + 1, n)];
    let terms2 = [base2 * f_nth_der(n + 2, 0), base2 * f_nth_der(n + 2, n)];

    let term1_min;
    let term1_max;
    let term2_min;
    let term2_max;
    if terms1[0] < terms1[1] {
        [term1_min, term1_max] = terms1;
    } else {
        [term1_max, term1_min] = terms1;
    }
    if terms2[0] < terms2[1] {
        [term2_min, term2_max] = terms2;
    } else {
        [term2_max, term2_min] = terms2;
    }

    RBounds {
        min: term1_min + term2_min,
        max: term1_max + term2_max,
    }
}

fn main() {
    let real = f_nth_der(2, M);
    let approx = lagrange_2nd_der(N, M);
    let err = real - approx;
    let r = err_2nd_der(N, M);

    println!("f^({})(x{}) = {}", K, M, real);
    println!("L^({})(x{}) = {}", K, M, approx);
    println!("R_{{{},{}}}(x{}) = {}", N, K, M, real - approx);
    println!("Bounds = {:?}", r);
    println!("Is R in bounds? {}", r.min < err && err < r.max);
}
