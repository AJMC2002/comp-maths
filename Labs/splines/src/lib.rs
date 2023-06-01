use std::error::Error;

use integration::Function;
use ndarray::{Array1, Array2};
use tridiagonal_matrix::tridiagonal_matrix_algorithm;

fn m_coeffs(fun: &mut Function) -> Result<Array1<f64>, Box<dyn Error>> {
    //Calculate mu and lambda vals beforehand
    let mus: Array1<f64> = Array1::from_iter((1..fun.n).map(|_| fun.h() / (fun.h() + fun.h())));
    let lambdas: Array1<f64> = 1. - &mus;

    //Initialize the matrix for the system to find all m_is
    let mut m_system: Array2<f64> = Array2::zeros((fun.n, fun.n + 1));
    //Comply with the first restriction S'(x0)=f'(x0) and S'(xN)=f'(xN)
    m_system[[0, 0]] = 1.;
    m_system[[0, fun.n]] = fun.kth_der(1, 0.0);
    m_system[[fun.n - 1, fun.n - 1]] = 1.;
    m_system[[fun.n - 1, fun.n]] = fun.kth_der(1, fun.n as f64);
    //Add the other coefficients (the mu & lambda arrays start from i=1 so their index is i-1)
    for i in 1..fun.n - 1 {
        m_system[[i, i - 1]] = mus[i - 1];
        m_system[[i, i]] = 2.;
        m_system[[i, i + 1]] = lambdas[i - 1];
        m_system[[i, fun.n]] = 3.
            * (lambdas[i - 1] * (fun.f((i + 1) as f64) - fun.f(i as f64)) / fun.h()
                + mus[i - 1] * (fun.f(i as f64) - fun.f((i - 1) as f64)) / fun.h())
    }

    //Return the solution vector
    Ok(tridiagonal_matrix_algorithm(&m_system))
}

pub fn spline_coeffs(fun: &mut Function) -> Result<Array2<f64>, Box<dyn Error>> {
    //Initialize array to store all coeffs
    let mut spline_coeffs: Array2<f64> = Array2::zeros((fun.n - 1, 4));

    //Get the m coeffs function
    let ms = m_coeffs(fun)?;

    //Store y_i, m_i and calculate a_i and b_i
    for i in 0..fun.n - 1 {
        spline_coeffs[[i, 0]] = fun.f(i as f64);
        spline_coeffs[[i, 1]] = ms[i];
        spline_coeffs[[i, 2]] = 6. / fun.h()
            * ((fun.f((i + 1) as f64) - fun.f(i as f64)) / fun.h() - (ms[i + 1] + 2. * ms[i]) / 3.);
        spline_coeffs[[i, 3]] = 12. / fun.h().powi(2)
            * ((ms[i + 1] + ms[i]) / 2. - (fun.f((i + 1) as f64) - fun.f(i as f64)) / fun.h());
    }

    Ok(spline_coeffs)
}

pub fn spline(coeffs: Array1<f64>, x_i: f64) -> Box<dyn Fn(f64) -> f64> {
    let y = coeffs[0];
    let m = coeffs[1];
    let a = coeffs[2];
    let b = coeffs[3];
    Box::new(move |x| y + m * (x - x_i) + a * (x - x_i).powi(2) / 2. + b * (x - x_i).powi(3) / 6.)
}
