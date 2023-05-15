use crate::{Function, IntegralResult};
use rand::Rng;

fn value(fun: &Function, i_sample: &[f64]) -> f64 {
    fun.h() * i_sample.iter().map(|&i| fun.f(i)).sum::<f64>()
}

fn error(fun: &Function, i_sample: &[f64]) -> f64 {
    let variance = (fun.b() - fun.a())
        * (i_sample.iter().map(|&i| fun.f(i).powi(2)).sum::<f64>() / fun.n as f64
            - (i_sample.iter().map(|&i| fun.f(i)).sum::<f64>() / fun.n as f64).powi(2));
    (variance / fun.n as f64).sqrt()
}

pub fn monte_carlo(fun: &Function) -> IntegralResult {
    let mut rng = rand::thread_rng();
    let i_sample: Vec<f64> = (0..fun.n)
        .map(|_| rng.gen_range(0.0..=fun.n as f64))
        .collect();

    IntegralResult::new(value(fun, &i_sample), error(fun, &i_sample))
}
