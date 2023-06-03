use crate::{Function, IntegralResult};

pub fn trapezium(fun: &Function) -> IntegralResult {
    IntegralResult::new(
        fun.h() / 2.
            * (0..=fun.n)
                .map(|i| fun.f(i as f64) + fun.f((i + 1) as f64))
                .sum::<f64>(),
        fun.max_kth_der(2) * fun.h().powi(2) * (fun.b() - fun.a()) / 12.0,
    )
}
