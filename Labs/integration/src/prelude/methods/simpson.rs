use crate::{Function, IntegralResult};

pub fn simpson(fun: &Function) -> IntegralResult {
    IntegralResult::new(
        fun.h() / 6.
            * (0..fun.n)
                .step_by(2)
                .map(|i| fun.f(i as f64) + 4. * fun.f(i as f64 + 0.5) + fun.f((i + 1) as f64))
                .sum::<f64>(),
        fun.max_kth_der(4) * fun.h().powi(4) * (fun.b() - fun.a()) / 180.0,
    )
}
