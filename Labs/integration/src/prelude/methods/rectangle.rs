use crate::Function;
use crate::IntegralResult;

pub fn rect_left(fun: &Function) -> IntegralResult {
    IntegralResult::new(
        fun.h() * (0..fun.n).map(|i| fun.f(i as f64)).sum::<f64>(),
        fun.max_kth_der(1) * fun.h() * (fun.b() - fun.a()) / 2.0,
    )
}

pub fn rect_center(fun: &Function) -> IntegralResult {
    IntegralResult::new(
        fun.h() * (0..fun.n).map(|i| fun.f(i as f64 + 0.5)).sum::<f64>(),
        fun.max_kth_der(2) * fun.h().powi(2) * (fun.b() - fun.a()) / 24.0,
    )
}

pub fn rect_right(fun: &Function) -> IntegralResult {
    IntegralResult::new(
        fun.h() * (0..fun.n).map(|i| fun.f((i + 1) as f64)).sum::<f64>(),
        fun.max_kth_der(1) * fun.h() * (fun.b() - fun.a()) / 2.0,
    )
}
