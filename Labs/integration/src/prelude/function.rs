use crate::factorial;

pub struct Function {
    f: Box<dyn Fn(f64) -> f64>,
    a: f64,
    b: f64,
    pub n: usize,
}

impl Function {
    pub fn new(f: Box<dyn Fn(f64) -> f64>, a: f64, b: f64) -> Self {
        Function { f, a, b, n: 0 }
    }

    pub fn a(&self) -> f64 {
        self.a
    }

    pub fn b(&self) -> f64 {
        self.b
    }

    pub fn h(&self) -> f64 {
        (self.b - self.a) / (self.n - 1) as f64
    }

    pub fn x(&self, i: f64) -> f64 {
        self.a + self.h() * i
    }

    pub fn f(&self, i: f64) -> f64 {
        (self.f)(self.x(i))
    }

    pub fn kth_der(&self, k: usize, i: f64) -> f64 {
        (2 - k as i64 + 1..=2).product::<i64>() as f64 * self.x(i).powi(2 - k as i32)
            + (-1_f64).powi(k as i32 - 1) * factorial(k - 1) * self.x(i).powi(-(k as i32))
                / 10_f64.ln()
    }

    pub fn max_kth_der(&self, k: usize) -> f64 {
        let num = 10_000;
        let step = (self.b - self.a) / (num - 1) as f64;
        (0..num)
            .map(|i| self.kth_der(k, i as f64 * step))
            .fold(0.0, |max, x| -> f64 { max.max(x.abs()) })
    }

    pub fn min_kth_der(&self, k: usize) -> f64 {
        let num = 10_000;
        let step = (self.b - self.a) / (num - 1) as f64;
        (0..num)
            .map(|i| self.kth_der(k, i as f64 * step))
            .fold(0.0, |min, x| -> f64 { min.min(x.abs()) })
    }

    #[allow(non_snake_case)]
    fn F(&self, x: f64) -> f64 {
        x.powi(3) / 3_f64 + x.log10() * x - x / 10_f64.ln()
    }

    #[allow(non_snake_case)]
    pub fn I(&self) -> f64 {
        self.F(self.b) - self.F(self.a)
    }
}
