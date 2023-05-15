pub struct IntegralResult {
    pub val: f64,
    pub err: f64,
}

impl IntegralResult {
    pub fn new(val: f64, err: f64) -> Self {
        IntegralResult { val, err }
    }
}
