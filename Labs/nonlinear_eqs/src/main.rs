use std::rc::Rc;

static STEP: f64 = 1. / 1_000_000.;

fn main() {
    println!("Hello, world!");
}

struct Function {
    function: Box<dyn Fn(f64) -> f64>,
    derivative: Option<Rc<Function>>,
}

impl Function {
    fn new(function: Box<dyn Fn(f64) -> f64>, derivative: Option<Rc<Function>>) -> Self {
        Self {
            function,
            derivative,
        }
    }

    fn f(&self, x: f64) -> f64 {
        (self.function)(x)
    }

    fn df(&self, x: f64) -> f64 {
        match self.derivative.clone() {
            Some(fun) => fun.f(x),
            None => 0.0,
        }
    }
}
