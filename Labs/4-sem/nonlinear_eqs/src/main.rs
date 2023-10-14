use std::rc::Rc;

static STEP: f64 = 1. / 1_000_000.;

fn main() {
    let fun = Function::new(
        Box::new(|x: f64| -> f64 { (-x).exp() + x.powi(3) - 3. }),
        Some(Rc::new(Function::new(
            Box::new(|x: f64| -> f64 { -(-x).exp() + 3. * x.powi(2) }),
            None,
        ))),
    );

    println!("{}", fun.f(0. + STEP));
     println!("{}", fun.df(0. + STEP));
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
