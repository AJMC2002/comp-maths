use std::error::Error;

use integration::Function;
use splines::{spline, spline_coeffs};
use tabled::Tabled;

const X_VALUES: [f64; 4] = [0.53, 0.43, 0.86, 0.67];

fn main() -> Result<(), Box<dyn Error>> {
    let fun = &mut Function::new(Box::new(|x| x.powi(2) + x.log10()), 0.4, 0.9);
    fun.n = 3;

    let mut tables: Vec<Vec<Row>> = (0..X_VALUES.len()).map(|_| Vec::new()).collect();

    for n in [3, 5].into_iter().chain((10..=100).step_by(10)) {
        fun.n = n;
        let spline_coeffs = spline_coeffs(fun)?;
        println!("{}", spline_coeffs);
        for (idx, &x) in X_VALUES.iter().enumerate() {
            let i = ((x - fun.a()) / fun.h()).floor();
            println!("{} {}", i, (x - fun.a()) / fun.h());
            let s = spline(spline_coeffs.row(i as usize).to_owned(), fun.x(i));

            tables[idx].push(Row {
                n,
                Delta_f_n: (s(x) - fun.f((x - fun.a()) / fun.h())).abs(),
                delta_f_n: (s(x) - fun.f((x - fun.a()) / fun.h())).abs()
                    / fun.f((x - fun.a()) / fun.h()).abs()
                    * 100.,
            })
        }
    }
    print!("{:?}", tables);

    Ok(())
}

#[derive(Tabled, Debug)]
#[allow(non_snake_case)]
struct Row {
    n: usize,
    Delta_f_n: f64,
    delta_f_n: f64,
}
