use integration::{factorial, Function};
use splines::{spline, spline_coeffs};
use tabled::{
    settings::{object::Segment, Format, Modify},
    Table, Tabled,
};

const X_VALUES: [f64; 4] = [0.53, 0.43, 0.86, 0.67];

fn main() {
    let fun = &mut Function::new(Box::new(|x| x.powi(2) + x.log10()), 0.4, 0.9);

    let mut tables: Vec<Vec<Row>> = (0..X_VALUES.len()).map(|_| Vec::new()).collect();

    for n in [3, 5].into_iter().chain((10..=100).step_by(10)) {
        fun.n = n;
        let spline_coeffs = spline_coeffs(fun);
        for (idx, &x) in X_VALUES.iter().enumerate() {
            let i = (x - fun.a()) / fun.h();
            let s = spline(
                spline_coeffs.row(i.floor() as usize).to_owned(),
                fun.x(i.floor()),
            );

            tables[idx].push(Row {
                n,
                Delta_f_n: (s(x) - fun.f(i)).abs(),
                delta_f_n: (s(x) - fun.f(i)).abs() / fun.f(i).abs() * 100.,
                R_n: fun.max_kth_der(fun.n as isize) / factorial(n as isize)
                    * (0..=n)
                        .filter_map(|j| match j != i.floor() as usize {
                            true => Some((i.floor() - j as f64) * fun.h()),
                            false => None,
                        })
                        .product::<f64>(),
            })
        }
    }

    for (x, table) in X_VALUES.into_iter().zip(tables.into_iter()) {
        let table_str = Table::new(table)
            .with(
                Modify::new(Segment::new(1.., 1..)).with(Format::content(|x| format!("{:.15}", x))),
            )
            .to_string();

        println!("*** x = {} ***", x);
        println!("{}\n", table_str);
    }
}

#[derive(Tabled, Debug)]
#[allow(non_snake_case)]
struct Row {
    n: usize,
    Delta_f_n: f64,
    delta_f_n: f64,
    R_n: f64,
}
