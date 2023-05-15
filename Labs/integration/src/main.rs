use integration::prelude::*;
use strum::IntoEnumIterator;
use tabled::{
    settings::{
        object::{Rows, Segment},
        Format, Modify,
    },
    Table, Tabled,
};

fn main() {
    let fun = &mut Function::new(Box::new(|x| x.powi(2) + x.log10()), 0.4, 0.9);

    section1(fun);

    println!("\n");

    section2(fun);
}

#[derive(Tabled)]
#[allow(non_snake_case)]
struct Table1Row {
    j: u32,
    n: usize,
    I: f64,
    Delta_I: f64,
    delta_I: f64,
    R: f64,
    ratio: f64,
}

fn section1(fun: &mut Function) {
    let j_range = 1..16;

    let integ_data: Vec<IntegralResult> = j_range
        .clone()
        .map(|j| -> IntegralResult {
            fun.n = 2_usize.pow(j);
            monte_carlo(fun)
        })
        .collect();

    #[allow(non_snake_case)]
    let records: Vec<Table1Row> = j_range
        .map(|j| {
            let approx = integ_data.get(j as usize - 1).unwrap();
            let Delta_I = (fun.I() - approx.val).abs();
            let delta_I = Delta_I / fun.I() * 100.0;
            let mut ratio = 0.0;
            if j < 15 {
                let next_approx = integ_data.get(j as usize).unwrap();
                ratio = Delta_I / (fun.I() - next_approx.val).abs();
            }

            Table1Row {
                j,
                n: 2_usize.pow(j),
                I: approx.val,
                Delta_I,
                delta_I,
                R: approx.err,
                ratio,
            }
        })
        .collect();

    let table = Table::new(records)
        .with(Modify::new(Segment::new(1.., 2..)).with(Format::content(|x| format!("{:.15}", x))))
        .to_string();

    println!("*** Section 1 ***\n");
    println!("Using: Monte Carlo method");
    println!("{}", table);
}

#[derive(Tabled)]
#[allow(non_snake_case)]
struct Table2Row {
    Method: String,
    I: f64,
    Delta_I: f64,
    delta_I: f64,
    R: f64,
}

fn section2(fun: &mut Function) {
    fun.n = 10000;

    let records: Vec<Table2Row> = Methods::iter()
        .map(|m| {
            let res = m.method()(fun);
            Table2Row {
                Method: m.name(),
                I: res.val,
                Delta_I: (fun.I() - res.val).abs(),
                delta_I: (fun.I() - res.val).abs() / fun.I() * 100.0,
                R: res.err,
            }
        })
        .collect();

    let table = Table::new(records)
        .with(Modify::new(Rows::new(1..)).with(Format::content(|x| format!("{:.15}", x))))
        .to_string();

    println!("*** Section 2 ***\n");
    println!("{}", table)
}
