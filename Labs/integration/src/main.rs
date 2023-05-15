use integration::prelude::*;
use quote::quote;
use tabled::{
    settings::{
        object::{Rows, Segment},
        Format, Modify,
    },
    Table, Tabled,
};

fn main() {
    let fun = &mut Function::new(|x| x.powi(2) + x.log10(), 0.4, 0.9);

    // Section 1
    {
        let j_range = 1..16;
        let integ_data: Vec<IntegralResult> = j_range
            .clone()
            .map(|j| -> IntegralResult {
                fun.n = 2_usize.pow(j);
                monte_carlo(fun)
            })
            .collect();

        #[derive(Tabled)]
        #[allow(non_snake_case)]
        struct Row {
            j: u32,
            n: usize,
            I: f64,
            Delta_I: f64,
            delta_I: f64,
            R: f64,
            ratio: f64,
        }

        let table_vec: Vec<Row> = j_range
            .map(|j| {
                let approx = integ_data.get(j as usize - 1).unwrap();
                #[allow(non_snake_case)]
                let Delta_I = (fun.I() - approx.val).abs();
                #[allow(non_snake_case)]
                let delta_I = Delta_I / fun.I() * 100.0;
                let mut ratio = 0.0;
                if j < 15 {
                    let next_approx = integ_data.get(j as usize).unwrap();
                    ratio = Delta_I / (fun.I() - next_approx.val).abs();
                }

                Row {
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

        let table = Table::new(table_vec)
            .with(
                Modify::new(Segment::new(1.., 2..)).with(Format::content(|x| format!("{:.15}", x))),
            )
            .to_string();

        println!("{}", table);
    }

    println!("\n");

    // Section 2
    {
        fun.n = 10000;

        #[derive(Tabled)]
        #[allow(non_snake_case)]
        struct Row {
            method: String,
            I: f64,
            Delta_I: f64,
            delta_I: f64,
            R: f64,
        }

        let table_vec: Vec<Row> = [
            rect_left,
            rect_right,
            rect_center,
            trapezium,
            simpson,
            monte_carlo,
        ]
        .into_iter()
        .map(|method| {
            let res = method(fun);
            Row {
                method: quote!(method).to_string(),
                I: res.val,
                Delta_I: (fun.I() - res.val).abs(),
                delta_I: (fun.I() - res.val).abs() / fun.I() * 100.0,
                R: res.err,
            }
        })
        .collect();

        let table = Table::new(table_vec)
            .with(Modify::new(Rows::new(1..)).with(Format::content(|x| format!("{:.15}", x))))
            .to_string();

        println!("{}", table)
    }
}
