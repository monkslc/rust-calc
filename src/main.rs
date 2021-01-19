use std::env;

use rust_calc::eval_expression;

fn main() {
    let expression: String = env::args().skip(1).collect();
    println!("{}", eval_expression(&expression));
}
