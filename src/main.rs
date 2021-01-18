use std::io::{self, Read};

use rust_calc::eval_expression;

fn main() {
    let mut expression = String::new();
    let mut stdin = io::stdin();
    stdin
        .read_to_string(&mut expression)
        .expect("Failed to get expression");

    eval_expression(&expression);
}
