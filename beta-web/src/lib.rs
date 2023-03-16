use beta_core::{parser, interpreter};

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run(source: &str) -> String {
    let mut p = interpreter::Program::from_expression(
        &parser::parse_string(source).unwrap()
    );

    while program::is_reducable(&p) {
        p = program::reduce(&p);
    }

    format!("{}", program)
}
