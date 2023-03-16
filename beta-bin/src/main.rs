use beta_core::{parser, program, interpreter};
use std::io::{self, Write};

fn main() {
    loop {
        print!("λ> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        io::stdin().read_line(&mut line).unwrap();

        let output = parser::parse_string(&line);

        if let Some(expression) = output {
            let mut term = program::Program::from_expression(&expression);

            while interpreter::is_reducable(&term) {
                term = interpreter::reduce(&term);
            }

            println!("{term}");
        } else {
            println!("invalid expression");
        }
    }
}
