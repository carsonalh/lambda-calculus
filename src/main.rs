use std::io::{self, Write};

pub mod program;
pub mod interpreter;
pub mod lexer;
pub mod parser;

fn main() {
    // this is a just a little REPL program that demonstrates how it all works
    loop {
        print!("λ> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        io::stdin().read_line(&mut line).unwrap();

        let output = parser::parse_string(&line);

        if let Some(expression) = output {
            let mut term = program::Program::from_expression(&expression);

            let mut i = 0;

            while interpreter::is_reducable(&term) {
                term = interpreter::reduce(&term);

                if i >= 20 {
                    break;
                }

                i += 1;
            }

            let simplified = interpreter::simplify(&term);

            println!("{simplified}");
        } else {
            println!("invalid expression");
        }
    }
}
