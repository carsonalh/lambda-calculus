use std::io::{self, Write};

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

        if let Some(term) = output {
            let simplified =
                interpreter::simplify_expression(&interpreter::Program::from_expression(&term));
            println!("{simplified}");
        } else {
            println!("invalid expression");
        }
    }
}
