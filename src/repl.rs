use anyhow::Result;
use std::io::{self, Write};

use crate::{
    eval::{self, Evaluator},
    parser::Parser,
};

pub fn start() -> Result<()> {
    let stdin = io::stdin();
    let stdout = io::stdout();
    println!("Feel free to type in commands");

    let mut evalator = Evaluator::new();

    loop {
        print!(">>");
        stdout.lock().flush()?;
        let mut line = String::new();
        stdin.read_line(&mut line)?;
        let trimmed_line = line.trim();

        let mut parser = Parser::new(trimmed_line);

        match evalator.eval_program(parser.parse()) {
            Ok(result) => {
                if !matches!(result, eval::Value::Idle) {
                    println!("{result}");
                }
            }
            Err(err) => println!("{}", err),
        }
    }
}
