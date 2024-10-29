use anyhow::Result;
use std::io::{self, Write};

use crate::{
    eval::{self, Env, Evaluator},
    parser::Parser,
};

pub fn start() -> Result<()> {
    let stdin = io::stdin();
    let stdout = io::stdout();
    println!("Feel free to type in commands");

    let env = Env::new();
    let mut evalator = Evaluator::new(env, io::stdout(), io::stderr());

    loop {
        print!(">>");
        stdout.lock().flush()?;
        let mut line = String::new();
        stdin.read_line(&mut line)?;

        line = line.trim().to_string();
        let mut parser = Parser::new(line);

        match evalator.eval(parser.parse()) {
            Ok(result) => {
                if !matches!(result, eval::Value::Idle) {
                    println!("{result}");
                }
            }
            Err(err) => println!("{}", err),
        }
    }
}
