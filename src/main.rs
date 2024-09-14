use std::{env, fs, process};

use monkelang::{eval::Evaluator, lexer::Lexer, parser::Parser, repl};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <command>", &args[0]);
        process::exit(1);
    }

    let command = &args[1];

    match command.as_str() {
        "repl" => {
            if let Err(err) = repl::start() {
                eprintln!("{}", err);
            }
        }
        "tokenize" => {
            if args.len() < 3 {
                eprintln!("Usage: {} <command>", &args[0]);
                process::exit(1);
            }
            let file_path = &args[2];
            let file_contents = fs::read_to_string(file_path).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", file_path);
                String::new()
            });

            let lexer = Lexer::new(file_contents);
            let mut err = 0;
            for token in lexer {
                match token {
                    Ok(tok) => println!("{tok}"),
                    Err(msg) => {
                        err = 65;
                        eprintln!("{}", msg);
                    }
                };
            }

            process::exit(err);
        }
        "parse" => {
            if args.len() < 3 {
                eprintln!("Usage: {} <command>", &args[0]);
                process::exit(1);
            }
            let file_path = &args[2];
            let file_contents = fs::read_to_string(file_path).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", file_path);
                String::new()
            });

            let mut parser = Parser::new(file_contents);
            println!("{}", parser.parse());
        }
        "eval" => {
            if args.len() < 3 {
                eprintln!("Usage: {} <command>", &args[0]);
                process::exit(1);
            }
            let file_path = &args[2];
            let file_contents = fs::read_to_string(file_path).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", file_path);
                String::new()
            });

            let mut parser = Parser::new(file_contents);
            let mut evaluator = Evaluator::new();
            let program = parser.parse();

            match evaluator.eval(program) {
                Ok(result) => println!("{result}"),
                Err(err) => println!("{}", err),
            };
        }
        _ => eprint!("Unknown command: {}", command),
    }
}
