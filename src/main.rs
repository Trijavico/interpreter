use std::{env, process};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <command>", &args[0]);
        process::exit(1);
    }

    let command = &args[1];

    match command.as_str() {
        "tokenize" => todo!(),
        "parse" => todo!(),
        "eval" => todo!(),
        _ => eprint!("Unknown command: {}", command),
    }
}
