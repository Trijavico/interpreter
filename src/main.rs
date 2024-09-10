use std::{env, fs, process};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <command>", &args[0]);
        process::exit(1);
    }

    let command = &args[1];

    match command.as_str() {
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
        }
        "parse" => todo!(),
        "eval" => todo!(),
        _ => eprint!("Unknown command: {}", command),
    }
}
