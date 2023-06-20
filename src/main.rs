mod expr;
mod interpreter;
mod parser;
mod scanner;
mod stmt;
mod token;
mod token_type;

use std::env;
use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead, Read, Write};
use std::process;

// static mut HAD_ERROR: bool = false;

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() > 1 {
        println!("Usage: rlox [script]");
        process::exit(64);
    } else if args.len() == 2 {
        run_file(&args[1])?;
    } else if args.len() == 1 {
        run_prompt()?;
    }
    Ok(())
}

fn run_prompt() -> io::Result<()> {
    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut line = String::new();
        stdin.lock().read_line(&mut line)?;
        if line.is_empty() {
            break;
        } else {
            run(&line);
            // unsafe { HAD_ERROR = false };
        }
    }

    Ok(())
}

fn run_file(args: &str) -> io::Result<()> {
    let mut file = File::open(args)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    run(&contents);
    // if unsafe { HAD_ERROR } == true {
    //     process::exit(65)
    // };
    Ok(())
}

fn run(source: &str) {
    match scanner::scan(source) {
        Ok(tokens) => match parser::parse(&tokens) {
            Ok(stmts) => {
                match interpreter::interpret(&stmts) {
                    Ok(_) => (),
                    Err(e) => println!("Runtime error: {}", e),
                };
            }
            Err(e) => println!("Parse error: {}", e),
        },
        Err(e) => println!("Lexing error: {}", e),
    }
}

// fn error(line: usize, message: &str) {
//     report(line, "", message);
// }

// fn report(line: usize, place: &str, message: &str) {
//     eprint!("[line {}] Error{}: {}", line, place, message);
//     unsafe { HAD_ERROR = true };
// }
