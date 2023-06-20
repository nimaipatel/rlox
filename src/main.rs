mod environment;
mod expr;
mod interpreter;
mod parser;
mod scanner;
mod stmt;
mod tests;
mod token;
mod token_type;

use std::env;
use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead, Read, Write};
use std::process;

use environment::Environment;
use parser::parse;

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
    let mut env = Environment::new();
    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut line = String::new();
        stdin.lock().read_line(&mut line)?;
        if line.is_empty() {
            break;
        } else {
            run(&mut env, &line);
            // unsafe { HAD_ERROR = false };
        }
    }

    Ok(())
}

fn run_file(args: &str) -> io::Result<()> {
    let mut file = File::open(args)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let mut env = Environment::new();
    run(&mut env, &contents);
    Ok(())
}

fn run(env: &mut Environment, source: &str) {
    match scanner::scan(source) {
        Ok(tokens) => {
            let (stmts, errs) = parse(&tokens);
            if errs.is_empty() {
                match interpreter::interpret(env, &stmts) {
                    Ok(_) => (),
                    Err(e) => println!("Runtime error: {}", e),
                }
            } else {
                errs.iter().for_each(|e| println!("{}", e));
            }
        }
        Err(e) => println!("Lexing error: {}", e),
    }
}