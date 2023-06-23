mod environment;
mod expr;
mod interpreter;
mod runtime_error;
mod parser;
mod scanner;
mod stmt;
mod token;
mod token_type;
mod lox_type;

use std::cell::RefCell;
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead, Read, Write};
use std::rc::Rc;

use environment::Environment;

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args().collect::<Vec<_>>();
    match &args[..] {
        [_] => run_prompt()?,
        [_, script_name] => run_file(&script_name)?,
        [prog_name, ..] => println!("Usage: {} [script]", prog_name),
        [] => unreachable!(),
    }
    Ok(())
}

fn run_prompt() -> io::Result<()> {
    let env = Environment::new(None);
    let env = Rc::new(RefCell::new(env));
    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut line = String::new();
        stdin.lock().read_line(&mut line)?;
        if line.is_empty() {
            break;
        } else {
            run(Rc::clone(&env), &line);
            // unsafe { HAD_ERROR = false };
        }
    }

    Ok(())
}

fn run_file(args: &str) -> io::Result<()> {
    let mut file = File::open(args)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let env = Environment::new(None);
    let env = Rc::new(RefCell::new(env));
    run(env, &contents);
    Ok(())
}

fn run(env: Rc<RefCell<Environment>>, source: &str) {
    match scanner::scan(source) {
        Ok(tokens) => {
            let (stmts, errs) = parser::parse(&tokens);
            // dbg!(&stmts, &errs);
            if errs.is_empty() {
                match interpreter::interpret(env, &stmts) {
                    Ok(_) => (),
                    Err(e) => println!("Runtime error: {}", e),
                }
            } else {
                errs.iter().for_each(|e| println!("Parsing error: {}", e));
            }
        }
        Err(e) => println!("Lexing error: {}", e),
    }
}
