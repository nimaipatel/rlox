mod environment;
mod expr;
mod interpreter;
mod lox_type;
mod parser;
mod runtime_error;
mod scanner;
mod stmt;
mod token;
mod token_type;

use std::env;
use std::error::Error;
use std::fs::File;
use std::io::{self, Read};

fn main() -> Result<(), Box<dyn Error>> {
    let args = env::args().collect::<Vec<_>>();
    match &args[..] {
        // [_] => run_prompt()?,
        [_, script_name] => run_file(&script_name)?,
        [prog_name, ..] => println!("Usage: {} [script]", prog_name),
        [] => unreachable!(),
    }
    Ok(())
}

// fn run_prompt() -> io::Result<()> {
//     let env = Environment::fresh();
//     let stdin = io::stdin();
//     loop {
//         print!("> ");
//         io::stdout().flush()?;
//         let mut line = String::new();
//         stdin.lock().read_line(&mut line)?;
//         if line.is_empty() {
//             break;
//         } else {
//             todo!()
//             // run(Rc::clone(&env), &line);
//         }
//     }

//     Ok(())
// }

fn run_file(args: &str) -> io::Result<()> {
    let mut file = File::open(args)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;
    run(&source);
    Ok(())
}

fn run<'a>(source: &'a str) {
    match scanner::scan(&source) {
        Ok(tokens) => {
            let (stmts, errs) = parser::parse(&tokens);
            if errs.is_empty() {
                match interpreter::interpret(&stmts) {
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
