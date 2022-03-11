mod eval;

pub use crate::eval::*;
pub use crate::eval::eval_line;

use std::io::{stdin, stdout, Write};
use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        return dialog();
    } else {
        return script(args.get(1).unwrap());
    }
}

fn script(filename: &String) -> std::io::Result<()> {
    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    match eval_main(&contents) {
        Ok(wv) => {
            for w in wv {
                match w {
                    Word::PrintSignal(s) => println!("{}", s),
                    Word::ExitSignal => return Ok(()),
                    _ => (),
                }
            }
        },
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
    
    return Ok(());
}

fn dialog() -> std::io::Result<()> {
    let mut bindm = Bind::new();

    loop{
        let mut buffer = String::new();
        print!("set-processor> ");
        stdout().flush()?;
        stdin().read_line(&mut buffer).expect("Fail to read line.");
        buffer.pop();
        
        match eval_line(&buffer, &bindm) {
            Ok((word, bindm_new)) => {
                if let Word::ExitSignal = word {
                    return Ok(());
                }

                println!("{}", word.to_string());
                bindm = bindm_new;
            },
            Err(e) => println!("{}", e),
        }
    }
}
