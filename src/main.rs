mod eval;

pub use crate::eval::*;
pub use crate::eval::eval_string;

use std::io::{stdin, stdout, Write};

fn main() -> std::io::Result<()> {
    let mut bindm = Bind::new();

    loop{
        let mut buffer = String::new();
        print!("set-processor> ");
        stdout().flush()?;
        stdin().read_line(&mut buffer).expect("Fail to read line.");
        buffer.pop();
        
        match eval_string(&buffer, &bindm) {
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

