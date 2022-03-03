mod eval;

pub use crate::eval::*;
pub use crate::eval::eval_string;

use std::io::{stdin, stdout, Write};

fn main() -> std::io::Result<()> {
    let mut bindv = Vec::new();

    loop{
        let mut buffer = String::new();
        print!("set-processor> ");
        stdout().flush()?;
        stdin().read_line(&mut buffer).expect("Fail to read line.");
        buffer.pop();
        if buffer == "exit" {
            break;
        }
        
        match eval_string(&buffer, &bindv) {
            Ok((token, bindv_new)) => {
                println!("{}", token.to_string());
                bindv = bindv_new;
            },
            Err(e) => println!("{}", e),
        }
    }
    Ok(())
}

