mod purified_tokenlist;

pub use crate::purified_tokenlist::PurifiedTokenList;
pub use crate::purified_tokenlist::eval_string;

fn main() -> std::io::Result<()> {
    loop{
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer).expect("Fail to read line.");
        buffer.pop();
        if buffer == "exit" {
            break;
        }
        
        match eval_string(&buffer) {
            Ok(token) => println!("{}", token.to_string()),
            Err(e) => println!("{}", e),
        }
    }
    Ok(())
}

