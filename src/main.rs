mod purified_tokenlist;

pub use crate::purified_tokenlist::PurifiedTokenList;
pub use crate::purified_tokenlist::eval_string;

fn main() -> std::io::Result<()> {
    let mut bindv = Vec::new();

    loop{
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer).expect("Fail to read line.");
        buffer.pop();
        if buffer == "exit" {
            break;
        }
        
        match eval_string(&buffer, bindv.clone()) {
            Ok((token, bindv_new)) => {
                println!("{}", token.to_string());
                bindv = bindv_new;
            },
            Err(e) => println!("{}", e),
        }
    }
    Ok(())
}

