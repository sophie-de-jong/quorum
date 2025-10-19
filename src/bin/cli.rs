use quorum::fmt::DisplayExpr;
use quorum::{self, parser};
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let mut line = String::new();

    loop {
        print!("quorum> ");
        io::stdout().flush()?;

        line.clear();
        if io::stdin().read_line(&mut line)? == 0 {
            break;
        }

        let expr = match parser::parse_expr(&line) {
            Ok(expr) => expr,
            Err(err) => {
                eprintln!("{err}");
                continue;
            }
        };

        match quorum::simplify(expr) {
            Ok(simplified) => println!("=> {}", simplified.display()),
            Err(err) => eprintln!("{err}"),
        }
    }

    Ok(())
}
