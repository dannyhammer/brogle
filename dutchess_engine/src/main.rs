use dutchess_engine::*;

use std::io::{stdin, stdout, Write};

fn main() {
    let mut engine = Engine::new();

    let mut input = String::new();

    loop {
        print!("CLIENT > ");
        stdout().flush().expect("Failed to flush stdout");

        stdin()
            .read_line(&mut input)
            .expect("Failed to read user input");

        match engine.parse_input(&input) {
            Err(e) => {
                eprintln!("InputError: {e}");
                continue;
            }
            Ok(cmd) => {
                let response = engine.process_command(cmd);

                println!("DUTCHESS > {response}");
            }
        }
    }
}
