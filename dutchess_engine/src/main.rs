use dutchess_engine::{uci::UciCommand, uci::UciLogger, *};
use log::SetLoggerError;

use std::{
    io::{stdin, stdout, Write},
    sync::mpsc,
    thread,
};

static LOGGER: UciLogger = UciLogger;

pub fn init_logger() -> Result<(), SetLoggerError> {
    log::set_logger(&LOGGER).map(|()| log::set_max_level(log::LevelFilter::Info))
}

fn main() {
    let name = env!("CARGO_PKG_NAME");
    let version = env!("CARGO_PKG_VERSION");
    let authors = env!("CARGO_PKG_AUTHORS");
    println!("{name} v{version} by {authors}");

    // let (sender, receiver) = mpsc::channel();
    // let cloned = sender.clone();

    /*
    let input_handler = thread::spawn(move || loop {
        let mut buffer = String::new();

        stdin()
            .read_line(&mut buffer)
            .expect("Failed to read user input");

        match UciCommand::new(&buffer) {
            Ok(cmd) => {
                if UciCommand::Quit == cmd || cloned.send(buffer).is_err() {
                    return;
                }
            }
            Err(e) => {
                println!("{e}");
            }
        }
    });
     */

    let mut engine = Engine::new();

    run(engine).expect("Fatal IO error");
}

fn run(mut engine: Engine) -> std::io::Result<()> {
    let mut buffer = String::new();
    use UciCommand::*;
    loop {
        buffer.clear();
        stdin().read_line(&mut buffer)?;

        match UciCommand::new(&buffer) {
            Ok(cmd) => match dbg!(cmd) {
                Uci => {
                    //
                }
                Debug(status) => engine.debug(status),
                IsReady => {
                    //
                }
                SetOption(name, value) => {
                    //
                }
                Register(args) => {
                    //
                }
                UciNewGame => {
                    //
                }
                Position(fen, moves) => {
                    //
                }
                Go(search_opt) => {
                    //
                }
                Stop => {
                    //
                }
                PonderHit => {
                    //
                }
                Quit => {
                    return Ok(());
                }
            },
            Err(e) => {
                println!("{e}");
            }
        }
    }
}
