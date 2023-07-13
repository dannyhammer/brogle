use dutchess_engine::{uci::UciLogger, *};
use log::SetLoggerError;

static LOGGER: UciLogger = UciLogger;

pub fn init_logger() -> Result<(), SetLoggerError> {
    log::set_logger(&LOGGER).map(|()| log::set_max_level(log::LevelFilter::Info))
}

fn main() {
    let name = env!("CARGO_PKG_NAME");
    let version = env!("CARGO_PKG_VERSION");
    let authors = env!("CARGO_PKG_AUTHORS");
    println!("{name} v{version} by {authors}");

    let mut engine = Engine::default();
    engine.run().expect("Fatal IO error");
}
