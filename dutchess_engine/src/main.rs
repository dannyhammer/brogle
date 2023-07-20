use log::SetLoggerError;

use dutchess_engine::{uci::UciLogger, *};

static LOGGER: UciLogger = UciLogger;

pub fn init_logger() -> Result<(), SetLoggerError> {
    log::set_logger(&LOGGER).map(|()| log::set_max_level(log::LevelFilter::Info))
}

fn main() {
    init_logger().expect("Failed to initialize logger");

    let mut engine = Engine::default();
    engine.run().expect("Fatal IO error");
}
