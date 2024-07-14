pub mod core {
    pub mod board;
    pub mod color;
    pub mod error;
    pub mod game;
    pub mod magicgen;
    pub mod movegen;
    pub mod moves;
    pub mod piece;
    pub mod tile;
    pub mod utils;

    pub use board::*;
    pub use color::*;
    pub use error::*;
    pub use game::*;
    pub use magicgen::*;
    pub use movegen::*;
    pub use moves::*;
    pub use piece::*;
    pub use tile::*;
    pub use utils::*;
}

pub mod engine {

    // pub mod engine;
    // pub mod eval;
    // pub mod search;
    /// # Universal Chess Interface (UCI)
    ///
    ///
    /// ## Description
    ///
    /// [Source](https://gist.github.com/aliostad/f4470274f39d29b788c1b09519e67372)
    ///
    /// * The specification is independent of the operating system. For Windows,
    ///   the engine is a normal exe file, either a console or "real" windows application.
    ///
    /// * All communication is done via standard input and output with text commands.
    ///
    /// * The engine should boot and wait for input from the GUI,
    ///   the engine should wait for the `isready` or `setoption` command to set up its internal parameters
    ///   as the boot process should be as quick as possible.
    ///
    /// * The engine must always be able to process input from stdin, even while thinking.
    ///
    /// * All command strings the engine receives will end with `\n`,
    ///   also all commands the GUI receives should end with `\n`,
    ///   * Note: `\n` can be `0x0d` or `0x0a0d` or any combination depending on your OS.
    ///     If you use Engine and GUI in the same OS this should be no problem if you communicate in text mode,
    ///     but be aware of this when for example running a Linux engine in a Windows GUI.
    ///
    /// * Arbitrary white space between tokens is allowed.
    ///   * Example: `debug on\n` and  `   debug     on  \n` and `\t  debug \t  \t\ton\t  \n`
    ///
    /// * The engine will always be in forced mode which means it should never start calculating
    ///   or pondering without receiving a `go` command first.
    ///
    /// * Before the engine is asked to search on a position, there will always be a position command
    ///   to tell the engine about the current position.
    ///
    /// * By default all the opening book handling is done by the GUI,
    ///   but there is an option for the engine to use its own book ("OwnBook" option, see below)
    ///
    /// * If the engine or the GUI receives an unknown command or token it should just ignore it and try to
    ///   parse the rest of the string in this line.
    ///   * Examples: `joho debug on\n` should switch the debug mode on given that `joho` is not defined,
    ///             `debug joho on\n` will be undefined however.
    ///
    /// * If the engine receives a command which is not supposed to come, for example `stop` when the engine is
    ///   not calculating, it should also just ignore it.
    ///
    /// ## Move Format
    /// The move format is in long algebraic notation.
    ///
    /// A nullmove from the Engine to the GUI should be sent as `0000`.
    /// Examples:  `e2e4`, `e7e5`, `e1g1` (white short castling), `e7e8q` (for promotion)
    pub mod uci;
}

/*
pub mod prelude {
    pub use crate::board::*;
    pub use crate::game::*;
    pub use crate::movegen::*;
    pub use crate::moves::*;
    pub use crate::piece::*;
    pub use crate::tile::*;
}

pub use prelude::*;
 */
