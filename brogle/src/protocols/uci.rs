use std::{
    fmt::{self},
    io::{self, Write},
    str::FromStr,
    time::Duration,
};

use anyhow::{anyhow, bail, Context, Result};
use log::{info, warn};

/// Represents the arguments that can be sent to your engine via the `go` command.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct UciSearchOptions {
    /// ```text
    /// searchmoves <move_1> [<move_2> ... <move_i>]
    /// ```
    ///
    /// Restrict search to this moves only
    ///
    /// Example: After `position startpos` and `go infinite searchmoves e2e4 d2d4`
    /// the engine should only search the two moves `e2e4` and `d2d4` in the initial
    /// position.
    pub search_moves: Vec<String>,

    /// ```text
    /// ponder
    /// ```
    /// Start searching in pondering mode.
    ///
    /// Do not exit the search in ponder mode, even if it's mate!
    ///
    /// This means that the last move sent in in the position string is the ponder
    /// move. The engine can do what it wants to do, but after a `ponderhit` command
    /// it should execute the suggested move to ponder on. This means that the ponder
    /// move sent by the GUI can be interpreted as a recommendation about which move
    /// to ponder. However, if the engine decides to ponder on a different move, it
    /// should not display any mainlines as they are likely to be misinterpreted by
    /// the GUI because the GUI expects the engine to ponder on the suggested move.
    pub ponder: bool,

    /// ```text
    /// wtime <x>
    /// ```
    /// White has `x` milliseconds left on the clock.
    pub w_time: Option<Duration>,

    /// ```text
    /// btime <x>
    /// ```
    /// Black has `x` milliseconds left on the clock.
    pub b_time: Option<Duration>,

    /// ```text
    /// winc <x>
    /// ```
    /// White increment per move in milliseconds if `x > 0`.
    pub w_inc: Option<Duration>,

    /// ```text
    /// binc <x>
    /// ```
    /// Black increment per move in milliseconds if `x > 0`.
    pub b_inc: Option<Duration>,

    /// ```text
    /// movestogo <x>
    /// ```
    /// There are `x` moves to the next time control.
    ///
    /// This will only be sent if `x > 0`.
    ///
    /// If you don't get this and get the `wtime` and `btime`, it's sudden death.
    pub moves_to_go: Option<u32>,

    /// ```text
    /// depth <x>
    /// ```
    /// Search `x` plies only.
    pub depth: Option<u32>,

    /// ```text
    /// nodes <x>
    /// ```
    /// Search `x` nodes only.
    pub nodes: Option<u64>,

    /// ```text
    /// mate <x>
    /// ```
    /// Search for a mate in `x` moves.
    pub mate: Option<u32>,

    /// ```text
    /// movetime <x>
    /// ```
    /// Search exactly `x` milliseconds.
    pub move_time: Option<Duration>,

    /// ```text
    /// infinite
    /// ```
    /// Search until the `stop` command. Do not exit the search without being told
    /// so in this mode!
    pub infinite: bool,
}

impl Default for UciSearchOptions {
    /// A default search is analogous to `go infinite`.
    fn default() -> Self {
        Self {
            search_moves: vec![],
            ponder: false,
            w_time: None,
            b_time: None,
            w_inc: None,
            b_inc: None,
            moves_to_go: None,
            depth: None,
            nodes: None,
            mate: None,
            move_time: None,
            infinite: true,
        }
    }
}

/// Represents an engine that has implemented the
/// [Universal Chess Interface (UCI)](https://backscattering.de/chess/uci/) protocol.
///
/// Implementing this trait requires you to implement the following methods:
///  - [`UciEngine::position`] to set the state of the game in your engine.
///  - [`UciEngine::go`] to spawn a thread for searching the previously-set position.
///  - [`UciEngine::stop`] to kill the search thread and yield a `bestmove` result.
///
/// The remaining methods all have default implementations, all of which are detailed
/// in their documentation.
/// You may want to implement your own variants, depending on your engine's needs.
///
/// ## GUI to Engine
///
/// The following methods are for GUI to Engine communication, meaning they are
/// called when the UI (or user) sends a command (via `stdin`) to your engine:
///  - [UciEngine::uci]
///  - [UciEngine::debug]
///  - [UciEngine::isready]
///  - [UciEngine::setoption]
///  - [UciEngine::register]
///  - [UciEngine::ucinewgame]
///  - [UciEngine::position]
///  - [UciEngine::go]
///  - [UciEngine::stop]
///  - [UciEngine::ponderhit]
///  - [UciEngine::quit]
///
/// ## Engine to GUI
/// The following methods are for Engine to GUI communication, meaning they are
/// called when your engine needs to send a command (via `stdout`) to the UI:
///  - [UciEngine::id]
///  - [UciEngine::uciok]
///  - [UciEngine::readyok]
///  - [UciEngine::bestmove]
///  - [UciEngine::copyprotection]
///  - [UciEngine::registration]
///  - [UciEngine::info]
///  - [UciEngine::option]
pub trait UciEngine {
    /// Executes the provided [`UciCommand`].
    ///
    /// This is just a convenience method that `match`es on `cmd` and calls the
    /// appropriate method.
    ///
    /// You do not need to re-implement this.
    fn execute_uci_command(&mut self, cmd: UciCommand) -> Result<()> {
        use UciCommand::*;
        match cmd {
            Uci => self.uci(),
            Debug(status) => self.debug(status),
            IsReady => self.isready(),
            SetOption { name, value } => self.setoption(name, value),
            Register { name, code } => self.register(name, code),
            UciNewGame => self.ucinewgame(),
            Position { fen, moves } => self.position(fen, moves),
            Go(search_opt) => self.go(search_opt),
            Stop => self.stop(),
            PonderHit => self.ponderhit(),
            Quit => self.quit(),
        }
    }

    /// Parse a string of input, returning a [`UciCommand`], if possible.
    ///
    /// If not possible, bails with a standard `"unknown command X"` message.
    ///
    /// This is a convenience method to wrap [`UciCommand::parse`].
    fn parse_uci_input(input: &str) -> Result<UciCommand> {
        UciCommand::parse(input)
    }

    /// Send a response from this engine to the GUI via `stdout`.
    ///
    /// This formats the provided `cmd` as a string terminated with the EOL char `\n`.
    ///
    /// The only reason you'd every really want to overwrite this method is if you
    /// want to write to something other than `stdout`.
    fn send_uci_response<T: fmt::Display>(&self, cmd: UciResponse<T>) -> Result<()> {
        write!(io::stdout(), "{cmd}\n").context("Failed to write '{cmd}' and flush buffer")
    }

    /******************************************************************/
    /*                  GUI to Engine communication                   */
    /******************************************************************/

    /// Called when the engine receives:
    /// ```text
    /// uci
    /// ```
    ///
    /// # Protocol Description
    ///
    /// Tell engine to use the uci (universal chess interface).
    /// This will be sent once as a first command after program boot to tell the
    /// engine to switch to uci mode.
    ///
    /// After receiving the uci command the engine must identify itself with the `id`
    /// command and send the `option` commands to tell the GUI which engine settings
    /// the engine supports if any.
    ///
    /// After that the engine should send `uciok` to acknowledge the uci mode.
    /// If no uciok is sent within a certain time period, the engine task will be
    /// killed by the GUI.
    ///
    /// # Default
    ///
    /// The default implementation of this method calls, in order: [`UciEngine::id`],
    /// [`UciEngine::option`], and finally [`UciEngine::uciok`].
    fn uci(&mut self) -> Result<()> {
        info!("using default implementation of UciEngine::uci");
        // The engine must now identify itself
        self.id()?;

        // And send all available options
        self.option()?;

        // Engine has sent all parameters and is ready
        self.uciok()?;

        Ok(())
    }

    /// Called when the engine receives:
    /// ```text
    /// debug [on | off]
    /// ```
    /// where `on = true`
    ///
    /// # Protocol Description
    ///
    /// Switch the debug mode of the engine on and off.
    ///
    /// In debug mode the engine should send additional infos to the GUI.
    ///   - e.g. with the `info string` command, to help debugging.
    ///   - e.g. the commands that the engine has received etc.
    ///
    /// This mode should be switched off by default and this command can be sent any
    /// time, also when the engine is thinking.
    ///
    /// # Default
    ///
    /// The default implementation of this method does nothing and returns `Ok(())`.
    fn debug(&mut self, status: bool) -> Result<()> {
        info!("using default implementation of UciEngine::debug({status:?})");
        Ok(())
    }

    /// Called when the engine receives:
    /// ```text
    /// isready
    /// ```
    ///
    /// # Protocol Description
    ///
    /// This is used to synchronize the engine with the GUI.
    /// When the GUI has sent a command or multiple commands that can take some time
    /// to complete, this command can be used to wait for the engine to be ready
    /// again or to ping the engine to find out if it is still alive. E.g. this
    /// should be sent after setting the path to the table bases as this can take
    /// some time.
    ///
    /// This command is also required once before the engine is asked to do any
    /// search to wait for the engine to finish initializing.
    ///
    /// This command must always be answered with `readyok` and can be sent also
    /// when the engine is calculating in which case the engine should also
    /// immediately answer with `readyok` without stopping the search.
    ///
    /// # Default
    ///
    /// The default implementation of this method simply immediately sends a response
    /// of `readyok`.
    fn isready(&self) -> Result<()> {
        info!("using default implementation of UciEngine::isready");
        self.send_uci_response(UciResponse::<&str>::ReadyOk)
    }

    /// Called when the engine receives:
    /// ```text
    /// setoption name <id> [value <x>]
    /// ```
    ///
    /// # Protocol Description
    ///
    /// This is sent to the engine when the user wants to change the internal
    /// parameters of the engine. For the `button` type no value is needed.
    ///
    /// One string will be sent for each parameter and this will only be sent when
    /// the engine is waiting. The name and value of the option in `<id>` should not
    /// be case sensitive and can include spaces.
    ///
    /// The substrings `value` and `name` should be avoided in `<id>` and `<x>` to
    /// allow unambiguous parsing, for example do not use `<name> = draw value`.
    ///
    /// ## Examples
    ///
    /// Here are some strings for the example below:
    /// * `setoption name Nullmove value true\n`
    /// * `setoption name Selectivity value 3\n`
    /// * `setoption name Style value Risky\n`
    /// * `setoption name Clear Hash\n`
    /// * `setoption name NalimovPath value c:\chess\tb\4;c:\chess\tb\5\n`
    ///
    /// # Default
    ///
    /// The default implementation of this method does nothing and returns `Ok(())`.
    fn setoption(&mut self, name: String, value: Option<String>) -> Result<()> {
        info!("using default implementation of UciEngine::setoption({name:?}, {value:?})");
        Ok(())
    }

    /// Called when the engine receives:
    /// ```text
    /// registration [name <name> code <code> | later]
    /// ```
    ///
    /// You probably don't need to implement this. Registration is rare and arguably
    /// unnecessary.
    ///
    /// If called with `later`, then the `name` and `code` parameters will be `None`.
    /// Otherwise, they are likely to both be `Some`, though it is up to your engine
    /// to determine if both are necessary.
    ///
    /// # Protocol Description
    ///
    /// This is the command to try to register an engine or to tell the engine that
    /// registration will be done later. This command should always be sent if the
    /// engine has sent `registration error` at program startup.
    ///
    /// The following tokens are allowed:
    /// * `later` - The user doesn't want to register the engine now.
    /// * `name <x>` - The engine should be registered with the name `<x>`
    /// * `code <y>` - The engine should be registered with the code `<y>`
    ///
    /// ## Example:
    ///    `register later`
    ///    `register name Stefan MK code 4359874324`
    ///
    /// # Default
    ///
    /// The default implementation of this method does nothing and returns `Ok(())`.
    fn register(&mut self, name: Option<String>, code: Option<String>) -> Result<()> {
        info!("using default implementation of UciEngine::register({name:?}, {code:?})");
        Ok(())
    }

    /// Called when the engine receives:
    /// ```text
    /// ucinewgame
    /// ```
    ///
    /// # Protocol Description
    ///
    /// This is sent to the engine when the next search (started with `position`
    /// and `go`) will be from a different game. This can be a new game the engine
    /// should play or a new game it should analyze but also the next position from
    /// a test suite with positions only.
    ///
    /// If the GUI hasn't sent a `ucinewgame` before the first `position` command,
    /// the engine shouldn't expect any further ucinewgame commands as the GUI is
    /// probably not supporting the ucinewgame command. So the engine should not rely
    /// on this command even though all new GUIs should support it.
    ///
    /// As the engine's reaction to `ucinewgame` can take some time the GUI should
    /// always send `isready` after `ucinewgame` to wait for the engine to finish
    /// its operation.
    ///
    /// # Default
    ///
    /// The default implementation of this method does nothing and returns `Ok(())`.
    fn ucinewgame(&mut self) -> Result<()> {
        info!("using default implementation of UciEngine::ucinewgame");
        Ok(())
    }

    /// Called when the engine receives:
    /// ```text
    /// position [fen <fenstring> | startpos] [moves <move_1> [<move_2> ...]]
    /// ```
    ///
    /// # Protocol Description
    ///
    /// Set up the position described in FEN string on the internal board and
    /// play the moves on the internal chess board.
    ///
    /// If the game was played from the start position, the string `startpos` will
    /// be sent.
    ///
    /// Note: no `new` command is needed. However, if this position is from a
    /// different game than the last position sent to the engine, the GUI should have
    /// sent a `ucinewgame` in between.
    fn position(&mut self, fen: Option<String>, moves: Vec<String>) -> Result<()>;

    /// Called when the engine receives:
    /// ```text
    /// go [searchmoves <move_1> [<move_2> ...]] [ponder] [wtime <x>] [btime <x>] [winc <x>] [binc <x>] [movestogo <x>] [depth <x>] [nodes <x>] [mate <x>] [movetime <x>] [infinite]
    /// ```
    /// If no parameters are received, this should be treated as `go infinite`.
    ///
    /// # Protocol Description
    ///
    /// Start calculating on the current position set up with the `position` command.
    ///
    /// There are a number of commands that can follow this command, all will be sent
    /// in the same string. If one command is not sent its value should be
    /// interpreted as it would not influence the search.
    ///
    /// ## Arguments
    ///
    /// ```text
    /// searchmoves <move_1> [<move_2> ... <move_i>]
    /// ```
    /// Restrict search to this moves only
    ///
    /// Example: After `position startpos` and `go infinite searchmoves e2e4 d2d4`
    /// the engine should only search the two moves `e2e4` and `d2d4` in the initial
    /// position.
    ///
    /// ```text
    /// ponder
    /// ```
    /// Start searching in pondering mode.
    ///
    /// Do not exit the search in ponder mode, even if it's mate!
    ///
    /// This means that the last move sent in in the position string is the ponder
    /// move. The engine can do what it wants to do, but after a `ponderhit` command
    /// it should execute the suggested move to ponder on. This means that the ponder
    /// move sent by the GUI can be interpreted as a recommendation about which move
    /// to ponder. However, if the engine decides to ponder on a different move, it
    /// should not display any mainlines as they are likely to be misinterpreted by
    /// the GUI because the GUI expects the engine to ponder on the suggested move.
    ///
    /// ```text
    /// wtime <x>
    /// ```
    /// White has `x` milliseconds left on the clock.
    ///
    /// ```text
    /// btime <x>
    /// ```
    /// Black has `x` milliseconds left on the clock.
    ///
    /// ```text
    /// winc <x>
    /// ```
    /// White increment per move in milliseconds if `x > 0`.
    ///
    /// ```text
    /// binc <x>
    /// ```
    /// Black increment per move in milliseconds if `x > 0`.
    ///
    /// ```text
    /// movestogo <x>
    /// ```
    /// There are `x` moves to the next time control.
    ///
    /// This will only be sent if `x > 0`.
    ///
    /// If you don't get this and get the `wtime` and `btime`, it's sudden death.
    ///
    /// ```text
    /// depth <x>
    /// ```
    /// Search `x` plies only.
    ///
    /// ```text
    /// nodes <x>
    /// ```
    /// Search `x` nodes only.
    ///
    /// ```text
    /// mate <x>
    /// ```
    /// Search for a mate in `x` moves.
    ///
    /// ```text
    /// movetime <x>
    /// ```
    /// Search exactly `x` milliseconds.
    ///
    /// ```text
    /// infinite
    /// ```
    /// Search until the `stop` command. Do not exit the search without being told
    /// so in this mode!
    fn go(&mut self, options: UciSearchOptions) -> Result<()>;

    /// Called when the engine receives:
    /// ```text
    /// stop
    /// ```
    ///
    /// Your engine should probably call [`UciEngine::bestmove`] before/within this.
    ///
    /// # Protocol Description
    ///
    /// Stop calculating as soon as possible.
    ///
    /// Don't forget the `bestmove` and possibly the `ponder` token when finishing
    /// the search.
    fn stop(&mut self) -> Result<()>;

    /// Called when the engine receives:
    /// ```text
    /// ponderhit
    /// ```
    ///
    /// # Protocol Description
    ///
    /// The user has played the expected move. This will be sent if the engine was
    /// told to ponder on the same move the user has played. The engine should
    /// continue searching but switch from pondering to normal search.
    ///
    /// # Default
    ///
    /// The default implementation of this method does nothing and returns `Ok(())`.
    fn ponderhit(&self) -> Result<()> {
        info!("using default implementation of UciEngine::ponderhit");
        Ok(())
    }

    /// Called when the engine receives:
    /// ```text
    /// quit
    /// ```
    ///
    /// This does not necessarily need to immediately exit, but should make sure it
    /// performs any necessary clean-up before exiting.
    ///
    /// It is likely that you want this command to be a special case in your
    /// engine's event handler.
    ///
    /// # Protocol Description
    ///
    /// Quit the program as soon as possible.
    ///
    /// # Default
    ///
    /// The default implementation of this method does nothing and returns `Ok(())`.
    fn quit(&mut self) -> Result<()> {
        info!("using default implementation of UciEngine::quit");
        Ok(())
    }

    /******************************************************************/
    /*                  Engine to GUI communication                   */
    /******************************************************************/

    /// Send the `id` message to `stdout` as follows:
    /// ```text
    /// id name <x>
    /// id author <x>
    /// ```
    ///
    /// # Protocol Description
    ///
    /// ## Example
    ///
    /// ```text
    /// name <x>
    /// ```
    /// This must be sent after receiving the uci command to identify the engine,
    /// e.g. `id name Shredder X.Y\n`
    ///
    ///
    /// ```text
    /// author <x>
    /// ```
    /// Ths must be sent after receiving the uci command to identify the engine,
    /// e.g. `id author Stefan MK\n`
    ///
    /// # Default
    ///
    /// The default implementation of this method sends the following:
    /// ```text
    /// id name <name> <version>\n
    /// id author [<author> | <author_1>, <author_2>, ...]\n
    /// ```
    /// Where:
    /// - `<name>` is fetched from the `CARGO_PKG_NAME` env var.
    /// - `<version>` is fetched from the `CARGO_PKG_VERSION` env var.
    /// - `<author>` is fetched from the `CARGO_PKG_AUTHORS` env var.
    fn id(&self) -> Result<()> {
        info!("using default implementation of UciEngine::id");
        let name = format!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
        let author = env!("CARGO_PKG_AUTHORS").replace(":", ", ");

        self.send_uci_response(UciResponse::Id { name, author })
    }

    /// Send the `uciok` message to `stdout` as follows:
    /// ```text
    /// uciok
    /// ```
    ///
    /// # Protocol Description
    ///
    /// Must be sent after the `id` and optional `option`s to tell the GUI that the
    /// engine has sent all infos and is ready in uci mode.
    ///
    /// # Default
    ///
    /// The default implementation of this method simply immediately sends a response
    /// of `uciok`.
    fn uciok(&self) -> Result<()> {
        info!("using default implementation of UciEngine::uciok");
        self.send_uci_response(UciResponse::<&str>::UciOk)
    }

    /// Send the `readyok` message to `stdout` as follows:
    /// ```text
    /// readyok
    /// ```
    ///
    /// # Protocol Description
    ///
    /// This must be sent when the engine has received an `isready` command and has
    /// processed all input and is ready to accept new commands now.
    ///
    /// It is usually sent after a command that can take some time to be able to wait
    /// for the engine, but it can be used anytime, even when the engine is
    /// searching, and must always be answered with `isready`.
    ///
    /// # Default
    ///
    /// The default implementation of this method simply immediately sends a response
    /// of `readyok`.
    fn readyok(&self) -> Result<()> {
        info!("using default implementation of UciEngine::readyok");
        self.send_uci_response(UciResponse::<&str>::ReadyOk)
    }

    /// Send the `bestmove` message to `stdout` as follows:
    /// ```text
    /// bestmove <move_1> [ponder <move_2>]
    /// ```
    ///
    /// # Protocol Description
    ///
    /// The engine has stopped searching and found the move `<move>` best in this
    /// position.
    ///
    /// The engine can send the move it likes to ponder on. The engine must not
    /// start pondering automatically.
    ///
    /// This command must always be sent if the engine stops searching, also in
    /// pondering mode if there is a `stop` command, so for every `go` command a
    /// `bestmove` command is needed!
    ///
    /// Directly before that the engine should send a final `info` command with the
    /// final search information, the the GUI has the complete statistics about the
    /// last search.
    ///
    /// # Default
    ///
    /// The default implementation of this method simply immediately sends a response
    /// of `bestmove <bestmove> [ponder <ponder>]`.
    fn bestmove<T: fmt::Display>(&self, bestmove: T, ponder: Option<T>) -> Result<()> {
        self.send_uci_response(UciResponse::<T>::BestMove { bestmove, ponder })
    }

    /// Send the `copyprotection` message to `stdout` as follows:
    /// ```text
    /// copyprotection [checking | ok | error]
    /// ```
    ///
    /// # Protocol Description
    ///
    /// This is needed for copy-protected engines. After the `uciok` command the
    /// engine can tell the GUI, that it will check the copy protection now. This is
    /// done by `copyprotection checking`.
    ///
    /// If the check is ok the engine should send `copyprotection ok`, otherwise
    /// `copyprotection error`. If there is an error the engine should not function
    /// properly but should not quit alone. If the engine reports
    /// `copyprotection error`
    /// the GUI should not use this engine and display an error message instead!
    ///
    /// The code in the engine can look like this:
    /// ```text
    /// TellGUI("copyprotection checking\n");
    /// // ... check the copy protection here ...
    /// if(ok)
    ///     TellGUI("copyprotection ok\n");
    /// else
    ///     TellGUI("copyprotection error\n");
    /// ```
    ///
    /// # Default
    ///
    /// The default implementation of this method simply immediately sends a response
    /// of `checking` followed by `ok`.
    fn copyprotection(&self) -> Result<()> {
        info!("using default implementation of UciEngine::copyprotection");
        self.send_uci_response(UciResponse::<&str>::CopyProtection(
            UciCheckingStatus::Checking,
        ))?;
        self.send_uci_response(UciResponse::<&str>::CopyProtection(UciCheckingStatus::Ok))
    }

    /// Send the `registration` message to `stdout` as follows:
    /// ```text
    /// registration [checking | ok | error]
    /// ```
    ///
    /// # Protocol Description
    ///
    /// Analog to the `copyprotection` command the engine can send
    /// `registration checking` after the uciok command followed by either
    /// `registration ok` or `registration error`. Also after every attempt to
    /// register the engine it should answer with `registration checking` and then
    /// either `registration ok` or `registration error`.
    ///
    /// In contrast to the `copyprotection` command, the GUI can use the engine after
    /// the engine has reported an error, but should inform the user that the engine
    /// is not properly registered and might not use all its features.
    ///
    /// In addition the GUI should offer to open a dialog to enable registration of
    /// the engine. To try to register an engine the GUI can send the `register`
    /// command. The GUI has to always answer with the `register` command if the
    /// engine sends `registration error` at engine startup (this can also be done
    /// with `register later`) and tell the user somehow that the engine is not
    /// registered. This way the engine knows that the GUI can deal with the
    /// registration procedure and the user will be informed that the engine is not
    /// properly registered.
    ///
    /// # Default
    ///
    /// The default implementation of this method simply immediately sends a response
    /// of `checking` followed by `ok`.
    fn registration(&self) -> Result<()> {
        info!("using default implementation of UciEngine::registration");
        self.send_uci_response(UciResponse::<&str>::Registration(
            UciCheckingStatus::Checking,
        ))?;
        self.send_uci_response(UciResponse::<&str>::Registration(UciCheckingStatus::Ok))
    }

    /// Send the `info` message to `stdout` as follows:
    /// ```text
    /// info [depth <x>] [seldepth <x>] [time <x>] [nodes <x>] [pv <move_1> [<move_2> ... <move_i>]] [score [cp <x> | mate <y>] [lowerbound | upperbound]] [currmove <move>] [currmovenumber <x>] [hashfull <x>] [nps <x>] [tbhits <x>] [sbhits <x>] [cpuload <x>] [string <str>] [refutation <move_1> <move_2> [... <move_i>]] [currline [cpunr] <move_1> [... <move_i>]]
    /// ```
    ///
    /// # Protocol Description
    ///
    /// The engine wants to send information to the GUI. This should be done whenever
    /// one of the info has changed.
    ///
    /// The engine can send only selected infos or multiple infos with one info
    /// command, e.g.`info currmove e2e4 currmovenumber 1` or
    /// `info depth 12 nodes 123456 nps 100000`.
    ///
    /// Also all infos belonging to the pv should be sent together e.g.
    /// `info depth 2 score cp 214 time 1242 nodes 2124 nps 34928 pv e2e4 e7e5 g1f3`.
    ///
    /// I suggest to start sending `currmove`, `currmovenumber`, `currline` and
    /// `refutation` only after one second to avoid too much traffic.
    ///
    /// ## Arguments
    ///
    /// ```text
    /// depth <x>
    /// ```
    /// Search depth (in plies).
    ///
    /// ```text
    /// seldepth <x>
    /// ```
    /// Selective search depth (in plies),
    ///
    /// If the engine sends `seldepth` there must also be a `depth` present in the
    /// same string.
    ///
    /// ```text
    /// time <x>
    /// ```
    /// The time searched (in ms).
    /// This should be sent together with the `pv`.
    ///
    /// ```text
    /// nodes <x>
    /// ```
    /// `<x>` nodes searched.
    /// The engine should send this info regularly.
    ///
    /// ```text
    /// pv <move_1> [<move_2> ... <move_i>]
    /// ```
    /// The best line found.
    ///
    /// ```text
    /// multipv <num>
    /// ```
    /// This for the multi pv mode.
    ///
    /// For the best move/pv add `multipv 1` in the string when you send the pv.
    ///
    /// In *k*-best mode always send all k variants in k strings together.
    ///
    /// ```text
    /// score [cp <x> | mate <y> | lowerbound | upperbound]
    /// ```
    ///
    ///   - `cp <x>`
    /// The score from the engine's point of view in centipawns.
    ///
    ///   - `mate <y>`
    /// Mate in `y` moves, not plies.
    ///
    /// If the engine is getting mated, use negative values for `y`.
    ///
    ///   - `lowerbound`
    /// The score is just a lower bound.
    ///
    ///   - `upperbound`
    /// The score is just an upper bound.
    ///
    /// ```text
    /// currmove <move>
    /// ```
    /// Currently searching this move
    ///
    /// ```text
    /// Currmovenumber <x>
    /// ```
    /// Currently searching move number `x`, for the first move `x` should be `1` not
    /// `0`.
    ///
    /// ```text
    /// hashfull <x>
    /// ```
    /// The hash is `x` permill full.
    ///
    /// The engine should send this info regularly.
    ///
    /// ```text
    /// nps <x>
    /// ```
    /// `x` nodes per second searched.
    ///
    /// The engine should send this info regularly.
    ///
    /// ```text
    /// tbhits <x>
    /// ```
    /// `x` positions where found in the endgame table bases.
    ///
    /// ```text
    /// sbhits <x>
    /// ```
    /// `x` positions where found in the shredder endgame databases.
    ///
    /// ```text
    /// cpuload x
    /// ```
    /// The cpu usage of the engine is `x` permill.
    ///
    /// ```text
    /// string <str>
    /// ```
    /// Any string `str` which will be displayed be the engine.
    ///
    /// If there is a string command the rest of the line will be interpreted as
    /// `str`.
    ///
    /// ```text
    /// refutation <move_1> <move_2> ... <move_i>
    /// ```
    /// Move `<move_1>` is refuted by the line `<move_2> ... <move_1>`.
    /// `i` can be any number `>= 1`.
    ///
    /// Example: after move `d1h5` is searched, the engine can send
    /// `info refutation d1h5 g6h5` if `g6h5` is the best answer after
    /// `d1h5` or if `g6h5` refutes the move `d1h5`.
    ///
    /// If there is no refutation for `d1h5` found, the engine should just send
    /// `info refutation d1h5`.
    ///
    /// The engine should only send this if the option `UCI_ShowRefutations` is set
    /// to `true`.
    ///
    /// ```text
    /// currline <cpnunr> <move_1> [<move_2> ... <move_i>]
    /// ```
    /// This is the current line the engine is calculating. `cpunr` is the number of
    /// the cpu if the engine is running on more than one cpu. `cpunr = 1,2,3...`.
    ///
    /// if the engine is just using one cpu, `cpunr` can be omitted.
    ///
    /// If `cpunr` is greater than `1`, always send all *k* lines in *k* strings
    /// together.
    ///
    /// The engine should only send this if the option `UCI_ShowCurrLine` is set to
    /// `true`.
    ///
    /// # Default
    ///
    /// The default implementation of this method simply immediately sends a response
    /// of `info <info>`.
    fn info(&self, info: UciInfo) -> Result<()> {
        // info!("using default implementation of UciEngine::info({info:?})");
        self.send_uci_response(UciResponse::<String>::Info(info))
    }

    /// Send the `option` message to `stdout` as follows:
    /// ```text
    /// option name <id> type <t> [default <x>] [min <x>] [max <x>] [var <x>]
    /// ```
    ///
    /// # Protocol Description
    ///
    /// This command tells the GUI which parameters can be changed in the engine.
    ///
    /// This should be sent once at engine startup after the `uci` and the `id`
    /// commands if any parameter can be changed in the engine
    ///
    /// The GUI should parse this and build a dialog for the user to change the
    /// settings. Note that not every option needs to appear in this dialog as some
    /// options like `Ponder`, `UCI_AnalyseMode`, etc. are better handled elsewhere
    /// or are set automatically.
    ///
    /// If the user wants to change some settings, the GUI will send a `setoption`
    /// command to the engine. Note that the GUI need not send the setoption command
    /// when starting the engine for every option if it doesn't want to change the
    /// default value.
    ///
    /// For all allowed combinations see the examples below, as some combinations of
    /// this tokens don't make sense. One string will be sent for each parameter.
    ///
    /// ## Arguments
    ///
    /// ```text
    /// name <id>
    /// ```
    /// The option has the name `<id>`.
    ///
    /// Certain options have a fixed value for `<id>`, which means that the semantics
    /// of this option is fixed. Usually those options should not be displayed in the
    /// normal engine options window of the GUI but get a special treatment.
    /// `Pondering`, for example, should be set automatically when pondering is
    /// enabled or disabled in the GUI options. The same for `UCI_AnalyseMode` which
    /// should also be set automatically by the GUI. All those certain options have
    /// the prefix `UCI_` except for the first 6 options below. If the GUI gets an
    /// unknown option with the prefix `UCI_`, it should just ignore it and not
    /// display it in the engine's options dialog.
    ///
    /// ### Special Options
    ///
    ///   - `id = Hash`, type `spin`
    ///
    /// The value in MB for memory for hash tables can be changed.
    ///
    /// This should be answered with the first `setoptions` command at program boot
    /// if the engine has sent the appropriate `option name Hash` command, which
    /// should be supported by all engines! So the engine should use a very small
    /// hash first as default.
    ///
    ///   - `id = NalimovPath`, type `string`
    ///
    /// This is the path on the hard disk to the Nalimov compressed format.
    ///
    /// Multiple directories can be concatenated with `;`.
    ///
    ///   - `id = NalimovCache`, type `spin`
    ///
    /// This is the size in MB for the cache for the nalimov table bases.
    ///
    /// These last two options should also be present in the initial options exchange
    /// dialog when the engine is booted if the engine supports it.
    ///
    ///   - `id = Ponder`, type `check`
    ///
    /// This means that the engine is able to ponder. The GUI will send this whenever
    /// pondering is possible or not.
    ///
    /// Note: The engine should not start pondering on its own if this is enabled,
    /// this option is only needed because the engine might change its time
    /// management algorithm when pondering is allowed.
    ///
    ///   - `id = OwnBook`, type `check`
    ///
    /// This means that the engine has its own book which is accessed by the engine
    /// itself. if this is set, the engine takes care of the opening book and the GUI
    /// will never execute a move out of its book for the engine. If this is set to
    /// `false` by the GUI, the engine should not access its own book.
    ///
    ///   - `id = MultiPV`, type `spin`
    ///
    /// The engine supports multi best line or *k*-best mode. the default value is
    /// `1`.
    ///
    ///   - `id = UCI_ShowCurrLine`, type `check`, should be `false` by default.
    ///
    /// The engine can show the current line it is calculating. see `info currline`
    /// above.
    ///
    ///   - `id = UCI_ShowRefutations`, type `check`, should be `false` by default.
    ///
    /// The engine can show a move and its refutation in a line.
    /// See `info refutations` above.
    ///
    ///   - `id = UCI_LimitStrength`, type `check`, should be `false` by default,
    ///
    /// The engine is able to limit its strength to a specific Elo number.
    ///
    /// This should always be implemented together with `UCI_Elo`.
    ///
    ///   - `id = UCI_Elo`, type `spin`
    ///
    /// The engine can limit its strength in Elo within this interval.
    ///
    /// If `UCI_LimitStrength` is set to `false`, this value should be ignored. If
    /// `UCI_LimitStrength` is set to true, the engine should play with this
    /// specific strength. This should always be implemented together with
    /// `UCI_LimitStrength`.
    ///
    ///   - `id = UCI_AnalyseMode`, type `check`
    ///
    /// The engine wants to behave differently when analysing or playing a game. For
    /// example when playing it can use some kind of learning. This is set to `false`
    /// if the engine is playing a game, otherwise it is `true`.
    ///
    ///   - `id = UCI_Opponent`, type `string`
    ///
    /// With this command the GUI can send the name, title, elo and if the engine is
    /// playing a human or computer to the engine.
    ///
    /// The format of the string has to be
    /// `[GM | IM | FM | WGM | WIM | none] [<elo> | none] [computer | human] <name>`.
    ///
    /// #### Examples
    ///```text
    /// setoption name UCI_Opponent value GM 2800 human Gary Kasparov
    /// setoption name UCI_Opponent value none none computer Shredder
    /// ```
    ///
    ///   - `id = UCI_EngineAbout`, type `string`
    ///
    /// With this command, the engine tells the GUI information about itself, for
    /// example a license text.
    ///
    /// Usually it doesn't make sense that the GUI changes this text with the
    /// setoption command.
    ///
    /// #### Examples
    ///```text
    /// option name UCI_EngineAbout type string default Shredder by Stefan Meyer-Kahlen, see www.shredderchess.com
    /// ```
    ///
    ///   - `id = UCI_ShredderbasesPath`, type `string`
    ///
    /// This is either the path to the folder on the hard disk containing the
    /// Shredder endgame databases or the path and filename of one Shredder endgame
    /// database.
    ///
    ///   - `id = UCI_SetPositionValue`, type `string`
    ///
    /// The GUI can send this to the engine to tell the engine to use a certain value
    /// in centipawns from white's point of view if evaluating this specific position.
    ///
    /// The string can have the formats: value + fen | clear + fen | clearall
    ///
    /// ### Option Types
    ///
    ///```text
    /// type <t>
    /// ```
    /// The option has type `<t>`. There are 5 different types of options the engine
    /// can send.
    ///
    ///```text
    /// check
    /// ```
    /// A checkbox that can either be `true` or `false`.
    ///
    ///```text
    /// spin
    /// ```
    /// A spin wheel that can be an integer in a certain range.
    ///
    ///```text
    /// combo
    /// ```
    /// A combo box that can have different predefined strings as a value.
    ///
    ///```text
    /// button
    /// ```
    /// A button that can be pressed to send a command to the engine.
    ///
    ///```text
    /// string
    /// ```
    /// A text field that has a string as a value
    ///
    /// An empty string has the value `<empty>`
    ///
    /// ### Option Fields
    ///```text
    /// default <x>
    /// ```
    /// The default value of this parameter is `<x>`.
    ///
    ///```text
    /// min <x>
    /// ```
    /// The minimum value of this parameter is `<x>`.
    ///
    ///```text
    /// max <x>
    /// ```
    /// The maximum value of this parameter is `<x>`.
    ///
    ///```text
    /// var <x>
    /// ```
    /// A predefined value of this parameter is `<x>`.
    ///
    /// ## Examples
    /// Here are 5 strings for each of the 5 possible types of options:
    ///```text
    /// option name Nullmove type check default true
    /// option name Selectivity type spin default 2 min 0 max 4
    /// option name Style type combo default Normal var Solid var Normal var Risky
    /// option name NalimovPath type string default c:\
    /// option name Clear Hash type button
    /// ```
    ///
    /// # Default
    ///
    /// The default implementation of this method does nothing and returns `Ok(())`.
    fn option(&self) -> Result<()> {
        info!("using default implementation of UciEngine::option");
        Ok(())
    }
}

/// Commands that are sent from the GUI to your engine via `stdin`.
///
/// Each of these corresponds to a method on the [`UciEngine`] trait.
/// For example, when your engine receives `uci`, it should execute
/// [`UciEngine::uci`].
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum UciCommand {
    /// The `uci` command.
    ///
    /// See [`UciEngine::uci`] for more details.
    Uci,

    /// The `debug` command.
    ///
    ///  - `on` <=> `true`
    ///  - `off` <=> `false`
    ///
    /// See [`UciEngine::debug`] for more details.
    Debug(bool),

    /// The `isready` command.
    ///
    /// See [`UciEngine::isready`] for more details.
    IsReady,

    /// The `setoption` command.
    ///
    /// See [`UciEngine::setoption`] for more details.
    SetOption { name: String, value: Option<String> },

    /// The `register` command.
    ///
    /// See [`UciEngine::register`] for more details.
    Register {
        name: Option<String>,
        code: Option<String>,
    },

    /// The `ucinewgame` command.
    ///
    /// See [`UciEngine::ucinewgame`] for more details.
    UciNewGame,

    /// The `position` command.
    ///
    /// See [`UciEngine::position`] for more details.
    Position {
        fen: Option<String>,
        moves: Vec<String>,
    },

    /// The `go` command.
    ///
    /// See [`UciEngine::go`] for more details.
    Go(UciSearchOptions),

    /// The `stop` command.
    ///
    /// See [`UciEngine::stop`] for more details.
    Stop,

    /// The `ponderhit` command.
    ///
    /// See [`UciEngine::ponderhit`] for more details.
    PonderHit,

    /// The `quit` command.
    ///
    /// See [`UciEngine::quit`] for more details.
    Quit,
}

impl UciCommand {
    /// Parse a string of input, returning a [`UciCommand`], if possible.
    ///
    /// If not possible, bails with a standard "unknown command X" message.
    pub fn parse(input: &str) -> Result<Self> {
        // Split into the first word and the remaining arguments
        let (first, rest) = input.split_once(' ').unwrap_or((input, ""));
        let rest = rest.trim();

        // Attempt to match to UCI commands
        match first.trim() {
            "uci" => Ok(Self::Uci),
            "debug" => Self::parse_debug(rest),
            "isready" => Ok(Self::IsReady),
            "setoption" => Self::parse_setoption(rest),
            "register" => Self::parse_register(rest),
            "ucinewgame" => Ok(Self::UciNewGame),
            "position" => Self::parse_position(rest),
            "go" => Self::parse_go(rest),
            "stop" => Ok(Self::Stop),
            "ponderhit" => Ok(Self::PonderHit),
            "quit" => Ok(Self::Quit),
            _ => bail!("unknown command {input:?}"),
        }
    }

    /// Attempt to parse the arguments of [`UciCommand::Debug`].
    pub fn parse_debug(args: &str) -> Result<Self> {
        // This one's simple
        match args {
            "on" => Ok(Self::Debug(true)),
            "off" => Ok(Self::Debug(false)),
            _ => bail!("usage: debug [on | off]"),
        }
    }

    /// Attempt to parse the arguments of [`UciCommand::SetOption`].
    pub fn parse_setoption(args: &str) -> Result<Self> {
        let (_, rest) = args
            .split_once("name")
            .ok_or(anyhow!("usage: setoption name <name> [value <value>]"))?;

        let (name, rest) = rest.split_once("value").unwrap_or((rest, "None"));

        let value = match rest {
            "" => bail!("usage: setoption name <name> [value <value>]"),
            "None" => None,
            other => Some(other.trim().to_string()),
        };

        Ok(UciCommand::SetOption {
            name: name.trim().to_string(),
            value,
        })
    }

    /// Attempt to parse the arguments of [`UciCommand::Register`].
    pub fn parse_register(args: &str) -> Result<Self> {
        // A `None` value represents "register later"
        let (name, code) = if args.starts_with("later") {
            (None, None)
        } else if args.starts_with("name") {
            // Otherwise, we need to fetch the name and code.
            let (_, args) = args
                .split_once("name")
                .ok_or(anyhow!("usage: register <later | name <name> code <code>>"))?;

            let (name, code) = args
                .split_once("code")
                .ok_or(anyhow!("usage: register <later | name <name> code <code>>"))?;

            // Ensure a code was supplied
            if code.is_empty() {
                bail!("usage: register <later | name <name> code <code>>");
            }

            (Some(name.trim().to_string()), Some(code.trim().to_string()))
        } else {
            bail!("usage: register <later | name <name> code <code>>");
        };

        Ok(UciCommand::Register { name, code })
    }

    /// Attempt to parse the arguments of [`UciCommand::Position`].
    pub fn parse_position(args: &str) -> Result<Self> {
        let (fen, moves) = if let Some((pos, moves)) = args.split_once("moves") {
            if moves.is_empty() {
                bail!("usage: position <fen <FEN> | startpos> [moves move_1 [move_2 ...]]");
            }
            (
                pos,
                moves.split_whitespace().map(|s| s.to_string()).collect(),
            )
        } else {
            (args, vec![])
        };

        let fen = if fen.starts_with("fen") {
            Some(fen.trim_start_matches("fen").trim().to_string())
        } else if args.starts_with("startpos") {
            None
        } else {
            bail!("usage: position <fen <FEN> | startpos> [moves move_1 [move_2 ...]]");
        };

        Ok(UciCommand::Position { fen, moves })
    }

    /// Attempt to parse the arguments of [`UciCommand::Go`].
    pub fn parse_go(args: &str) -> Result<Self> {
        // Parse an argument
        fn parse<T: FromStr>(arg: &str, input: Option<&str>) -> Result<T> {
            let input = input.ok_or(anyhow!("usage: go {arg} <x>"))?;
            input.parse().or(Err(anyhow!("invalid argument: {input}")))
        }
        // Parse a duration
        fn parse_duration(arg: &str, input: Option<&str>) -> Result<Duration> {
            Ok(Duration::from_millis(parse(arg, input)?))
        }

        // Start with some default options
        let mut opt = UciSearchOptions::default();

        let mut args = args.split_whitespace();
        while let Some(arg) = args.next() {
            match arg {
                "searchmoves" => {
                    warn!("Warning: Current implementation assumes `searchmoves <list of moves>` is the last argument to `go`");
                    // TODO: Can `searchmoves` come before anything else?
                    opt.search_moves = args.map(|arg| arg.to_string()).collect();
                    break;
                }
                // Our engine can think during the opponent's turn
                "ponder" => opt.ponder = true,
                "wtime" => opt.w_time = Some(parse_duration(arg, args.next())?),
                // "wtime" => opt.w_time = Some(Duration::from_millis(args.next().ok_or(anyhow!("usage: go wtime <x>"))?.parse()?)),
                "btime" => opt.b_time = Some(parse_duration(arg, args.next())?),
                "winc" => opt.w_inc = Some(parse_duration(arg, args.next())?),
                "binc" => opt.b_inc = Some(parse_duration(arg, args.next())?),
                "movestogo" => opt.moves_to_go = Some(parse(arg, args.next())?),
                "depth" => opt.depth = Some(parse(arg, args.next())?),
                "nodes" => opt.nodes = Some(parse(arg, args.next())?),
                "mate" => opt.mate = Some(parse(arg, args.next())?),
                "movetime" => opt.move_time = Some(parse_duration(arg, args.next())?),
                "infinite" => opt.infinite = true,
                _ => bail!(
                    "usage: go [searchmoves move_1 [move_2 ...]] [ponder] [wtime <x>] [btime <x>] [winc <x>] [binc <x>] [movestogo <x>] [depth <x>] [nodes <x>] [mate <x>] [movetime <x>] [infinite]"
                ),
            }
        }

        Ok(UciCommand::Go(opt))
    }
}

/// Represents the status of the `copyprotection` and `registration` commands.
#[derive(Debug, Clone)]
pub enum UciCheckingStatus {
    /// The engine is checking the status of `copyprotection` or `registration`.
    Checking,

    /// All is well. Check was successful. No further action needed.
    Ok,

    /// An error occurred when checking `copyprotection` or `registration`
    Error,
}

impl fmt::Display for UciCheckingStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Checking => write!(f, "checking"),
            Self::Ok => write!(f, "ok"),
            Self::Error => write!(f, "error"),
        }
    }
}

/// # Responses sent from the Engine to the GUI via `stdout`.
///
/// These are all the commands the interface gets from the engine.
#[derive(Debug, Clone)]
pub enum UciResponse<T: fmt::Display = String> {
    /// ```text
    /// id name <x>
    /// id author <x>
    /// ```
    Id { name: T, author: T },

    /// ```text
    /// uciok
    /// ```
    UciOk,

    /// ```text
    /// readyok
    /// ```
    ReadyOk,

    /// ```text
    /// bestmove <move_1> [ponder <move_2>]
    /// ```
    BestMove { bestmove: T, ponder: Option<T> },

    /// ```text
    /// copyprotection [checking | ok | error]
    /// ```
    CopyProtection(UciCheckingStatus),

    /// ```text
    /// registration [checking | ok | error]
    /// ```
    Registration(UciCheckingStatus),

    /// ```text
    /// info [depth <x>] [seldepth <x>] [time <x>] [nodes <x>] [pv <move_1> [<move_2> ... <move_i>]] [score [cp <x> | mate <y>] [lowerbound | upperbound]] [currmove <move>] [currmovenumber <x>] [hashfull <x>] [nps <x>] [tbhits <x>] [sbhits <x>] [cpuload <x>] [string <str>] [refutation <move_1> <move_2> [... <move_i>]] [currline [cpunr] <move_1> [... <move_i>]]
    /// ```
    Info(UciInfo),

    /// ```text
    /// option name <id> type <t> [default <x>] [min <x>] [max <x>] [var <x>]
    /// ```
    Option(UciOption<T>),
}

impl<T: fmt::Display> fmt::Display for UciResponse<T> {
    /// Responses are formatted to display appropriately according to the UCI specifications.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Id { name, author } => write!(f, "id name {name}\nid author {author}"),
            Self::UciOk => write!(f, "uciok"),
            Self::ReadyOk => write!(f, "readyok"),
            Self::BestMove { bestmove, ponder } => {
                if let Some(ponder) = ponder {
                    write!(f, "bestmove {bestmove} ponder {ponder}")
                } else {
                    write!(f, "bestmove {bestmove}")
                }
            }
            Self::CopyProtection(status) => write!(f, "copyprotection {status}"),
            Self::Registration(status) => write!(f, "registration {status}"),
            Self::Info(info) => write!(f, "info {info}"),
            Self::Option(opt) => write!(f, "option {opt}"),
        }
    }
}

/*
/// Bounds for the `score` argument of the `info` response.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UciBound {
    /// The score is just a lowerbound.
    Lowerbound,

    /// The score is just an upperbound.
    Upperbound,
}

impl fmt::Display for UciBound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lowerbound => write!(f, "lowerbound"),
            Self::Upperbound => write!(f, "upperbound"),
        }
    }
}

/// Represents the type of score for the `score` argument of the `info` response.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UciScoreType {
    /// The score from the engine's point of view in centipawns.
    Centipawns(i32),

    /// Mate in `<y>` moves (not plies).
    Mate(u32),
}

impl fmt::Display for UciScoreType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Centipawns(x) => write!(f, "cp {x}"),
            Self::Mate(y) => write!(f, "mate {y}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UciScore {
    /// Either `cp` or `mate`.
    score_type: UciScoreType,

    /// Either `lowerbound` or `upperbound`.
    bound: Option<UciBound>,
}

impl fmt::Display for UciScore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(bound) = self.bound {
            write!(f, "{} {bound}", self.score_type)
        } else {
            write!(f, "{}", self.score_type)
        }
    }
}
 */

/// Represents all information that can be sent with the `info` command.
///
/// See [`UciEngine::info`] for more details.
#[derive(Debug, Clone, Default)]
pub struct UciInfo {
    /// ```text
    /// depth <x>
    /// ```
    /// Search depth (in plies).
    pub depth: Option<String>,

    /// ```text
    /// seldepth <x>
    /// ```
    /// Selective search depth (in plies),
    ///
    /// If the engine sends `seldepth` there must also be a `depth` present in the
    /// same string.
    pub seldepth: Option<String>,

    /// ```text
    /// time <x>
    /// ```
    /// The time searched (in ms).
    /// This should be sent together with the `pv`.
    pub time: Option<String>,

    /// ```text
    /// nodes <x>
    /// ```
    /// `<x>` nodes searched.
    /// The engine should send this info regularly.
    pub nodes: Option<String>,

    /// ```text
    /// pv <move_1> [<move_2> ... <move_i>]
    /// ```
    /// The best line found.
    pub pv: Vec<String>,

    /// ```text
    /// multipv <num>
    /// ```
    /// This for the multi pv mode.
    ///
    /// For the best move/pv add `multipv 1` in the string when you send the pv.
    ///
    /// In *k*-best mode always send all k variants in k strings together.
    pub multipv: Option<String>,

    /// ```text
    /// score [cp <x> | mate <y> | lowerbound | upperbound]
    /// ```
    ///
    ///   - `cp <x>`
    /// The score from the engine's point of view in centipawns.
    ///
    ///   - `mate <y>`
    /// Mate in `y` moves, not plies.
    ///
    /// If the engine is getting mated, use negative values for `y`.
    ///
    ///   - `lowerbound`
    /// The score is just a lower bound.
    ///
    ///   - `upperbound`
    /// The score is just an upper bound.
    pub score: Option<String>,

    /// ```text
    /// currmove <move>
    /// ```
    /// Currently searching this move
    pub currmove: Option<String>,

    /// ```text
    /// Currmovenumber <x>
    /// ```
    /// Currently searching move number `x`, for the first move `x` should be `1` not
    /// `0`.
    pub currmovenumber: Option<String>,

    /// ```text
    /// hashfull <x>
    /// ```
    /// The hash is `x` permill full.
    ///
    /// The engine should send this info regularly.
    pub hashfull: Option<String>,

    /// ```text
    /// nps <x>
    /// ```
    /// `x` nodes per second searched.
    ///
    /// The engine should send this info regularly.
    pub nps: Option<String>,

    /// ```text
    /// tbhits <x>
    /// ```
    /// `x` positions where found in the endgame table bases.
    pub tbhits: Option<String>,

    /// ```text
    /// sbhits <x>
    /// ```
    /// `x` positions where found in the shredder endgame databases.
    pub sbhits: Option<String>,

    /// ```text
    /// cpuload x
    /// ```
    /// The cpu usage of the engine is `x` permill.
    pub cpuload: Option<String>,

    /// ```text
    /// string <str>
    /// ```
    /// Any string `str` which will be displayed be the engine.
    ///
    /// If there is a string command the rest of the line will be interpreted as
    /// `str`.
    pub string: Option<String>,

    /// ```text
    /// refutation <move_1> <move_2> ... <move_i>
    /// ```
    /// Move `<move_1>` is refuted by the line `<move_2> ... <move_1>`.
    /// `i` can be any number `>= 1`.
    ///
    /// Example: after move `d1h5` is searched, the engine can send
    /// `info refutation d1h5 g6h5` if `g6h5` is the best answer after
    /// `d1h5` or if `g6h5` refutes the move `d1h5`.
    ///
    /// If there is no refutation for `d1h5` found, the engine should just send
    /// `info refutation d1h5`.
    ///
    /// The engine should only send this if the option `UCI_ShowRefutations` is set
    /// to `true`.
    pub refutation: Vec<String>,

    /// ```text
    /// currline <cpnunr> <move_1> [<move_2> ... <move_i>]
    /// ```
    /// This is the current line the engine is calculating. `cpunr` is the number of
    /// the cpu if the engine is running on more than one cpu. `cpunr = 1,2,3...`.
    ///
    /// if the engine is just using one cpu, `cpunr` can be omitted.
    ///
    /// If `cpunr` is greater than `1`, always send all *k* lines in *k* strings
    /// together.
    ///
    /// The engine should only send this if the option `UCI_ShowCurrLine` is set to
    /// `true`.
    pub currline: Vec<String>,
}

impl UciInfo {
    /// Creates a new, empty, [`UciInfo`] struct.
    pub fn new() -> Self {
        Self::default()
    }

    /// Consumes `self` and adds the provided `depth` value.
    pub fn depth(mut self, depth: impl fmt::Display) -> Self {
        self.depth = Some(depth.to_string());
        self
    }

    /// Consumes `self` and adds the provided `seldepth` value.
    pub fn seldepth(mut self, seldepth: impl fmt::Display) -> Self {
        self.seldepth = Some(seldepth.to_string());
        self
    }

    /// Consumes `self` and adds the provided `time` value.
    pub fn time(mut self, time: impl fmt::Display) -> Self {
        self.time = Some(time.to_string());
        self
    }

    /// Consumes `self` and adds the provided `nodes` value.
    pub fn nodes(mut self, nodes: impl fmt::Display) -> Self {
        self.nodes = Some(nodes.to_string());
        self
    }

    /// Consumes `self` and adds the provided `multipv` value.
    pub fn multipv(mut self, multipv: impl fmt::Display) -> Self {
        self.multipv = Some(multipv.to_string());
        self
    }

    /// Consumes `self` and adds the provided `score` value.
    pub fn score(mut self, score: impl fmt::Display) -> Self {
        self.score = Some(score.to_string());
        self
    }

    /// Consumes `self` and adds the provided `currmove` value.
    pub fn currmove(mut self, currmove: impl fmt::Display) -> Self {
        self.currmove = Some(currmove.to_string());
        self
    }

    /// Consumes `self` and adds the provided `currmovenumber` value.
    pub fn currmovenumber(mut self, currmovenumber: impl fmt::Display) -> Self {
        self.currmovenumber = Some(currmovenumber.to_string());
        self
    }

    /// Consumes `self` and adds the provided `hashfull` value.
    pub fn hashfull(mut self, hashfull: impl fmt::Display) -> Self {
        self.hashfull = Some(hashfull.to_string());
        self
    }

    /// Consumes `self` and adds the provided `nps` value.
    pub fn nps(mut self, nps: impl fmt::Display) -> Self {
        self.nps = Some(nps.to_string());
        self
    }

    /// Consumes `self` and adds the provided `tbhits` value.
    pub fn tbhits(mut self, tbhits: impl fmt::Display) -> Self {
        self.tbhits = Some(tbhits.to_string());
        self
    }

    /// Consumes `self` and adds the provided `sbhits` value.
    pub fn sbhits(mut self, sbhits: impl fmt::Display) -> Self {
        self.sbhits = Some(sbhits.to_string());
        self
    }

    /// Consumes `self` and adds the provided `cpuload` value.
    pub fn cpuload(mut self, cpuload: impl fmt::Display) -> Self {
        self.cpuload = Some(cpuload.to_string());
        self
    }

    /// Consumes `self` and adds the provided `string` value.
    pub fn string(mut self, string: impl fmt::Display) -> Self {
        self.string = Some(string.to_string());
        self
    }

    /// Consumes `self` and adds the provided `pv` value.
    pub fn pv<T: fmt::Display>(mut self, pv: impl IntoIterator<Item = T>) -> Self {
        self.pv = pv.into_iter().map(|x| x.to_string()).collect();
        self
    }

    /// Consumes `self` and adds the provided `refutation` value.
    pub fn refutation<T: fmt::Display>(mut self, refutation: impl IntoIterator<Item = T>) -> Self {
        self.refutation = refutation.into_iter().map(|x| x.to_string()).collect();
        self
    }

    /// Consumes `self` and adds the provided `currline` value.
    pub fn currline<T: fmt::Display>(mut self, currline: impl IntoIterator<Item = T>) -> Self {
        self.currline = currline.into_iter().map(|x| x.to_string()).collect();
        self
    }
}

impl fmt::Display for UciInfo {
    /// An info command will only display data that it has.
    ///
    /// Any `None` fields are not displayed.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(x) = &self.depth {
            write!(f, "depth {x} ")?;
        }
        if let Some(x) = &self.seldepth {
            write!(f, "seldepth {x} ")?
        }
        if let Some(x) = &self.time {
            write!(f, "time {x} ")?;
        }
        if let Some(x) = &self.nodes {
            write!(f, "nodes {x} ")?;
        }
        if let Some(x) = &self.multipv {
            write!(f, "multipv {x} ")?;
        }
        if let Some(x) = &self.score {
            write!(f, "score cp {x} ")?;
        }
        if let Some(x) = &self.currmove {
            write!(f, "currmove {x} ")?;
        }
        if let Some(x) = &self.currmovenumber {
            write!(f, "currmovenumber {x} ")?;
        }
        if let Some(x) = &self.hashfull {
            write!(f, "hashfull {x} ")?;
        }
        if let Some(x) = &self.nps {
            write!(f, "nps {x} ")?;
        }
        if let Some(x) = &self.tbhits {
            write!(f, "tbhits {x} ")?;
        }
        if let Some(x) = &self.sbhits {
            write!(f, "sbhits {x} ")?;
        }
        if let Some(x) = &self.cpuload {
            write!(f, "cpuload {x} ")?;
        }
        if let Some(x) = &self.string {
            write!(f, "string {x} ")?;
        }
        if !self.refutation.is_empty() {
            write!(f, "refutation {}", self.refutation.join(" "))?;
        }
        if !self.currline.is_empty() {
            write!(f, "currline {}", self.currline.join(" "))?;
        }
        if !self.pv.is_empty() {
            write!(f, "pv {}", self.pv.join(" "))?;
        }
        Ok(())
    }
}

/// Represents a UCI-compatible option that can be modified for your Engine.
///
/// See [`UciEngine::option`] for more details.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct UciOption<T: fmt::Display = String> {
    /// Name of the option.
    pub name: T,

    /// What type of option it is.
    pub opt_type: UciOptionType<T>,
}

impl<T: fmt::Display> UciOption<T> {
    /// Create a new [`UciOption`] with the provided name and type.
    pub fn new(name: T, opt_type: UciOptionType<T>) -> Self {
        Self { name, opt_type }
    }

    /// Create a new [`UciOption`] of type [`UciOptionType::Check`].
    pub fn check(name: T, default: bool) -> Self {
        Self::new(name, UciOptionType::Check { default })
    }

    /// Create a new [`UciOption`] of type [`UciOptionType::Spin`].
    pub fn spin(name: T, default: i32, min: i32, max: i32) -> Self {
        Self::new(name, UciOptionType::Spin { default, min, max })
    }

    /// Create a new [`UciOption`] of type [`UciOptionType::Combo`].
    pub fn combo(name: T, default: T, vars: impl IntoIterator<Item = T>) -> Self {
        Self::new(
            name,
            UciOptionType::Combo {
                default,
                vars: vars.into_iter().collect(),
            },
        )
    }

    /// Create a new [`UciOption`] of type [`UciOptionType::Button`].
    pub fn button(name: T) -> Self {
        Self::new(name, UciOptionType::Button)
    }

    /// Create a new [`UciOption`] of type [`UciOptionType::String`].
    pub fn string(name: T, default: T) -> Self {
        Self::new(name, UciOptionType::String { default })
    }
}

impl<T: fmt::Display> fmt::Display for UciOption<T> {
    /// An option is displayed as `name <name> type <type>`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "name {} type {}", self.name, self.opt_type)
    }
}

/// Represents the type of UCI-compatible options your engine can expose to the GUI.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum UciOptionType<T: fmt::Display = String> {
    ///```text
    /// check
    /// ```
    /// A checkbox that can either be `true` or `false`.
    Check { default: bool },

    ///```text
    /// spin
    /// ```
    /// A spin wheel that can be an integer in a certain range.
    Spin { default: i32, min: i32, max: i32 },

    ///```text
    /// combo
    /// ```
    /// A combo box that can have different predefined strings as a value.
    Combo { default: T, vars: Vec<T> },

    ///```text
    /// button
    /// ```
    /// A button that can be pressed to send a command to the engine.
    Button,

    ///```text
    /// string
    /// ```
    /// A text field that has a string as a value
    ///
    /// An empty string has the value `<empty>`
    String { default: T },
}

impl<T: fmt::Display> fmt::Display for UciOptionType<T> {
    /// Option types are displayed like [these examples](https://backscattering.de/chess/uci/#engine-option-examples).
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UciOptionType::Check { default } => write!(f, "check default {default}"),
            UciOptionType::Spin { default, min, max } => {
                write!(f, "spin default {default} min {min} max {max}")
            }
            UciOptionType::Combo { default, vars } => {
                write!(f, "combo default {default}")?;
                for var in vars {
                    write!(f, " var {var}")?;
                }
                Ok(())
            }
            UciOptionType::Button => write!(f, "button"),
            UciOptionType::String { default } => write!(f, "string default {default}"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const DEFAULT_FEN: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

    #[test]
    fn parse_debug() {
        assert_eq!(
            UciCommand::parse("debug on").unwrap(),
            UciCommand::Debug(true)
        );
        assert_eq!(
            UciCommand::parse("debug off").unwrap(),
            UciCommand::Debug(false)
        );
    }

    #[test]
    fn parse_position() {
        assert_eq!(
            UciCommand::parse("position startpos").unwrap(),
            UciCommand::Position {
                fen: None,
                moves: vec![],
            },
        );

        assert_eq!(
            UciCommand::parse(
                "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            )
            .unwrap(),
            UciCommand::Position {
                fen: Some(DEFAULT_FEN.into()),
                moves: vec![],
            },
        );

        assert_eq!(
            UciCommand::parse("position startpos moves e2e4 e7e5").unwrap(),
            UciCommand::Position {
                fen: None,
                moves: vec!["e2e4".into(), "e7e5".into()],
            },
        );

        assert_eq!(UciCommand::parse(
            "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 moves e2e4 e7e5").unwrap(),
            UciCommand::Position {
                fen: Some(DEFAULT_FEN.into()),
                moves: vec!["e2e4".into(), "e7e5".into()],
            },
        );
    }

    #[test]
    fn parse_register() {
        assert_eq!(
            UciCommand::parse("register later").unwrap(),
            UciCommand::Register {
                name: None,
                code: None
            }
        );

        assert_eq!(
            UciCommand::parse("register name Bill Cipher code 42").unwrap(),
            UciCommand::Register {
                name: Some("Bill Cipher".into()),
                code: Some("42".into())
            }
        );
    }

    #[test]
    fn parse_setoption() {
        assert!(
            UciCommand::parse("setoption name Test Option value 0").unwrap()
                == UciCommand::SetOption {
                    name: "Test Option".into(),
                    value: Some("0".into()),
                },
        );
    }

    #[test]
    fn parse_go() {
        assert!(
            UciCommand::parse("go infinite").unwrap()
                == UciCommand::Go(UciSearchOptions {
                    infinite: true,
                    ..Default::default()
                }),
        );

        assert!(
            UciCommand::parse("go btime 30000 wtime 30000 winc 10 binc 42").unwrap()
                == UciCommand::Go(UciSearchOptions {
                    b_time: Some(Duration::from_millis(30_000)),
                    w_time: Some(Duration::from_millis(30_000)),
                    w_inc: Some(Duration::from_millis(10)),
                    b_inc: Some(Duration::from_millis(42)),
                    ..Default::default()
                }),
        );

        assert!(
            UciCommand::parse("go infinite searchmoves e2e4 d2d4").unwrap()
                == UciCommand::Go(UciSearchOptions {
                    infinite: true,
                    search_moves: vec![String::from("e2e4"), String::from("d2d4")],
                    ..Default::default()
                }),
        );
    }

    #[test]
    fn test_option_types() {
        let check: UciResponse<&str> = UciResponse::Option(UciOption::check("Nullmove", true));
        assert_eq!(
            format!("{check}"),
            "option name Nullmove type check default true"
        );

        let spin: UciResponse<&str> = UciResponse::Option(UciOption::spin("Selectivity", 2, 0, 4));
        assert_eq!(
            format!("{spin}"),
            "option name Selectivity type spin default 2 min 0 max 4"
        );

        let combo: UciResponse<&str> = UciResponse::Option(UciOption::combo(
            "Style",
            "Normal",
            ["Solid", "Normal", "Risky"],
        ));
        assert_eq!(
            format!("{combo}"),
            "option name Style type combo default Normal var Solid var Normal var Risky"
        );

        let string: UciResponse<&str> =
            UciResponse::Option(UciOption::string("NalimovPath", "c:\\"));
        assert_eq!(
            format!("{string}"),
            "option name NalimovPath type string default c:\\"
        );

        let button: UciResponse<&str> = UciResponse::Option(UciOption::button("Clear Hash"));
        assert_eq!(format!("{button}"), "option name Clear Hash type button");
    }
}
