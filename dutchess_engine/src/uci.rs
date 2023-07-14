use std::{
    fmt,
    io::{self, Write},
    str::FromStr,
};

use log::{Level, Metadata, Record};

use crate::utils::DEFAULT_FEN;

pub struct UciLogger;

impl log::Log for UciLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        // TODO: Does this need to be adjusted to Trace?
        metadata.level() <= Level::Info
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            // println!("{} - {}", record.level(), record.args());
            println!("{}", record.args())
        }
    }

    fn flush(&self) {
        std::io::stdout().flush().expect("oopsie");
    }
}

pub type UciResult<T> = Result<T, InvalidUciError>;

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
pub struct SearchOptions<'a> {
    moves: Vec<&'a str>,
    w_time: Option<i32>,
    b_time: Option<i32>,
    w_inc: Option<u32>,
    b_inc: Option<u32>,
    moves_to_go: Option<u32>,
    depth: Option<u32>,
    nodes: Option<u64>,
    mate: Option<u32>,
    move_time: Option<u32>,
    infinite: bool,
    ponder: bool,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum InvalidUciError {
    NoInputProvided,
    UnrecognizedCommand,
    NotEnoughArguments,
    InvalidArgument,
}
// impl Error for InvalidUciError {
//     //
// }

/// Represents a type that has implemented the Universal Chess Interface (UCI) protocol.
pub trait UciEngine {
    /// The main I/O loop that handles communication between the engine and other program(s).
    ///
    /// Upon receiving `uci`, your engine should call this function, and will only leave this function
    /// when a `quit` message is received.
    fn uci_loop(&mut self) -> std::io::Result<()> {
        // The engine received `uci`, so follow the appropriate protocol
        self.uci()?;

        // For convenience, import the enum variants.
        use UciCommand::*;

        let mut buffer = String::new();
        loop {
            buffer.clear();
            io::stdin().read_line(&mut buffer)?;

            // Attempt to parse the user input
            let cmd = match UciCommand::new(&buffer) {
                Ok(cmd) => cmd,
                Err(e) => {
                    eprintln!("InputError: {e:?}");
                    println!("Unrecognized command: {buffer}");

                    // UCI protocol states to continue running when invalid input is received.
                    continue;
                }
            };

            // Handle the command appropriately
            match cmd {
                Uci => self.uci()?,
                Debug(status) => self.debug(status),
                IsReady => self.isready()?,
                SetOption(name, value) => self.setoption(&name, &value),
                Register(registration) => self.register(registration),
                UciNewGame => self.ucinewgame(),
                Position(fen, moves) => self.position(fen, moves),
                Go(search_opt) => self.go(search_opt),
                Stop => self.stop(),
                PonderHit => self.ponderhit(),
                Quit => return Ok(self.quit()),
            }
        }
    }

    /* GUI to Engine communication */

    /// Called when the engine receives the `uci` command.
    ///
    /// Tell engine to use the uci (universal chess interface),
    /// this will be sent once as a first command after program boot
    /// to tell the engine to switch to uci mode.
    ///
    /// After receiving the uci command the engine must identify itself with the `id` command
    /// and send the `option` commands to tell the GUI which engine settings the engine supports if any.
    /// After that the engine should send `uciok` to acknowledge the uci mode.
    /// If no uciok is sent within a certain time period, the engine task will be killed by the GUI.
    fn uci(&mut self) -> io::Result<()>;

    /// Called upon `debug [on | off]` where `on = true`.
    ///
    /// Switch the debug mode of the engine on and off.
    ///
    /// In debug mode the engine should send additional infos to the GUI, e.g. with the `info string` command,
    /// to help debugging, e.g. the commands that the engine has received etc.
    /// This mode should be switched off by default and this command can be sent
    /// any time, also when the engine is thinking.
    fn debug(&mut self, status: bool);

    /// Called when the engine receives `isready`.
    ///
    /// This is used to synchronize the engine with the GUI. When the GUI has sent a command or
    /// multiple commands that can take some time to complete,
    /// this command can be used to wait for the engine to be ready again or
    /// to ping the engine to find out if it is still alive.
    ///
    /// E.g. this should be sent after setting the path to the tablebases as this can take some time.
    /// This command is also required once before the engine is asked to do any search
    /// to wait for the engine to finish initializing.
    /// This command must always be answered with `readyok` and can be sent also when the engine is calculating
    /// in which case the engine should also immediately answer with `readyok` without stopping the search.
    fn isready(&self) -> io::Result<()>;

    /// Called when the engine receives `setoption name <name> [ value <value> ]`.
    /// `setoption name <id> [value <x>]`
    ///
    /// This is sent to the engine when the user wants to change the internal parameters
    /// of the engine. For the `button` type no value is needed.
    ///
    /// One string will be sent for each parameter and this will only be sent when the engine is waiting.
    /// The name and value of the option in <id> should not be case sensitive and can include spaces.
    /// The substrings `value` and `name` should be avoided in `<id>` and `<x>` to allow unambiguous parsing,
    /// for example do not use `<name> = draw value`.
    ///
    /// # Examples
    /// Here are some strings for the example below:
    /// * `setoption name Nullmove value true\n`
    /// * `setoption name Selectivity value 3\n`
    /// * `setoption name Style value Risky\n`
    /// * `setoption name Clear Hash\n`
    /// * `setoption name NalimovPath value c:\chess\tb\4;c:\chess\tb\5\n`
    fn setoption(&mut self, name: &str, value: &str);

    /// Called when the engine receives `registration [name <name> code <code> | later]`.
    ///
    /// This is the command to try to register an engine or to tell the engine that registration
    /// will be done later. This command should always be sent if the engine has sent `registration error`
    /// at program startup.
    ///
    /// The following tokens are allowed:
    /// * `later`
    ///    * The user doesn't want to register the engine now.
    /// * `name <x>`
    ///    * The engine should be registered with the name `<x>`
    /// * `code <y>`
    ///    * The engine should be registered with the code `<y>`
    ///
    /// If called with `later`, then the `registration` parameter will be `None`.
    /// Otherwise, it will be `Some(name, code)`.
    ///
    /// # Example:
    ///    `register later`
    ///    `register name Stefan MK code 4359874324`
    fn register(&mut self, registration: Option<(&str, &str)>);

    /// Called when the engine receives `ucinewgame`.
    ///
    /// This is sent to the engine when the next search (started with `position` and `go`) will be from
    /// a different game. This can be a new game the engine should play or a new game it should analyze but
    /// also the next position from a test suite with positions only.
    ///
    /// If the GUI hasn't sent a `ucinewgame` before the first `position` command, the engine shouldn't
    /// expect any further ucinewgame commands as the GUI is probably not supporting the ucinewgame command.
    /// So the engine should not rely on this command even though all new GUIs should support it.
    ///
    /// As the engine's reaction to `ucinewgame` can take some time the GUI should always send `isready`
    /// after `ucinewgame` to wait for the engine to finish its operation.
    fn ucinewgame(&mut self);

    /// Called when the engine receives `position [ startpos | fen <fen> ] move <move1> ... <movei>`
    ///
    /// Set up the position described in FEN string on the internal board and
    /// play the moves on the internal chess board.
    ///
    /// If the game was played  from the start position the string `startpos` will be sent
    /// Note: no `new` command is needed. However, if this position is from a different game than
    /// the last position sent to the engine, the GUI should have sent a `ucinewgame` in between.
    fn position(&mut self, fen: &str, moves: Vec<&str>);

    /// Called when the engine receives `go <search options>`.
    ///
    /// Start calculating on the current position set up with the `position` command.
    ///
    /// There are a number of commands that can follow this command, all will be sent in the same string.
    ///
    /// If one command is not sent its value should be interpreted as it would not influence the search.
    /// * `searchmoves <move1> .... <movei>`
    ///
    /// Restrict search to this moves only
    /// Example: After `position startpos` and `go infinite searchmoves e2e4 d2d4`
    /// 	the engine should only search the two moves `e2e4` and `d2d4` in the initial position.
    ///
    /// * `ponder`
    ///     
    /// Start searching in pondering mode.
    ///
    /// Do not exit the search in ponder mode, even if it's mate!
    /// 	This means that the last move sent in in the position string is the ponder move.
    /// 	The engine can do what it wants to do, but after a `ponderhit` command
    /// 	it should execute the suggested move to ponder on. This means that the ponder move sent by
    /// 	the GUI can be interpreted as a recommendation about which move to ponder. However, if the
    /// 	engine decides to ponder on a different move, it should not display any mainlines as they are
    /// 	likely to be misinterpreted by the GUI because the GUI expects the engine to ponder
    ///    on the suggested move.
    ///
    /// * `wtime <x>`
    ///
    /// White has `x` milliseconds left on the clock
    ///
    /// * `btime <x>`
    ///
    /// Black has `x` milliseconds left on the clock
    ///
    /// * `winc <x>`
    ///
    /// White increment per move in milliseconds if `x > 0`
    ///
    /// * `binc <x>`
    ///
    /// Black increment per move in milliseconds if `x > 0`
    ///
    /// * `movestogo <x>`
    ///  
    /// There are `x` moves to the next time control,
    /// 	this will only be sent if `x > 0`,
    /// 	if you don't get this and get the `wtime` and `btime` it's sudden death
    ///
    /// * `depth <x>`
    ///
    /// search `x` plies only.
    ///
    /// * `nodes <x>`
    ///   
    /// Search `x` nodes only,
    ///
    /// * `mate <x>`
    ///
    /// Search for a mate in `x` moves
    ///
    /// * `movetime <x>`
    ///
    /// Search exactly `x` mseconds
    ///
    /// * `infinite`
    ///
    /// Search until the `stop` command. Do not exit the search without being told so in this mode!
    fn go(&mut self, search_opt: SearchOptions);

    /// Called when the engine receives `stop`.
    ///
    /// Stop calculating as soon as possible,
    /// don't forget the "bestmove" and possibly the "ponder" token when finishing the search
    fn stop(&mut self);

    /// Called when the engine receives `ponderhit`.
    ///
    /// The user has played the expected move. This will be sent if the engine was told to ponder on the same move
    /// the user has played. The engine should continue searching but switch from pondering to normal search.
    fn ponderhit(&self);

    /// Called when the engine receives `quit`.
    ///
    /// Quit the program as soon as possible
    fn quit(&mut self);

    /* Engine to GUI communication */

    /// Send the `id` message to `stdout.`
    ///
    /// Will send two messages- one for `name` and one for `author`.
    ///
    /// # Example
    ///
    /// `id name Shredder\n`
    ///
    /// `id name Stefan MK\n`
    fn id(&self) -> io::Result<()>;

    /// Send the `uciok` message to `stdout.`
    ///
    /// Must be sent after the `id` and optional `option`s to tell the GUI that the engine
    /// has sent all infos and is ready in uci mode.
    fn uciok(&self) -> io::Result<()>;

    /// Send the `readyok` message to `stdout.`
    ///
    /// This must be sent when the engine has received an `isready` command and has
    /// processed all input and is ready to accept new commands now.
    /// It is usually sent after a command that can take some time to be able to wait for the engine,
    /// but it can be used anytime, even when the engine is searching,
    /// and must always be answered with `isready`.
    fn readyok(&self) -> io::Result<()>;

    /// Send the `bestmove <move1> [ ponder <move2> ]` message to `stdout.`
    ///
    /// The engine has stopped searching and found the move <move> best in this position.
    /// the engine can send the move it likes to ponder on. The engine must not start pondering automatically.
    /// this command must always be sent if the engine stops searching, also in pondering mode if there is a
    /// `stop` command, so for every `go` command a `bestmove` command is needed!
    /// Directly before that the engine should send a final "info" command with the final search information,
    /// the the GUI has the complete statistics about the last search.
    fn bestmove(&self) -> io::Result<()>;

    /// Send the `copyprotection [ checking | ok | error ]` message to `stdout.`
    fn copyprotection(&self) -> io::Result<()>;

    /// Send the `registration[ checking | ok | error ]` message to `stdout.`
    fn registeration(&self) -> io::Result<()>;

    /// Send the `info [ STUFF ]` message to `stdout.`
    fn info(&self) -> io::Result<()>;

    /// Send the `option [ STUFF ]` message to `stdout.`
    fn option(&self) -> io::Result<()>;
}

/// # UCI Commands (GUI to Engine)
///
/// These are all the commands the engine gets from the interface.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum UciCommand<'a> {
    Uci,

    // on/off
    Debug(bool),

    IsReady,

    // <name> <value>
    SetOption(&'a str, &'a str),

    // `None` -> `later`
    // `Some` -> `(name, code)`
    Register(Option<(&'a str, &'a str)>),

    UciNewGame,

    // <fen> <moves>
    Position(&'a str, Vec<&'a str>),

    Go(SearchOptions<'a>),

    Stop,

    PonderHit,

    Quit,
}

impl<'a> UciCommand<'a> {
    pub fn new(input: &'a str) -> UciResult<Self> {
        // Leading and trailing whitespace is ignored
        let input = input.trim();

        // Check if the provided input has multiple components to it
        if let Some((first, rest)) = input.split_once(' ') {
            let rest = rest.trim();
            match first.trim() {
                "debug" => Self::parse_debug(rest),
                "setoption" => Self::parse_setoption(rest),
                "register" => Self::parse_register(rest),
                "position" => Self::parse_position(rest),
                "go" => Self::parse_go(rest),
                _ => Err(InvalidUciError::UnrecognizedCommand),
            }
        } else {
            // If not, it may be a single-worded command, so check that
            match input {
                "uci" => Ok(Self::Uci),
                "isready" => Ok(Self::IsReady),
                "ucinewgame" => Ok(Self::UciNewGame),
                "stop" => Ok(Self::Stop),
                "ponderhit" => Ok(Self::PonderHit),
                "quit" => Ok(Self::Quit),
                _ => Err(InvalidUciError::UnrecognizedCommand),
            }
        }
    }

    fn parse_debug(args: &'a str) -> UciResult<Self> {
        // This one's simple
        match args {
            "on" => Ok(Self::Debug(true)),
            "off" => Ok(Self::Debug(false)),
            _ => Err(InvalidUciError::InvalidArgument),
        }
    }

    fn parse_setoption(args: &'a str) -> UciResult<Self> {
        // First, we fetch the `value` field
        let (name, value) = args
            .split_once("value")
            .ok_or(InvalidUciError::NotEnoughArguments)?;

        // Then, we fetch the `name` of the option
        let (_, name) = name
            .split_once("name")
            .ok_or(InvalidUciError::NotEnoughArguments)?;

        Ok(UciCommand::SetOption(name.trim(), value.trim()))
    }

    fn parse_register(args: &'a str) -> UciResult<Self> {
        // A `None` value represents "register later"
        let registration = if args.starts_with("later") {
            None
        } else if args.starts_with("name") {
            // Otherwise, we need to fetch the name and code.
            let (_, args) = args
                .split_once("name")
                .ok_or(InvalidUciError::NotEnoughArguments)?;
            let (name, code) = args
                .split_once("code")
                .ok_or(InvalidUciError::NotEnoughArguments)?;
            Some((name.trim(), code.trim()))
        } else {
            return Err(InvalidUciError::InvalidArgument);
        };

        Ok(UciCommand::Register(registration))
    }

    fn parse_position(args: &'a str) -> UciResult<Self> {
        // First, we check if there are any moves provided
        let (pos, moves) = if let Some((pos, moves)) = args.split_once("moves") {
            (pos, moves.split_whitespace().collect())
        } else {
            (args, vec![])
        };

        // Now, we parse the position, which is either `startpos` or `fen <str>`
        let pos = if pos.starts_with("fen") {
            let (_, fen) = pos
                .split_once("fen")
                .ok_or(InvalidUciError::NotEnoughArguments)?;
            fen.trim()
        } else if pos.starts_with("startpos") {
            DEFAULT_FEN
        } else {
            return Err(InvalidUciError::InvalidArgument);
        };

        Ok(UciCommand::Position(pos, moves))
    }

    fn parse_go(args: &'a str) -> UciResult<Self> {
        let mut opt = SearchOptions::default();

        // reduces redundant code
        fn parse<T: FromStr>(input: Option<&str>) -> UciResult<T> {
            input
                .ok_or(InvalidUciError::NotEnoughArguments)?
                .parse()
                .map_err(|_| InvalidUciError::InvalidArgument)
        }

        let mut args = args.split_whitespace();
        while let Some(arg) = args.next() {
            match arg {
                "searchmoves" => {
                    // TODO: Can `searchmoves` come before anything else?
                    opt.moves = args.collect();
                    break;
                }
                "ponder" => opt.ponder = true,
                "wtime" => opt.w_time = Some(parse(args.next())?),
                "btime" => opt.b_time = Some(parse(args.next())?),
                "winc" => opt.w_inc = Some(parse(args.next())?),
                "binc" => opt.b_inc = Some(parse(args.next())?),
                "movestogo" => opt.moves_to_go = Some(parse(args.next())?),
                "depth" => opt.depth = Some(parse(args.next())?),
                "nodes" => opt.nodes = Some(parse(args.next())?),
                "mate" => opt.mate = Some(parse(args.next())?),
                "movetime" => opt.move_time = Some(parse(args.next())?),
                "infinite" => opt.infinite = true,
                _ => return Err(InvalidUciError::InvalidArgument),
            }
        }

        Ok(UciCommand::Go(opt))
    }
}

/// # Responses sent from the Engine to the GUI via `stdout`.
///
/// These are all the commands the interface gets from the engine.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum UciResponse<'a> {
    /// `id`
    ///
    /// * `name <x>`
    ///
    /// This must be sent after receiving the `uci` command to identify the engine,
    ///
    /// e.g. `id name Shredder X.Y\n`
    ///
    /// * `author <x>`
    ///
    /// This must be sent after receiving the `uci` command to identify the engine,
    ///
    /// e.g. `id author Stefan MK\n`
    Id(&'a str /* name */, &'a str /* author */),

    /// `uciok`
    ///
    /// Must be sent after the id and optional options to tell the GUI that the engine
    /// has sent all infos and is ready in uci mode.
    UciOk,

    /// `readyok`
    ///
    /// This must be sent when the engine has received an `isready` command and has
    /// processed all input and is ready to accept new commands now.
    /// It is usually sent after a command that can take some time to be able to wait for the engine,
    /// but it can be used anytime, even when the engine is searching,
    /// and must always be answered with `isready`.
    ReadyOk,

    /// `bestmove <move1> [ ponder <move2> ]`
    ///
    /// The engine has stopped searching and found the move <move> best in this position.
    /// the engine can send the move it likes to ponder on. The engine must not start pondering automatically.
    /// this command must always be sent if the engine stops searching, also in pondering mode if there is a
    /// `stop` command, so for every `go` command a `bestmove` command is needed!
    /// Directly before that the engine should send a final "info" command with the final search information,
    /// the the GUI has the complete statistics about the last search.
    BestMove(String /* bestmove */, Option<String> /* ponder */),

    /// `copyprotection`
    ///
    /// this is needed for copy-protected engines. After the uciok command the engine can tell the GUI,
    /// that it will check the copy protection now. This is done by `copyprotection checking`.
    /// If the check is ok the engine should send `copyprotection ok`, otherwise `copyprotection error`.
    /// If there is an error the engine should not function properly but should not quit alone.
    /// If the engine reports `copyprotection error` the GUI should not use this engine
    /// and display an error message instead!
    ///
    /// The code in the engine can look like this:
    /// ```
    /// // TellGUI("copyprotection checking\n");
    /// // ... check the copy protection here ...
    /// // if(ok)
    /// //     TellGUI("copyprotection ok\n");
    /// // else
    /// //     TellGUI("copyprotection error\n");
    /// ```
    CopyProtection(&'static str),

    /// `registration`
    ///
    /// This is needed for engines that need a username and/or a code to function with all features.
    /// Analog to the `copyprotection` command the engine can send `registration checking`
    /// after the uciok command followed by either `registration ok` or `registration error`.
    /// Also after every attempt to register the engine it should answer with `registration checking`
    /// and then either `registration ok` or `registration error`.
    /// In contrast to the `copyprotection` command, the GUI can use the engine after the engine has
    /// reported an error, but should inform the user that the engine is not properly registered
    /// and might not use all its features.
    /// In addition the GUI should offer to open a dialog to
    /// enable registration of the engine. To try to register an engine the GUI can send
    /// the `register` command.
    /// The GUI has to always answer with the `register` command if the engine sends `registration error`
    /// at engine startup (this can also be done with `register later`)
    /// and tell the user somehow that the engine is not registered.
    /// This way the engine knows that the GUI can deal with the registration procedure and the user
    /// will be informed that the engine is not properly registered.
    Registration(&'static str),

    /// `info`
    ///
    /// The engine wants to send information to the GUI. This should be done whenever one of the info has changed.
    /// The engine can send only selected infos or multiple infos with one info command,
    ///
    /// e.g. `info currmove e2e4 currmovenumber 1` or `info depth 12 nodes 123456 nps 100000`.
    ///
    /// Also all infos belonging to the pv should be sent together
    ///
    /// e.g. `info depth 2 score cp 214 time 1242 nodes 2124 nps 34928 pv e2e4 e7e5 g1f3`
    ///
    /// I suggest to start sending `currmove`, `currmovenumber`, `currline` and `refutation` only after one second
    /// to avoid too much traffic.
    ///
    /// Additional info:
    /// * `depth <x>`
    ///
    /// Search depth in plies.
    ///
    /// * `seldepth <x>`
    ///
    /// Selective search depth in plies.
    /// If the engine sends `seldepth` there must also be a `depth` present in the same string.
    ///
    /// * `time <x>`
    ///
    /// The time searched in ms, this should be sent together with the `pv`.
    ///
    /// * nodes <x>
    ///
    /// `x` nodes searched, the engine should send this info regularly.
    ///
    /// * `pv <move1> ... <movei>`
    ///
    /// The best line found.
    ///
    /// * `multipv <num>`
    /// This for the multi pv mode.
    /// 	for the best move/pv add `multipv 1` in the string when you send the `pv`.
    /// 	in `k`-best mode always send all `k` variants in `k` strings together.
    ///
    /// * `score`
    /// 	* cp <x>
    /// 		the score from the engine's point of view in centipawns.
    /// 	* mate <y>
    /// 		mate in y moves, not plies.
    /// 		If the engine is getting mated use negative values for y.
    /// 	* lowerbound
    ///       the score is just a lower bound.
    /// 	* upperbound
    /// 	   the score is just an upper bound.
    /// * currmove <move>
    /// 	currently searching this move
    /// * currmovenumber <x>
    /// 	currently searching move number x, for the first move x should be 1 not 0.
    /// * hashfull <x>
    /// 	the hash is x permill full, the engine should send this info regularly
    /// * nps <x>
    /// 	x nodes per second searched, the engine should send this info regularly
    /// * tbhits <x>
    /// 	x positions where found in the endgame table bases
    /// * sbhits <x>
    /// 	x positions where found in the shredder endgame databases
    /// * cpuload <x>
    /// 	the cpu usage of the engine is x permill.
    /// * string <str>
    /// 	any string str which will be displayed be the engine,
    /// 	if there is a string command the rest of the line will be interpreted as <str>.
    /// * refutation <move1> <move2> ... <movei>
    ///    move <move1> is refuted by the line <move2> ... <movei>, i can be any number >= 1.
    ///    Example: after move d1h5 is searched, the engine can send
    ///    "info refutation d1h5 g6h5"
    ///    if g6h5 is the best answer after d1h5 or if g6h5 refutes the move d1h5.
    ///    if there is no refutation for d1h5 found, the engine should just send
    ///    "info refutation d1h5"
    /// 	The engine should only send this if the option "UCI_ShowRefutations" is set to true.
    /// * currline <cpunr> <move1> ... <movei>
    ///    this is the current line the engine is calculating. <cpunr> is the number of the cpu if
    ///    the engine is running on more than one cpu. <cpunr> = 1,2,3....
    ///    if the engine is just using one cpu, <cpunr> can be omitted.
    ///    If <cpunr> is greater than 1, always send all k lines in k strings together.
    /// 	The engine should only send this if the option "UCI_ShowCurrLine" is set to true.
    Info(UciInfo),

    /// `option`
    ///
    /// This command tells the GUI which parameters can be changed in the engine.
    /// This should be sent once at engine startup after the "uci" and the "id" commands
    /// if any parameter can be changed in the engine.
    /// The GUI should parse this and build a dialog for the user to change the settings.
    /// Note that not every option needs to appear in this dialog as some options like
    /// "Ponder", "UCI_AnalyseMode", etc. are better handled elsewhere or are set automatically.
    /// If the user wants to change some settings, the GUI will send a "setoption" command to the engine.
    /// Note that the GUI need not send the setoption command when starting the engine for every option if
    /// it doesn't want to change the default value.
    /// For all allowed combinations see the examples below,
    /// as some combinations of this tokens don't make sense.
    /// One string will be sent for each parameter.
    ///
    /// * name <id>
    /// 	The option has the name id.
    /// 	Certain options have a fixed value for <id>, which means that the semantics of this option is fixed.
    /// 	Usually those options should not be displayed in the normal engine options window of the GUI but
    /// 	get a special treatment. "Pondering" for example should be set automatically when pondering is
    /// 	enabled or disabled in the GUI options. The same for "UCI_AnalyseMode" which should also be set
    /// 	automatically by the GUI. All those certain options have the prefix "UCI_" except for the
    /// 	first 6 options below. If the GUI gets an unknown Option with the prefix "UCI_", it should just
    /// 	ignore it and not display it in the engine's options dialog.
    /// 	* <id> = Hash, type is spin
    /// 		the value in MB for memory for hash tables can be changed,
    /// 		this should be answered with the first "setoptions" command at program boot
    /// 		if the engine has sent the appropriate "option name Hash" command,
    /// 		which should be supported by all engines!
    /// 		So the engine should use a very small hash first as default.
    /// 	* <id> = NalimovPath, type string
    /// 		this is the path on the hard disk to the Nalimov compressed format.
    /// 		Multiple directories can be concatenated with ";"
    /// 	* <id> = NalimovCache, type spin
    /// 		this is the size in MB for the cache for the nalimov table bases
    /// 		These last two options should also be present in the initial options exchange dialog
    /// 		when the engine is booted if the engine supports it
    /// 	* <id> = Ponder, type check
    /// 		this means that the engine is able to ponder.
    /// 		The GUI will send this whenever pondering is possible or not.
    /// 		Note: The engine should not start pondering on its own if this is enabled, this option is only
    /// 		needed because the engine might change its time management algorithm when pondering is allowed.
    /// 	* <id> = OwnBook, type check
    /// 		this means that the engine has its own book which is accessed by the engine itself.
    /// 		if this is set, the engine takes care of the opening book and the GUI will never
    /// 		execute a move out of its book for the engine. If this is set to false by the GUI,
    /// 		the engine should not access its own book.
    /// 	* <id> = MultiPV, type spin
    /// 		the engine supports multi best line or k-best mode. the default value is 1
    /// 	* <id> = UCI_ShowCurrLine, type check, should be false by default,
    /// 		the engine can show the current line it is calculating. see "info currline" above.
    /// 	* <id> = UCI_ShowRefutations, type check, should be false by default,
    /// 		the engine can show a move and its refutation in a line. see "info refutations" above.
    /// 	* <id> = UCI_LimitStrength, type check, should be false by default,
    /// 		The engine is able to limit its strength to a specific Elo number,
    /// 	   This should always be implemented together with "UCI_Elo".
    /// 	* <id> = UCI_Elo, type spin
    /// 		The engine can limit its strength in Elo within this interval.
    /// 		If UCI_LimitStrength is set to false, this value should be ignored.
    /// 		If UCI_LimitStrength is set to true, the engine should play with this specific strength.
    /// 	   This should always be implemented together with "UCI_LimitStrength".
    /// 	* <id> = UCI_AnalyseMode, type check
    /// 	   The engine wants to behave differently when analysing or playing a game.
    /// 	   For example when playing it can use some kind of learning.
    /// 	   This is set to false if the engine is playing a game, otherwise it is true.
    /// 	 * <id> = UCI_Opponent, type string
    /// 	   With this command the GUI can send the name, title, elo and if the engine is playing a human
    /// 	   or computer to the engine.
    /// 	   The format of the string has to be [GM|IM|FM|WGM|WIM|none] [<elo>|none] [computer|human] <name>
    /// 	   Examples:
    /// 	   "setoption name UCI_Opponent value GM 2800 human Gary Kasparov"
    /// 	   "setoption name UCI_Opponent value none none computer Shredder"
    /// 	 * <id> = UCI_EngineAbout, type string
    /// 	   With this command, the engine tells the GUI information about itself, for example a license text,
    /// 	   usually it doesn't make sense that the GUI changes this text with the setoption command.
    /// 	   Example:
    /// 		"option name UCI_EngineAbout type string default Shredder by Stefan Meyer-Kahlen, see www.shredderchess.com"
    /// 	* <id> = UCI_ShredderbasesPath, type string
    /// 		this is either the path to the folder on the hard disk containing the Shredder endgame databases or
    /// 		the path and filename of one Shredder endgame datbase.
    ///    * <id> = UCI_SetPositionValue, type string
    ///       the GUI can send this to the engine to tell the engine to use a certain value in centipawns from white's
    ///       point of view if evaluating this specifix position.
    ///       The string can have the formats:
    ///       <value> + <fen> | clear + <fen> | clearall
    ///
    /// * `type <t>`
    /// 	The option has type t.
    /// 	There are 5 different types of options the engine can send
    /// 	* check
    /// 		a checkbox that can either be true or false
    /// 	* spin
    /// 		a spin wheel that can be an integer in a certain range
    /// 	* combo
    /// 		a combo box that can have different predefined strings as a value
    /// 	* button
    /// 		a button that can be pressed to send a command to the engine
    /// 	* string
    /// 		a text field that has a string as a value,
    /// 		an empty string has the value "<empty>"
    /// * default <x>
    /// 	the default value of this parameter is x
    /// * min <x>
    /// 	the minimum value of this parameter is x
    /// * max <x>
    /// 	the maximum value of this parameter is x
    /// * var <x>
    /// 	a predefined value of this parameter is x
    ///
    /// Examples:
    ///    Here are 5 strings for each of the 5 possible types of options
    ///    "option name Nullmove type check default true\n"
    ///      "option name Selectivity type spin default 2 min 0 max 4\n"
    ///    "option name Style type combo default Normal var Solid var Normal var Risky\n"
    ///    "option name NalimovPath type string default c:\\n"
    ///    "option name Clear Hash type button\n"
    Option(UciOption<'a>),
}

impl<'a> UciResponse<'a> {
    pub fn send(&self) -> io::Result<()> {
        // let resp = format!("{response}\n");
        let resp = self.to_string();
        let mut handle = io::stdout().lock();
        handle.write_all(resp.as_ref())?;
        handle.flush()
    }
}

impl<'a> fmt::Display for UciResponse<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let response = match self {
            Self::Id(name, author) => {
                format!("id name {name}\nid author {author}")
            }
            Self::UciOk => format!("uciok"),
            Self::ReadyOk => format!("readyok"),
            Self::BestMove(bestmove, ponder) => {
                if let Some(ponder) = ponder {
                    format!("bestmove {bestmove} ponder {ponder}")
                } else {
                    format!("bestmove {bestmove}")
                }
            }
            Self::CopyProtection(status) => {
                format!("copyprotection {status}")
            }
            Self::Registration(status) => {
                format!("registration {status}")
            }
            Self::Info(info) => {
                format!("info {info}")
            }
            Self::Option(opt) => {
                format!("option {opt}")
            }
        };
        write!(f, "{response}\n")
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct UciInfo {
    depth: Option<String>,
    seldepth: Option<String>,
    time: Option<String>,
    nodes: Option<String>,
    pv: Vec<String>,
    multipv: Option<String>,
    score: Option<String>,
    currmove: Option<String>,
    currmovenumber: Option<String>,
    hashfull: Option<String>,
    nps: Option<String>,
    tbhits: Option<String>,
    sbhits: Option<String>,
    cpuload: Option<String>,
    string: Option<String>,
    refutation: Vec<String>,
    currline: Vec<String>,
}

impl fmt::Display for UciInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(depth) = &self.depth {
            f.write_str("depth ")?;
            f.write_str(&depth)?;
        }

        f.write_str("\n")

        // write!(f, "name {} type {}", self.name, self.opt_type)
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct UciOption<'a> {
    name: &'a str,
    opt_type: UciOptionType,
    default: Option<String>,
    min: Option<String>,
    max: Option<String>,
    var: Option<String>,
}

impl<'a> fmt::Display for UciOption<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "name {} type {}", self.name, self.opt_type)
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum UciOptionType {
    Check,
    Spin,
    Combo,
    Button,
    String,
}

impl fmt::Display for UciOptionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            _ => todo!(),
        }

        // write!(f, "name {} type {}", self.name, self.opt_type)
    }
}

// Examples:
// ---
//
// This is how the communication when the engine boots can look like:
//
// GUI     engine
//
// // tell the engine to switch to UCI mode
// uci
//
// // engine identify
//       id name Shredder
// 		id author Stefan MK
//
// // engine sends the options it can change
// // the engine can change the hash size from 1 to 128 MB
// 		option name Hash type spin default 1 min 1 max 128
//
// // the engine supports Nalimov endgame tablebases
// 		option name NalimovPath type string default <empty>
// 		option name NalimovCache type spin default 1 min 1 max 32
//
// // the engine can switch off Nullmove and set the playing style
// 	   option name Nullmove type check default true
//   		option name Style type combo default Normal var Solid var Normal var Risky
//
// // the engine has sent all parameters and is ready
// 		uciok
//
// // Note: here the GUI can already send a "quit" command if it just wants to find out
// //       details about the engine, so the engine should not initialize its internal
// //       parameters before here.
// // now the GUI sets some values in the engine
// // set hash to 32 MB
// setoption name Hash value 32
//
// // init tbs
// setoption name NalimovCache value 1
// setoption name NalimovPath value d:\tb;c\tb
//
// // waiting for the engine to finish initializing
// // this command and the answer is required here!
// isready
//
// // engine has finished setting up the internal values
// 		readyok
//
// // now we are ready to go
//
// // if the GUI is supporting it, tell the engine that is is
// // searching on a game that it hasn't searched on before
// ucinewgame
//
// // if the engine supports the "UCI_AnalyseMode" option and the next search is supposed to
// // be an analysis, the GUI should set "UCI_AnalyseMode" to true if it is currently
// // set to false with this engine
// setoption name UCI_AnalyseMode value true
//
// // tell the engine to search infinite from the start position after 1.e4 e5
// position startpos moves e2e4 e7e5
// go infinite
//
// // the engine starts sending infos about the search to the GUI
// // (only some examples are given)
//
// 		info depth 1 seldepth 0
// 		info score cp 13  depth 1 nodes 13 time 15 pv f1b5
// 		info depth 2 seldepth 2
// 		info nps 15937
// 		info score cp 14  depth 2 nodes 255 time 15 pv f1c4 f8c5
// 		info depth 2 seldepth 7 nodes 255
// 		info depth 3 seldepth 7
// 		info nps 26437
// 		info score cp 20  depth 3 nodes 423 time 15 pv f1c4 g8f6 b1c3
// 		info nps 41562
// 		....
//
// // here the user has seen enough and asks to stop the searching
// stop
//
// // the engine has finished searching and is sending the bestmove command
// // which is needed for every "go" command sent to tell the GUI
// // that the engine is ready again
// 		bestmove g1f3 ponder d8f6

#[cfg(test)]
mod test {
    use super::*;

    fn parse_test(input: &str, expected: UciCommand) {
        let parsed = UciCommand::new(input);
        assert!(parsed.is_ok());
        assert_eq!(parsed.unwrap(), expected);
    }
    #[test]
    fn parse_lone_commands() {
        parse_test("uci", UciCommand::Uci);
        parse_test("isready", UciCommand::IsReady);
        parse_test("ucinewgame", UciCommand::UciNewGame);
        parse_test("stop", UciCommand::Stop);
        parse_test("ponderhit", UciCommand::PonderHit);
        parse_test("quit", UciCommand::Quit);
    }

    #[test]
    fn parse_debug() {
        parse_test("debug on", UciCommand::Debug(true));
        parse_test("debug off", UciCommand::Debug(false));
    }

    #[test]
    fn parse_position() {
        parse_test(
            "position startpos",
            UciCommand::Position(DEFAULT_FEN, vec![]),
        );

        parse_test(
            "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            UciCommand::Position(DEFAULT_FEN, vec![]),
        );

        parse_test(
            "position startpos moves e2e4 e7e5",
            UciCommand::Position(DEFAULT_FEN, vec!["e2e4", "e7e5"]),
        )
    }

    #[test]
    fn parse_register() {
        parse_test("register later", UciCommand::Register(None));

        parse_test(
            "register name Bill Cipher code 42",
            UciCommand::Register(Some(("Bill Cipher", "42"))),
        );
    }

    #[test]
    fn parse_setoption() {
        parse_test(
            "setoption name Test Option value 0",
            UciCommand::SetOption("Test Option", "0"),
        );
    }

    #[test]
    fn parse_go() {
        parse_test(
            "go infinite",
            UciCommand::Go(SearchOptions {
                infinite: true,
                ..Default::default()
            }),
        );

        parse_test(
            "go btime 30000 wtime 30000 winc 10 binc 42",
            UciCommand::Go(SearchOptions {
                b_time: Some(30_000),
                w_time: Some(30_000),
                w_inc: Some(10),
                b_inc: Some(42),
                ..Default::default()
            }),
        );

        parse_test(
            "go infinite searchmoves e2e4 d2d4",
            UciCommand::Go(SearchOptions {
                infinite: true,
                moves: vec!["e2e4", "d2d4"],
                ..Default::default()
            }),
        );
    }
}
