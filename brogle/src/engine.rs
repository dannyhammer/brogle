use std::{
    io::{self, Read, Write},
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Sender},
        Arc, RwLock,
    },
    time::Duration,
};

use anyhow::{bail, Context, Result};
use brogle_core::{print_perft, BitBoard, Game, Move, Position, Tile, FEN_STARTPOS};
use log::{error, warn};
use threadpool::ThreadPool;

use super::{
    protocols::{UciCommand, UciEngine, UciOption, UciResponse, UciSearchOptions},
    search::{Search, SearchResult},
    Evaluator,
};

/// Represents the possible communication protocols supported by this engine.
///
/// Presently, only [UCI](https://backscattering.de/chess/uci/) is supported.
#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash, Default)]
enum EngineProtocol {
    #[default]
    Uci,
}

/// A chess engine responds to inputs (such as from a GUI or terminal) and
/// responds with computed outputs. The most common modern protocol is UCI.
///
/// The engine does not keep track of game state in the way you might think.
/// It does not have a "game loop" that iterates until the game ends.
/// Rather, it holds the current state of the board, and contains search-related
/// information.
///
/// The use case of the engine is analogous to a function. You provide the engine
/// with an initial board state (and possibly a list of moves to apply to that
/// state), then tell it to analyze the board and find the most optimal move that
/// can be made, given some search parameters. It will then yield what it thinks
/// is the best move to make.
#[derive(Debug)]
pub struct Engine {
    /// State of the game, including castling rights, piece placement, move history,
    /// etc.
    game: Game,

    // /// Transposition table for game states.
    // ttable: TranspositionTable,
    //
    /// Whether to display additional information in `info` commands.
    ///
    /// Defaults to `false`.`
    debug: Arc<AtomicBool>,

    /// Atomic boolean to determine whether the engine is currently running a search.
    is_searching: Arc<AtomicBool>,

    /// Result of the last-executed search.
    search_result: Arc<RwLock<SearchResult>>,

    /// Pool for spawning search and input threads.
    pool: ThreadPool,

    /// Handles sending events to the internal event pump.
    sender: Option<Sender<EngineCommand>>,
    //
    // /// List of available configuration options
    // options: todo!(),
}

impl Engine {
    /// Construct a new [`Engine`] with default parameters.
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct a new [`Engine`] from provided FEN string.
    pub fn from_fen(fen: &str) -> Result<Self> {
        Game::from_fen(fen).map(|game| Self {
            game,
            ..Default::default()
        })
    }

    /// Main entrypoint of the engine.
    ///
    /// This function launches the engine and awaits user input via `stdin`.
    pub fn run(&mut self) -> Result<()> {
        // Print some metadata about the engine
        let name = env!("CARGO_PKG_NAME");
        let version = env!("CARGO_PKG_VERSION");
        let authors = env!("CARGO_PKG_AUTHORS").replace(':', ", "); // Split multiple authors by comma-space
        println!("{name} {version} by {authors}");

        // Create (and store) the channel(s) for communication
        let (sender, receiver) = mpsc::channel();
        self.sender = Some(sender.clone());

        // Spin up a thread for handling user/GUI input
        self.pool.execute(move || {
            if let Err(err) = Self::user_input_handler(sender) {
                error!("{err}");
            }
        });

        // Main event loop: Handle inputs from various sources
        for cmd in receiver {
            match cmd {
                EngineCommand::Help => self.help(),
                EngineCommand::Show => self.show(),
                EngineCommand::History => self.history(),
                EngineCommand::Perft {
                    depth,
                    pretty,
                    split,
                } => self.perft(depth, pretty, split),
                EngineCommand::Fen(fen) => self.fen(fen)?,
                EngineCommand::Eval(game) => self.eval(*game),
                EngineCommand::Moves(from, debug) => self.moves(from, debug),
                EngineCommand::Bench => todo!("Implement `bench` command"),
                EngineCommand::MakeMove(moves) => self.make_move(moves)?,
                EngineCommand::Undo => self.undo(),
                EngineCommand::Option(name) => self.option(name),
                EngineCommand::UciCommand(uci_cmd) => self.execute_uci_command(uci_cmd)?,
                EngineCommand::UciResponse(uci_resp) => self.send_uci_response(*uci_resp)?,
                EngineCommand::Exit => break,
            }
        }

        Ok(())
    }

    /// Enter a perpetual loop to handle input over `stdin` from the user/GUI.
    ///
    /// This function only exits if an error occurs it receives the input 'quit'  
    fn user_input_handler(sender: Sender<EngineCommand>) -> Result<()> {
        let mut buffer = String::with_capacity(2048);

        loop {
            // Clear the buffer, read input, and trim the trailing newline
            buffer.clear();
            let bytes = io::stdin()
                .read_line(&mut buffer)
                .context("Failed to read line when parsing UCI commands")?;
            let buf = buffer.trim();

            // For ctrl + d
            if 0 == bytes {
                warn!("Engine received input of 0 bytes and is quitting");
                sender
                    .send(EngineCommand::Exit)
                    .context("Received empty input from stdin")?;
                return Ok(());
            }

            // Ignore empty lines
            if buf.is_empty() {
                continue;
            }

            // Attempt to parse the user input
            let cmd = match Self::parse_command(buf) {
                Ok(cmd) => cmd,
                Err(err) => {
                    // UCI protocol states to continue running when invalid input is received.
                    eprintln!("{err}");
                    continue;
                }
            };

            sender
                .send(cmd)
                .context("Failed to send command {buf:?} to engine")?;
        }
    }

    /// Sets the flag that the search should stop.
    fn stop_search(&mut self) {
        self.is_searching.store(false, Ordering::Relaxed);
    }

    /// Sets the flag to signal that the engine is searching
    fn start_search(&mut self) {
        self.is_searching.store(true, Ordering::Relaxed);
    }

    /// Yields `true` if the engine is currently searching.
    fn is_searching(&self) -> bool {
        self.is_searching.load(Ordering::Relaxed)
    }

    /// Called when `ucinewgame` command is received. Resets all game-specific options.
    fn new_game(&mut self) {
        self.game = Game::default();
    }

    /// Parses an input string a yields an [`EngineCommand`], if possible.
    fn parse_command(input: &str) -> Result<EngineCommand> {
        let (cmd, rest) = if input.contains(' ') {
            input.split_once(' ').unwrap()
        } else {
            (input, "")
        };

        match cmd {
            "help" => Ok(EngineCommand::Help),
            "show" => Ok(EngineCommand::Show),
            "history" => Ok(EngineCommand::History),
            "perft" => Self::parse_perft_command(rest),
            "eval" => Self::parse_eval_command(rest),
            "move" => Self::parse_move_command(rest),
            "moves" => Self::parse_moves_command(rest),
            "fen" => Self::parse_fen_command(rest),
            "option" => Self::parse_option_command(rest),
            "undo" => Ok(EngineCommand::Undo),
            "bench" => Ok(EngineCommand::Bench),
            "quit" | "exit" => Ok(EngineCommand::Exit),
            _ => Self::parse_uci_input(input).map(EngineCommand::UciCommand),
        }
    }

    /// Parses the custom `perft` command
    fn parse_perft_command(rest: &str) -> Result<EngineCommand> {
        let mut args = rest.split_ascii_whitespace();

        let Some(depth) = args.next() else {
            bail!("usage: perft <depth> [pretty] [split]");
        };

        let Ok(depth) = depth.parse() else {
            bail!("usage: perft <depth> [pretty] [split]");
        };

        let pretty = rest
            .split_ascii_whitespace()
            .any(|arg| arg.to_ascii_lowercase() == "pretty");

        let split = rest
            .split_ascii_whitespace()
            .any(|arg| arg.to_ascii_lowercase() == "split");

        Ok(EngineCommand::Perft {
            depth,
            pretty,
            split,
        })
    }

    /// Parses the custom `eval` command
    fn parse_eval_command(rest: &str) -> Result<EngineCommand> {
        let mut args = rest.split_ascii_whitespace();

        let mut game = Game::default();

        if let Some(arg) = args.next() {
            if arg.to_ascii_lowercase() == "startpos" {
                game = Game::default();
            } else if let Ok(parsed) = Game::from_fen(arg) {
                game = parsed;
            } else {
                bail!("usage: eval [FEN]");
            }
        }

        Ok(EngineCommand::Eval(Box::new(game)))
    }

    /// Parses the custom `fen` command
    fn parse_fen_command(rest: &str) -> Result<EngineCommand> {
        let mut args = rest.split_ascii_whitespace();

        let mut fen = None;
        if let Some(arg) = args.next() {
            if arg.to_ascii_lowercase() == "startpos" {
                fen = Some(FEN_STARTPOS.to_string());
            } else if Position::from_fen(arg).is_ok() {
                fen = Some(arg.to_string());
            } else {
                bail!("usage: fen [FEN]");
            }
        }

        Ok(EngineCommand::Fen(fen))
    }

    /// Parses the custom `move` command
    fn parse_move_command(rest: &str) -> Result<EngineCommand> {
        if rest.is_empty() {
            bail!("usage: move <move1> [move2 move3 ...]");
        }

        let mut moves = vec![];

        for arg in rest.split_ascii_whitespace() {
            if Move::is_uci(arg) {
                moves.push(arg.to_string());
            } else {
                bail!("Invalid move {arg:?}");
            }
        }

        Ok(EngineCommand::MakeMove(moves))
    }

    /// Parses the custom `moves` command
    fn parse_moves_command(rest: &str) -> Result<EngineCommand> {
        let debug = rest
            .split_ascii_whitespace()
            .any(|arg| arg.to_ascii_lowercase() == "debug");

        let mut args = rest.split_ascii_whitespace();
        let from = if let Some(from) = args.next() {
            let Ok(from) = Tile::from_str(from) else {
                bail!("usage: moves [square] [debug]")
            };
            Some(from)
        } else {
            None
        };

        Ok(EngineCommand::Moves(from, debug))
    }

    /// Parses the `option` command
    fn parse_option_command(rest: &str) -> Result<EngineCommand> {
        let mut args = rest.split_ascii_whitespace();

        let Some(arg) = args.next() else {
            bail!("usage: option <name>");
        };

        let name = arg.to_string();

        Ok(EngineCommand::Option(name))
    }

    /// Executes the `help` command, displaying a list of available commands this engine has.
    fn help(&self) {
        println!("available commands: help, perft, show, history, fen, move, moves, eval, uci, bench, undo");
    }

    /// Executes the `show` command, printing the current state of the board.
    fn show(&self) {
        println!("{:?}", self.game.position());
    }

    /// Executes the `history` command, printing all moves made on this current game.
    fn history(&self) {
        // TODO: Replace with to_pgn function
        println!(
            "{}",
            self.game
                .history()
                .iter()
                .enumerate()
                .map(|(i, m)| format!("{}) {m}", i + 1))
                .collect::<Vec<_>>()
                .join(" ")
        );
    }

    /// Executes the `perft` command, performing `perft(depth)` for benchmarking and testing.
    fn perft(&self, depth: usize, pretty: bool, split: bool) {
        // Man, I wish I could just pass `split` and `pretty` in directly
        if split {
            if pretty {
                print_perft::<true, true>(self.game.position(), depth);
            } else {
                print_perft::<false, true>(self.game.position(), depth);
            }
        } else if pretty {
            print_perft::<true, false>(self.game.position(), depth);
        } else {
            print_perft::<false, false>(self.game.position(), depth);
        }
    }

    /// Executes the `fen` command, setting the position of the board and displaying the current state as a FEN string.
    fn fen(&mut self, fen: Option<String>) -> Result<()> {
        if let Some(fen) = fen {
            self.game = Game::from_fen(&fen)?;
        }
        println!("{}", self.game.position().to_fen());
        Ok(())
    }

    /// Executes the `eval` command, calling the engine's internal evaluator on the current game state, printing the result.
    fn eval(&self, game: Game) {
        println!("{}", Evaluator::new(&game).eval());
    }

    /// Executes the `moves` command, displaying all legal moves available.
    fn moves(&self, from: Option<Tile>, debug: bool) {
        if let Some(from) = from {
            let moves = self
                .game
                .legal_moves()
                .into_iter()
                .filter(|mv| mv.from() == from);
            let mut mobility = BitBoard::default();
            for mv in moves {
                mobility |= BitBoard::from_tile(mv.to());
            }
            println!("{mobility}");
        } else {
            let mut moves = self
                .game
                .legal_moves()
                .into_iter()
                .map(|m| {
                    if debug {
                        format!("{m:?}")
                    } else {
                        format!("{m}")
                    }
                })
                .collect::<Vec<_>>();
            moves.sort();
            println!("{}", moves.join(" "));
        }
    }

    /// Executes the `move` command, applying the provided move(s) to the current position.
    fn make_move(&mut self, moves: Vec<String>) -> Result<()> {
        for mv_string in moves {
            let mv = Move::from_uci(&self.game, &mv_string)?;
            self.game.make_move(mv);
        }

        Ok(())
    }

    /// Executes the `undo` command, un-making the previously-made move.
    fn undo(&mut self) {
        self.game.unmake_move();
    }

    /// Executes the `option` command, displaying the current value of a provided engine option.
    fn option(&self, name: String) {
        println!("{name}=UNSET");
    }
}

impl Read for Engine {
    /// [`Engine`] can `read` from `stdin`.
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        io::stdin().read(buf)
    }
}

impl Write for Engine {
    /// [`Engine`] can `write` to `stdout`.
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        io::stdout().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        io::stdout().flush()
    }
}

/// Represents a custom command that can be sent to this engine.
#[derive(Clone, Debug)]
pub enum EngineCommand {
    /// For displaying the list of available commands.
    Help,

    /// Run a perft at the provided depth.
    Perft {
        depth: usize,
        pretty: bool,
        split: bool,
    },

    /// Pretty-print the current state of the board.
    Show,

    /// Display the moves made during this game.
    History,

    /// Show the current state of of the board as a FEN string.
    Fen(Option<String>),

    /// Make the list of moves applied to the board.
    MakeMove(Vec<String>),

    /// Show all legal moves from the current position.
    Moves(Option<Tile>, bool),

    /// Evaluates the current position.
    Eval(Box<Game>),

    /// Benchmark this engine.
    Bench,

    /// Undo the last move made.
    Undo,

    /// View the current value of an option
    Option(String),

    /// Wraps a [`UciCommand`], sent by a UCI-compatible GUI to the engine.
    UciCommand(UciCommand),

    /// Wraps a [`UciResponse`], to be sent by the engine to a GUI.
    UciResponse(Box<UciResponse>),

    /// Exit the engine as quickly as possible.
    Exit,
}

impl UciEngine for Engine {
    /* GUI to Engine communication */

    fn debug(&mut self, status: bool) -> Result<()> {
        self.debug.store(status, Ordering::Relaxed);
        Ok(())
    }

    fn setoption(&mut self, name: String, value: Option<String>) -> Result<()> {
        match name.as_str() {
            "placeholder" => todo!(),
            _ => {
                if let Some(value) = value {
                    eprintln!("Unrecognized option {name:?} with value {value:?}")
                } else {
                    eprintln!("Unrecognized option {name:?}")
                }
            }
        }
        Ok(())
    }

    fn ucinewgame(&mut self) -> Result<()> {
        self.new_game();
        Ok(())
    }

    fn position(&mut self, fen: Option<String>, moves: Vec<String>) -> Result<()> {
        // Apply the FEN to the game state
        if let Some(fen) = fen {
            self.game = Game::from_fen(&fen)?;
        } else {
            self.game = Game::default();
        }

        // Now, if there are any moves, apply them as well.
        for mv in moves {
            let mv = Move::from_uci(self.game.position(), &mv)?;
            self.game.make_move_checked(mv)?;
        }

        Ok(())
    }

    fn go(&mut self, options: UciSearchOptions) -> Result<()> {
        if self.is_searching() {
            warn!("Engine was told to search while it is already searching. Stopping current search...");
            self.stop_search();
        }

        // Flip the flag to signal a search has begun.
        self.start_search();

        // Compute remaining time
        let time_remaining = if let Some(movetime) = options.move_time {
            movetime
        } else if self.game.current_player().is_white() {
            options.w_time.unwrap_or(Duration::MAX)
        } else if self.game.current_player().is_black() {
            options.b_time.unwrap_or(Duration::MAX)
        } else {
            Duration::MAX
        };

        eprintln!("Received duration of {time_remaining:?}");

        // Clone the arcs for whether we're searching and our found results
        let is_searching = Arc::clone(&self.is_searching);
        // let timeout = Duration::from_secs_f32(time_remaining.as_secs_f32() / 20.0); // 5% time remaining
        let timeout = Duration::from_secs_f32(time_remaining.as_secs_f32() / 100.0); // 1% time remaining
        let result = Arc::clone(&self.search_result);
        let sender = self.sender.clone().unwrap();

        let game = self.game.clone();
        let max_depth = options.depth.unwrap_or(10) as usize; // TODO: Increase to 127 or 255 once you have TT set up

        self.pool.execute(move || {
            let mut depth = 1;

            while depth <= max_depth && is_searching.load(Ordering::Relaxed) {
                let cloned_stopper = Arc::clone(&is_searching);
                let cloned_result = Arc::clone(&result);

                // Start the search
                let search = Search::new(
                    &game,
                    timeout,
                    cloned_stopper,
                    cloned_result,
                    sender.clone(),
                );
                // .with_options(options.clone());

                // If we received an error, that means the search was stopped externally
                match search.start(depth) {
                    Ok(_data) => {}
                    Err(_) => break,
                }

                depth += 1;
            }

            // If this line of code is reached, it means the search has stopped on its own
            // We can do the same actions as if we have received the "stop" command
            if let Err(err) = sender.send(EngineCommand::UciCommand(UciCommand::Stop)) {
                error!("Failed to send 'stop' to engine after search concluded: {err:?}");
            }
        });

        Ok(())
    }

    fn stop(&mut self) -> Result<()> {
        // Only need to stop if we're already searching. Otherwise, don't need to send anything new
        if self.is_searching() {
            self.stop_search();

            let Ok(res) = self.search_result.read() else {
                bail!("Failed to acquire read access to engine.search_result")
            };

            self.bestmove(res.bestmove, res.ponder)?;
        }

        Ok(())
    }

    fn ponderhit(&self) -> Result<()> {
        todo!("Handle ponderhit")
    }

    /* Engine to GUI communication */

    fn option(&self) -> Result<()> {
        let options: [UciOption<&str>; 0] = [
            // All available options will be defined here.
            // UciOption::check("TestOpt Check", false),
            // UciOption::spin("TestOpt Spin", -8, i32::MIN, i32::MAX),
            // UciOption::combo("TestOpt Combo", "Apple", ["Apple", "Banana", "Strawberry"]),
            // UciOption::button("TestOpt Button"),
            // UciOption::string("TestOpt String", "defaultVal"),
            // UciOption::check("Nullmove", true),
            // UciOption::spin("Selectivity", 2, 0, 4),
            // UciOption::combo("Style", "Normal", ["Solid", "Normal", "Risky"]),
            // UciOption::string("NalimovPath", "c:\\"),
            // UciOption::button("Clear Hash"),
            // UciOption::spin("Threads", 1, 1, 1),
            // UciOption::spin("Hash", 1, 1, 1),
        ];

        for opt in options {
            self.send_uci_response(UciResponse::Option(opt))?;
        }
        Ok(())
    }
}

impl Default for Engine {
    /// Default engine starts with standard piece set up.
    fn default() -> Self {
        let game = Game::from_fen(FEN_STARTPOS).unwrap();

        // Initialize the engine to have the first legal move selected as it's best by default
        let bestmove = game.legal_moves().first().copied().unwrap_or_default();

        Self {
            game,
            // ttable: TranspositionTable::default(),
            debug: Arc::default(),
            is_searching: Arc::default(),
            search_result: Arc::new(RwLock::new(SearchResult::new(bestmove))),
            pool: ThreadPool::new(num_cpus::get()),
            sender: None,
        }
    }
}