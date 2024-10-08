use std::{
    io::{self, Read, Write},
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Receiver, Sender},
        Arc, LazyLock, Mutex,
    },
    time::{Duration, Instant},
};

use anyhow::{bail, Context, Result};
use chessie::{print_perft, Bitboard, Color, Game, Move, Position, Square, FEN_STARTPOS};
use log::{error, warn};
use threadpool::ThreadPool;

use super::{
    protocols::{
        UciCommand, UciEngine, UciInfo, UciOption, UciResponse, UciScore, UciSearchOptions,
    },
    search::{Score, Searcher, TTable, DEFAULT_TTABLE_SIZE},
    Evaluator, BENCHMARK_FENS, MAX_DEPTH,
};

/// Threadpool from which to spawn threads for searches, user input, etc.
pub static POOL: LazyLock<ThreadPool> = LazyLock::new(|| ThreadPool::new(num_cpus::get()));

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

    /// Transposition table for game states.
    ttable: Arc<Mutex<TTable>>,

    /// Whether to display additional information in `info` commands.
    ///
    /// Defaults to `false`.`
    debug: Arc<AtomicBool>,

    /// Atomic boolean to determine whether the engine is currently running a search.
    is_searching: Arc<AtomicBool>,

    /// Handles sending events to the internal event pump.
    sender: Sender<EngineCommand>,

    /// Handles receiving events in the internal event pump.
    receiver: Receiver<EngineCommand>,
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

        // Spin up a thread for handling user/GUI input
        let sender = self.sender.clone();
        POOL.execute(move || {
            if let Err(err) = Self::user_input_handler(sender) {
                error!("{err}");
            }
        });

        // Main event loop: Handle inputs from various sources
        while let Ok(cmd) = self.receiver.recv() {
            let res = match cmd {
                EngineCommand::Help => self.help(),
                EngineCommand::Show => self.show(),
                EngineCommand::Perft {
                    depth,
                    pretty,
                    split,
                } => self.perft(depth, pretty, split),
                EngineCommand::Fen(fen) => self.fen(fen),
                EngineCommand::Eval(game) => self.eval(*game),
                EngineCommand::Moves(from, debug) => self.moves(from, debug),
                EngineCommand::Bench(depth) => self.bench(depth),
                EngineCommand::MakeMove(moves) => self.make_move(moves),
                EngineCommand::Option(name) => self.option(name),
                EngineCommand::UciCommand(uci_cmd) => self.execute_uci_command(uci_cmd),
                EngineCommand::UciResponse(uci_resp) => self.send_uci_response(*uci_resp),
                EngineCommand::Exit => break,
            };

            if let Err(err) = res {
                eprintln!("{err}");
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
        self.clear_ttable();
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
            "perft" => Self::parse_perft_command(rest),
            "eval" => Self::parse_eval_command(rest),
            "move" => Self::parse_move_command(rest),
            "moves" => Self::parse_moves_command(rest),
            "fen" => Self::parse_fen_command(rest),
            "option" => Self::parse_option_command(rest),
            "bench" => Self::parse_bench_command(rest),
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

        let game = if let Some(arg) = args.next() {
            if arg.to_ascii_lowercase() == "startpos" {
                Some(Game::default())
            } else if let Ok(parsed) = Game::from_fen(arg) {
                Some(parsed)
            } else {
                bail!("usage: eval [FEN]");
            }
        } else {
            None
        };

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
            let Ok(from) = Square::from_str(from) else {
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

    /// Parses the `bench` command
    fn parse_bench_command(rest: &str) -> Result<EngineCommand> {
        let mut args = rest.split_ascii_whitespace();

        let mut depth = 5;

        if let Some(d) = args.next() {
            let Ok(d) = d.parse() else {
                bail!("usage: bench [depth]");
            };

            depth = d
        }

        Ok(EngineCommand::Bench(depth))
    }

    /// Executes the `help` command, displaying a list of available commands this engine has.
    fn help(&self) -> Result<()> {
        println!(
            "available commands: help, perft, show, option, fen, move, moves, eval, uci, bench"
        );
        Ok(())
    }

    /// Executes the `show` command, printing the current state of the board.
    fn show(&self) -> Result<()> {
        println!("{:?}", self.game.position());
        Ok(())
    }

    /// Executes the `perft` command, performing `perft(depth)` for benchmarking and testing.
    pub fn perft(&self, depth: usize, pretty: bool, split: bool) -> Result<()> {
        // Man, I wish I could just pass `split` and `pretty` in directly
        if split {
            if pretty {
                print_perft::<true, true>(&self.game, depth);
            } else {
                print_perft::<false, true>(&self.game, depth);
            }
        } else if pretty {
            print_perft::<true, false>(&self.game, depth);
        } else {
            print_perft::<false, false>(&self.game, depth);
        }
        Ok(())
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
    fn eval(&self, game: Option<Game>) -> Result<()> {
        if let Some(game) = game {
            println!("{}", Evaluator::new(&game).eval(Color::White));
        } else {
            println!("{}", Evaluator::new(&self.game).eval(Color::White));
        }
        Ok(())
    }

    /// Executes the `moves` command, displaying all legal moves available.
    fn moves(&self, from: Option<Square>, debug: bool) -> Result<()> {
        if let Some(from) = from {
            let moves = self
                .game
                .get_legal_moves()
                .into_iter()
                .filter(|mv| mv.from() == from);
            let mut mobility = Bitboard::default();
            for mv in moves {
                mobility |= Bitboard::from_square(mv.to());
            }
            println!("{mobility}");
        } else {
            let mut moves = self
                .game
                .get_legal_moves()
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
        Ok(())
    }

    /// Executes the `move` command, applying the provided move(s) to the current position.
    pub fn make_move<T: AsRef<str>>(&mut self, moves: impl IntoIterator<Item = T>) -> Result<()> {
        for mv_string in moves {
            let mv = Move::from_uci(&self.game, mv_string.as_ref())?;
            self.game.make_move(mv);
        }

        Ok(())
    }

    /// Executes the `option` command, displaying the current value of a provided engine option.
    fn option(&self, name: String) -> Result<()> {
        println!("{name}=UNSET");
        Ok(())
    }

    /// Clears the transposition table
    fn clear_ttable(&mut self) {
        *self.ttable.lock().unwrap() = TTable::default();
    }

    fn bench(&mut self, depth: u32) -> Result<()> {
        error!("Benchmarking not currently implemented due to requiring a refactor of search code");
        self.clear_ttable();
        println!(
            "Starting benchmark of {} positions at depth {depth}",
            BENCHMARK_FENS.len()
        );

        let starttime = Instant::now();
        let mut nodes_searched = 0;

        for fen in &BENCHMARK_FENS[..1] {
            self.game = Game::from_fen(fen)?;

            // Doesn't work because the search happens in it's own thread, which ends *after* this loop ends.
            // Need to refactor search code so that `Searcher::start` launches the iterative deepening loop.
            /*
            let search_opts = UciSearchOptions {
                depth: Some(depth),
                ..Default::default()
            };
            let go_cmd = EngineCommand::UciCommand(UciCommand::Go(search_opts));
            self.sender.as_ref().unwrap().send(go_cmd)?;
             */
            nodes_searched += 0;
        }

        let elapsed = starttime.elapsed();

        let info = UciInfo::new().string(format!("{} seconds", elapsed.as_secs()));
        self.info(info)?;

        let nps = nodes_searched as f32 / elapsed.as_secs_f32();
        let info = UciInfo::new().nodes(nodes_searched).nps(nps);
        self.info(info)?;

        self.clear_ttable();
        Ok(())
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

    /// Show the current state of of the board as a FEN string.
    Fen(Option<String>),

    /// Make the list of moves applied to the board.
    MakeMove(Vec<String>),

    /// Show all legal moves from the current position.
    Moves(Option<Square>, bool),

    /// Evaluates the current position.
    Eval(Box<Option<Game>>),

    /// Benchmark this engine at the provided depth.
    Bench(u32),

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
            "Hash" => {
                let Some(value) = value else {
                    bail!("usage: setoption name hash value <value>");
                };

                let mb = value
                    .parse::<usize>()
                    .context(format!("expected integer. got {value:?}"))?;

                *self.ttable.lock().unwrap() = TTable::new(mb * 1_048_576); // multiply by # bytes in MB
            }

            "Clear Hash" => self.clear_ttable(),

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
        let starttime = Instant::now();

        // Flip the flag to signal a search has begun.
        self.start_search();

        // If `movetime` was supplied, search that long.
        let (soft_timeout, hard_timeout) = if let Some(movetime) = options.move_time {
            (movetime, movetime)
        } else {
            // Otherwise, search based on time remaining and increment
            let (time, inc) = if self.game.side_to_move().is_white() {
                (options.w_time, options.w_inc)
            } else {
                (options.b_time, options.b_inc)
            };

            let (time, inc) = (time.unwrap_or(Duration::MAX), inc.unwrap_or(Duration::ZERO));

            (
                time / 20 + inc / 2, // Soft Timeout: 5% of time remaining + 50% time increment
                time / 5 + inc / 2,  // Hard Timeout: 20% of time remaining + 50% time increment
            )
        };

        // eprintln!("Starting search with soft timeout: {soft_timeout:?} and hard timeout: {hard_timeout:?}");

        // Clone the arcs for whether we're searching and our found results
        let is_searching = Arc::clone(&self.is_searching);
        let sender = self.sender.clone();

        let game = self.game.clone();
        let ttable = Arc::clone(&self.ttable);
        let max_depth = options.depth.unwrap_or(MAX_DEPTH as u32);

        // Initialize bestmove to the first move available, if there are any
        let mut bestmove = game.get_legal_moves().first().cloned();

        POOL.execute(move || {
            let mut ttable = ttable.lock().unwrap();

            // Iterative Deepening
            for depth in 1..=max_depth {
                // If we've been told to stop, or if we've hit the soft timeout, exit the loop
                if !is_searching.load(Ordering::Relaxed) || starttime.elapsed() >= soft_timeout {
                    break;
                }

                // Create a search instance with the appropriate thread data
                let search = Searcher::new(
                    &game,
                    &mut ttable,
                    starttime,
                    hard_timeout,
                    Arc::clone(&is_searching),
                );

                // If we received an error, that means the search was stopped externally
                match search.start(depth) {
                    // Send info via UCI
                    Ok(data) => {
                        let elapsed = starttime.elapsed();

                        bestmove = data.bestmove;

                        // Determine whether the score is an evaluation or a "mate in y"
                        // Assistance provided by @Ciekce on Discord
                        // https://github.com/Ciekce/Stormphrax/blob/main/src/search.cpp#L1163
                        let score = if data.score.is_mate() {
                            let dist = Score::MATE - data.score.abs(); // distance to mate (in plies)
                            let moves_to_mate = if data.score.0 > 0 {
                                dist.0 + 1
                            } else {
                                -dist.0
                            } / 2;
                            UciScore::mate(moves_to_mate)
                        } else {
                            UciScore::cp(data.score.0)
                        };

                        // Construct and send an `info` command
                        let info = UciInfo::default()
                            .depth(depth)
                            .score(score)
                            .nodes(data.nodes_searched)
                            .nps((data.nodes_searched as f32 / elapsed.as_secs_f32()).trunc())
                            .time(elapsed.as_millis())
                            .pv(data.bestmove);
                        // .pv(&data.pv[0]);

                        let info_resp = Box::new(UciResponse::Info(Box::new(info)));
                        if let Err(err) = sender.send(EngineCommand::UciResponse(info_resp)) {
                            error!("Failed to send 'info' to engine during search: {err:?}");
                        }
                    }

                    // Search was stopped abruptly; exit iterative deepening loop
                    Err(_) => break,
                }
            }
            // If this line of code is reached, it means the search has stopped.
            // So we need to send a bestmove and ensure the search flag is set to false

            let bestmove = bestmove.map(|mv| mv.to_string());
            let ponder = None;
            let bestmove_resp = Box::new(UciResponse::BestMove { bestmove, ponder });

            if let Err(err) = sender.send(EngineCommand::UciResponse(bestmove_resp)) {
                error!("Failed to send 'bestmove' to engine after search concluded: {err:?}");
            }

            if let Err(err) = sender.send(EngineCommand::UciCommand(UciCommand::Stop)) {
                error!("Failed to send 'stop' to engine after search concluded: {err:?}");
            }
        });

        Ok(())
    }

    fn stop(&mut self) -> Result<()> {
        // Only need to stop if we're already searching
        if self.is_searching() {
            self.stop_search();
        }

        Ok(())
    }

    fn ponderhit(&self) -> Result<()> {
        todo!("Handle ponderhit")
    }

    /* Engine to GUI communication */

    fn option(&self) -> Result<()> {
        let options = [
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
            UciOption::button("Clear Hash"),
            // UciOption::spin("Threads", 1, 1, 1),
            UciOption::spin("Hash", DEFAULT_TTABLE_SIZE as i32 / 1_048_576, 1, 2_048), // I got 2048 from Stockfish: https://github.com/official-stockfish/Stockfish/blob/9fb58328e363d84e3cf720b018e639b139ba95c2/src/engine.cpp#L48
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
        let (sender, receiver) = mpsc::channel();
        Self {
            game: Game::default(),
            ttable: Arc::default(),
            debug: Arc::default(),
            is_searching: Arc::default(),
            sender,
            receiver,
        }
    }
}
