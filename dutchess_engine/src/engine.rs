use std::{
    io,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, RwLock,
    },
    thread,
    time::Duration,
};

use anyhow::{bail, Result};
use dutchess_core::{print_perft, print_split_perft, Game, Move, Position, FEN_STARTPOS};

use super::{
    search::{Search, SearchResult},
    uci::{UciEngine, UciInfo, UciOption, UciResponse, UciSearchOptions},
    Evaluator,
};

// type TranspositionTable = ();

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash, Default)]
enum EngineProtocol {
    #[default]
    UCI,
}

/*
#[derive(PartialEq, Eq, Clone, Debug, Default)]
pub(crate) enum SearchStatus {
    #[default]
    NotStarted,
    InProgress,
    Done,
}
 */

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
    /// State of the game, including castling rights, piece placement, etc.
    ///
    /// This is analogous to a FEN string.
    game: Game,

    /// Transposition table for game states.
    // ttable: TranspositionTable,

    /// Communication protocol
    // protocol: EngineProtocol,
    debug: bool,

    /// Atomic boolean to determine whether the engine is currently running a search
    // search_status: Arc<RwLock<SearchStatus>>,
    is_searching: Arc<AtomicBool>,

    /// Result of the last-executed search
    search_result: Arc<RwLock<SearchResult>>,
    // search_handle: Option<JoinHandle<()>>,
    info: Arc<RwLock<UciInfo>>,
    // options: todo!(),
}

impl Engine {
    pub fn new() -> Self {
        Self {
            game: Game::new(),
            ..Default::default()
        }
    }

    pub fn from_fen(fen: &str) -> Result<Self> {
        let game = Game::from_fen(fen)?;
        Ok(Self {
            game,
            ..Default::default()
        })
    }

    /// Main entrypoint of the engine
    pub fn run(&mut self) -> io::Result<()> {
        let name = env!("CARGO_PKG_NAME");
        let version = env!("CARGO_PKG_VERSION");
        let authors = env!("CARGO_PKG_AUTHORS");
        println!("{name} {version} by {authors}");

        self.uci_loop()
    }

    fn get_uci_options(&self) -> impl Iterator<Item = UciOption<&str>> {
        [
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
        ]
        .into_iter()
    }

    /// Sets the flag that the search should stop.
    fn stop_search(&mut self) {
        self.is_searching.store(false, Ordering::Relaxed);
        // *self.search_status.write().unwrap() = SearchStatus::Done;
    }

    /// Sets the flag that the search should be started.
    fn start_search(&mut self) {
        self.is_searching.store(true, Ordering::Relaxed);
        // *self.search_status.write().unwrap() = SearchStatus::InProgress;
    }

    /// Yields `true` if the engine is currently searching.
    fn is_searching(&self) -> bool {
        self.is_searching.load(Ordering::Relaxed)
        // *self.search_status.read().unwrap() == SearchStatus::InProgress
    }

    // fn block_until_done(&mut self) -> Option<()> {
    //     self.search_handle
    //         .take()
    //         .map(|handle| handle.join().unwrap())
    // }

    /// Called when `ucinewgame` command is received. Resets all game-specific options.
    fn new_game(&mut self) {}

    /*
    fn send_bestmove(&self) -> io::Result<()> {
        // let bestmove = self.legal_moves().first().unwrap().to_string();
        // let ponder = self.legal_moves().last().unwrap().to_string();

        // let bestmove = std::mem::take(&mut *self.bestmove.lock().unwrap());
        // let ponder = self.ponder.lock().unwrap().take();
        let bestmove = self.bestmove.lock().unwrap().clone();
        let ponder = self.ponder.lock().unwrap().clone();

        self.bestmove(bestmove, ponder)
    }
     */

    /*
    /// Parses the custom `search` command
    fn parse_search_command(&self, rest: &str) -> Result<EngineCommand, String> {
        let mut args = rest.split_ascii_whitespace();

        let Some(depth) = args.next() else {
            return Err(format!("usage: search <depth> [iterative]"));
        };

        let Ok(depth) = depth.parse() else {
            return Err(format!("usage: search <depth> [iterative]"));
        };

        let iterative = rest
            .split_ascii_whitespace()
            .any(|arg| arg.to_ascii_lowercase() == "iterative");

        Ok(EngineCommand::Search { depth, iterative })
    }
      */

    /// Parses the custom `perft` command
    fn parse_perft_command(&self, rest: &str) -> Result<EngineCommand> {
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
    fn parse_eval_command(&self, rest: &str) -> Result<EngineCommand> {
        let mut args = rest.split_ascii_whitespace();

        let mut game = self.game.clone();

        if let Some(arg) = args.next() {
            if arg.to_ascii_lowercase() == "startpos" {
                game = Game::standard_setup();
            } else if let Ok(parsed) = Game::from_fen(arg) {
                game = parsed;
            } else {
                bail!("usage: eval [FEN]");
            }
        }

        Ok(EngineCommand::Eval(game))
    }

    /// Parses the custom `fen` command
    fn parse_fen_command(&self, rest: &str) -> Result<EngineCommand> {
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
    fn parse_move_command(&self, rest: &str) -> Result<EngineCommand> {
        if rest.is_empty() {
            bail!("usage: move <move1> [move2 move3 ...]");
        }

        let mut moves = vec![];
        let mut pos = self.game.position().clone();

        for arg in rest.split_ascii_whitespace() {
            match Move::from_uci(&pos, arg) {
                Ok(mv) => {
                    if let Err(err) = pos.make_move_checked(mv) {
                        bail!("Invalid move: {err}");
                    }
                    moves.push(mv);
                }
                Err(err) => bail!("{err}"),
            }
        }

        Ok(EngineCommand::Move(moves))
    }

    /// Parses custom commands
    fn parse_custom_command(&self, input: &str) -> Result<EngineCommand> {
        let (cmd, rest) = if input.contains(' ') {
            input.split_once(' ').unwrap()
        } else {
            (input, "")
        };

        match cmd {
            "help" => Ok(EngineCommand::Help),
            "show" => Ok(EngineCommand::Show),
            "history" => Ok(EngineCommand::History),
            // "search" => self.parse_search_command(rest),
            "perft" => self.parse_perft_command(rest),
            "eval" => self.parse_eval_command(rest),
            "move" => self.parse_move_command(rest),
            "moves" => Ok(EngineCommand::Moves),
            "fen" => self.parse_fen_command(rest),
            _ => bail!(
                "{} does not recognize command {input:?}\nRun 'help' for a list of commands",
                env!("CARGO_PKG_NAME")
            ),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum EngineCommand {
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
    Move(Vec<Move>),

    /// Show all legal moves from the current position
    Moves,

    /// Evaluates the current position.
    Eval(Game),
    /*
    /// Search the current position up to the provided depth.
    Search { depth: u32, iterative: bool },
     */
}

impl UciEngine for Engine {
    /* GUI to Engine communication */

    fn custom_command(&mut self, input: &str) -> io::Result<()> {
        // Parse the command, returning an `io::Error` on error
        let cmd = self
            .parse_custom_command(input)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

        match cmd {
            EngineCommand::Help => {
                println!(
                    "available commands: help, perft, uci, bench, show, move, moves, undo, eval"
                )
            }

            EngineCommand::Show => println!("{:?}", self.game.position()),

            // TODO: Replace with to_pgn function
            EngineCommand::History => println!(
                "{}",
                self.game
                    .history()
                    .into_iter()
                    .enumerate()
                    .map(|(i, m)| format!("{}) {m}", i + 1))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),

            EngineCommand::Perft {
                depth,
                pretty,
                split,
            } => {
                if split {
                    if pretty {
                        print_split_perft::<true>(self.game.position(), depth);
                    } else {
                        print_split_perft::<false>(self.game.position(), depth);
                    }
                } else {
                    if pretty {
                        print_perft::<true>(self.game.position(), depth);
                    } else {
                        print_perft::<false>(self.game.position(), depth);
                    }
                }
            }

            EngineCommand::Fen(fen) => {
                if let Some(fen) = fen {
                    self.game = Game::from_fen(&fen).unwrap();
                }
                println!("{}", self.game.position().to_fen())
            }

            EngineCommand::Eval(game) => println!("{}", Evaluator::new(&game).eval()),

            EngineCommand::Move(moves) => self.game.make_moves(moves),

            EngineCommand::Moves => {
                let mut moves = self
                    .game
                    .legal_moves()
                    .into_iter()
                    .map(|m| m.to_string())
                    .collect::<Vec<_>>();
                moves.sort();
                println!("{}", moves.join(" "))
            } /*
              EngineCommand::Search {
                  depth,
                  iterative: _,
              } => {
                  let mut options = UciSearchOptions::default();
                  options.depth = Some(depth);
                  let search = Search::new(self.game.clone(), Arc::clone(&self.searching))
                      .with_options(options);
                  let res = search.start();
                  // let res = search(self.game, depth);
                  if let Some(bestmove) = res.bestmove {
                      println!("bestmove {bestmove} ({})", res.score);
                  } else {
                      println!("No bestmove found in search. Score={}", res.score);
                  }
                  // println!("{:?}", search(&self.game, depth)),
              } // EngineCommand::Moves => println!("{:?}", self.game),
               */
        }

        Ok(())
    }

    // Engine receive a `uci` command
    fn uci(&mut self) -> io::Result<()> {
        // The engine must now identify itself
        self.id()?;

        // And send all available options
        self.option()?;

        // Engine has sent all parameters and is ready
        self.uciok()?;

        Ok(())
    }

    fn debug(&mut self, status: bool) -> io::Result<()> {
        self.debug = status;
        Ok(())
    }

    fn isready(&self) -> io::Result<()> {
        let resp: UciResponse<&str> = UciResponse::ReadyOk;
        resp.send()
    }

    fn setoption(&mut self, name: &str, value: &str) -> io::Result<()> {
        match name {
            _ => eprintln!("Unrecognized option `{name}` with value `{value}`"),
        }
        Ok(())
    }

    fn register(&mut self, registration: Option<(&str, &str)>) -> io::Result<()> {
        // No registration necessary :)
        _ = registration;

        // match registration {
        //     UciRegistration::Later => {}
        //     UciRegistration::Now(name, code) => {}
        // }
        Ok(())
    }

    fn ucinewgame(&mut self) -> io::Result<()> {
        self.new_game();
        Ok(())
    }

    fn position(&mut self, fen: &str, moves: Vec<&str>) -> io::Result<()> {
        // Apply the FEN to the game state
        // _ = self.setup(fen); // ignore any errors if they occur.
        // self.game = Game::from_fen(fen).unwrap();
        self.game = Game::from_fen(fen).unwrap();

        // Now, if there are any moves, apply them as well.
        // self.game
        //     .apply_moves(moves.into_iter().map(|m| Move::from_uci(m).unwrap()));

        for mv in moves {
            let mv = Move::from_uci(self.game.position(), mv).unwrap();
            self.game.make_move(mv);
        }

        Ok(())
    }

    fn go(&mut self, options: UciSearchOptions) -> io::Result<()> {
        // Arena sent this to our engine
        // go wtime 300000 btime 300000 winc 0 binc 0

        if self.is_searching() {
            eprintln!("Engine is already searching");
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

        // Clone the arcs for whether we're searching and our found results
        let is_searching = Arc::clone(&self.is_searching);
        let timeout = Duration::from_secs_f32(time_remaining.as_secs_f32() / 20.0); // 5% time remaining
                                                                                    // let timeout = Duration::from_secs(1);
        let result = Arc::clone(&self.search_result);
        let info = Arc::clone(&self.info);

        let game = self.game.clone();
        let max_depth = options.depth.unwrap_or(10) as usize; // TODO: Increase to 127 or 255 once you have TT set up

        thread::spawn(move || {
            let mut depth = 1;

            while depth <= max_depth && is_searching.load(Ordering::Relaxed) {
                let cloned_stopper = Arc::clone(&is_searching);
                let cloned_result = Arc::clone(&result);
                let cloned_info = Arc::clone(&info);

                // Start the search
                let mut search =
                    Search::new(&game, timeout, cloned_stopper, cloned_result, cloned_info)
                        .with_options(options.clone());

                // Start the search
                // let res = match search.start(depth) {
                //     Ok(res) => res,
                //     Err(e) => {
                //         eprintln!("[depth={depth}] {e}");
                //         break;
                //     }
                // };

                // If we received an error, that means the search was stopped externally
                if let Err(_err) = search.start(depth) {
                    // eprintln!("[depth={depth}] {_err}");
                    break;
                }

                // Send the updated info, now that the search has concluded
                let info = info.read().unwrap();

                // Now send the info to the GUI
                // Can't call `self.info` because we're inside a thread
                let resp: UciResponse<&str> = UciResponse::Info(info.clone());
                _ = resp.send();

                depth += 1;
            }

            // TODO: If this line of code is reached, it means the search has stopped on its own
            // So, we need to store `false` in the stopper, and send bestmove.
            // On the other hand, if the engine received `stop`, then we do NOT need to send bestmove here.

            if is_searching.load(Ordering::Relaxed) {
                is_searching.store(false, Ordering::Relaxed);
                let res = result.read().unwrap();
                let bestmove_string = res.bestmove.map(|mv| mv.to_string()).unwrap_or_default();
                let ponder_string = res.ponder.map(|p| p.to_string());

                let resp = UciResponse::BestMove(bestmove_string, ponder_string);
                _ = resp.send();
            }
        });

        Ok(())
    }

    fn stop(&mut self) -> io::Result<()> {
        // Only need to stop if we're already searching. Otherwise, don't need to send anything new
        if self.is_searching() {
            self.stop_search();

            let res = self.search_result.read().unwrap();
            let bestmove = res.bestmove.unwrap_or_default().to_string(); // Default to illegal move
            let ponder = res.ponder.map(|p| p.to_string());

            self.bestmove(bestmove, ponder)?;
        }
        Ok(())
    }

    fn ponderhit(&self) -> io::Result<()> {
        todo!("Handle ponderhit")
    }

    fn quit(&mut self) -> io::Result<()> {
        Ok(())
    }

    /* Engine to GUI communication */
    fn id(&self) -> io::Result<()> {
        let name = format!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
        let author = format!("{}", env!("CARGO_PKG_AUTHORS"));

        let resp = UciResponse::Id(name, author);
        resp.send()
    }

    fn uciok(&self) -> io::Result<()> {
        let resp: UciResponse<&str> = UciResponse::UciOk;
        resp.send()
    }

    fn readyok(&self) -> io::Result<()> {
        let resp: UciResponse<&str> = UciResponse::ReadyOk;
        resp.send()
    }

    fn bestmove(&self, bestmove: String, ponder: Option<String>) -> io::Result<()> {
        // https://backscattering.de/chess/uci/#engine-bestmove-info
        let info = self.info.read().unwrap();
        self.info(info.clone())?;

        let resp = UciResponse::BestMove(bestmove, ponder);
        resp.send()
    }

    fn copyprotection(&self) -> io::Result<()> {
        let resp = UciResponse::CopyProtection("checking");
        resp.send()?;

        // This engine isn't copy protected, so do nothing here.

        let resp = UciResponse::CopyProtection("ok");
        resp.send()
    }

    fn registration(&self) -> io::Result<()> {
        let resp = UciResponse::Registration("checking");
        resp.send()?;

        // This engine requires no registration, so do nothing here.

        let resp = UciResponse::Registration("ok");
        resp.send()
    }

    fn info(&self, info: UciInfo) -> io::Result<()> {
        let resp: UciResponse<&str> = UciResponse::Info(info);
        resp.send()
    }

    fn option(&self) -> io::Result<()> {
        for opt in self.get_uci_options() {
            let resp = UciResponse::Option(opt);
            resp.send()?;
        }
        Ok(())
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self {
            game: Game::from_fen(
                // "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
                FEN_STARTPOS,
            )
            .unwrap(),
            // ttable: TranspositionTable::default(),
            // protocol: EngineProtocol::UCI,
            debug: false,
            is_searching: Arc::default(),
            // search_status: Arc::default(),
            search_result: Arc::default(),
            info: Arc::default(),
            // protocol: EngineProtocol::default(),
        }
    }
}
