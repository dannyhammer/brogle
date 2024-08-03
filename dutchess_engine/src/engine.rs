use std::{
    io,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
    thread,
    time::{Duration, Instant},
};

use anyhow::Result;

use super::{
    search::{Search, SearchResult},
    uci::{UciEngine, UciInfo, UciOption, UciResponse, UciSearchMode},
    Evaluator,
};
use dutchess_core::{print_perft, print_split_perft, Game, Move, Position, FEN_STARTPOS};

// type TranspositionTable = ();

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash, Default)]
pub enum EngineProtocol {
    #[default]
    UCI,
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
    /// State of the game, including castling rights, piece placement, etc.
    ///
    /// This is analogous to a FEN string.
    game: Game,

    /// Transposition table for game states.
    // ttable: TranspositionTable,

    /// Communication protocol
    // protocol: EngineProtocol,
    debug: bool,

    searching: Arc<AtomicBool>,

    search_result: Arc<Mutex<SearchResult>>,
    // search_handle: Option<JoinHandle<()>>,
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

    fn get_uci_options(&self) -> impl Iterator<Item = UciOption> {
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
        self.searching.store(false, Ordering::Relaxed);
    }

    /// Sets the flag that the search should be started.
    fn start_search(&mut self) {
        self.searching.store(true, Ordering::Relaxed);
    }

    /// Yields `true` if the engine is currently searching.
    fn is_searching(&self) -> bool {
        self.searching.load(Ordering::Relaxed)
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

        Ok(EngineCommand::Search(depth, iterative))
    }

    /// Parses the custom `perft` command
    fn parse_perft_command(&self, rest: &str) -> Result<EngineCommand, String> {
        let mut args = rest.split_ascii_whitespace();

        let Some(depth) = args.next() else {
            return Err(format!("usage: perft <depth> [pretty] [split]"));
        };

        let Ok(depth) = depth.parse() else {
            return Err(format!("usage: perft <depth> [pretty] [split]"));
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
    fn parse_eval_command(&self, rest: &str) -> Result<EngineCommand, String> {
        let mut args = rest.split_ascii_whitespace();

        let mut pos = self.game.position().clone();

        if let Some(arg) = args.next() {
            if arg.to_ascii_lowercase() == "startpos" {
                pos = Position::standard_setup();
            } else if let Ok(parsed) = Position::from_fen(arg) {
                pos = parsed;
            } else {
                return Err(format!("usage: eval [FEN]"));
            }
        }

        Ok(EngineCommand::Eval(pos))
    }

    /// Parses the custom `fen` command
    fn parse_fen_command(&self, rest: &str) -> Result<EngineCommand, String> {
        let mut args = rest.split_ascii_whitespace();

        let mut fen = None;
        if let Some(arg) = args.next() {
            if arg.to_ascii_lowercase() == "startpos" {
                fen = Some(FEN_STARTPOS.to_string());
            } else if Position::from_fen(arg).is_ok() {
                fen = Some(arg.to_string());
            } else {
                return Err(format!("usage: fen [FEN]"));
            }
        }

        Ok(EngineCommand::Fen(fen))
    }

    /// Parses the custom `move` command
    fn parse_move_command(&self, rest: &str) -> Result<EngineCommand, String> {
        if rest.is_empty() {
            return Err(format!("usage: move <move1> [move2 move3 ...]"));
        }

        let mut moves = vec![];
        let mut pos = self.game.position().clone();

        for arg in rest.split_ascii_whitespace() {
            match Move::from_uci(&pos, arg) {
                Ok(mv) => {
                    if let Err(err) = pos.make_move_checked(mv) {
                        return Err(format!("Invalid move: {err}"));
                    }
                    moves.push(mv);
                }
                Err(err) => return Err(format!("{err}")),
            }
        }

        Ok(EngineCommand::Move(moves))
    }

    /// Parses custom commands
    fn parse_custom_command(&self, input: &str) -> Result<EngineCommand, String> {
        let (cmd, rest) = if input.contains(' ') {
            input.split_once(' ').unwrap()
        } else {
            (input, "")
        };

        match cmd {
            "help" => Ok(EngineCommand::Help),
            "show" => Ok(EngineCommand::Show),
            "search" => self.parse_search_command(rest),
            "perft" => self.parse_perft_command(rest),
            "eval" => self.parse_eval_command(rest),
            "move" => self.parse_move_command(rest),
            // "moves" => Ok(EngineCommand::Moves),
            "fen" => self.parse_fen_command(rest),
            _ => Err(format!("Failed to parse custom command: {input:?}")),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
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

    /// Show the current state of of the board as a FEN string.
    Fen(Option<String>),

    /// Make the list of moves applied to the board.
    Move(Vec<Move>),

    /// Evaluates the current position.
    Eval(Position),

    /// Search the current position up to the provided depth.
    Search(u32, bool),
}

impl UciEngine for Engine {
    /* GUI to Engine communication */

    fn custom_command(&mut self, input: &str) -> io::Result<()> {
        let cmd = match self.parse_custom_command(input) {
            Ok(cmd) => cmd,
            Err(err) => {
                eprintln!("{err}");
                return Ok(());
            }
        };

        match cmd {
            EngineCommand::Help => {
                println!("available commands: help, perft, uci, bench, show, moves, undo, eval")
            }
            EngineCommand::Show => println!("{:?}", self.game.position()),
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
            EngineCommand::Eval(pos) => println!("{}", Evaluator::new(&pos).eval()),
            EngineCommand::Move(moves) => self.game.make_moves(moves),
            EngineCommand::Search(depth, _) => {
                let search = Search::new(self.game.clone(), depth);
                let res = search.start();
                // let res = search(self.game, depth);
                if let Some(bestmove) = res.bestmove {
                    println!("bestmove {bestmove} ({})", res.score);
                } else {
                    println!("No bestmove found in search. Score={}", res.score);
                }
                // println!("{:?}", search(&self.game, depth)),
            } // EngineCommand::Moves => println!("{:?}", self.game),
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
        let resp = UciResponse::ReadyOk;
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

    fn go(&mut self, mode: UciSearchMode) -> io::Result<()> {
        // Arena sent this to our engine
        // go wtime 300000 btime 300000 winc 0 binc 0

        if self.is_searching() {
            eprintln!("Engine is already searching");
            self.stop_search();
        }

        // Flip the flag to signal a search has begun.
        self.start_search();

        // Clone the arcs for whether we're searching and our found results
        let stopper = Arc::clone(&self.searching);
        let result = Arc::clone(&self.search_result);

        // Parse the timeout duration
        let timeout = match mode {
            UciSearchMode::Infinite => Duration::MAX,
            UciSearchMode::Ponder => {
                // thread::spawn(move || {
                //     println!("Pondering infinitely");
                //     todo!()
                // });
                todo!("Implement ponder")
            }
            UciSearchMode::Timed(search_opt) => {
                if let (Some(wtime), Some(btime)) = (search_opt.w_time, search_opt.b_time) {
                    let remaining = if self.game.position().current_player().is_white() {
                        wtime
                    } else {
                        btime
                    };

                    Duration::from_secs_f64(remaining.as_secs_f64() / 300.0)
                } else if let Some(movetime) = search_opt.move_time {
                    movetime
                } else {
                    eprintln!("Warning: No movetime specified. Defaulting to infinite.");
                    Duration::MAX
                }
            }
        };

        let state = self.game.clone();

        let starttime = Instant::now();
        // eprintln!("TIMEOUT: {timeout:?}");

        let mut depth = 1;
        let max_depth = 10;
        let mut bestmove = None;

        thread::spawn(move || {
            // Search ends if we've timed out, been told to stop, or reached our depth limit
            while starttime.elapsed() < timeout
                && depth < max_depth
                && stopper.load(Ordering::Relaxed)
            {
                // Obtain a result from the search
                let mut search = Search::new(state.clone(), depth);
                // Iterative deepening
                if let Some(ponder) = bestmove {
                    search = search.with_ponder(ponder);
                }
                let res = search.start();
                let elapsed = starttime.elapsed();
                bestmove = res.bestmove;

                // Construct a new message to be sent
                let info = UciInfo::new()
                    .depth(depth)
                    // .seldepth(seldepth)
                    // .multipv(multipv)
                    .score(format!("cp {}", res.score))
                    .nodes(res.nodes_searched)
                    .nps((res.nodes_searched as f64 / elapsed.as_secs_f64()) as usize)
                    // .tbhits(tbhits)
                    // .time(format!("{:?}", starttime.elapsed()))
                    .pv([format!("{}", res.bestmove.unwrap())]);
                let resp = UciResponse::Info(info);

                // Now send the info to the GUI
                resp.send()?;

                // Finally, store the info from the search
                *result.lock().unwrap() = res;

                depth += 1;
            }

            let bestmove_string = bestmove.map(|m| m.to_string()).unwrap_or_default();
            // let ponder = res.ponder.map(|p| p.to_string());
            let ponder = None;
            let resp = UciResponse::BestMove(bestmove_string, ponder);
            resp.send()
        });

        Ok(())
    }

    fn stop(&mut self) -> io::Result<()> {
        self.stop_search();

        // let bestmove = std::mem::take(&mut *self.bestmove.lock().unwrap());
        // let ponder = self.ponder.lock().unwrap().take();
        // let bestmove = self.bestmove.lock().unwrap().clone();
        // let ponder = self.ponder.lock().unwrap().clone();
        let res = self.search_result.lock().unwrap();
        let bestmove = res.bestmove.map(|m| m.to_string()).unwrap_or_default();
        // let ponder = res.ponder.map(|p| p.to_string());
        let ponder = None;

        self.bestmove(bestmove, ponder)
    }

    fn ponderhit(&self) -> io::Result<()> {
        todo!("Handle ponderhit")
    }

    fn quit(&mut self) -> io::Result<()> {
        // std::process::exit(0);
        Ok(())
    }

    /* Engine to GUI communication */
    fn id(&self) -> io::Result<()> {
        let name = format!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
        let author = env!("CARGO_PKG_AUTHORS");

        let resp = UciResponse::Id(&name, author);
        resp.send()
    }
    fn uciok(&self) -> io::Result<()> {
        let resp = UciResponse::UciOk;
        resp.send()
    }
    fn readyok(&self) -> io::Result<()> {
        let resp = UciResponse::ReadyOk;
        resp.send()
    }
    fn bestmove(&self, bestmove: String, ponder: Option<String>) -> io::Result<()> {
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
        let resp = UciResponse::Info(info);
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
            // game: Game::from_fen(FEN_STARTPOS).unwrap(),
            game: Game::from_fen(
                "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            )
            .unwrap(),
            // ttable: TranspositionTable::default(),
            // protocol: EngineProtocol::UCI,
            debug: false,
            searching: Arc::default(),
            search_result: Arc::default(),
            // protocol: EngineProtocol::default(),
        }
    }
}
