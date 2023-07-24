use std::{
    io,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
    thread,
    time::{Duration, Instant},
};

use crate::{
    search::{search, SearchResult},
    uci::{
        SearchOptions, UciEngine, UciInfo, UciOption, UciOptionType, UciResponse, UciResult,
        UciSearchMode,
    },
};
use chess::{Board, ChessMove, MoveGen};
// use dutchess_core::{Game, Move};

type TranspositionTable = ();

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash, Default)]
pub enum EngineProtocol {
    #[default]
    UCI,
}

#[derive(Debug, Default)]
pub struct Engine {
    /// State of the game, including castling rights, piece placement, etc.
    ///
    /// This is analogous to a FEN string.
    // game: Game,
    game: Board,

    /// Transposition table for game states.
    ttable: TranspositionTable,

    /// Communication protocol
    protocol: EngineProtocol,

    debug: bool,

    searching: Arc<AtomicBool>,

    search_result: Arc<Mutex<SearchResult>>,
    // search_handle: Option<JoinHandle<()>>,
}

impl Engine {
    /*
    pub fn new(game: Game) -> Self {
        Self {
            game,
            ..Default::default()
        }
    }
     */
    pub fn new(game: Board) -> Self {
        Self {
            game,
            ..Default::default()
        }
    }

    pub fn from_fen(fen: &str) -> Result<Self, String> {
        let game = Board::from_str(fen).map_err(|e| e.to_string())?;
        Ok(Self::new(game))
    }

    /// Main entrypoint of the engine
    pub fn run(&mut self) -> io::Result<()> {
        let name = env!("CARGO_PKG_NAME");
        let version = env!("CARGO_PKG_VERSION");
        let authors = env!("CARGO_PKG_AUTHORS");
        println!("{name} v{version} by {authors}");

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
            UciOption::check("Nullmove", true),
            UciOption::spin("Selectivity", 2, 0, 4),
            UciOption::combo("Style", "Normal", ["Solid", "Normal", "Risky"]),
            UciOption::string("NalimovPath", "c:\\"),
            UciOption::button("Clear Hash"),
        ]
        .into_iter()
    }

    /// Non-blocking search

    /// Sets the flag that the search should stop.
    fn stop_search(&mut self) {
        self.searching.store(false, Ordering::Relaxed);
    }

    /// Sets the flag that the search should be started.
    fn start_search(&mut self) {
        self.searching.store(true, Ordering::Relaxed);
    }

    fn is_searching(&self) -> bool {
        self.searching.load(Ordering::Relaxed)
    }

    // fn block_until_done(&mut self) -> Option<()> {
    //     self.search_handle
    //         .take()
    //         .map(|handle| handle.join().unwrap())
    // }

    fn new_game(&mut self) {
        // *self = Self::new();
    }

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
}

impl UciEngine for Engine {
    /* GUI to Engine communication */

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
            _ => eprintln!("Unrecognized option `{name}`"),
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
        // *self = Self::new();
        self.new_game();
        Ok(())
    }

    fn position(&mut self, fen: &str, moves: Vec<&str>) -> io::Result<()> {
        // Apply the FEN to the game state
        // _ = self.setup(fen); // ignore any errors if they occur.
        // self.game = Game::from_fen(fen).unwrap();
        self.game = Board::from_str(fen).unwrap();

        // Now, if there are any moves, apply them as well.
        // self.game
        //     .apply_moves(moves.into_iter().map(|m| Move::from_uci(m).unwrap()));

        for mv in moves {
            let mv = ChessMove::from_str(mv).unwrap();
            self.game = self.game.make_move_new(mv);
        }

        Ok(())
    }

    fn go(&mut self, mode: UciSearchMode) -> io::Result<()> {
        // Arena sent this to our engine
        // go wtime 300000 btime 300000 winc 0 binc 0

        if self.searching.load(Ordering::Relaxed) {
            // eprintln!("Engine is already searching")
            self.stop_search();
        }

        // Flip the flag to signal a search has begun.
        self.start_search();

        // Clone the arcs for whether we're searching and our found results
        let stopper = Arc::clone(&self.searching);
        let result = Arc::clone(&self.search_result);

        let timeout = match mode {
            UciSearchMode::Infinite => Duration::MAX,
            UciSearchMode::Ponder => {
                // thread::spawn(move || {
                //     println!("Pondering infinitely");
                //     todo!()
                // });
                todo!()
            }
            UciSearchMode::Timed(search_opt) => {
                if let (Some(wtime), Some(btime)) = (search_opt.w_time, search_opt.b_time) {
                    // if self.game.state().current_player().is_white() {
                    if self.game.side_to_move() == chess::Color::White {
                        wtime
                    } else {
                        btime
                    }
                } else if let Some(movetime) = search_opt.move_time {
                    movetime
                } else {
                    eprintln!("Warning: No movetime specified");
                    Duration::MAX
                }
            }
        };

        // let state = self.game.state().clone();
        let state = self.game.clone();

        let starttime = Instant::now();

        let mut depth = 0;
        let max_depth = 10;

        thread::spawn(move || {
            loop {
                // Search ends if we've timed out, been told to stop, or reached out depth limit
                if depth == max_depth
                    || starttime.elapsed() >= timeout
                    || !stopper.load(Ordering::Relaxed)
                {
                    break;
                }

                // Obtain a result from the search
                let res = search(state, depth);

                // Construct a new message to be sent
                let info = UciInfo::new().depth(depth);
                // .seldepth(seldepth)
                // .multipv(multipv)
                // .score(score)
                // .nodes(nodes)
                // .nps(nps)
                // .tbhits(tbhits)
                // .time(time)
                // .pv(pv);
                let resp = UciResponse::Info(info);

                // Now send the info to the GUI
                _ = resp.send();

                // Finally, store the info from the search
                *result.lock().unwrap() = res;

                depth += 1;
            }

            let res = result.lock().unwrap();
            let bestmove = res.bestmove.to_string();
            // let ponder = res.ponder.map(|p| p.to_string());
            let ponder = None;
            let resp = UciResponse::BestMove(bestmove, ponder);
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
        let bestmove = res.bestmove.to_string();
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

/*
impl Default for Engine {
    fn default() -> Self {
        Self {
            state: GameState::default(),
            ttable: TranspositionTable::default(),
            protocol: EngineProtocol::UCI,
            debug: false,
            searching: Arc::default(),
            search_result: Arc::default(),
        }
    }
}
 */
