use std::{
    fmt, io,
    str::FromStr,
    sync::{
        atomic::{self, AtomicBool, Ordering},
        Arc,
    },
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

use crate::uci::{
    SearchOptions, UciEngine, UciInfo, UciOption, UciOptionType, UciResponse, UciResult,
};
use dutchess_core::{
    utils::DEFAULT_FEN, BitBoard, ChessBoard, Color, File, Move, Piece, PieceKind, Rank, Tile,
};

use chess::{Board, MoveGen};

/*
#[derive(PartialEq, Eq, Debug, Hash)]
pub enum MoveLegality {
    /// Move is completely legal
    Legal, // TODO: Contain the move?

    /// Can't capture the same color piece
    CaptureSameColor(Color),

    /// King is in check by the pieces at the provided positions
    PutsKingInCheck(Vec<Tile>),

    /// That piece doesn't move that way.
    InvalidMovement(PieceKind),

    /// There isn't a piece at the selected tile
    NoPieceAtTile(Tile),
}

impl MoveLegality {
    pub fn is_legal(&self) -> bool {
        *self == Self::Legal
    }
}

impl fmt::Display for MoveLegality {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            Self::Legal => format!("Move is legal"),
            Self::CaptureSameColor(color) => {
                format!("Cannot capture a piece with the same color ({color})")
            }
            Self::PutsKingInCheck(attacking) => format!("That move puts your King in check by the pieces at the following positions: {attacking:?}"),
            Self::InvalidMovement(kind) => format!("The {} doesn't move that way...", kind.name()),
            Self::NoPieceAtTile(from) => format!("There's no piece at {from} to be moved"),
        };

        write!(f, "{reason}")
    }
}
 */

/*********************************************************************************
 * Game board
*********************************************************************************/

type TranspositionTable = ();

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash, Default)]
pub enum EngineProtocol {
    #[default]
    UCI,
}

#[derive(Debug)]
pub struct Engine {
    /// State of the game, including castling rights, piece placement, etc.
    ///
    /// This is analogous to a FEN string.
    state: GameState,

    /// BitBoard representation of the game board.
    board: ChessBoard,

    /// A history of all moves made in this game.
    history: Vec<Move>,

    /// A list of all available (legal) moves at the current game state.
    legal_moves: Vec<Move>,

    /// Transposition table for game states.
    ttable: TranspositionTable,

    /// Communication protocol
    protocol: EngineProtocol,

    debug: bool,

    searching: Arc<AtomicBool>,
    // search_handle: Option<JoinHandle<()>>,
}

impl Engine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_fen(fen: &str) -> Result<Self, String> {
        let mut s = Self::new();
        s.setup(fen)?;
        Ok(s)
    }

    pub fn fen(&self) -> String {
        self.state.to_fen()
    }

    pub fn setup(&mut self, fen: &str) -> Result<(), String> {
        self.state = GameState::from_fen(fen)?;
        self.board = ChessBoard::from(self.state.pieces);
        self.generate_legal_moves();
        Ok(())
    }

    /// Fetch the piece at the requested position, if it exists.
    fn get(&self, tile: Tile) -> Option<Piece> {
        *self.state.piece(tile)
    }

    /// Set the piece at the specified position.
    fn set(&mut self, tile: Tile, piece: Piece) {
        self.board.set(tile, piece); // Update the bitboards
        *self.state.piece_mut(tile) = Some(piece); // Update the state
    }

    /// Remove a piece from the specified position, returning it if it exists.
    fn clear(&mut self, tile: Tile) -> Option<Piece> {
        self.board.clear(tile); // Update the bitboards
        self.state.piece_mut(tile).take() // Update the state
    }

    /// Returns `true` if the move was made successful, and `false` if it cannot be made.
    fn apply_move(&mut self, chessmove: Move) -> bool {
        let Some(piece) = self.clear(chessmove.from()) else { return false };

        if let Some(promotion) = chessmove.promote() {
            self.set(chessmove.to(), piece.promoted(promotion));
        } else {
            self.set(chessmove.to(), piece);
        }

        self.history.push(chessmove);
        self.generate_legal_moves();
        true
    }

    fn apply_moves(&mut self, moves: impl IntoIterator<Item = Move>) -> bool {
        moves
            .into_iter()
            .all(|chessmove| self.apply_move(chessmove))
    }

    // pub fn is_legal(&mut self, from: Tile, to: Tile) -> bool {
    //     self.legal_moves().contains(&Move::new(from, to))
    // }

    pub fn legal_moves(&self) -> &[Move] {
        &self.legal_moves
    }

    pub fn legal_moves_of(&self, tile: Tile) -> BitBoard {
        let mut moves = BitBoard::default();

        for legal in self.legal_moves() {
            if legal.from() == tile {
                moves.set_index(legal.to().index());
            }
        }

        moves
    }

    fn generate_legal_moves(&mut self) {
        self.legal_moves.clear();

        let board = Board::from_str(&self.fen()).unwrap();

        self.legal_moves
            .extend(MoveGen::new_legal(&board).map(|legal| {
                Move::from_indices(legal.get_source().to_index(), legal.get_dest().to_index())
                    .unwrap()
            }))
    }

    /// Main entrypoint of the engine
    pub fn run(&mut self) -> std::io::Result<()> {
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
    fn search(&mut self, search_opt: SearchOptions) {
        if self.searching.load(Ordering::Relaxed) {
            // eprintln!("Engine is already searching")
            self.stop_search();
        }
        // "Unstop" the search
        self.start_search();

        let searching = Arc::clone(&self.searching);

        // if search_opt.infinite {
        //     //
        // }

        let movetime = search_opt
            .move_time
            // .map(|micros| Duration::from_micros(micros))
            .unwrap_or(Duration::MAX);

        let starttime = Instant::now();

        thread::spawn(move || {
            while searching.load(Ordering::Relaxed) && starttime.elapsed() < movetime {
                // info depth 1 seldepth 1 multipv 1 score cp 38 nodes 20 nps 10000 tbhits 0 time 2 pv d2d4
                let info = UciInfo::new().depth(1);
                let resp = UciResponse::Info(info);
                _ = resp.send();

                // thread::sleep(Duration::from_secs(1));
            }

            // Ensure that the search has stopped, even if we broke out of the loop for other reasons.
            searching.store(false, Ordering::Relaxed);
        });
    }

    /// Sets the flag that the search should stop.
    fn stop_search(&mut self) {
        self.searching.store(false, Ordering::Relaxed);
    }

    /// Sets the flag that the search should be started.
    fn start_search(&mut self) {
        self.searching.store(true, Ordering::Relaxed);
    }

    // fn block_until_done(&mut self) -> Option<()> {
    //     self.search_handle
    //         .take()
    //         .map(|handle| handle.join().unwrap())
    // }

    fn new_game(&mut self) {
        // *self = Self::new();
    }
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

    fn debug(&mut self, status: bool) {
        self.debug = status;
    }

    fn isready(&self) -> io::Result<()> {
        let resp = UciResponse::ReadyOk;
        resp.send()
    }

    fn setoption(&mut self, name: &str, value: &str) {
        match name {
            _ => eprintln!("Unrecognized option `{name}`"),
        }
    }

    fn register(&mut self, registration: Option<(&str, &str)>) {
        // No registration necessary :)
        _ = registration;

        // match registration {
        //     UciRegistration::Later => {}
        //     UciRegistration::Now(name, code) => {}
        // }
    }

    fn ucinewgame(&mut self) {
        // *self = Self::new();
        self.new_game();
    }

    fn position(&mut self, fen: &str, moves: Vec<&str>) {
        // Apply the FEN to the game state
        _ = self.setup(fen); // ignore any errors if they occur.

        // Now, if there are any moves, apply them as well.
        self.apply_moves(moves.into_iter().map(|san| Move::from_uci(san).unwrap()));
    }

    fn go(&mut self, search_opt: SearchOptions) {
        self.search(search_opt);
    }

    fn stop(&mut self) -> io::Result<()> {
        self.stop_search();

        let bestmove = self.legal_moves().first().unwrap().to_string();
        let ponder = self.legal_moves().last().unwrap().to_string();

        self.bestmove(bestmove, Some(ponder))?;

        Ok(())
    }

    fn ponderhit(&self) {
        todo!("Handle ponderhit")
    }

    fn quit(&mut self) {
        // std::process::exit(0);
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
    fn registeration(&self) -> io::Result<()> {
        let resp = UciResponse::Registration("checking");
        resp.send()?;

        // This engine requires no registration, so do nothing here.

        let resp = UciResponse::Registration("ok");
        resp.send()
    }
    fn info(&self, info: UciInfo) -> io::Result<()> {
        // let resp = UciResponse::Info(info);
        // resp.send()

        todo!()
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
        let state = GameState::default();
        let board = ChessBoard::from(state.pieces);
        let mut s = Self {
            state,
            board,
            history: Vec::with_capacity(128),
            // Don't need more than 218
            // https://www.chess.com/forum/view/general/whats-the-maximum-number-of-moves-possible
            legal_moves: Vec::with_capacity(256),
            ttable: TranspositionTable::default(),
            protocol: EngineProtocol::UCI,
            debug: false,
            // search_handle: None,
            // searching: Arc::new(AtomicBool::new(false)),
            searching: Arc::default(),
        };
        s.generate_legal_moves();
        s
    }
}

// impl Index<Tile> for Engine {
//     type Output = Option<Piece>;
//     fn index(&self, index: Tile) -> &Self::Output {
//         &self.state.pieces[index]
//     }
// }

/// Represents the current state of the game, including move counters
///
/// Analogous to a FEN string.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct GameState {
    /// Can be indexed by [`Position`]
    pieces: [Option<Piece>; 64],

    current_player: Color,
    can_white_kingside_castle: bool,
    can_white_queenside_castle: bool,
    can_black_kingside_castle: bool,
    can_black_queenside_castle: bool,
    en_passant_tile: Option<Tile>,

    /// Used to enforce the fifty-move rule.
    /// Is incremented after each move.
    /// Is reset after a capture or a pawn moves.
    halfmove: u8,

    /// Number of moves since the beginning of the game/
    /// A fullmove is a complete turn by white and then by black.
    fullmove: u8,
}

impl GameState {
    fn from_fen(fen: &str) -> Result<Self, &'static str> {
        let mut split = fen.split(' ');
        let placements = split.next().ok_or("Empty string is not valid FEN")?;

        if placements.matches('/').count() != 7 {
            return Err(
                "Valid FEN placements must have 8 rows delimited with 7 forward slashes `/`",
            );
        }

        let mut pieces = [None; 64];
        // Have to reverse this so that white appears on the bottom
        for (rank, placements) in placements.split('/').rev().enumerate() {
            let mut file = 0;
            for piece_char in placements.chars() {
                if let Ok(kind) = PieceKind::from_char(piece_char) {
                    let piece = if piece_char.is_ascii_uppercase() {
                        Piece::new(Color::White, kind)
                    } else {
                        Piece::new(Color::Black, kind)
                    };

                    pieces[rank * 8 + file] = Some(piece);
                    file += 1;
                } else {
                    let Some(empty) = piece_char.to_digit(10) else { return Err("Invalid non-char digit when parsing FEN string") };
                    file += empty as usize
                }
            }
        }

        let active_color = split.next().unwrap_or_else(|| {
            let x = "w";
            eprintln!("Warning: No active color specified; defaulting to {x}");
            x
        });
        let current_player = match active_color {
            "w" | "W" => Color::White,
            "b" | "B" => Color::Black,
            _ => return Err("Active color must be either `w` or `b`"),
        };

        let castling = split.next().unwrap_or_else(|| {
            let x = "KQkq";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let can_white_kingside_castle = castling.contains('K');
        let can_white_queenside_castle = castling.contains('Q');
        let can_black_kingside_castle = castling.contains('k');
        let can_black_queenside_castle = castling.contains('q');

        let en_passant_target = split.next().unwrap_or_else(|| {
            let x = "-";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let en_passant_tile = match en_passant_target {
            "-" => None,
            tile => {
                let Ok(pos) = Tile::from_uci(tile) else { return Err("Invalid En Passant target when parsing FEN string") };
                Some(pos)
            }
        };

        let halfmove = split.next().unwrap_or_else(|| {
            let x = "0";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let Ok(halfmove) = halfmove.parse() else { return Err("Invalid halfmove; must be numeric") };

        let fullmove = split.next().unwrap_or_else(|| {
            let x = "1";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let Ok(fullmove) = fullmove.parse() else { return Err("Invalid fullmove; must be numeric") };

        Ok(Self {
            pieces,
            current_player,
            can_white_kingside_castle,
            can_white_queenside_castle,
            can_black_kingside_castle,
            can_black_queenside_castle,
            en_passant_tile,
            halfmove,
            fullmove,
        })
    }

    pub fn to_fen(&self) -> String {
        let mut placements: [String; 8] = Default::default();

        for rank in Rank::iter() {
            let mut empty_spaces = 0;
            for file in File::iter() {
                if let Some(piece) = self.piece(file + rank) {
                    if empty_spaces != 0 {
                        placements[rank.index()] += &empty_spaces.to_string();
                        empty_spaces = 0;
                    }
                    placements[rank.index()] += &piece.to_string();
                } else {
                    empty_spaces += 1;
                }
            }

            if empty_spaces != 0 {
                placements[rank.index()] += &empty_spaces.to_string();
            }
        }
        placements.reverse();
        let placements = placements.join("/");

        let active_color = self.current_player;

        let mut castling = String::with_capacity(4);
        if self.can_white_kingside_castle {
            castling += "K"
        }
        if self.can_white_queenside_castle {
            castling += "Q"
        }
        if self.can_black_kingside_castle {
            castling += "k"
        }
        if self.can_black_queenside_castle {
            castling += "q"
        }
        if castling.is_empty() {
            castling = String::from("-");
        }

        let en_passant_target = if let Some(tile) = self.en_passant_tile {
            tile.to_string()
        } else {
            String::from("-")
        };

        let halfmove = self.halfmove;
        let fullmove = self.fullmove;

        let fen = format!(
            "{placements} {active_color} {castling} {en_passant_target} {halfmove} {fullmove}"
        );

        fen
    }

    fn piece(&self, tile: Tile) -> &Option<Piece> {
        &self.pieces[tile]
    }

    fn piece_mut(&mut self, tile: Tile) -> &mut Option<Piece> {
        &mut self.pieces[tile]
    }
}

impl Default for GameState {
    fn default() -> Self {
        // Safe unwrap: Default FEN is always valid
        Self::from_fen(DEFAULT_FEN).unwrap()
    }
}

impl fmt::Display for GameState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board = String::with_capacity(577);

        for rank in Rank::iter() {
            for _ in File::iter() {
                board += "+---";
            }

            board += "+\n";

            for file in File::iter() {
                let occupant = if let Some(piece) = self.piece(file + rank) {
                    piece.to_string()
                } else {
                    String::from(" ")
                };

                board += &format!("| {occupant} ");
            }

            board += "|\n";
        }
        for _ in File::iter() {
            board += "+---";
        }
        board += "+";

        write!(f, "{board}")
    }
}

/*********************************************************************************
 * Game logic
*********************************************************************************/

/*********************************************************************************
 * Utility functions
*********************************************************************************/
