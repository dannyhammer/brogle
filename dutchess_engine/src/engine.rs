use std::{fmt, io, ops::Index};

use crate::{
    moves_for,
    uci::{SearchOptions, UciCommand, UciOption, UciOptionType, UciResponse},
    utils::DEFAULT_FEN,
    BitBoard, ChessBoard, Color, File, Piece, PieceKind, Rank, Tile,
};

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

/*********************************************************************************
 * Game board
*********************************************************************************/

type TranspositionTable = ();

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub enum EngineProtocol {
    UCI,
}

#[derive(PartialEq, Eq, Debug, Hash)]
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
}

impl Engine {
    pub fn new() -> Self {
        let state = GameState::default();
        let board = ChessBoard::from(state.pieces);
        let s = Self {
            state,
            board,
            history: Vec::with_capacity(64),
            legal_moves: Vec::with_capacity(1024),
            ttable: (),
            protocol: EngineProtocol::UCI,
            debug: true,
        };
        // s.generate_all_legal_moves();
        s
    }

    pub fn fen(&self) -> String {
        self.state.to_fen()
    }

    pub fn piece_at(&self, tile: Tile) -> Option<Piece> {
        *self.state.piece(tile)
        // self.board.get(tile)
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
    pub fn make_move(&mut self, from: Tile, to: Tile) -> bool {
        let Some(piece) = self.clear(from) else { return false };
        self.set(to, piece);
        true
    }

    pub fn is_legal(&mut self, from: Tile, to: Tile) -> bool {
        // TODO: Check if exists in all legal moves.
        self.legal_moves().contains(&Move::new(from, to))
    }

    pub fn legal_moves(&self) -> &[Move] {
        &self.legal_moves
    }

    pub fn attacked_by(&self, piece: &Piece) -> Vec<Tile> {
        vec![]
    }

    pub fn legal_moves_for(&self, piece: &Piece, tile: Tile) -> BitBoard {
        moves_for(piece, tile, &self.board)
    }

    pub fn legal_moves_of(&self, tile: Tile) -> Option<BitBoard> {
        self.piece_at(tile)
            .map(|piece| moves_for(&piece, tile, &self.board))
    }

    /// Main entrypoint of the engine
    pub fn run(&mut self) -> std::io::Result<()> {
        let mut buffer = String::new();
        loop {
            buffer.clear();
            io::stdin().read_line(&mut buffer)?;

            let mut split = buffer.split_whitespace();

            if let Some(protocol) = split.next() {
                match protocol {
                    "uci" => self.uci_loop()?,
                    _ => {
                        eprintln!(
                            "Unimplemented protocol: {protocol}.\nImplemented protocols: UCI, "
                        );
                        return Ok(());
                    }
                }
            }
        }
    }

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

    /* UCI-related functions */

    // Engine receive a `uci` command
    pub fn uci(&mut self) -> io::Result<()> {
        // The engine must now identify itself
        self.id()?;
        // And send all available options
        self.option()?;
        // Engine has sent all parameters and is ready
        self.uciok()?;

        Ok(())
    }
    pub fn debug(&mut self, status: bool) {
        self.debug = status;
    }
    pub fn isready(&self) -> io::Result<()> {
        let resp = UciResponse::ReadyOk;
        resp.send()
    }
    pub fn setoption(&mut self, name: &str, value: &str) {
        // match name {
        // }
    }
    pub fn register(&mut self, registration: Option<(&str, &str)>) {
        // No registration necessary :)
        _ = registration;

        // match registration {
        //     UciRegistration::Later => {}
        //     UciRegistration::Now(name, code) => {}
        // }
    }
    pub fn ucinewgame(&mut self) {}
    pub fn position(&mut self, fen: &str, moves: Vec<&str>) {
        // Apply the FEN to the game state
        // self.set_state(fen)

        // Now, if there are any moves, apply them as well.
        for mv in moves {
            //
        }
    }
    pub fn go(&mut self, search_opt: SearchOptions) {}
    pub fn stop(&self) {}
    pub fn ponderhit(&self) {}
    pub fn quit(&mut self) {}

    /* Engine to GUI communication */
    fn id(&self) -> io::Result<()> {
        let resp = UciResponse::Id("Dutchess", "BillCipher");
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
    fn bestmove(&self) -> io::Result<()> {
        let resp = UciResponse::BestMove("nullmove".to_string(), None);
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
    fn info(&self) -> io::Result<()> {
        // let resp = UciResponse::Info("checking");
        // resp.send()?
        println!("info TestInfo 0");
        Ok(())
    }
    fn option(&self) -> io::Result<()> {
        for opt in self.get_uci_options() {
            let resp = UciResponse::Option(opt);
            resp.send()?;
        }
        Ok(())
    }

    fn get_uci_options(&self) -> impl Iterator<Item = UciOption> {
        [
            // All available options will be defined here.
        ]
        .into_iter()
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Engine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board = String::with_capacity(577);

        for rank in Rank::iter() {
            for _ in File::iter() {
                board += "+---";
            }

            board += "+\n";

            for file in File::iter() {
                let occupant = if let Some(piece) = self.get(file + rank) {
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

impl Index<Tile> for Engine {
    type Output = Option<Piece>;
    fn index(&self, index: Tile) -> &Self::Output {
        &self.state.pieces[index]
    }
}

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
                        placements[rank.0 as usize] += &empty_spaces.to_string();
                        empty_spaces = 0;
                    }
                    placements[rank.0 as usize] += &piece.to_string();
                } else {
                    empty_spaces += 1;
                }
            }

            if empty_spaces != 0 {
                placements[rank.0 as usize] += &empty_spaces.to_string();
            }
        }
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
        // &self.pieces[tile]
        &self.pieces[tile.index()]
    }

    fn piece_mut(&mut self, tile: Tile) -> &mut Option<Piece> {
        &mut self.pieces[tile]
    }
}

impl Default for GameState {
    fn default() -> Self {
        // Safe unwrap: Default FEN is always valid
        Self::from_fen(DEFAULT_FEN).unwrap()
        // Self::from_fen("8/8/8/2R5/2r5/8/8/8 w KQkq - 0 1").unwrap()
        // Self::from_fen("8/8/8/2B5/2B5/8/8/8 w KQkq - 0 1").unwrap()
        // Self::from_fen("2B5/2B5/2B5/2B5/2B5/2B5/2B5/2B5 w KQkq - 0 1").unwrap()
        // Self::from_fen("R7/1R6/2R5/8/8/8/6R1/8 w KQkq - 0 1").unwrap()
        // Self::from_fen("8/n7/8/2n5/8/5n2/8/8 w KQkq - 0 1").unwrap()
        // Self::from_fen("8/k7/8/2k5/8/5k2/8/8 w KQkq - 0 1").unwrap()
        // Self::from_fen("8/8/8/2b5/8/8/8/8 w KQkq - 0 1").unwrap()
    }
}

impl fmt::Display for GameState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_fen())
    }
}

/*********************************************************************************
 * Game logic
*********************************************************************************/

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Move {
    from: Tile,
    to: Tile,
}

impl Move {
    pub fn new(from: Tile, to: Tile) -> Self {
        // Self(from | to << 4)
        Self { from, to }
    }

    pub fn from(&self) -> Tile {
        self.from
    }

    pub fn to(&self) -> Tile {
        self.to
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.from(), self.to())
    }
}

/*********************************************************************************
 * Utility functions
*********************************************************************************/

pub struct MoveGenerator {}

impl MoveGenerator {
    //
}

// fn index(file: File, rank: Rank) -> usize {
//     8 * rank.index() + file.index()
// }
