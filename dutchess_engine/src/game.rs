use std::{fmt, ops::Index};

use crate::{BitBoard, BitBoards, Color, File, Piece, PieceKind, Position, Rank};

#[derive(PartialEq, Eq, Debug, Hash)]
pub enum MoveLegality {
    /// Move is completely legal
    Legal, // TODO: Contain the move?

    /// Can't capture the same color piece
    CaptureSameColor(Color),

    /// King is in check by the pieces at the provided positions
    PutsKingInCheck(Vec<Position>),

    /// That piece doesn't move that way.
    InvalidMovement(PieceKind),

    /// There isn't a piece at the selected tile
    NoPieceAtTile(Position),
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

#[derive(PartialEq, Eq, Debug, Hash)]
pub struct ChessBoard {
    state: ChessBoardState,
    board: BitBoards,
    history: Vec<Move>,
    legal_moves: Vec<Move>,
}

impl ChessBoard {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn fen(&self) -> String {
        self.state.to_fen()
    }

    pub fn piece_at(&self, tile: Position) -> Option<Piece> {
        *self.state.piece(tile)
        // self.board.get(tile)
    }

    /// Fetch the piece at the requested position, if it exists.
    fn get(&self, tile: Position) -> Option<Piece> {
        *self.state.piece(tile)
    }

    /// Set the piece at the specified position.
    fn set(&mut self, tile: Position, piece: Piece) {
        self.board.set(tile, piece); // Update the bitboards
        *self.state.piece_mut(tile) = Some(piece); // Update the state
    }

    /// Remove a piece from the specified position, returning it if it exists.
    fn clear(&mut self, tile: Position) -> Option<Piece> {
        self.board.clear(tile); // Update the bitboards
        self.state.piece_mut(tile).take() // Update the state
    }

    /// Returns `true` if the move was made successful, and `false` if it cannot be made.
    pub fn make_move(&mut self, from: Position, to: Position) -> bool {
        let Some(piece) = self.clear(from) else { return false };
        self.set(to, piece);
        true
    }

    /// Returns whether the move is legal to make, given the current game state.
    /*
    pub fn legality(&mut self, from: Position, to: Position) -> MoveLegality {
        // Can't make a move if `from` has no piece!
        let Some(piece) = self.get(from) else { return MoveLegality::NoPieceAtTile(from) };

        if let Some(target) = self.get(to) {
            // Cannot capture your own team's piece
            if piece.color() == target.color() {
                return MoveLegality::CaptureSameColor(piece.color());
            }
        }

        MoveLegality::Legal
    }
     */

    pub fn is_legal(&mut self, from: Position, to: Position) -> bool {
        // TODO: Check if exists in all legal moves.
        self.legal_moves().contains(&Move::new(from, to))
    }

    pub fn legal_moves(&self) -> &[Move] {
        &self.legal_moves
    }

    /*
    fn generate_all_legal_moves(&mut self) {
        // self.legal_moves.extend(self.generate_pawn_moves())

        for tile in Position::iter() {
            if let Some(piece) = self.get(tile) {
                // let legal_tiles = match piece.kind() {
                // PieceKind::Pawn => self.generate_pawn_positions(tile),
                // PieceKind::Knight => self.generate_knight_moves(),
                // PieceKind::Bishop => self.generate_bishop_moves(),
                // PieceKind::Rook => self.generate_rook_moves(),
                // PieceKind::Queen => self.generate_queen_moves(),
                // PieceKind::King => self.generate_king_moves(),
                //     _ => todo!(),
                // };
            }
        }

        /*
        for piece in self.pieces() {
        }
         */
    }
     */

    pub fn attacked_by(&self, piece: &Piece) -> Vec<Position> {
        vec![]
    }

    pub fn legal_moves_of(&self, piece: &Piece, tile: Position) -> BitBoard {
        self.board.moves_for(piece, tile)
    }
}

impl Default for ChessBoard {
    fn default() -> Self {
        let state = ChessBoardState::default();
        let board = BitBoards::from(state.pieces);
        let mut s = Self {
            state,
            board,
            history: Vec::with_capacity(64),
            legal_moves: Vec::with_capacity(1024),
        };
        // s.generate_all_legal_moves();
        s
    }
}

impl fmt::Display for ChessBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board = String::new();

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

        write!(f, "{}", board)
    }
}

impl Index<Position> for ChessBoard {
    type Output = Option<Piece>;
    fn index(&self, index: Position) -> &Self::Output {
        &self.state.pieces[index]
    }
}

/// Represents the current state of the game, including move counters
///
/// Analogous to a FEN string.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ChessBoardState {
    /// Can be indexed by [`Position`]
    pieces: [Option<Piece>; 64],

    current_player: Color,
    can_white_kingside_castle: bool,
    can_white_queenside_castle: bool,
    can_black_kingside_castle: bool,
    can_black_queenside_castle: bool,
    en_passant_tile: Option<Position>,

    /// Used to enforce the fifty-move rule.
    /// Is incremented after each move.
    /// Is reset after a capture or a pawn moves.
    halfmove: u8,

    /// Number of moves since the beginning of the game/
    /// A fullmove is a complete turn by white and then by black.
    fullmove: u8,
}

impl ChessBoardState {
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
                let Ok(pos) = Position::from_uci(tile) else { return Err("Invalid En Passant target when parsing FEN string") };
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

    fn piece(&self, tile: Position) -> &Option<Piece> {
        // &self.pieces[tile]
        &self.pieces[tile.index()]
    }

    fn piece_mut(&mut self, tile: Position) -> &mut Option<Piece> {
        &mut self.pieces[tile]
    }
}

impl Default for ChessBoardState {
    fn default() -> Self {
        // Safe unwrap: Default FEN is always valid
        Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
        // Self::from_fen("8/8/8/2R5/2r5/8/8/8 w KQkq - 0 1").unwrap()
        // Self::from_fen("8/8/8/2B5/2B5/8/8/8 w KQkq - 0 1").unwrap()
        // Self::from_fen("2B5/2B5/2B5/2B5/2B5/2B5/2B5/2B5 w KQkq - 0 1").unwrap()
        // Self::from_fen("R7/1R6/2R5/8/8/8/6R1/8 w KQkq - 0 1").unwrap()
        // Self::from_fen("8/n7/8/2n5/8/5n2/8/8 w KQkq - 0 1").unwrap()
        // Self::from_fen("8/k7/8/2k5/8/5k2/8/8 w KQkq - 0 1").unwrap()
        // Self::from_fen("8/8/8/2b5/8/8/8/8 w KQkq - 0 1").unwrap()
    }
}

impl fmt::Display for ChessBoardState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_fen())
    }
}

/*********************************************************************************
 * Game logic
*********************************************************************************/

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Move {
    from: Position,
    to: Position,
}

impl Move {
    pub fn new(from: Position, to: Position) -> Self {
        // Self(from | to << 4)
        Self { from, to }
    }

    pub fn from(&self) -> Position {
        self.from
    }

    pub fn to(&self) -> Position {
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
