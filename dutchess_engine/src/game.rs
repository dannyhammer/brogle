use std::{
    fmt,
    ops::{Deref, Index},
};

use crate::{Color, File, Piece, PieceKind, Position, Rank};

/*********************************************************************************
 * Game board
*********************************************************************************/

#[derive(PartialEq, Eq, Debug, Hash, Default)]
pub struct ChessBoard {
    state: ChessBoardState,
    board: BitBoards,
    history: Vec<Move>,
}

impl ChessBoard {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn fen(&self) -> String {
        self.state.to_fen()
    }

    /// Fetch the piece at the requested position, if it exists.
    pub fn get(&self, tile: Position) -> Option<Piece> {
        *self.state.piece(tile)
    }

    /// Set the piece at the specified position.
    pub fn set(&mut self, tile: Position, piece: Piece) {
        *self.state.piece_mut(tile) = Some(piece);
        self.board.set(tile, piece);
    }

    /// Remove a piece from the specified position, returning it if it exists.
    pub fn clear(&mut self, tile: Position) -> Option<Piece> {
        self.board.clear(tile);
        self.state.piece_mut(tile).take()
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
        for (rank, placements) in placements.split('/').enumerate() {
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
                        placements[*rank as usize] += &empty_spaces.to_string();
                        empty_spaces = 0;
                    }
                    placements[*rank as usize] += &piece.to_string();
                } else {
                    empty_spaces += 1;
                }
            }

            if empty_spaces != 0 {
                placements[*rank as usize] += &empty_spaces.to_string();
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
        &self.pieces[tile]
    }

    fn piece_mut(&mut self, tile: Position) -> &mut Option<Piece> {
        &mut self.pieces[tile]
    }
}

impl Default for ChessBoardState {
    fn default() -> Self {
        // Safe unwrap: Default FEN is always valid
        Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
    }
}

impl fmt::Display for ChessBoardState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_fen())
    }
}

/*********************************************************************************
 * Board representations
*********************************************************************************/

/*
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ChessArrayBoard([Option<Piece>; 64]);

impl ChessArrayBoard {
    fn to_bitboard(&self, f: impl FnOnce(&Piece) -> bool + Copy) -> BitBoard {
        let mut bits = 0;

        for (i, piece) in self.iter().enumerate() {
            if let Some(piece) = piece {
                if f(piece) {
                    bits |= 1 << i;
                }
            }
        }

        BitBoard(bits)
    }

    fn set(&mut self, index: usize, piece: Piece) {
        self[index] = Some(piece);
    }

    fn get(&mut self, index: usize) -> Option<Piece> {
        self[index]
    }
}

impl Deref for ChessArrayBoard {
    type Target = [Option<Piece>; 64];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ChessArrayBoard {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Default for ChessArrayBoard {
    fn default() -> Self {
        Self([None; 64])
    }
}
 */

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct BitBoards {
    /// All tiles occupied by a piece
    occupied: BitBoard,
    /// All unoccupied tiles
    empty: BitBoard,

    /// All tiles occupied by a specific color
    white: BitBoard,
    black: BitBoard,

    /// All tiles occupied by a specific piece kind
    pawn: BitBoard,
    knight: BitBoard,
    bishop: BitBoard,
    rook: BitBoard,
    queen: BitBoard,
    king: BitBoard,
    /*
    /// All tiles attacked by a specific color
    white_attack: BitBoard,
    black_attack: BitBoard,
     */
}

impl BitBoards {
    fn get(&self, tile: Position) -> Option<Piece> {
        let color = self.color_at(tile)?;
        let kind = self.kind_at(tile)?;
        Some(Piece::new(color, kind))
    }

    fn set(&mut self, tile: Position, piece: Piece) {
        let index = tile.index();
        match piece.color() {
            Color::White => self.white.set_index(index),
            Color::Black => self.black.set_index(index),
        }

        match piece.kind() {
            PieceKind::Pawn => self.pawn.set_index(index),
            PieceKind::Knight => self.knight.set_index(index),
            PieceKind::Bishop => self.bishop.set_index(index),
            PieceKind::Rook => self.rook.set_index(index),
            PieceKind::Queen => self.queen.set_index(index),
            PieceKind::King => self.king.set_index(index),
        }

        self.occupied.set_index(index);
    }

    fn clear(&mut self, tile: Position) {
        self.white.clear(tile);
        self.black.clear(tile);
        self.pawn.clear(tile);
        self.knight.clear(tile);
        self.bishop.clear(tile);
        self.rook.clear(tile);
        self.queen.clear(tile);
        self.king.clear(tile);
    }

    fn color_at(&self, tile: Position) -> Option<Color> {
        if !self.occupied.get(tile) {
            return None;
        }

        if self.white.get(tile) {
            Some(Color::White)
        } else {
            Some(Color::Black)
        }
    }

    fn kind_at(&self, tile: Position) -> Option<PieceKind> {
        if !self.occupied.get(tile) {
            return None;
        }

        if self.pawn.get(tile) {
            Some(PieceKind::Pawn)
        } else if self.knight.get(tile) {
            Some(PieceKind::Knight)
        } else if self.bishop.get(tile) {
            Some(PieceKind::Bishop)
        } else if self.rook.get(tile) {
            Some(PieceKind::Rook)
        } else if self.queen.get(tile) {
            Some(PieceKind::Queen)
        } else {
            Some(PieceKind::King)
        }
    }
}

impl From<[Option<Piece>; 64]> for BitBoards {
    fn from(value: [Option<Piece>; 64]) -> Self {
        let mut boards = Self::default();

        for (i, piece) in value.into_iter().enumerate() {
            if let Some(piece) = piece {
                boards.set(Position::from_index(i).unwrap(), piece)
            } else {
                boards.empty.set_index(i);
            }
        }

        boards
    }
}

/*
fn set_bit(bits: u8, n: u8) -> u8 {
    bits | (1 << n)
}

fn clear_bit(bits: u8, n: u8) -> u8 {
    bits & !(1 << n)
}

fn flip_bit(bits: u8, n: u8) -> u8 {
    bits ^ (1 << n)
}

fn check_bit(bits: u8, n: u8) -> u8 {
    bits & (1 << n)
}
 */

/// A [`BitBoard`] represents the game board as a set of bits.
/// They are used for various computations, such as fetching valid moves or computing move costs.
///
/// The internal representation is a 64-bit binary number, so the values will represent the entire board.
/// They are color-agnostic, with the low order bits representing the "lower" half of the board.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
#[repr(transparent)]
pub struct BitBoard(u64);

impl BitBoard {
    fn from_color<'a>(pieces: impl IntoIterator<Item = &'a Option<Piece>>, color: Color) -> Self {
        Self::from_eq(pieces, |piece| piece.color() == color)
    }

    fn from_kind<'a>(pieces: impl IntoIterator<Item = &'a Option<Piece>>, kind: PieceKind) -> Self {
        Self::from_eq(pieces, |piece| piece.kind() == kind)
    }

    fn from_eq<'a>(
        pieces: impl IntoIterator<Item = &'a Option<Piece>>,
        f: impl FnOnce(&Piece) -> bool + Copy,
    ) -> Self {
        let mut bits = 0;

        for (i, piece) in pieces.into_iter().enumerate() {
            if let Some(piece) = piece {
                if f(&piece) {
                    bits |= 1 << i;
                }
            }
        }

        Self(bits)
    }

    fn file_mask(file: File) -> Self {
        // Mask for the A (first) file
        static FILE_A_MASK: u64 = 0x0101010101010101;
        Self(FILE_A_MASK << *file)
    }

    fn rank_mask(rank: Rank) -> Self {
        // Mask for the 1 (first) rank
        static RANK_1_MASK: u64 = 0x000000000000000FF;
        Self(RANK_1_MASK << *rank)
    }

    fn set(&mut self, tile: Position) {
        // debug_assert!(tile.index() < 64, "Index must be between [0,64)");
        // self.0 |= 1 << tile.index();
        self.set_index(tile.index())
    }

    fn get(&self, tile: Position) -> bool {
        // debug_assert!(tile.index() < 64, "Index must be between [0,64)");
        // (self.0 & 1 << tile.index()) != 0
        self.get_index(tile.index())
    }

    fn clear(&mut self, tile: Position) {
        // debug_assert!(tile.index() < 64, "Index must be between [0,64)");
        // self.0 ^= !(1 << tile.index());
        self.clear_index(tile.index())
    }

    fn set_index(&mut self, index: usize) {
        debug_assert!(index < 64, "Index must be between [0,64)");
        self.0 |= 1 << index;
    }

    fn get_index(&self, index: usize) -> bool {
        debug_assert!(index < 64, "Index must be between [0,64)");
        (self.0 & 1 << index) != 0
    }

    fn clear_index(&mut self, index: usize) {
        debug_assert!(index < 64, "Index must be between [0,64)");
        self.0 ^= !(1 << index);
    }

    fn rank_up(self, n: u8) -> Self {
        Self(*self >> (8 * n))
    }

    fn rank_down(self, n: u8) -> Self {
        Self(*self << (8 * n))
    }

    fn file_up(self, n: u8) -> Self {
        Self(*self >> n)
    }

    fn file_down(self, n: u8) -> Self {
        Self(*self << n)
    }
}

// TODO: Should we just impl deref here? Or do we need more fine-grained control?
impl Deref for BitBoard {
    type Target = u64;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/*
impl<T> Index<Piece> for [T; 12] {
    type Output = T;

    fn index(&self, piece: Piece) -> &Self::Output {
        let idx = match piece.kind() {
            PieceKind::Pawn => 0,
            PieceKind::Knight => 1,
            PieceKind::Bishop => 2,
            PieceKind::Rook => 3,
            PieceKind::Queen => 4,
            PieceKind::King => 5,
        };
        let idx = if piece.is_white() { idx } else { idx + 6 };
        &self[idx]
    }
}
 */

impl fmt::Display for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bytes = self.to_ne_bytes();
        write!(
            f,
            "{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}",
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        )
    }
}

/*
// Operations
impl BitAnd for BitBoard {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self::new(self.0.bitand(rhs.0))
    }
}

impl BitAndAssign for BitBoard {
    fn bitand_assign(&mut self, rhs: Self) {
        //
    }
}

impl BitOr for BitBoard {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self::new(self.0.bitor(rhs.0))
    }
}

impl BitXor for BitBoard {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Self::new(self.0.bitxor(rhs.0))
    }
}

impl Not for BitBoard {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self::new(self.0.not())
    }
}
 */

/*
// Formatting
impl fmt::Binary for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:b}", self.0)
    }
}
impl fmt::UpperHex for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:X}", self.0)
    }
}
impl fmt::LowerHex for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x}", self.0)
    }
}
 */

/*********************************************************************************
 * Game logic
*********************************************************************************/

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Move {
    from: Position,
    to: Position,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.from, self.to)
    }
}

/*********************************************************************************
 * Utility functions
*********************************************************************************/

pub struct MoveGenerator {}

impl MoveGenerator {
    //
}
