use std::{
    fmt,
    ops::{BitAnd, BitAndAssign, BitOr, BitXor, Deref, DerefMut, Not},
};

/*********************************************************************************
 * Constants
*********************************************************************************/
const BOARD_SIZE: usize = 8;

/*********************************************************************************
 * General Enums
*********************************************************************************/

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum Color {
    White,
    Black,
}

impl Color {
    fn is_white(&self) -> bool {
        *self == Self::White
    }

    fn is_black(&self) -> bool {
        *self == Self::Black
    }

    fn from_char(color: char) -> Self {
        debug_assert!(
            color.is_ascii(),
            "Non-ASCII characters are not valid for chess pieces"
        );

        if color.is_ascii_uppercase() {
            Self::White
        } else {
            Self::Black
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum PieceClass {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl PieceClass {
    /// https://en.wikipedia.org/wiki/Chess_piece_relative_value#Standard_valuations
    fn value(&self) -> u32 {
        match self {
            Self::Pawn => 1,
            Self::Knight => 3,
            Self::Bishop => 3,
            Self::Rook => 5,
            Self::Queen => 9,
            // A King is invaluable
            Self::King => u32::MAX,
        }
    }

    fn from_char(class: char) -> Option<Self> {
        match class.to_ascii_uppercase() {
            'P' => Some(Self::Pawn),
            'N' => Some(Self::Knight),
            'B' => Some(Self::Bishop),
            'R' => Some(Self::Rook),
            'Q' => Some(Self::Queen),
            'K' => Some(Self::King),
            _ => None,
        }
    }
}

impl PartialOrd for PieceClass {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value().partial_cmp(&other.value())
    }
}
impl Ord for PieceClass {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value().cmp(&other.value())
    }
}

impl fmt::Display for PieceClass {
    /// By default, piece classes display as uppercase chars (white)
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let class = match self {
            Self::Pawn => 'P',
            Self::Knight => 'N',
            Self::Bishop => 'B',
            Self::Rook => 'R',
            Self::Queen => 'Q',
            Self::King => 'K',
        };
        write!(f, "{class}",)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum Rank {
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum File {
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
}

/*********************************************************************************
 * Game pieces
*********************************************************************************/
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
struct Piece {
    color: Color,
    class: PieceClass,
}

impl Piece {
    fn new(color: Color, class: PieceClass) -> Self {
        Self { color, class }
    }

    fn from_char(piece: char) -> Option<Piece> {
        let class = PieceClass::from_char(piece)?;
        let color = Color::from_char(piece);

        Some(Piece::new(color, class))
    }
}

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let piece = if self.color.is_white() {
            self.class.to_string()
        } else {
            self.class.to_string().to_ascii_lowercase()
        };

        write!(f, "{piece}")
    }
}

/*********************************************************************************
 * Game board
*********************************************************************************/
#[derive(PartialEq, Eq, Debug, Hash)]
struct ChessTile {
    resident: Option<Piece>,
    rank: Rank,
    file: File,
}

#[derive(PartialEq, Eq, Debug, Hash)]
struct ChessBoard {
    // TODO: Figure out how many bitboards we actually need...
    bitboards: Vec<BitBoard>,
    tiles: [[ChessTile; BOARD_SIZE]; BOARD_SIZE],
    state: ChessBoardState,
}

impl ChessBoard {
    fn from_fen(fen: &FEN) -> Self {
        todo!()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
struct ChessBoardState {
    pieces: [[Option<Piece>; BOARD_SIZE]; BOARD_SIZE],
    current_player: Color,
    can_white_kingside_castle: bool,
    can_white_queenside_castle: bool,
    can_black_kingside_castle: bool,
    can_black_queenside_castle: bool,
    en_passant_tile: Option<u8>,
    /// Used to enforce the fifty-move rule.
    /// Is incremented after each move.
    /// Is reset after a capture or a pawn moves.
    halfmove_clock: u64,
    /// Number of moves since the beginning of the game/
    /// A fullmove is a complete turn by white and then by black.
    fullmove_number: u64,
}

impl ChessBoardState {
    fn from_fen(fen: &FEN) -> Result<Self, String> {
        todo!()
    }
}

impl Default for ChessBoardState {
    fn default() -> Self {
        // Safe unwrap: Default FEN is always valid.
        Self::from_fen(&FEN::default()).unwrap()
    }
}

/*********************************************************************************
 * Bitboards
*********************************************************************************/

/// A [`BitBoard`] represents the game board as a set of bits.
/// They are used for various computations, such as fetching valid moves or computing move costs.
///
/// The internal representation is a 64-bit binary number, so the values will represent the entire board.
/// They are color-agnostic, with the low order bits representing the "lower" half of the board.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
struct BitBoard(u64);

impl BitBoard {
    fn new(bits: u64) -> Self {
        Self(bits)
    }
}

// TODO: Should we just impl deref here? Or do we need more fine-grained control?
impl Deref for BitBoard {
    type Target = u64;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for BitBoard {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
 * FEN Strings
*********************************************************************************/
struct FEN(String);

impl FEN {
    fn new(fen: String) -> Self {
        Self(fen)
    }

    fn from_str(fen: &str) -> Self {
        Self::new(fen.to_string())
    }

    fn to_bitboard(&self, piece: &Piece) -> BitBoard {
        todo!("Convert FEN strings to BitBoard, based on provided piece")
    }
}

impl Default for FEN {
    fn default() -> Self {
        // Self::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
        Self::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    }
}

/*********************************************************************************
 * Player stuff
*********************************************************************************/

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
struct Player {
    color: Color,
}

/*********************************************************************************
 * Game logic
*********************************************************************************/

enum GameResult {
    Draw(String),
    Checkmate(Color),
    Stalemate(Color),
}

#[derive(Debug)]
struct Game {
    board: ChessBoard,
    white: Player,
    black: Player,
}

impl Game {
    fn new() -> Self {
        todo!()
    }

    fn play(mut self) -> GameResult {
        GameResult::Draw(format!("not done yet"))
    }
}
