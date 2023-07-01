use std::{
    fmt,
    ops::{BitAnd, BitAndAssign, BitOr, BitXor, Deref, DerefMut, Index, IndexMut, Not},
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

    // Internal; don't call this yourself
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
#[repr(transparent)]
struct Rank(u8);

impl Rank {
    fn from_char(rank: char) -> Result<Self, &'static str> {
        let rank = rank.to_digit(10).ok_or("Ranks must be a valid number")? as u8;
        if rank < 1 || rank > 8 {
            return Err("Ranks must be between [1,8]");
        }
        Ok(Self(rank))
    }
}

impl Deref for Rank {
    type Target = u8;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
struct File(u8);

impl File {
    fn from_char(file: char) -> Result<Self, &'static str> {
        let file = match file {
            'a' => 0,
            'b' => 1,
            'c' => 2,
            'd' => 3,
            'e' => 4,
            'f' => 5,
            'g' => 6,
            'h' => 7,
            _ => return Err("Files must be between [a,h]"),
        };
        Ok(File(file))
    }
}

impl Deref for File {
    type Target = u8;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
struct Position {
    file: File,
    rank: Rank,
}

impl Position {
    fn from_uci_notation(tile: &str) -> Result<Self, &'static str> {
        let mut chars = tile.chars();
        let file = match chars.next() {
            Some(c) => File::from_char(c)?,
            None => return Err("Invalid tile parsed; no `file` found!"),
        };

        let rank = match chars.next() {
            Some(c) => Rank::from_char(c)?,
            None => return Err("Invalid tile parsed; no `rank` found!"),
        };

        Ok(Self { file, rank })
    }
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
// #[derive(PartialEq, Eq, Debug, Hash)]
// struct ChessTile {
//     resident: Option<Piece>,
//     rank: Rank,
//     file: File,
// }

#[derive(PartialEq, Eq, Debug, Hash, Default)]
pub struct ChessBoard {
    // tiles: [[ChessTile; BOARD_SIZE]; BOARD_SIZE],
    pub state: ChessBoardState,
    // Index by [`PieceClass`] to get the bitboard of that piece's locations
    // white: [BitBoard; 6],
    // black: [BitBoard; 6],
    /*
    white_pawns: BitBoard,
    white_knights: BitBoard,
    white_bishops: BitBoard,
    white_rooks: BitBoard,
    white_queens: BitBoard,
    white_king: BitBoard,

    black_pawns: BitBoard,
    black_knights: BitBoard,
    black_bishops: BitBoard,
    black_rooks: BitBoard,
    black_queens: BitBoard,
    black_king: BitBoard,
     */
}

impl ChessBoard {
    fn from_fen(fen: &FEN) -> Self {
        let state = ChessBoardState::from_fen(fen);
        Self { state }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ChessBoardState {
    // pieces: [[Option<Piece>; BOARD_SIZE]; BOARD_SIZE],
    // Index by [`PieceClass`] to get the bitboard of that piece's locations
    pub white: [BitBoard; 6],
    pub black: [BitBoard; 6],

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
    fn from_fen(fen: &FEN) -> Self {
        // All of the `unwrap`s here are safe because a `FEN` instance is guaranteed to be valid.
        let mut split = fen.split(' ');

        let placements = split.next().unwrap();
        let (white, black) = Self::parse_piece_placement(placements);

        let current_player = match split.next().unwrap() {
            "w" | "W" => Color::White,
            "b" | "B" => Color::Black,
            _ => unreachable!(),
        };

        let castling = split.next().unwrap();
        let can_white_kingside_castle = castling.contains('K');
        let can_white_queenside_castle = castling.contains('Q');
        let can_black_kingside_castle = castling.contains('k');
        let can_black_queenside_castle = castling.contains('q');

        let en_passant_tile = match split.next().unwrap() {
            "-" => None,
            tile => Some(Position::from_uci_notation(tile).unwrap()),
        };

        let halfmove = split.next().unwrap().parse().unwrap();
        let fullmove = split.next().unwrap().parse().unwrap();

        Self {
            white,
            black,
            current_player,
            can_white_kingside_castle,
            can_white_queenside_castle,
            can_black_kingside_castle,
            can_black_queenside_castle,
            en_passant_tile,
            halfmove,
            fullmove,
        }
    }

    /// Internal helper for parsing piece placement from a *valid* FEN string.
    ///
    /// This should never be called directly with a parameter that did not come from a FEN instance.
    fn parse_piece_placement(placements: &str) -> ([BitBoard; 6], [BitBoard; 6]) {
        let mut white = [BitBoard::default(); 6];
        let mut black = [BitBoard::default(); 6];

        // TODO: Reverse this iter?
        for (rank, pieces) in placements.split('/').enumerate() {
            let mut file = 0;
            for piece_char in pieces.chars() {
                if let Some(kind) = PieceClass::from_char(piece_char) {
                    println!("Placing {piece_char}({kind:?}) on {file} {rank}");
                    if piece_char.is_ascii_uppercase() {
                        white[kind].place_piece(File(file), Rank(rank as u8))
                    } else {
                        black[kind].place_piece(File(file), Rank(rank as u8))
                    }
                    file += 1;
                } else {
                    file += piece_char.to_digit(10).unwrap() as u8;
                }
            }
        }

        // println!("{:#064b}", *white[PieceClass::Pawn]);
        println!("{}", white[PieceClass::Pawn]);
        (white, black)
    }
}

impl Default for ChessBoardState {
    fn default() -> Self {
        Self::from_fen(&FEN::default())
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
pub struct BitBoard(u64);

impl BitBoard {
    fn new(bits: u64) -> Self {
        Self(bits)
    }

    fn place_piece(&mut self, file: File, rank: Rank) {
        self.0 |= 1 << (*file + *rank * 8);
    }

    // fn place_at_index(&mut self, index: usize) {
    //     self.0 |= 1 << index;
    // }

    pub fn positions(&self) -> impl Iterator<Item = (usize, usize)> {
        self.to_be_bytes()
            .into_iter()
            .enumerate()
            .map(|(i, b)| (i, b as usize))
    }

    pub fn contains(&self, tile: &usize) -> bool {
        (self.0 & 1 << *tile as u64) != 0
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

impl Index<PieceClass> for [BitBoard; 6] {
    type Output = BitBoard;

    fn index(&self, index: PieceClass) -> &Self::Output {
        let idx = match index {
            PieceClass::Pawn => 0,
            PieceClass::Knight => 1,
            PieceClass::Bishop => 2,
            PieceClass::Rook => 3,
            PieceClass::Queen => 4,
            PieceClass::King => 5,
        };

        &self[idx]
    }
}

impl IndexMut<PieceClass> for [BitBoard; 6] {
    fn index_mut(&mut self, index: PieceClass) -> &mut Self::Output {
        let idx = match index {
            PieceClass::Pawn => 0,
            PieceClass::Knight => 1,
            PieceClass::Bishop => 2,
            PieceClass::Rook => 3,
            PieceClass::Queen => 4,
            PieceClass::King => 5,
        };

        &mut self[idx]
    }
}

impl fmt::Display for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // let bytes = self.to_ne_bytes();
        let bytes = self.to_be_bytes();
        // let bytes = self.to_le_bytes();
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
 * FEN Strings
*********************************************************************************/
// TODO: When creating this, store the individual components...
struct FEN(String);

impl FEN {
    fn new(fen: String) -> Result<Self, &'static str> {
        let mut split = fen.split(' ');
        let placements = split.next().ok_or("Empty string is not valid FEN")?;

        if placements.matches('/').count() != 7 {
            return Err(
                "Valid FEN placements must have 8 rows delimited with 7 forward slashes `/`",
            );
        }

        let active_color = split.next().unwrap_or_else(|| {
            let x = "w";
            eprintln!("Warning: No active color specified; defaulting to {x}");
            x
        });

        let castling = split.next().unwrap_or_else(|| {
            let x = "KQkq";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });

        let en_passant_target = split.next().unwrap_or_else(|| {
            let x = "-";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });

        let halfmove = split.next().unwrap_or_else(|| {
            let x = "0";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });

        let fullmove = split.next().unwrap_or_else(|| {
            let x = "1";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });

        Ok(Self(format!(
            "{placements} {active_color} {castling} {en_passant_target} {halfmove} {fullmove}"
        )))
    }

    fn from_str(fen: &str) -> Result<Self, &'static str> {
        Self::new(fen.to_string())
    }

    fn to_bitboard(&self, piece: &Piece) -> BitBoard {
        todo!("Convert FEN strings to BitBoard, based on provided piece")
    }
}

impl Default for FEN {
    fn default() -> Self {
        // Self::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
        // Safe unwrap: default FEN is always valid
        Self::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
    }
}

impl Deref for FEN {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/*********************************************************************************
 * Player stuff
*********************************************************************************/

// #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
// struct Player {
//     color: Color,
// }

/*********************************************************************************
 * Game logic
*********************************************************************************/

/*
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
 */

/*********************************************************************************
 * Utility functions
*********************************************************************************/
