use std::{
    fmt,
    ops::{Add, AddAssign, Deref, DerefMut, Index, IndexMut, Sub, SubAssign},
};

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

/*********************************************************************************
 * General Enums
*********************************************************************************/

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Color {
    White,
    Black,
    // Red,
    // Blue
}

impl Color {
    pub fn is_white(&self) -> bool {
        *self == Self::White
    }

    pub fn is_black(&self) -> bool {
        *self == Self::Black
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", if self.is_white() { 'w' } else { 'b' })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
#[repr(u8)]
pub enum PieceKind {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl PieceKind {
    fn from_bits(bits: u8) -> PieceKind {
        match bits {
            0 => Self::Pawn,
            1 => Self::Knight,
            2 => Self::Bishop,
            3 => Self::Rook,
            4 => Self::Queen,
            5 => Self::King,
            _ => panic!("Invalid bit pattern {bits:#b} ({bits})"),
        }
    }

    fn bits(&self) -> u8 {
        // SAFETY: This type is `repr(u8)`
        // See: https://doc.rust-lang.org/reference/items/enumerations.html#pointer-casting
        unsafe { *(self as *const Self as *const u8) }
    }

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

    fn iter() -> impl Iterator<Item = Self> {
        static PIECE_KINDS: [PieceKind; 6] = [
            PieceKind::Pawn,
            PieceKind::Knight,
            PieceKind::Bishop,
            PieceKind::Rook,
            PieceKind::Queen,
            PieceKind::King,
        ];

        PIECE_KINDS.into_iter()
    }
}

impl PartialOrd for PieceKind {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value().partial_cmp(&other.value())
    }
}

impl Ord for PieceKind {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value().cmp(&other.value())
    }
}

impl fmt::Display for PieceKind {
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

/*********************************************************************************
 * Game pieces
*********************************************************************************/
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Piece(u8);

impl Piece {
    fn new(color: Color, kind: PieceKind) -> Self {
        // 0000 1000 => white
        // 0001 0000 => black
        let color = if color.is_white() { 8 } else { 16 };
        Self(kind.bits() | color)
    }

    pub fn color(&self) -> Color {
        // Check for the color bit for black
        if self.0 & 16 == 0 {
            Color::White
        } else {
            Color::Black
        }
    }

    pub fn kind(&self) -> PieceKind {
        // Clear the color bits (8 + 16 = 24)
        PieceKind::from_bits(self.0 & !24)
        // PieceKind::from_bits(self.0 & !8 & !16)
    }
}

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let piece = if self.color().is_white() {
            self.kind().to_string()
        } else {
            self.kind().to_string().to_ascii_lowercase()
        };

        write!(f, "{piece}")
    }
}

/*
struct BitPiece(u8);
impl BitPiece {
    fn new(color: Color, kind: PieceKind) -> Self {
        let color = if color.is_white() { 8 } else { 16 };
        Self(kind as u8 | color)
    }

    fn color(&self) -> Color {
        if self.0 & 16 == 0 {
            Color::Black
        } else {
            Color::White
        }
    }

    fn kind(&self) -> PieceKind {

    }
}
 */

/*********************************************************************************
 * Positional stuff
*********************************************************************************/

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(transparent)]
pub struct Rank(u8);

impl Rank {
    fn iter() -> impl Iterator<Item = Self> {
        (Self::min().0..=Self::max().0).map(|i| Self(i))
    }

    pub fn new(rank: u8) -> Result<Self, String> {
        let rank = Self(rank);
        if rank > Self::max() || rank < Self::min() {
            return Err(format!(
                "Invalid rank value {rank}. Ranks must be between [{},{}]",
                Self::min(),
                Self::max()
            ));
        }
        Ok(rank)
    }

    fn from_char(rank: char) -> Result<Self, String> {
        debug_assert!(rank.is_ascii(), "Rank chars must be ASCII!");
        Self::new(rank.to_digit(10).ok_or("Ranks must be a valid number")? as u8)
    }

    pub fn from_index(index: usize) -> Result<Self, String> {
        debug_assert!(index <= 64, "Rank indices must be [0,64)");
        Self::new(index as u8 % 8)
    }

    fn min() -> Self {
        Self(0)
    }

    fn max() -> Self {
        Self(7)
    }
}

impl Deref for Rank {
    type Target = u8;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Add for Rank {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.0 + rhs.0).expect("Attempted to add beyond Rank's bounds")
    }
}

impl Sub for Rank {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.0 - rhs.0).expect("Attempted to subtract beyond Rank's bounds")
    }
}

impl AddAssign for Rank {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl SubAssign for Rank {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl Add<File> for Rank {
    type Output = Position;
    fn add(self, file: File) -> Self::Output {
        Position::new(file, self)
    }
}

impl fmt::Display for Rank {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", 8 - self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct File(u8);

impl File {
    fn iter() -> impl Iterator<Item = Self> {
        (Self::min().0..=Self::max().0).map(|i| Self(i))
    }

    pub fn new(file: u8) -> Result<Self, String> {
        let file = Self(file);
        if file > Self::max() || file < Self::min() {
            return Err(format!(
                "Invalid file value {file}. Files must be between [{},{}]",
                Self::min(),
                Self::max()
            ));
        }
        Ok(file)
    }

    pub fn from_char(file: char) -> Result<Self, String> {
        debug_assert!(file.is_ascii(), "File chars must be ASCII!");

        // Subtract the ASCII value for `a` (or `A`) to zero the number
        let file = file as u8 - if file.is_ascii_lowercase() { 'a' } else { 'A' } as u8;

        if file >= 8 {
            return Err(format!("Files must be between [a,h]"));
        }

        Self::new(file)
    }

    pub fn from_index(index: usize) -> Result<Self, String> {
        debug_assert!(index <= 64, "File indices must be [0,64)");
        Self::new(index as u8 / 8)
    }

    fn min() -> Self {
        Self(0)
    }

    fn max() -> Self {
        Self(7)
    }
}

impl Deref for File {
    type Target = u8;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Add for File {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.0 + rhs.0).expect("Attempted to add beyond File's bounds")
    }
}

impl Sub for File {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.0 - rhs.0).expect("Attempted to subtract beyond File's bounds")
    }
}

impl AddAssign for File {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl SubAssign for File {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl Add<Rank> for File {
    type Output = Position;
    fn add(self, rank: Rank) -> Self::Output {
        rank + self
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", (self.0 + 'a' as u8) as char)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Position(u8);
// pub struct Position {
//     file: File,
//     rank: Rank,
// }

impl Position {
    pub fn iter() -> impl Iterator<Item = Self> {
        File::iter()
            .map(|file| Rank::iter().map(move |rank| Self::new(file, rank)))
            .flatten()
    }

    pub fn new(file: File, rank: Rank) -> Self {
        Self(*file + *rank * 8)
        // Self { file, rank }
    }

    fn from_index(index: usize) -> Result<Self, String> {
        if index >= 64 {
            return Err(format!("Valid positions are indices [0,63)"));
        }
        Ok(Self(index as u8))
        // Ok(Self {
        //     file: File::from_index(index)?,
        //     rank: Rank::from_index(index)?,
        // })
    }

    pub fn file(&self) -> File {
        File(self.0 / 8)
        // self.file
    }

    pub fn rank(&self) -> Rank {
        Rank(self.0 % 8)
        // self.rank
    }

    fn index(self) -> usize {
        self.0 as usize
        // (*self.file() + *self.rank() * 8) as usize
    }

    pub fn color(&self) -> Color {
        if (*self.file() + *self.rank()) % 2 == 0 {
            Color::White
        } else {
            Color::Black
        }
    }

    fn from_uci(tile: &str) -> Result<Self, String> {
        let mut chars = tile.chars();
        let file = match chars.next() {
            Some(c) => File::from_char(c)?,
            None => return Err(format!("Invalid tile parsed \"{tile}\"; no `file` found!")),
        };

        let rank = match chars.next() {
            Some(c) => Rank::from_char(c)?,
            None => return Err(format!("Invalid tile parsed \"{tile}\"; no `rank` found!")),
        };

        Ok(Self::new(file, rank))
    }

    fn to_uci(self) -> String {
        format!("{}{}", self.file(), self.rank())
    }
}

impl TryFrom<usize> for Position {
    type Error = String;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Self::new(
            File::from_index(value)?,
            Rank::from_index(value)?,
        ))
    }
}

// impl Add<Rank> for Position {
//     type Output = Self;
//     fn add(mut self, rank: Rank) -> Self::Output {
//         self.rank += rank;
//         self
//     }
// }

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}

/*********************************************************************************
 * Game board
*********************************************************************************/

#[derive(PartialEq, Eq, Debug, Hash)]
pub struct ChessBoard {
    // white_pieces: Vec<Piece>,
    // black_pieces: Vec<Piece>,
    pub state: ChessBoardState,
}

impl ChessBoard {
    fn from_fen(fen: &FEN) -> Self {
        let state = ChessBoardState::from_fen(fen);
        // let white_pieces = Piece::from_fen(fen);
        Self {
            state,
            // white_pieces,
            // black_pieces,
        }
    }

    pub fn piece_at(&self, tile: Position) -> Option<Piece> {
        self.state.piece_at(tile)
    }

    fn place_at(&mut self, tile: Position, piece: Piece) {
        let bitboard = self.state.bitboard_mut(piece);
        bitboard.place_at(tile);
    }
}

impl Default for ChessBoard {
    fn default() -> Self {
        Self::from_fen(&FEN::default())
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
                let occupant = if let Some(piece) = self.piece_at(file + rank) {
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

// impl Index<Position> for ChessBoard {
//     type Output = Option<Piece>;
//     fn index(&self, index: Position) -> &Self::Output {

//     }
// }

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ChessBoardState {
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
            tile => Some(Position::from_uci(tile).unwrap()),
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

    pub fn to_fen(&self) -> FEN {
        let mut placements: [String; 8] = Default::default();

        for rank in Rank::iter() {
            let mut empty_spaces = 0;
            for file in File::iter() {
                if let Some(piece) = self.piece_at(file + rank) {
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

        FEN(fen)
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
                if let Some(kind) = PieceKind::from_char(piece_char) {
                    let bitboard = if piece_char.is_ascii_uppercase() {
                        &mut white
                    } else {
                        &mut black
                    };

                    bitboard[kind].place_at(File(file) + Rank(rank as u8));
                    file += 1;
                } else {
                    file += piece_char.to_digit(10).unwrap() as u8;
                }
            }
        }

        (white, black)
    }

    fn piece_at(&self, tile: Position) -> Option<Piece> {
        for kind in PieceKind::iter() {
            if self.white[kind].contains(tile) {
                return Some(Piece::new(Color::White, kind));
            } else if self.black[kind].contains(tile) {
                return Some(Piece::new(Color::Black, kind));
            }
        }

        /*
        for (i, white) in self.white.iter().enumerate() {
            if white.contains(tile) {
                let kind = PieceKind::Pawn;
                return Some(Piece {
                    kind,
                    color: Color::White,
                });
            }
        }

        for (i, black) in self.white.iter().enumerate() {
            if black.contains(tile) {
                let kind = PieceKind::Pawn;
                return Some(Piece {
                    kind,
                    color: Color::Black,
                });
            }
        }
         */

        None
    }

    fn bitboard(&self, piece: Piece) -> &BitBoard {
        if piece.color().is_white() {
            &self.white[piece.kind()]
        } else {
            &self.black[piece.kind()]
        }
    }

    fn bitboard_mut(&mut self, piece: Piece) -> &mut BitBoard {
        if piece.color().is_white() {
            &mut self.white[piece.kind()]
        } else {
            &mut self.black[piece.kind()]
        }
    }
}

impl Default for ChessBoardState {
    fn default() -> Self {
        Self::from_fen(&FEN::default())
    }
}

impl fmt::Display for ChessBoardState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self.to_fen())
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
    fn file_mask(file: File) -> Self {
        // Mask for the A (first) file
        let mask = 0x0101010101010101;
        Self(mask << *file)
    }

    fn rank_mask(rank: Rank) -> Self {
        // Mask for the 1 (first) rank
        let mask = 0x000000000000000FF;
        Self(mask << *rank)
    }

    // fn place_at_fr(&mut self, file: File, rank: Rank) {
    //     self.place_at(file + rank)
    // }

    fn place_at(&mut self, tile: Position) {
        self.place_at_index(tile.index())
    }

    fn contains(&self, tile: Position) -> bool {
        self.contains_index(tile.index())
    }

    fn remove(&mut self, tile: Position) {
        self.remove_index(tile.index());
    }

    fn place_at_index(&mut self, index: usize) {
        debug_assert!(index < 64, "Index must be between [0,64)");
        self.0 |= 1 << index;
    }

    fn remove_index(&mut self, index: usize) {
        debug_assert!(index < 64, "Index must be between [0,64)");
        self.0 ^= !(1 << index);
    }

    fn contains_index(&self, index: usize) -> bool {
        debug_assert!(index < 64, "Index must be between [0,64)");
        (self.0 & 1 << index) != 0
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

impl Index<PieceKind> for [BitBoard; 6] {
    type Output = BitBoard;

    fn index(&self, index: PieceKind) -> &Self::Output {
        let idx = match index {
            PieceKind::Pawn => 0,
            PieceKind::Knight => 1,
            PieceKind::Bishop => 2,
            PieceKind::Rook => 3,
            PieceKind::Queen => 4,
            PieceKind::King => 5,
        };

        &self[idx]
    }
}

impl IndexMut<PieceKind> for [BitBoard; 6] {
    fn index_mut(&mut self, index: PieceKind) -> &mut Self::Output {
        let idx = match index {
            PieceKind::Pawn => 0,
            PieceKind::Knight => 1,
            PieceKind::Bishop => 2,
            PieceKind::Rook => 3,
            PieceKind::Queen => 4,
            PieceKind::King => 5,
        };

        &mut self[idx]
    }
}

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
 * FEN Strings
*********************************************************************************/
// TODO: When creating this, store the individual components...
pub struct FEN(String);

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
}

impl Default for FEN {
    fn default() -> Self {
        // Safe unwrap: default FEN is always valid
        Self::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
        // Self::from_str("8/8/8/4p1K1/2k1P3/8/8/8 b - - 0 1").unwrap()
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

pub struct MoveGenerator {}

impl MoveGenerator {
    //
}
