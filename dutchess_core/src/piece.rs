use std::{
    fmt,
    ops::{Index, IndexMut, Not},
};

use crate::ChessError;

/// Represents the color of a player, piece, tile, etc. within a chess board.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub const fn is_white(&self) -> bool {
        // *self == Self::White
        matches!(self, Self::White)
    }

    pub const fn is_black(&self) -> bool {
        matches!(self, Self::Black)
    }

    pub const fn opponent(&self) -> Self {
        match self {
            Self::White => Self::Black,
            Self::Black => Self::White,
        }
    }

    pub const fn index(&self) -> usize {
        *self as usize
    }

    pub const fn bits(&self) -> u8 {
        *self as u8
    }
}

impl Not for Color {
    type Output = Self;
    fn not(self) -> Self::Output {
        self.opponent()
    }
}

impl<T> Index<Color> for [T; 2] {
    type Output = T;
    fn index(&self, index: Color) -> &Self::Output {
        &self[index.index()]
    }
}

impl<T> IndexMut<Color> for [T; 2] {
    fn index_mut(&mut self, index: Color) -> &mut Self::Output {
        &mut self[index.index()]
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
    pub(crate) const fn from_bits(bits: u8) -> PieceKind {
        match bits {
            0 => Self::Pawn,
            1 => Self::Knight,
            2 => Self::Bishop,
            3 => Self::Rook,
            4 => Self::Queen,
            5 => Self::King,
            _ => panic!("Invalid bit pattern, must be [0,6)"),
        }
    }

    pub const fn bits(&self) -> u8 {
        *self as u8
    }

    /// https://en.wikipedia.org/wiki/Chess_piece_relative_value#Standard_valuations
    const fn value(&self) -> u32 {
        match self {
            Self::Pawn => 1,
            Self::Knight => 3,
            Self::Bishop => 3,
            Self::Rook => 5,
            Self::Queen => 9,
            // A King is invaluable, but we need some high number for computing move values.
            Self::King => 30, // TODO: Better value for King
        }
    }

    pub const fn from_char(kind: char) -> Result<Self, ChessError> {
        match kind {
            'P' | 'p' => Ok(Self::Pawn),
            'N' | 'n' => Ok(Self::Knight),
            'B' | 'b' => Ok(Self::Bishop),
            'R' | 'r' => Ok(Self::Rook),
            'Q' | 'q' => Ok(Self::Queen),
            'K' | 'k' => Ok(Self::King),
            _ => Err(ChessError::InvalidPieceChar { val: kind }),
        }
    }

    pub fn from_str(kind: &str) -> Result<Self, ChessError> {
        if kind.is_empty() || kind.len() > 1 {
            return Err(ChessError::InvalidPieceNotation);
        }

        Self::from_char(kind.chars().next().unwrap())
    }

    pub const fn name(&self) -> &'static str {
        match self {
            Self::Pawn => "Pawn",
            Self::Knight => "Knight",
            Self::Bishop => "Bishop",
            Self::Rook => "Rook",
            Self::Queen => "Queen",
            Self::King => "King",
        }
    }

    pub const fn char(&self) -> char {
        match self {
            Self::Pawn => 'P',
            Self::Knight => 'N',
            Self::Bishop => 'B',
            Self::Rook => 'R',
            Self::Queen => 'Q',
            Self::King => 'K',
        }
    }

    pub const fn index(&self) -> usize {
        *self as usize
    }
}

impl PartialOrd for PieceKind {
    /// Pieces are ordered on the [`PieceKind::value`] function.
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value().partial_cmp(&other.value())
    }
}

impl Ord for PieceKind {
    /// Pieces are ordered on the [`PieceKind::value`] function.
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value().cmp(&other.value())
    }
}

impl<T> Index<PieceKind> for [T; 6] {
    type Output = T;
    fn index(&self, index: PieceKind) -> &Self::Output {
        &self[index.index()]
    }
}

impl<T> IndexMut<PieceKind> for [T; 6] {
    fn index_mut(&mut self, index: PieceKind) -> &mut Self::Output {
        &mut self[index.index()]
    }
}

impl fmt::Display for PieceKind {
    /// By default, piece classes display as uppercase chars (white)
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char())
    }
}

/*********************************************************************************
 * Game pieces
*********************************************************************************/
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Piece(u8);

impl Piece {
    pub const fn new(color: Color, kind: PieceKind) -> Self {
        // 0000 0000 => white
        // 0000 1000 => black
        let color = if color.is_white() { 0 } else { 8 };
        Self(kind.bits() | color)
        // if color.is_white() {
        //     Self(kind.bits())
        // } else {
        //     Self(kind.bits() << 1)
        // }
    }

    pub const fn color(&self) -> Color {
        // Check for the color bit for black
        if self.0 & 8 == 0 {
            Color::White
        } else {
            Color::Black
        }
        // if self.0 % 2 == 1 {
        //     Color::White
        // } else {
        //     Color::Black
        // }
    }

    pub const fn kind(&self) -> PieceKind {
        // Clear the color bit
        PieceKind::from_bits(self.0 & !8)
        // let bits = if self.is_white() { self.0 } else { self.0 >> 1 };
        // PieceKind::from_bits(bits)
    }

    pub const fn is_white(&self) -> bool {
        self.color().is_white()
    }

    pub const fn is_black(&self) -> bool {
        self.color().is_black()
    }

    pub const fn index(&self) -> usize {
        let offset = if self.is_white() { 0 } else { 6 };
        (self.kind().bits() + offset) as usize
    }

    pub const fn char(&self) -> char {
        if self.is_white() {
            self.kind().char().to_ascii_uppercase()
        } else {
            self.kind().char().to_ascii_lowercase()
        }
    }

    pub fn promote(&mut self, promotion: PieceKind) {
        *self = Self::new(self.color(), promotion)
    }

    pub const fn promoted(self, promotion: PieceKind) -> Self {
        Self::new(self.color(), promotion)
    }
}

impl<T> Index<Piece> for [T; 12] {
    type Output = T;
    fn index(&self, index: Piece) -> &Self::Output {
        &self[index.index()]
    }
}

impl<T> IndexMut<Piece> for [T; 12] {
    fn index_mut(&mut self, index: Piece) -> &mut Self::Output {
        &mut self[index.index()]
    }
}

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char())
    }
}
