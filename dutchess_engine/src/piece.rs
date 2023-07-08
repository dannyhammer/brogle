use std::{fmt, ops::Mul};

/// Represents the color of a player, piece, tile, etc. within a chess board.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub fn is_white(&self) -> bool {
        *self == Self::White
    }

    pub fn is_black(&self) -> bool {
        *self == Self::Black
    }

    pub fn opponent(&self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}

impl Mul<PieceKind> for Color {
    type Output = Piece;
    /// Multiplying a [`PieceKind`] by a [`Color`] will yield a [`Piece`]
    fn mul(self, rhs: PieceKind) -> Self::Output {
        Piece::new(self, rhs)
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
    pub(crate) fn from_bits(bits: u8) -> PieceKind {
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

    pub fn bits(&self) -> u8 {
        // SAFETY: This type is `repr(u8)`
        // See: https://doc.rust-lang.org/reference/items/enumerations.html#pointer-casting
        // unsafe { *(self as *const Self as *const u8) }
        *self as u8
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

    pub fn from_char(kind: char) -> Result<Self, String> {
        match kind {
            'P' | 'p' => Ok(Self::Pawn),
            'N' | 'n' => Ok(Self::Knight),
            'B' | 'b' => Ok(Self::Bishop),
            'R' | 'r' => Ok(Self::Rook),
            'Q' | 'q' => Ok(Self::Queen),
            'K' | 'k' => Ok(Self::King),
            _ => Err(format!("Invalid piece char '{kind}'")),
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::Pawn => "Pawn",
            Self::Knight => "Knight",
            Self::Bishop => "Bishop",
            Self::Rook => "Rook",
            Self::Queen => "Queen",
            Self::King => "King",
        }
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

impl Mul<Color> for PieceKind {
    type Output = Piece;
    /// Multiplying a [`PieceKind`] by a [`Color`] will yield a [`Piece`]
    fn mul(self, rhs: Color) -> Self::Output {
        Piece::new(rhs, self)
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
    pub fn new(color: Color, kind: PieceKind) -> Self {
        // 0000 0000 => white
        // 0000 1000 => black
        let color = if color.is_white() { 0 } else { 8 };
        Self(kind.bits() | color)
    }

    pub fn color(&self) -> Color {
        // Check for the color bit for black
        if self.0 & 8 == 0 {
            Color::White
        } else {
            Color::Black
        }
    }

    pub fn kind(&self) -> PieceKind {
        // Clear the color bit
        PieceKind::from_bits(self.0 & !8)
    }

    pub fn is_white(&self) -> bool {
        self.color().is_white()
    }

    pub fn is_black(&self) -> bool {
        self.color().is_black()
    }

    pub fn index(&self) -> usize {
        let offset = if self.is_white() { 0 } else { 6 };
        (self.kind().bits() + offset) as usize
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
