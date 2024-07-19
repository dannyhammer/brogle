use std::{
    fmt,
    ops::{Index, IndexMut},
};

use super::{ChessError, Color};

/// Represents the kind (or "class") that a chess piece can be.
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
    pub const fn promotions() -> [Self; 4] {
        [
            PieceKind::Queen,
            PieceKind::Rook,
            PieceKind::Knight,
            PieceKind::Bishop,
        ]
    }

    /// Creates a new [`PieceKind`] from a set of bits.
    ///
    /// `bits` must be `[0,5]`.
    ///
    /// # Panics
    /// If `bits` is greater than `5`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::PieceKind;
    /// let queen = PieceKind::from_bits_unchecked(4);
    /// assert_eq!(queen, PieceKind::Queen);
    /// ```
    pub const fn from_bits_unchecked(bits: u8) -> PieceKind {
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

    /// Fetches the internal bit value of this [`PieceKind`].
    ///
    /// Will always be `[0,5]`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::PieceKind;
    /// let bits = PieceKind::Queen.bits();
    /// assert_eq!(bits, 4);
    /// ```
    pub const fn bits(&self) -> u8 {
        *self as u8
    }

    /// Returns this [`PieceKind`] as a `usize`.
    ///
    /// Useful for indexing into lists.
    ///
    /// Will always be `[0,5]`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::PieceKind;
    /// let index = PieceKind::Queen.index();
    /// assert_eq!(index, 4);
    /// ```
    pub const fn index(&self) -> usize {
        *self as usize
    }

    /// Fetches the ["value"](https://en.wikipedia.org/wiki/Chess_piece_relative_value#Standard_valuations) of this [`PieceKind`].
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

    /// Creates a new [`PieceKind`] from a character, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// Will return a [`ChessError`] if `kind` is not a valid character.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::PieceKind;
    /// let queen = PieceKind::from_uci('Q');
    /// assert_eq!(queen, Ok(PieceKind::Queen));
    /// ```
    pub const fn from_uci(kind: char) -> Result<Self, ChessError> {
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

    /// Alias for [`PieceKind::from_uci`].
    pub const fn from_char(kind: char) -> Result<Self, ChessError> {
        Self::from_uci(kind)
    }

    /// Does the same as [`PieceKind::from_uci`], but only if `kind` is one character in length.
    pub const fn from_str(kind: &str) -> Result<Self, ChessError> {
        if kind.is_empty() || kind.len() > 1 {
            return Err(ChessError::InvalidPieceNotation);
        }

        Self::from_char(kind.as_bytes()[0] as char)
    }

    /// Fetches a human-readable name for this [`PieceKind`].
    ///
    /// Will always be capitalized.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::PieceKind;
    /// let queen = PieceKind::Queen.name();
    /// assert_eq!(queen, "Queen");
    /// ```
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

    /// Converts this [`PieceKind`] to a character, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// Will always be a capital letter.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::PieceKind;
    /// let queen = PieceKind::Queen;
    /// assert_eq!(queen.to_uci(), 'Q');
    /// ```
    pub const fn to_uci(&self) -> char {
        match self {
            Self::Pawn => 'P',
            Self::Knight => 'N',
            Self::Bishop => 'B',
            Self::Rook => 'R',
            Self::Queen => 'Q',
            Self::King => 'K',
        }
    }

    /// Alias for [`PieceKind::to_uci`].
    pub const fn char(&self) -> char {
        self.to_uci()
    }

    pub const fn is_pawn(&self) -> bool {
        matches!(self, Self::Pawn)
    }

    pub const fn is_king(&self) -> bool {
        matches!(self, Self::King)
    }

    pub const fn is_slider(&self) -> bool {
        match self {
            Self::Queen | Self::Rook | Self::Bishop => true,
            _ => false,
        }
    }

    pub const fn is_orthogonal_slider(&self) -> bool {
        match self {
            Self::Queen | Self::Rook => true,
            _ => false,
        }
    }

    pub const fn is_diagonal_slider(&self) -> bool {
        match self {
            Self::Queen | Self::Bishop => true,
            _ => false,
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
        write!(f, "{}", self.to_uci())
    }
}

/// Represents a chess piece on the game board.
///
/// Internally, this is represented as a `u8` with the following bit pattern:
///
/// ```text
///     0000 0 000
///      |   |  |
///      |   |  +- Represents the PieceKind.
///      |   +- Represents the Color. `0` for White, `1` for Black.
///      +- Unused.
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Piece(u8);

impl Piece {
    pub const WHITE_PAWN: Self = Self::new(Color::White, PieceKind::Pawn);
    pub const WHITE_ROOK: Self = Self::new(Color::White, PieceKind::Rook);
    pub const WHITE_KING: Self = Self::new(Color::White, PieceKind::King);
    pub const WHITE_QUEEN: Self = Self::new(Color::White, PieceKind::Queen);
    pub const WHITE_KNIGHT: Self = Self::new(Color::White, PieceKind::Knight);
    pub const WHITE_BISHOP: Self = Self::new(Color::White, PieceKind::Bishop);

    pub const BLACK_PAWN: Self = Self::new(Color::Black, PieceKind::Pawn);
    pub const BLACK_ROOK: Self = Self::new(Color::Black, PieceKind::Rook);
    pub const BLACK_KING: Self = Self::new(Color::Black, PieceKind::King);
    pub const BLACK_QUEEN: Self = Self::new(Color::Black, PieceKind::Queen);
    pub const BLACK_KNIGHT: Self = Self::new(Color::Black, PieceKind::Knight);
    pub const BLACK_BISHOP: Self = Self::new(Color::Black, PieceKind::Bishop);

    /// Creates a new [`Piece`] from the given [`Color`] and [`PieceKind`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Piece, Color, PieceKind};
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// assert_eq!(white_knight.to_string(), "N");
    /// ```
    pub const fn new(color: Color, kind: PieceKind) -> Self {
        // 0000 0000 => white
        // 0000 1000 => black
        let color = if color.is_white() { 0 } else { 8 };
        Self(kind.bits() | color)
    }

    /// Fetches the [`Color`] of this [`Piece`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Piece, Color, PieceKind};
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// assert_eq!(white_knight.color(), Color::White);
    /// ```
    pub const fn color(&self) -> Color {
        // Check for the color bit for black
        if self.0 & 8 == 0 {
            Color::White
        } else {
            Color::Black
        }
    }

    /// Fetches the [`PieceKind`] of this [`Piece`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Piece, Color, PieceKind};
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// assert_eq!(white_knight.kind(), PieceKind::Knight);
    /// ```
    pub const fn kind(&self) -> PieceKind {
        // Clear the color bit
        PieceKind::from_bits_unchecked(self.0 & !8)
    }

    /// Returns `true` if this [`Piece`]'s [`Color`] is `White`.
    pub const fn is_white(&self) -> bool {
        self.color().is_white()
    }

    /// Returns `true` if this [`Piece`]'s [`Color`] is `Black`.
    pub const fn is_black(&self) -> bool {
        self.color().is_black()
    }

    pub const fn is_pawn(&self) -> bool {
        self.kind().is_pawn()
    }

    pub const fn is_king(&self) -> bool {
        self.kind().is_king()
    }

    pub const fn is_slider(&self) -> bool {
        self.kind().is_slider()
    }

    pub const fn is_orthogonal_slider(&self) -> bool {
        self.kind().is_orthogonal_slider()
    }

    pub const fn is_diagonal_slider(&self) -> bool {
        self.kind().is_diagonal_slider()
    }

    pub const fn index(&self) -> usize {
        let offset = if self.is_white() { 0 } else { 6 };
        (self.kind().bits() + offset) as usize
    }

    /// Creates a new [`Piece`] from a character, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// Will return a [`ChessError`] if `piece` is not a valid character.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Piece, Color, PieceKind};
    /// let white_knight = Piece::from_uci('N').unwrap();
    /// assert_eq!(white_knight.color(), Color::White);
    /// assert_eq!(white_knight.kind(), PieceKind::Knight);
    /// ```
    pub const fn from_uci(piece: char) -> Result<Self, ChessError> {
        if let Ok(kind) = PieceKind::from_uci(piece) {
            let color = Color::from_case(piece);
            Ok(Self::new(color, kind))
        } else {
            Err(ChessError::InvalidPieceChar { val: piece })
        }
    }

    /// Converts this [`Piece`] into a character, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Piece, Color, PieceKind};
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// assert_eq!(white_knight.to_uci(), 'N');
    /// ```
    pub const fn to_uci(&self) -> char {
        if self.is_white() {
            self.kind().char().to_ascii_uppercase()
        } else {
            self.kind().char().to_ascii_lowercase()
        }
    }

    /// Alias for [`Piece::to_uci`].
    pub const fn char(&self) -> char {
        self.to_uci()
    }

    /// Promotes (or, in a less likely scenario, demotes) this [`Piece`] to a new [`PieceKind`], based on the value of `promotion`, consuming `self` in the process.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Piece, PieceKind};
    /// let pawn = Piece::from_uci('P').unwrap();
    /// let queen = pawn.promoted(PieceKind::Queen);
    /// assert_eq!(queen.kind(), PieceKind::Queen);
    /// ```
    pub const fn promoted(self, promotion: PieceKind) -> Self {
        Self::new(self.color(), promotion)
    }

    pub fn demoted(self) -> Self {
        self.promoted(PieceKind::Pawn)
    }

    /// Changes the [`Color`] of this [`Piece`] to the opponent's color.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Piece, Color};
    /// let mut michael_jackson = Piece::from_uci('k').unwrap();
    /// michael_jackson.change_color();
    /// assert_eq!(michael_jackson.color(), Color::White);
    /// ```
    pub fn change_color(&mut self) {
        *self = Self::new(self.color().opponent(), self.kind());
    }
}

impl<T> Index<Piece> for [T; 6] {
    type Output = T;
    fn index(&self, index: Piece) -> &Self::Output {
        &self[index.index()]
    }
}

impl<T> IndexMut<Piece> for [T; 6] {
    fn index_mut(&mut self, index: Piece) -> &mut Self::Output {
        &mut self[index.index()]
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
        write!(f, "{}", self.to_uci())
    }
}
