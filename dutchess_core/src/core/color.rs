use std::{
    fmt,
    ops::{Index, IndexMut, Not},
};

use super::ChessError;

/// Represents the color of a player, piece, tile, etc. within a chess board.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum Color {
    White,
    Black,
}

impl Color {
    /// Returns `true` if this [`Color`] is White.
    pub const fn is_white(&self) -> bool {
        // *self == Self::White
        matches!(self, Self::White)
    }

    /// Returns `true` if this [`Color`] is Black.
    pub const fn is_black(&self) -> bool {
        matches!(self, Self::Black)
    }

    /// Returns this [`Color`]'s opposite / inverse / enemy.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::Color;
    /// assert_eq!(Color::White.opponent(), Color::Black);
    /// ```
    pub const fn opponent(&self) -> Self {
        match self {
            Self::White => Self::Black,
            Self::Black => Self::White,
        }
    }

    /// Returns this [`Color`] as a `usize`.
    ///
    /// Will be `0` for White, `1` for Black.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::Color;
    /// assert_eq!(Color::White.index(), 0);
    /// assert_eq!(Color::Black.index(), 1);
    /// ```
    pub const fn index(&self) -> usize {
        *self as usize
    }

    /// Returns this [`Color`] as a `u8`.
    ///
    /// Will be `0` for White, `1` for Black.
    ///
    /// Useful for bit twiddling.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::Color;
    /// assert_eq!(Color::White.bits(), 0);
    /// assert_eq!(Color::Black.bits(), 1);
    /// ```
    pub const fn bits(&self) -> u8 {
        *self as u8
    }

    /// Creates a [`Color`] from a `char`, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::Color;
    /// let white = Color::from_uci('w');
    /// assert_eq!(white, Ok(Color::White));
    ///
    /// let err = Color::from_uci('x');
    /// assert!(err.is_err());
    /// ```
    pub const fn from_uci(color: char) -> Result<Self, ChessError> {
        match color {
            'w' | 'W' => Ok(Self::White),
            'b' | 'B' => Ok(Self::Black),
            val => Err(ChessError::InvalidColorChar { val }),
        }
    }

    /// Creates a [`Color`] from a `str`, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::Color;
    /// let white = Color::from_str("w");
    /// assert_eq!(white, Ok(Color::White));
    ///
    /// let err = Color::from_str("x");
    /// assert!(err.is_err());
    /// ```
    pub fn from_str(color: &str) -> Result<Self, ChessError> {
        match color {
            "w" | "W" => Ok(Self::White),
            "b" | "B" => Ok(Self::Black),
            _ => Err(ChessError::InvalidColorStr),
        }
    }

    /// Creates a [`Color`] based on the ASCII case of the provided character, with uppercase being White and lowercase being Black.
    ///
    /// Note this is intended to follow the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation, but can be used in odd ways, such as trying to find the color of the char `'z'` (Black).
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::Color;
    /// assert_eq!(Color::from_case('k'), Color::Black);
    /// ```
    pub const fn from_case(c: char) -> Self {
        if c.is_ascii_uppercase() {
            Self::White
        } else {
            Self::Black
        }
    }

    /// Converts this [`Color`] to a char, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::Color;
    /// assert_eq!(Color::White.to_uci(), 'w');
    /// ```
    pub const fn to_uci(&self) -> char {
        match self {
            Self::White => 'w',
            Self::Black => 'b',
        }
    }
}

impl Default for Color {
    /// The "default" color is [`Color::White`], as White traditionally moves first in Chess.
    fn default() -> Self {
        Self::White
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
        write!(f, "{}", self.to_uci())
    }
}
