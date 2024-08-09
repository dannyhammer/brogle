use std::{
    fmt,
    ops::{Index, IndexMut, Neg},
};

use anyhow::{bail, Result};

use super::NUM_COLORS;

/// Represents the color of a player, piece, tile, etc. within a chess board.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum Color {
    White,
    Black,
}

impl Color {
    /// An array of both colors, starting with White.
    pub const fn all() -> [Self; NUM_COLORS] {
        [Self::White, Self::Black]
    }

    /// An iterator over both colors, starting with White.
    pub fn iter() -> impl Iterator<Item = Self> {
        Self::all().into_iter()
    }

    /// Creates a new [`Color`] from a set of bits.
    ///
    /// `bits` must be `[0,1]`.
    ///
    /// # Panics
    /// If `bits` is greater than `1`.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::Color;
    /// let white = Color::from_bits(0);
    /// assert!(white.is_ok());
    /// assert_eq!(white.unwrap(), Color::White);
    ///
    /// let err = Color::from_bits(42);
    /// assert!(err.is_err());
    /// ```
    pub fn from_bits(bits: u8) -> Result<Self> {
        match bits {
            0 => Ok(Self::White),
            1 => Ok(Self::Black),
            _ => bail!("Invalid bits for Color: Bits must be between [0,1]. Got {bits}."),
        }
    }

    /// Creates a new [`Color`] from a set of bits, ignoring safety checks.
    ///
    /// `bits` must be `[0,1]`.
    ///
    /// # Panics
    /// If `bits` is greater than `1` and debug assertions are enabled.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::Color;
    /// let white = Color::from_bits(0);
    /// assert!(white.is_ok());
    /// assert_eq!(white.unwrap(), Color::White);
    ///
    /// let err = Color::from_bits(42);
    /// assert!(err.is_err());
    /// ```
    pub const fn from_bits_unchecked(bits: u8) -> Self {
        debug_assert!(
            bits <= 1,
            "Invalid bits for Color: Bits must be between [0,1]"
        );

        // Safety: Since `Color` is a `repr(u8)` enum, we can cast safely here.
        unsafe { std::mem::transmute(bits) }
    }

    /// Returns `true` if this [`Color`] is White.
    pub const fn is_white(&self) -> bool {
        *self as u8 & 1 == 0
    }

    /// Returns `true` if this [`Color`] is Black.
    pub const fn is_black(&self) -> bool {
        *self as u8 & 1 != 0
    }

    /// Returns a multiplier for negating numbers relative to this color.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::Color;
    /// assert_eq!(Color::White.negation_multiplier(), 1);
    /// assert_eq!(Color::Black.negation_multiplier(), -1);
    /// ```
    pub const fn negation_multiplier(&self) -> i8 {
        // TODO: Which of these 3 is faster?

        // A: Match
        // match self {
        //     Self::White => 1,
        //     Self::Black => -1,
        // }

        // B: Multiply
        // 1 - 2 * *self as i8

        // C: Shift
        1 - ((*self as i8) << 1)
    }

    /// Returns this [`Color`]'s opposite / inverse / enemy.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::Color;
    /// assert_eq!(Color::White.opponent(), Color::Black);
    /// assert_eq!(Color::Black.opponent(), Color::White);
    /// ```
    pub const fn opponent(&self) -> Self {
        Self::from_bits_unchecked(self.bits() ^ 1)
    }

    /// Returns this [`Color`] as a `usize`.
    ///
    /// Will be `0` for White, `1` for Black.
    ///
    /// Useful for indexing into lists.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::Color;
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
    /// # use brogle_core::Color;
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
    /// # use brogle_core::Color;
    /// let white = Color::from_uci('w');
    /// assert!(white.is_ok());
    /// assert_eq!(white.unwrap(), Color::White);
    ///
    /// let err = Color::from_uci('x');
    /// assert!(err.is_err());
    /// ```
    pub fn from_uci(color: char) -> Result<Self> {
        match color {
            'w' | 'W' => Ok(Self::White),
            'b' | 'B' => Ok(Self::Black),
            _ => bail!("Color must be either 'w' or 'b' (case-insensitive). Found {color}"),
        }
    }

    /// Creates a [`Color`] from a `str`, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::Color;
    /// let white = Color::from_str("w");
    /// assert!(white.is_ok());
    /// assert_eq!(white.unwrap(), Color::White);
    ///
    /// let err = Color::from_str("x");
    /// assert!(err.is_err());
    /// ```
    pub fn from_str(color: &str) -> Result<Self> {
        match color {
            "w" | "W" => Ok(Self::White),
            "b" | "B" => Ok(Self::Black),
            _ => {
                bail!("Color must be either \"w\" or \"b\" (case-insensitive). Found {color}")
            }
        }
    }

    /// Creates a [`Color`] based on the ASCII case of the provided character, with uppercase being White and lowercase being Black.
    ///
    /// Note this is intended to follow the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation, but can be used in odd ways, such as trying to find the color of the char `'z'` (Black).
    ///
    /// # Example
    /// ```
    /// # use brogle_core::Color;
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
    /// # use brogle_core::Color;
    /// assert_eq!(Color::White.to_uci(), 'w');
    /// ```
    pub const fn to_uci(&self) -> char {
        match self {
            Self::White => 'w',
            Self::Black => 'b',
        }
    }

    /// Converts this [`Color`] to a `str`, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::Color;
    /// assert_eq!(Color::White.as_str(), "w");
    /// ```
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::White => "w",
            Self::Black => "b",
        }
    }

    /// Fetches a human-readable name for this [`Color`].
    ///
    /// # Example
    /// ```
    /// # use brogle_core::Color;
    /// let white = Color::White;
    /// assert_eq!(white.name(), "white");
    /// ```
    pub const fn name(&self) -> &'static str {
        match self {
            Self::White => "white",
            Self::Black => "black",
        }
    }
}

impl Default for Color {
    /// The "default" color is [`Color::White`], as White traditionally moves first in Chess.
    fn default() -> Self {
        Self::White
    }
}

impl Neg for Color {
    type Output = Self;
    /// Negating [`Color::White`] yields [`Color::Black`] and vice versa.
    fn neg(self) -> Self::Output {
        self.opponent()
    }
}

impl<T> Index<Color> for [T; NUM_COLORS] {
    type Output = T;
    /// [`Color`] can be used to index into a list of two elements.
    fn index(&self, index: Color) -> &Self::Output {
        &self[index.index()]
    }
}

impl<T> IndexMut<Color> for [T; NUM_COLORS] {
    /// [`Color`] can be used to index into a list of two elements.
    fn index_mut(&mut self, index: Color) -> &mut Self::Output {
        &mut self[index.index()]
    }
}

impl AsRef<str> for Color {
    /// Alias for [`Color::as_str`].
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for Color {
    /// [`Color`]s are displayed in UCI format.
    ///
    /// See [`Color::to_uci`] for more detail.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}

impl fmt::Debug for Color {
    /// [`Color`]s are displayed in UCI format.
    ///
    /// See [`Color::to_uci`] for more detail.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}
