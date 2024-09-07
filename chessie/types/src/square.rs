use std::{
    fmt,
    ops::{Add, AddAssign, Index, IndexMut, Mul, Sub, SubAssign},
    str::FromStr,
    u8,
};

use anyhow::{bail, Context, Result};

use super::{Bitboard, Color};

/// Represents a single square on an `8x8` chess board.
///
/// Internally encoded using the following bit pattern:
/// ```text
///     00 000 000
///      |  |   |
///      |  |   +- Represents the File.
///      |  +- Represents the Rank.
///      +- Unused.
/// ```
///
/// This bit pattern is also known as [Least Significant File Mapping](https://www.chessprogramming.org/Square_Mapping_Considerations#Deduction_on_Files_and_Ranks),
/// so `square = file + rank * 8`. The indices of each square on the board is given as follows:
/// ```text
/// 8| 56 57 58 59 59 61 62 62
/// 7| 48 49 50 51 52 53 54 55
/// 6| 40 41 42 43 44 45 46 47
/// 5| 32 33 34 35 36 37 38 39
/// 4| 24 25 26 27 28 29 30 31
/// 3| 16 17 18 19 20 21 22 23
/// 2| 08 09 10 11 12 13 14 15
/// 1| 00 01 02 03 04 05 06 07
///  +------------------------
///    a  b  c  d  e  f  g  h   
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
#[repr(transparent)]
pub struct Square(pub(crate) u8);

impl Square {
    pub const A1: Self = Self::new(File::A, Rank::ONE);
    pub const A2: Self = Self::new(File::A, Rank::TWO);
    pub const A3: Self = Self::new(File::A, Rank::THREE);
    pub const A4: Self = Self::new(File::A, Rank::FOUR);
    pub const A5: Self = Self::new(File::A, Rank::FIVE);
    pub const A6: Self = Self::new(File::A, Rank::SIX);
    pub const A7: Self = Self::new(File::A, Rank::SEVEN);
    pub const A8: Self = Self::new(File::A, Rank::EIGHT);

    pub const B1: Self = Self::new(File::B, Rank::ONE);
    pub const B2: Self = Self::new(File::B, Rank::TWO);
    pub const B3: Self = Self::new(File::B, Rank::THREE);
    pub const B4: Self = Self::new(File::B, Rank::FOUR);
    pub const B5: Self = Self::new(File::B, Rank::FIVE);
    pub const B6: Self = Self::new(File::B, Rank::SIX);
    pub const B7: Self = Self::new(File::B, Rank::SEVEN);
    pub const B8: Self = Self::new(File::B, Rank::EIGHT);

    pub const C1: Self = Self::new(File::C, Rank::ONE);
    pub const C2: Self = Self::new(File::C, Rank::TWO);
    pub const C3: Self = Self::new(File::C, Rank::THREE);
    pub const C4: Self = Self::new(File::C, Rank::FOUR);
    pub const C5: Self = Self::new(File::C, Rank::FIVE);
    pub const C6: Self = Self::new(File::C, Rank::SIX);
    pub const C7: Self = Self::new(File::C, Rank::SEVEN);
    pub const C8: Self = Self::new(File::C, Rank::EIGHT);

    pub const D1: Self = Self::new(File::D, Rank::ONE);
    pub const D2: Self = Self::new(File::D, Rank::TWO);
    pub const D3: Self = Self::new(File::D, Rank::THREE);
    pub const D4: Self = Self::new(File::D, Rank::FOUR);
    pub const D5: Self = Self::new(File::D, Rank::FIVE);
    pub const D6: Self = Self::new(File::D, Rank::SIX);
    pub const D7: Self = Self::new(File::D, Rank::SEVEN);
    pub const D8: Self = Self::new(File::D, Rank::EIGHT);

    pub const E1: Self = Self::new(File::E, Rank::ONE);
    pub const E2: Self = Self::new(File::E, Rank::TWO);
    pub const E3: Self = Self::new(File::E, Rank::THREE);
    pub const E4: Self = Self::new(File::E, Rank::FOUR);
    pub const E5: Self = Self::new(File::E, Rank::FIVE);
    pub const E6: Self = Self::new(File::E, Rank::SIX);
    pub const E7: Self = Self::new(File::E, Rank::SEVEN);
    pub const E8: Self = Self::new(File::E, Rank::EIGHT);

    pub const F1: Self = Self::new(File::F, Rank::ONE);
    pub const F2: Self = Self::new(File::F, Rank::TWO);
    pub const F3: Self = Self::new(File::F, Rank::THREE);
    pub const F4: Self = Self::new(File::F, Rank::FOUR);
    pub const F5: Self = Self::new(File::F, Rank::FIVE);
    pub const F6: Self = Self::new(File::F, Rank::SIX);
    pub const F7: Self = Self::new(File::F, Rank::SEVEN);
    pub const F8: Self = Self::new(File::F, Rank::EIGHT);

    pub const G1: Self = Self::new(File::G, Rank::ONE);
    pub const G2: Self = Self::new(File::G, Rank::TWO);
    pub const G3: Self = Self::new(File::G, Rank::THREE);
    pub const G4: Self = Self::new(File::G, Rank::FOUR);
    pub const G5: Self = Self::new(File::G, Rank::FIVE);
    pub const G6: Self = Self::new(File::G, Rank::SIX);
    pub const G7: Self = Self::new(File::G, Rank::SEVEN);
    pub const G8: Self = Self::new(File::G, Rank::EIGHT);

    pub const H1: Self = Self::new(File::H, Rank::ONE);
    pub const H2: Self = Self::new(File::H, Rank::TWO);
    pub const H3: Self = Self::new(File::H, Rank::THREE);
    pub const H4: Self = Self::new(File::H, Rank::FOUR);
    pub const H5: Self = Self::new(File::H, Rank::FIVE);
    pub const H6: Self = Self::new(File::H, Rank::SIX);
    pub const H7: Self = Self::new(File::H, Rank::SEVEN);
    pub const H8: Self = Self::new(File::H, Rank::EIGHT);

    pub const MIN: u8 = 0;
    pub const MAX: u8 = 63;
    pub const COUNT: usize = 64;

    pub const KING_START_SQUARES: [Self; 2] = [Self::E1, Self::E8];
    pub const KINGSIDE_CASTLE_SQUARES: [Self; 2] = [Self::G1, Self::G8];
    pub const QUEENSIDE_CASTLE_SQUARES: [Self; 2] = [Self::C1, Self::C8];

    const FILE_MASK: u8 = 0b0000_0111;
    const RANK_MASK: u8 = 0b0011_1000;

    /// Returns an iterator over all available squares.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// let mut iter = Square::iter();
    /// assert_eq!(iter.len(), 64);
    /// assert_eq!(iter.next().unwrap(), Square::A1);
    /// assert_eq!(iter.last().unwrap(), Square::H8);
    /// ```
    pub fn iter() -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator<Item = Self> {
        (Self::MIN..=Self::MAX).map(Self)
    }

    /// Creates a new [`Square`] from the provided [`File`] and [`Rank`].
    ///
    /// # Example
    /// ```
    /// # use types::{Square, File, Rank};
    /// let c4 = Square::new(File::C, Rank::FOUR);
    /// assert_eq!(c4, Square::C4);
    /// ```
    pub const fn new(file: File, rank: Rank) -> Self {
        // least-significant file mapping
        Self(file.0 ^ rank.0 << 3)
    }

    /// Creates a new [`Square`] from the provided index value.
    ///
    /// The provided `index` must be `[0, 63]` or else an error is returned.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// let c4 = Square::from_index(26);
    /// assert!(c4.is_ok());
    /// assert_eq!(c4.unwrap(), Square::C4);
    /// ```
    pub fn from_index(index: usize) -> Result<Self> {
        Self::from_bits(index as u8)
    }

    /// Creates a new [`Square`] from the provided index value, without error checking.
    ///
    /// # Panics
    ///
    /// If `index` is greater than `63`.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// let c4 = Square::from_index_unchecked(26);
    /// assert_eq!(c4, Square::C4);
    /// ```
    pub const fn from_index_unchecked(index: usize) -> Self {
        debug_assert!(index < 64, "Index must be between [0,64)");
        Self(index as u8)
    }

    /// Creates a new [`Square`] from the provided `u8` value.
    ///
    /// The provided `bits` must be `[0, 63]` or else an error is returned.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// let c4 = Square::from_bits(26);
    /// assert!(c4.is_ok());
    /// assert_eq!(c4.unwrap(), Square::C4);
    /// ```
    pub fn from_bits(bits: u8) -> Result<Self> {
        if bits > Self::MAX {
            bail!(
                "Invalid bits for Square: Must be between [{}, {}]. Got {bits}",
                Self::MIN,
                Self::MAX
            );
        }
        Ok(Self(bits))
    }

    /// Creates a new [`Square`] from the provided `u8` value, without error checking.
    ///
    /// The provided `bits` must be `[0, 63]` or else an error is returned.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// let c4 = Square::from_bits_unchecked(26);
    /// assert_eq!(c4, Square::C4);
    /// ```
    pub const fn from_bits_unchecked(bits: u8) -> Self {
        Self(bits)
    }

    /// Flips this [`Square`], as if the board was rotated 180 degrees.
    ///
    /// This is equivalent to calling [`Square::flipped_rank`] and [`Square::flipped_file`]
    /// subsequently, but faster.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!(Square::A1.flipped(), Square::H8);
    /// assert_eq!(Square::C4.flipped(), Square::F5);
    /// ```
    pub const fn flipped(self) -> Self {
        Self(Self::MAX - self.0)
    }

    /// Flips the [`File`] of this [`Square`].
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!(Square::A1.flipped_file(), Square::H1);
    /// assert_eq!(Square::C4.flipped_file(), Square::F4);
    /// ```
    pub const fn flipped_file(self) -> Self {
        Self(self.0 ^ Self::FILE_MASK)
    }

    /// Flips the [`Rank`] of this [`Square`].
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!(Square::A1.flipped_rank(), Square::A8);
    /// assert_eq!(Square::C4.flipped_rank(), Square::C5);
    /// ```
    pub const fn flipped_rank(self) -> Self {
        Self(self.0 ^ Self::RANK_MASK)
    }

    /// If `color` is Black, flips this [`Square`].
    /// If `color` is White, does nothing.
    ///
    /// See [`Square::flipped`] for more.
    ///
    /// # Example
    /// ```
    /// # use types::{Color, Square};
    /// assert_eq!(Square::C4.relative_to(Color::White), Square::C4);
    /// assert_eq!(Square::C4.relative_to(Color::Black), Square::F5);
    /// ```
    pub const fn relative_to(self, color: Color) -> Self {
        match color {
            Color::White => self,
            Color::Black => self.flipped(),
        }
    }

    /// If `color` is Black, flips the [`Rank`] of this [`Square`].
    /// If `color` is White, does nothing.
    ///
    /// See [`Square::flipped_rank`] for more.
    ///
    /// # Example
    /// ```
    /// # use types::{Color, Square};
    /// assert_eq!(Square::E1.rank_relative_to(Color::White), Square::E1);
    /// assert_eq!(Square::E1.rank_relative_to(Color::Black), Square::E8);
    /// ```
    pub const fn rank_relative_to(self, color: Color) -> Self {
        match color {
            Color::White => self,
            Color::Black => self.flipped_rank(),
        }
    }

    /// If `color` is Black, flips the [`File`] of this [`Square`].
    /// If `color` is White, does nothing.
    ///
    /// See [`Square::flipped_file`] for more.
    ///
    /// # Example
    /// ```
    /// # use types::{Color, Square};
    /// assert_eq!(Square::A1.file_relative_to(Color::White), Square::A1);
    /// assert_eq!(Square::A1.file_relative_to(Color::Black), Square::H1);
    /// ```
    pub const fn file_relative_to(self, color: Color) -> Self {
        match color {
            Color::White => self,
            Color::Black => self.flipped_file(),
        }
    }

    /// Iterating over [`Square`]s increases their internal counter by 1.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!(Square::A1.next(), Some(Square::B1));
    /// assert_eq!(Square::H3.next(), Some(Square::A4));
    /// assert_eq!(Square::H8.next(), None);
    /// ```
    pub fn next(self) -> Option<Self> {
        (self.0 < Self::MAX).then(|| Self::from_bits_unchecked(self.0 + 1))
    }

    /// Iterating backwards over [`Square`]s decreases their internal counter by 1.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!(Square::A1.prev(), None);
    /// assert_eq!(Square::A4.prev(), Some(Square::H3));
    /// assert_eq!(Square::H8.prev(), Some(Square::G8));
    /// ```
    pub fn prev(self) -> Option<Self> {
        (self.0 > Self::MIN).then(|| Self::from_bits_unchecked(self.0 - 1))
    }

    /// Fetches the inner index value of the [`Square`], which represented as a [`u8`].
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!(Square::C4.inner(), 26);
    /// ```
    pub const fn inner(&self) -> u8 {
        self.0
    }

    /// Fetches the [`File`] of this [`Square`].
    ///
    /// # Example
    /// ```
    /// # use types::{Square, File};
    /// assert_eq!(Square::C4.file(), File::C);
    /// ```
    pub const fn file(&self) -> File {
        File(self.0 & Self::FILE_MASK) // Same as % 8
    }

    /// Fetches the [`Rank`] of this [`Square`].
    ///
    /// # Example
    /// ```
    /// # use types::{Square, Rank};
    /// assert_eq!(Square::C4.rank(), Rank::FOUR);
    /// ```
    pub const fn rank(&self) -> Rank {
        Rank(self.0 >> 3) // Same as / 8
    }

    /// Fetches the [`File`] and [`Rank`] of this [`Square`].
    ///
    /// # Example
    /// ```
    /// # use types::{Square, File, Rank};
    /// assert_eq!(Square::C4.parts(), (File::C, Rank::FOUR));
    /// ```
    pub const fn parts(&self) -> (File, Rank) {
        (self.file(), self.rank())
    }

    /// Fetches the inner index value of the [`Square`], casted to a [`usize`].
    ///
    /// Useful when using a [`Square`] to index into things.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!(Square::C4.index(), 26);
    /// ```
    pub const fn index(&self) -> usize {
        self.inner() as usize
    }

    /// Returns the [`Color`] of this [`Square`].
    ///
    /// This will return [`Color::White`] if the square is a "light" square
    /// and [`Color::Black`] if the square is a "dark" square.
    ///
    /// # Example
    /// ```
    /// # use types::{Color, Square};
    /// assert_eq!(Square::C4.color(), Color::White);
    /// assert_eq!(Square::C5.color(), Color::Black);
    /// ```
    pub const fn color(&self) -> Color {
        Color::from_bool(self.is_dark())
    }

    /// Returns `true` if this [`Square`] is a light square.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert!(Square::C4.is_light());
    /// ```
    pub const fn is_light(&self) -> bool {
        (Bitboard::DARK_SQUARES.0 & self.0 as u64) != 0
    }

    /// Returns `true` if this [`Square`] is a dark square.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert!(Square::C5.is_dark());
    /// ```
    pub const fn is_dark(&self) -> bool {
        !self.is_light()
    }

    /// Returns `true` if `self` and `other` lie on the same diagonal.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!(Square::C5.is_diagonal_to(Square::A3), true);
    /// assert_eq!(Square::H1.is_diagonal_to(Square::A8), true);
    /// assert_eq!(Square::F7.is_diagonal_to(Square::F7), true);
    /// assert_eq!(Square::A1.is_diagonal_to(Square::B3), false);
    /// assert_eq!(Square::A4.is_diagonal_to(Square::H3), false);
    /// assert_eq!(Square::A4.is_diagonal_to(Square::H4), false);
    /// ```
    pub const fn is_diagonal_to(&self, other: Self) -> bool {
        if self.0 == other.0 {
            return true;
        }

        (self.0 % 9 == other.0 % 9 || self.0 % 7 == other.0 % 7) // On same diag
            && self.rank().0 != other.rank().0 // Not on same rank
            && self.file().0 != other.file().0 // Not on same file
    }

    /// Creates a [`Square`] from a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// let c4 = Square::from_uci("c4");
    /// assert!(c4.is_ok());
    /// assert_eq!(c4.unwrap(), Square::C4);
    ///
    /// let err = Square::from_uci("z0");
    /// assert!(err.is_err());
    /// ```
    pub fn from_uci(square: &str) -> Result<Self> {
        let bytes = square.as_bytes();
        if square.len() != 2 {
            bail!("Invalid Square string: String must contain exactly 2 characters. Got {square}")
        }
        let file = File::from_char(bytes[0] as char)?;
        let rank = Rank::from_char(bytes[1] as char)?;

        Ok(Self::new(file, rank))
    }

    /// Converts this [`Square`] to a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!("c4", Square::C4.to_uci());
    /// ```
    pub fn to_uci(self) -> String {
        format!("{}{}", self.file(), self.rank())
    }

    /// Alias for [`Bitboard::from_square`].
    pub const fn bitboard(&self) -> Bitboard {
        Bitboard::from_square(*self)
    }

    /// Computes the distance between `self` and `other`.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!(Square::C4.distance_to(Square::C1), 3);
    /// assert_eq!(Square::A1.distance_to(Square::A8), 7);
    /// assert_eq!(Square::D6.distance_to(Square::D6), 0);
    /// assert_eq!(Square::E2.distance_to(Square::C6), 6);
    /// ```
    pub const fn distance_to(&self, other: Self) -> u8 {
        self.file().0.abs_diff(other.file().0) + self.rank().0.abs_diff(other.rank().0)
    }

    /// Computes the distance between `self` and the center of the board.
    ///
    /// The center squares are E4, E5, D4, and D5.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!(Square::D5.distance_from_center(), 0);
    /// assert_eq!(Square::E4.distance_from_center(), 0);
    /// assert_eq!(Square::A1.distance_from_center(), 6);
    /// ```
    pub fn distance_from_center(&self) -> u8 {
        self.distance_to(Self::E4)
            .min(self.distance_to(Self::E5))
            .min(self.distance_to(Self::D4).min(self.distance_to(Self::D5)))
    }

    /// Attempt to offset this [`Square`] by the file and rank offsets.
    ///
    /// If `self + offset` would exceed the bounds of this [`File`], then `None` is returned.
    ///
    /// # Example
    /// ```
    /// # use types::Square;
    /// assert_eq!(Square::C4.offset(1, 1), Some(Square::D5));
    /// assert_eq!(Square::C4.offset(-1, -1), Some(Square::B3));
    /// assert_eq!(Square::A1.offset(-1, -1), None);
    /// ```
    pub fn offset(&self, file_delta: i8, rank_delta: i8) -> Option<Self> {
        let file = self.file().offset(file_delta)?;
        let rank = self.rank().offset(rank_delta)?;

        Some(Self::new(file, rank))
    }

    /// Increments (if `color` is [`Color::White`]) or decrements (if `color` is [`Color::Black`]) the [`Rank`] of this [`Square`] by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use types::{Square, Color};
    /// assert_eq!(Square::C4.forward_by(Color::White, 1), Some(Square::C5));
    /// assert_eq!(Square::C4.forward_by(Color::Black, 1), Some(Square::C3));
    /// ```
    pub fn forward_by(&self, color: Color, n: u8) -> Option<Self> {
        self.offset(0, n as i8 * color.negation_multiplier())
    }

    /// Decrements (if `color` is [`Color::White`]) or increments (if `color` is [`Color::Black`]) the [`Rank`] of this [`Square`] by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use types::{Square, Color};
    /// assert_eq!(Square::C4.backward_by(Color::White, 1), Some(Square::C3));
    /// assert_eq!(Square::C4.backward_by(Color::Black, 1), Some(Square::C5));
    /// ```
    pub fn backward_by(&self, color: Color, n: u8) -> Option<Self> {
        self.offset(0, n as i8 * color.opponent().negation_multiplier())
    }

    /// Increments (if `color` is [`Color::White`]) or decrements (if `color` is [`Color::Black`]) the [`File`] of this [`Square`] by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use types::{Square, Color};
    /// assert_eq!(Square::C4.right_by(Color::White, 1), Some(Square::D4));
    /// assert_eq!(Square::C4.right_by(Color::Black, 1), Some(Square::B4));
    /// ```
    pub fn right_by(&self, color: Color, n: u8) -> Option<Self> {
        self.offset(n as i8 * color.negation_multiplier(), 0)
    }

    /// Decrements (if `color` is [`Color::White`]) or increments (if `color` is [`Color::Black`]) the [`File`] of this [`Square`] by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use types::{Square, Color};
    /// assert_eq!(Square::C4.left_by(Color::White, 1), Some(Square::B4));
    /// assert_eq!(Square::C4.left_by(Color::Black, 1), Some(Square::D4));
    /// ```
    pub fn left_by(&self, color: Color, n: u8) -> Option<Self> {
        self.offset(n as i8 * color.opponent().negation_multiplier(), 0)
    }
}

impl<T: AsRef<str>> PartialEq<T> for Square {
    fn eq(&self, other: &T) -> bool {
        self.to_string().eq(other.as_ref())
    }
}

impl FromStr for Square {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_uci(s)
    }
}

impl TryFrom<&str> for Square {
    type Error = anyhow::Error;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_uci(value)
    }
}

impl TryFrom<String> for Square {
    type Error = anyhow::Error;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::from_uci(&value)
    }
}

impl TryFrom<usize> for Square {
    type Error = anyhow::Error;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Self::from_index(value)
    }
}

impl TryFrom<u8> for Square {
    type Error = anyhow::Error;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::from_bits(value)
    }
}

impl TryFrom<i32> for Square {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        Self::from_bits(value as u8)
    }
}

impl Add<Rank> for Square {
    type Output = Self;
    fn add(self, rhs: Rank) -> Self::Output {
        Self::new(self.file(), self.rank() + rhs)
    }
}

impl Add<File> for Square {
    type Output = Self;
    fn add(self, rhs: File) -> Self::Output {
        Self::new(self.file() + rhs, self.rank())
    }
}

impl Sub<Rank> for Square {
    type Output = Self;
    fn sub(self, rhs: Rank) -> Self::Output {
        Self::new(self.file(), self.rank() - rhs)
    }
}

impl Sub<File> for Square {
    type Output = Self;
    fn sub(self, rhs: File) -> Self::Output {
        Self::new(self.file() - rhs, self.rank())
    }
}

impl AddAssign<Rank> for Square {
    fn add_assign(&mut self, rhs: Rank) {
        *self = *self + rhs;
    }
}

impl SubAssign<Rank> for Square {
    fn sub_assign(&mut self, rhs: Rank) {
        *self = *self - rhs;
    }
}

impl AddAssign<File> for Square {
    fn add_assign(&mut self, rhs: File) {
        *self = *self + rhs;
    }
}

impl SubAssign<File> for Square {
    fn sub_assign(&mut self, rhs: File) {
        *self = *self - rhs;
    }
}

impl<T> Index<Square> for [T; Square::COUNT] {
    type Output = T;
    fn index(&self, index: Square) -> &Self::Output {
        &self[index.index()]
    }
}

impl<T> IndexMut<Square> for [T; Square::COUNT] {
    fn index_mut(&mut self, index: Square) -> &mut Self::Output {
        &mut self[index.index()]
    }
}

impl<T> Index<Square> for [[T; File::COUNT]; Rank::COUNT] {
    type Output = T;
    fn index(&self, index: Square) -> &Self::Output {
        &self[index.file()][index.rank()]
    }
}

impl<T> IndexMut<Square> for [[T; File::COUNT]; Rank::COUNT] {
    fn index_mut(&mut self, index: Square) -> &mut Self::Output {
        &mut self[index.file()][index.rank()]
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}

impl fmt::Debug for Square {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.to_uci(), self.0)
    }
}

#[derive(Clone, Copy, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct Rank(pub(crate) u8);

impl Rank {
    pub const ONE: Self = Self(0);
    pub const TWO: Self = Self(1);
    pub const THREE: Self = Self(2);
    pub const FOUR: Self = Self(3);
    pub const FIVE: Self = Self(4);
    pub const SIX: Self = Self(5);
    pub const SEVEN: Self = Self(6);
    pub const EIGHT: Self = Self(7);

    pub const MIN: u8 = 0;
    pub const MAX: u8 = 7;
    pub const COUNT: usize = 8;

    /// An array of all [`Rank`]s, in ascending order.
    pub fn all() -> [Self; Self::COUNT] {
        [
            Self::ONE,
            Self::TWO,
            Self::THREE,
            Self::FOUR,
            Self::FIVE,
            Self::SIX,
            Self::SEVEN,
            Self::EIGHT,
        ]
    }

    /// Returns an iterator over all available ranks
    ///
    /// # Example
    /// ```
    /// # use types::Rank;
    /// let mut iter = Rank::iter();
    /// assert_eq!(iter.len(), 8);
    /// assert_eq!(iter.next().unwrap(), Rank::ONE);
    /// assert_eq!(iter.last().unwrap(), Rank::EIGHT);
    /// ```
    pub fn iter() -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator<Item = Self> {
        Self::all().into_iter()
    }

    /// Construct a new [`Rank`] from the provided value.
    pub fn new(rank: u8) -> Result<Self> {
        if rank > Self::MAX {
            bail!(
                "Invalid int for Rank: Must be between [{}, {}]. Got {rank}",
                Self::MIN,
                Self::MAX
            );
        }

        Ok(Self::new_unchecked(rank))
    }

    /// Construct a new [`Rank`] from the provided value, ignoring safety checks.
    ///
    /// Do not use this unless you have previously guaranteed that the input value is within bounds.
    pub const fn new_unchecked(rank: u8) -> Self {
        Self(rank)
    }

    /// First rank relative to `color`.
    pub const fn first(color: Color) -> Self {
        [Self::ONE, Self::EIGHT][color.index()]
    }

    pub const fn second(color: Color) -> Self {
        [Self::TWO, Self::SEVEN][color.index()]
    }

    pub const fn third(color: Color) -> Self {
        [Self::THREE, Self::SIX][color.index()]
    }

    pub const fn fourth(color: Color) -> Self {
        [Self::FOUR, Self::FIVE][color.index()]
    }

    pub const fn fifth(color: Color) -> Self {
        [Self::FIVE, Self::FOUR][color.index()]
    }

    pub const fn sixth(color: Color) -> Self {
        [Self::SIX, Self::THREE][color.index()]
    }

    /// Rank just before promoting a pawn
    pub const fn seventh(color: Color) -> Self {
        [Self::SEVEN, Self::TWO][color.index()]
    }

    pub const fn eighth(color: Color) -> Self {
        [Self::EIGHT, Self::ONE][color.index()]
    }

    pub fn from_char(rank: char) -> Result<Self> {
        debug_assert!(rank.is_ascii(), "Rank chars must be ASCII!");

        let rank_int = rank.to_digit(10).context(format!(
            "Invalid char for Rank: Must be between [1, 8]. Got {rank}"
        ))?;

        let rank = rank_int.checked_sub(1).context(format!(
            "Invalid char for Rank: Must be between [1, 8]. Got {rank}"
        ))?;

        Self::new(rank as u8)
    }

    pub const fn inner(&self) -> u8 {
        self.0
    }

    /// Obtain the inner value as a `usize`.
    ///
    /// Useful for indexing.
    pub const fn index(&self) -> usize {
        self.inner() as usize
    }

    pub const fn char(&self) -> char {
        (self.0 + b'1') as char
    }

    pub const fn as_str(&self) -> &'static str {
        match self.0 {
            0 => "1",
            1 => "2",
            2 => "3",
            3 => "4",
            4 => "5",
            5 => "6",
            6 => "7",
            7 => "8",
            _ => unreachable!(),
        }
    }

    /// `const` analog of `==`.
    pub const fn is(&self, other: &Self) -> bool {
        self.0 == other.0
    }

    /// Alias for [`Bitboard::from_rank`].
    pub const fn bitboard(&self) -> Bitboard {
        Bitboard::from_rank(*self)
    }

    /// Attempt to offset this [`Rank`] by the provided `delta`.
    ///
    /// If `self + delta` would exceed the bounds of this [`Rank`], then `None` is returned.
    ///
    /// # Example
    /// ```
    /// # use types::Rank;
    /// assert_eq!(Rank::FOUR.offset(1), Some(Rank::FIVE));
    /// assert_eq!(Rank::FOUR.offset(-1), Some(Rank::THREE));
    /// assert_eq!(Rank::ONE.offset(-1), None);
    /// ```
    pub fn offset(self, delta: i8) -> Option<Self> {
        let bits = self.0.checked_add_signed(delta)?;
        (bits <= Self::MAX).then_some(Self::new_unchecked(bits))
    }

    pub const fn flipped(self) -> Self {
        Self(Self::MAX - self.0)
    }

    pub const fn relative_to(self, color: Color) -> Self {
        match color {
            Color::White => self,
            Color::Black => self.flipped(),
        }
    }
}

macro_rules! impl_try_from_num {
    ($t: ty) => {
        impl_try_from_num!($t, u8);
        impl_try_from_num!($t, u16);
        impl_try_from_num!($t, u32);
        impl_try_from_num!($t, u64);
        impl_try_from_num!($t, usize);
        impl_try_from_num!($t, i8);
        impl_try_from_num!($t, i16);
        impl_try_from_num!($t, i32);
        impl_try_from_num!($t, i64);
        impl_try_from_num!($t, isize);
    };
    ($t: ty, $from:ty) => {
        impl TryFrom<$from> for $t {
            type Error = anyhow::Error;
            fn try_from(value: $from) -> Result<Self, Self::Error> {
                Self::new(value as u8)
            }
        }
    };
}

macro_rules! impl_binary_ops_with_num {
    // Entrypoint; impl ops on everything
    ($t: ty) => {
        // Implement ops on Self
        impl_binary_ops_with_num!($t, Add, AddAssign, add, add_assign, +);
        impl_binary_ops_with_num!($t, Sub, SubAssign, sub, sub_assign, -);
        impl_binary_ops_with_num!($t, Mul, MulAssign, mul, mul_assign, *);
        impl_binary_ops_with_num!($t, Div, DivAssign, div, div_assign, /);

        // Implement ops on primitives
        impl_binary_ops_with_num!($t, u8);
        impl_binary_ops_with_num!($t, u16);
        impl_binary_ops_with_num!($t, u32);
        impl_binary_ops_with_num!($t, u64);
        impl_binary_ops_with_num!($t, usize);
        impl_binary_ops_with_num!($t, i8);
        impl_binary_ops_with_num!($t, i16);
        impl_binary_ops_with_num!($t, i32);
        impl_binary_ops_with_num!($t, i64);
        impl_binary_ops_with_num!($t, isize);
    };

    // Impl all four ops (and assigns) for given RHS
    ($t:ty, $rhs:ty) => {
        impl_binary_ops_with_num!($t, $rhs, Add, AddAssign, add, add_assign, +);
        impl_binary_ops_with_num!($t, $rhs, Sub, SubAssign, sub, sub_assign, -);
        impl_binary_ops_with_num!($t, $rhs, Mul, MulAssign, mul, mul_assign, *);
        impl_binary_ops_with_num!($t, $rhs, Div, DivAssign, div, div_assign, /);
    };

    // Impl op and op_assign for Self
    ($t:ty, $op:tt, $op_assign:tt, $func:ident, $func_assign:ident, $op_tok:tt) => {
        impl std::ops::$op for $t {
            type Output = Self;
            fn $func(self, rhs: Self) -> Self::Output {
                Self::new(self.0 $op_tok rhs.0)
                    .expect("Attempted to add {self} and {rhs}, which is beyond Rank's bounds")
            }
        }

        impl std::ops::$op_assign for $t {
            fn $func_assign(&mut self, rhs: Self) {
                *self = *self $op_tok rhs;
            }
        }
    };

    // Impl op and op_assign for Rhs
    ($t:ty, $rhs:ty, $op:tt, $op_assign:tt, $func:ident, $func_assign:ident, $op_tok:tt) => {
        impl std::ops::$op<$rhs> for $t {
            type Output = Self;
            fn $func(self, rhs: $rhs) -> Self::Output {
                Self::new(self.0 $op_tok rhs as u8)
                    .expect("Attempted to add {self} and {rhs}, which is beyond Rank's bounds")
            }
        }

        impl std::ops::$op_assign<$rhs> for $t {
            fn $func_assign(&mut self, rhs: $rhs) {
                *self = *self $op_tok rhs;
            }
        }
    };
}

impl_binary_ops_with_num!(Rank);
impl_try_from_num!(Rank);

impl<T: AsRef<str>> PartialEq<T> for Rank {
    fn eq(&self, other: &T) -> bool {
        self.as_ref().eq(other.as_ref())
    }
}

impl TryFrom<char> for Rank {
    type Error = anyhow::Error;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        Self::from_char(value)
    }
}

impl From<Square> for Rank {
    fn from(value: Square) -> Self {
        value.rank()
    }
}

impl Add<char> for Rank {
    type Output = Self;
    fn add(self, rhs: char) -> Self::Output {
        self + Self::from_char(rhs).expect("Attempted to add {self} with invalid rank char")
    }
}

impl Sub<char> for Rank {
    type Output = Self;
    fn sub(self, rhs: char) -> Self::Output {
        self - Self::from_char(rhs).expect("Attempted to sub {self} with invalid rank char")
    }
}

impl AddAssign<char> for Rank {
    fn add_assign(&mut self, rhs: char) {
        *self = *self + rhs;
    }
}

impl SubAssign<char> for Rank {
    fn sub_assign(&mut self, rhs: char) {
        *self = *self - rhs;
    }
}

impl Mul<File> for Rank {
    type Output = Square;
    fn mul(self, file: File) -> Self::Output {
        Square::new(file, self)
    }
}

impl<T> Index<Rank> for [T; Rank::COUNT] {
    type Output = T;
    fn index(&self, index: Rank) -> &Self::Output {
        &self[index.0 as usize]
    }
}

impl<T> IndexMut<Rank> for [T; Rank::COUNT] {
    fn index_mut(&mut self, index: Rank) -> &mut Self::Output {
        &mut self[index.0 as usize]
    }
}

impl AsRef<str> for Rank {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for Rank {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char())
    }
}

impl fmt::Debug for Rank {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.char(), self.0)
    }
}

#[derive(Clone, Copy, Eq, PartialOrd, Ord, Hash, Default)]
pub struct File(pub(crate) u8);

impl File {
    pub const A: Self = Self(0);
    pub const B: Self = Self(1);
    pub const C: Self = Self(2);
    pub const D: Self = Self(3);
    pub const E: Self = Self(4);
    pub const F: Self = Self(5);
    pub const G: Self = Self(6);
    pub const H: Self = Self(7);

    pub const MIN: u8 = 0;
    pub const MAX: u8 = 7;
    pub const COUNT: usize = 8;

    /// An array of all [`File`]s, in ascending order.
    pub fn all() -> [Self; Self::COUNT] {
        [
            Self::A,
            Self::B,
            Self::C,
            Self::D,
            Self::E,
            Self::F,
            Self::G,
            Self::H,
        ]
    }

    /// Returns an iterator over all available files.
    ///
    /// # Example
    /// ```
    /// # use types::File;
    /// let mut iter = File::iter();
    /// assert_eq!(iter.len(), 8);
    /// assert_eq!(iter.next().unwrap(), File::A);
    /// assert_eq!(iter.last().unwrap(), File::H);
    /// ```
    pub fn iter() -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator<Item = Self> {
        Self::all().into_iter()
    }

    pub fn new(file: u8) -> Result<Self> {
        if file > Self::MAX {
            bail!(
                "Invalid int for File: Must be between [{}, {}]. Got {file}",
                Self::MIN,
                Self::MAX
            );
        }
        Ok(Self::new_unchecked(file))
    }

    pub const fn new_unchecked(file: u8) -> Self {
        Self(file)
    }

    pub fn from_char(file: char) -> Result<Self> {
        debug_assert!(file.is_ascii(), "File chars must be ASCII!");

        // Subtract the ASCII value for `a` (or `A`) to zero the number
        let file_int = file as u8 - if file.is_ascii_lowercase() { 'a' } else { 'A' } as u8;

        if file_int > Self::MAX {
            bail!(
                "Invalid char for File: Must be between [{}, {}]. Got {file}",
                'a',
                'h'
            );
        }

        Self::new(file_int)
    }

    /// `const` analog of `==`.
    pub const fn is(&self, other: &Self) -> bool {
        self.0 == other.0
    }

    pub const fn inner(&self) -> u8 {
        self.0
    }

    /// Obtain the inner value as a `usize`.
    ///
    /// Useful for indexing.
    pub const fn index(&self) -> usize {
        self.inner() as usize
    }

    pub const fn char(&self) -> char {
        (self.0 + b'a') as char
    }

    pub const fn as_str(&self) -> &'static str {
        match self.0 {
            0 => "a",
            1 => "b",
            2 => "c",
            3 => "d",
            4 => "e",
            5 => "f",
            6 => "g",
            7 => "h",
            _ => unreachable!(),
        }
    }

    /// Alias for [`Bitboard::from_file`].
    pub const fn bitboard(&self) -> Bitboard {
        Bitboard::from_file(*self)
    }

    /// Attempt to offset this [`File`] by the provided `delta`.
    ///
    /// If `self + delta` would exceed the bounds of this [`File`], then `None` is returned.
    ///
    /// # Example
    /// ```
    /// # use types::File;
    /// assert_eq!(File::C.offset(1), Some(File::D));
    /// assert_eq!(File::C.offset(-1), Some(File::B));
    /// assert_eq!(File::A.offset(-1), None);
    /// ```
    pub fn offset(self, delta: i8) -> Option<Self> {
        let bits = self.0.checked_add_signed(delta)?;
        (bits <= Self::MAX).then_some(Self::new_unchecked(bits))
    }

    /// Flips this [`File`].
    pub const fn flipped(self) -> Self {
        Self(Self::MAX - self.0)
    }
}

impl_binary_ops_with_num!(File);
impl_try_from_num!(File);

impl<T: AsRef<str>> PartialEq<T> for File {
    fn eq(&self, other: &T) -> bool {
        self.as_ref().eq(other.as_ref())
    }
}

impl TryFrom<char> for File {
    type Error = anyhow::Error;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        Self::from_char(value)
    }
}

impl From<Square> for File {
    fn from(value: Square) -> Self {
        value.file()
    }
}

impl Add<char> for File {
    type Output = Self;
    fn add(self, rhs: char) -> Self::Output {
        self + Self::from_char(rhs).expect("Attempted to add {self} with invalid file char")
    }
}

impl Sub<char> for File {
    type Output = Self;
    fn sub(self, rhs: char) -> Self::Output {
        self - Self::from_char(rhs).expect("Attempted to sub {self} with invalid file char")
    }
}

impl AddAssign<char> for File {
    fn add_assign(&mut self, rhs: char) {
        *self = *self + rhs;
    }
}

impl SubAssign<char> for File {
    fn sub_assign(&mut self, rhs: char) {
        *self = *self - rhs;
    }
}

impl Mul<Rank> for File {
    type Output = Square;
    fn mul(self, rank: Rank) -> Self::Output {
        Square::new(self, rank)
    }
}

impl<T> Index<File> for [T; File::COUNT] {
    type Output = T;
    fn index(&self, index: File) -> &Self::Output {
        &self[index.0 as usize]
    }
}

impl<T> IndexMut<File> for [T; File::COUNT] {
    fn index_mut(&mut self, index: File) -> &mut Self::Output {
        &mut self[index.0 as usize]
    }
}

impl AsRef<str> for File {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char())
    }
}

impl fmt::Debug for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.char(), self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_squares() {
        // Test the four corners
        let a1 = Square::new(File(0), Rank(0));
        assert_eq!(a1.to_string(), "a1");

        let h1 = Square::new(File(7), Rank(0));
        assert_eq!(h1.to_string(), "h1");

        let a8 = Square::new(File(0), Rank(7));
        assert_eq!(a8.to_string(), "a8");

        let h8 = Square::new(File(7), Rank(7));
        assert_eq!(h8.to_string(), "h8");

        // And some arbitrary location near the middle
        let d4 = Square::new(File(3), Rank(3));
        assert_eq!(d4.to_string(), "d4")
    }

    #[test]
    fn test_parsing() {
        assert_eq!(Rank::ONE, Rank::try_from('1').unwrap());
        assert_eq!(Rank::EIGHT, Rank::try_from('8').unwrap());
        assert_eq!(Rank::ONE, Rank::try_from(0).unwrap());
        assert_eq!(Rank::EIGHT, Rank::try_from(7).unwrap());

        assert_eq!(File::A, File::try_from('a').unwrap());
        assert_eq!(File::H, File::try_from('h').unwrap());
        assert_eq!(File::A, File::try_from(0).unwrap());
        assert_eq!(File::H, File::try_from(7).unwrap());

        assert!(Rank::try_from('0').is_err());
        assert!(Rank::try_from('9').is_err());
        assert!(File::try_from('z').is_err());

        // Now test squares as a whole
        assert_eq!(Square::try_from("a1").unwrap(), Square::A1);
        assert_eq!(Square::try_from(0).unwrap(), Square::A1);
        assert_eq!(Square::try_from("h8").unwrap(), Square::H8);
        assert_eq!(Square::try_from(63).unwrap(), Square::H8);
        assert_eq!(Square::try_from("d4").unwrap(), Square::D4);

        assert!(Square::try_from("a").is_err());
        assert!(Square::try_from("1").is_err());
        assert!(Square::try_from("").is_err());
        assert!(Square::try_from(-1).is_err());
        assert!(Square::try_from(64).is_err());
    }

    #[test]
    fn test_math() {
        assert_eq!(File::A * Rank::ONE, Square::A1);
        assert_eq!(Rank::ONE * File::A, Square::A1);
        assert_eq!(Rank::FOUR * File::G, Square::G4);

        assert_eq!(Rank::EIGHT - 1, Rank::SEVEN);
        assert_eq!(Rank::FOUR + 2, Rank::SIX);

        assert_eq!(File::A + 3, File::D);
        assert_eq!(File::G - 2, File::E);
    }

    #[test]
    fn test_indexing() {
        let mut board = [0; 64];
        board[Square::D5] = u8::MAX;
        assert_eq!(board[35], u8::MAX);

        let mut board = [[0; 8]; 8];
        board[Square::D5] = u8::MAX;
        assert_eq!(board[File::D][Rank::FIVE], u8::MAX);
    }
}
