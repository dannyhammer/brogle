use std::{
    fmt,
    ops::{Add, AddAssign, Index, IndexMut, Mul, Sub, SubAssign},
    str::FromStr,
};

use anyhow::{bail, Context, Result};

use super::{Bitboard, Color};

/// Represents a single tile (or square) on an `8x8` chess board.
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
/// so `tile = file + rank * 8`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
#[repr(transparent)]
pub struct Tile(pub(crate) u8);

impl Tile {
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
    // const RANK_MASK: u8 = 0b0011_1000;

    /// Returns an iterator over all available tiles.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// let mut iter = Tile::iter();
    /// assert_eq!(iter.len(), 64);
    /// assert_eq!(iter.next().unwrap(), Tile::A1);
    /// assert_eq!(iter.last().unwrap(), Tile::H8);
    /// ```
    pub fn iter() -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator<Item = Self> {
        (Self::MIN..=Self::MAX).map(Self)
    }

    /// Creates a new [`Tile`] from the provided [`File`] and [`Rank`].
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Tile, File, Rank};
    /// let c4 = Tile::new(File::C, Rank::FOUR);
    /// assert_eq!(c4, Tile::C4);
    /// ```
    pub const fn new(file: File, rank: Rank) -> Self {
        // least-significant file mapping
        Self(file.0 ^ rank.0 << 3)
    }

    /// Creates a new [`Tile`] from the provided index value.
    ///
    /// The provided `index` must be `[0, 63]` or else an error is returned.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// let c4 = Tile::from_index(26);
    /// assert!(c4.is_ok());
    /// assert_eq!(c4.unwrap(), Tile::C4);
    /// ```
    pub fn from_index(index: usize) -> Result<Self> {
        Self::from_bits(index as u8)
    }

    /// Creates a new [`Tile`] from the provided index value, without error checking.
    ///
    /// # Panics
    ///
    /// If `index` is greater than `63`.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// let c4 = Tile::from_index_unchecked(26);
    /// assert_eq!(c4, Tile::C4);
    /// ```
    pub const fn from_index_unchecked(index: usize) -> Self {
        debug_assert!(index < 64, "Index must be between [0,64)");
        Self(index as u8)
    }

    /// Creates a new [`Tile`] from the provided `u8` value.
    ///
    /// The provided `bits` must be `[0, 63]` or else an error is returned.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// let c4 = Tile::from_bits(26);
    /// assert!(c4.is_ok());
    /// assert_eq!(c4.unwrap(), Tile::C4);
    /// ```
    pub fn from_bits(bits: u8) -> Result<Self> {
        if bits > Self::MAX {
            bail!(
                "Invalid bits for Tile: Must be between [{}, {}]. Got {bits}",
                Self::MIN,
                Self::MAX
            );
        }
        Ok(Self(bits))
    }

    /// Creates a new [`Tile`] from the provided `u8` value, without error checking.
    ///
    /// The provided `bits` must be `[0, 63]` or else an error is returned.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// let c4 = Tile::from_bits_unchecked(26);
    /// assert_eq!(c4, Tile::C4);
    /// ```
    pub const fn from_bits_unchecked(bits: u8) -> Self {
        Self(bits)
    }

    /// Flips this [`Tile`], viewing it from the opponent's perspective.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!(Tile::A1.flipped(), Tile::H8);
    /// assert_eq!(Tile::C4.flipped(), Tile::F5);
    /// ```
    pub const fn flipped(self) -> Self {
        Self(Self::MAX - self.0)
    }

    /// Flips the [`File`] of this [`Tile`]..
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!(Tile::A1.flipped_file(), Tile::H1);
    /// assert_eq!(Tile::C4.flipped_file(), Tile::F4);
    /// ```
    pub const fn flipped_file(self) -> Self {
        Self::new(self.file().flipped(), self.rank())
    }

    /// Flips the [`Rank`] of this [`Tile`]..
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!(Tile::A1.flipped_rank(), Tile::A8);
    /// assert_eq!(Tile::C4.flipped_rank(), Tile::C5);
    /// ```
    pub const fn flipped_rank(self) -> Self {
        Self::new(self.file(), self.rank().flipped())
    }

    /// If `color` is Black, flips this [`Tile`].
    /// If `color` is White, does nothing.
    ///
    /// See [`Tile::flipped`] for more.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Color, Tile};
    /// assert_eq!(Tile::C4.relative_to(Color::White), Tile::C4);
    /// assert_eq!(Tile::C4.relative_to(Color::Black), Tile::F5);
    /// ```
    pub const fn relative_to(self, color: Color) -> Self {
        match color {
            Color::White => self,
            Color::Black => self.flipped(),
        }
    }

    /// If `color` is Black, flips the [`Rank`] of this [`Tile`].
    /// If `color` is White, does nothing.
    ///
    /// See [`Tile::flipped_rank`] for more.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Color, Tile};
    /// assert_eq!(Tile::E1.rank_relative_to(Color::White), Tile::E1);
    /// assert_eq!(Tile::E1.rank_relative_to(Color::Black), Tile::E8);
    /// ```
    pub const fn rank_relative_to(self, color: Color) -> Self {
        match color {
            Color::White => self,
            Color::Black => self.flipped_rank(),
        }
    }

    /// If `color` is Black, flips the [`File`] of this [`Tile`].
    /// If `color` is White, does nothing.
    ///
    /// See [`Tile::flipped_file`] for more.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Color, Tile};
    /// assert_eq!(Tile::A1.file_relative_to(Color::White), Tile::A1);
    /// assert_eq!(Tile::A1.file_relative_to(Color::Black), Tile::H1);
    /// ```
    pub const fn file_relative_to(self, color: Color) -> Self {
        match color {
            Color::White => self,
            Color::Black => self.flipped_file(),
        }
    }

    /// Iterating over [`Tile`]s increases their internal counter by 1.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!(Tile::A1.next(), Some(Tile::B1));
    /// assert_eq!(Tile::H3.next(), Some(Tile::A4));
    /// assert_eq!(Tile::H8.next(), None);
    /// ```
    pub fn next(self) -> Option<Self> {
        (self.0 < Self::MAX).then(|| Self::from_bits_unchecked(self.0 + 1))
    }

    /// Iterating backwards over [`Tile`]s decreases their internal counter by 1.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!(Tile::A1.prev(), None);
    /// assert_eq!(Tile::A4.prev(), Some(Tile::H3));
    /// assert_eq!(Tile::H8.prev(), Some(Tile::G8));
    /// ```
    pub fn prev(self) -> Option<Self> {
        (self.0 > Self::MIN).then(|| Self::from_bits_unchecked(self.0 - 1))
    }

    /// Fetches the inner index value of the [`Tile`], which represented as a [`u8`].
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!(Tile::C4.inner(), 26);
    /// ```
    pub const fn inner(&self) -> u8 {
        self.0
    }

    /// Fetches the [`File`] of this [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Tile, File};
    /// assert_eq!(Tile::C4.file(), File::C);
    /// ```
    pub const fn file(&self) -> File {
        File(self.0 & Self::FILE_MASK) // Same as % 8
    }

    /// Fetches the [`Rank`] of this [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Tile, Rank};
    /// assert_eq!(Tile::C4.rank(), Rank::FOUR);
    /// ```
    pub const fn rank(&self) -> Rank {
        Rank(self.0 >> 3) // Same as / 8
    }

    /// Fetches the [`File`] and [`Rank`] of this [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Tile, File, Rank};
    /// assert_eq!(Tile::C4.parts(), (File::C, Rank::FOUR));
    /// ```
    pub const fn parts(&self) -> (File, Rank) {
        (self.file(), self.rank())
    }

    /// Fetches the inner index value of the [`Tile`], casted to a [`usize`].
    ///
    /// Useful when using a [`Tile`] to index into things.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!(Tile::C4.index(), 26);
    /// ```
    pub const fn index(&self) -> usize {
        self.inner() as usize
    }

    /// Returns `true` if this [`Tile`] is a light square.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert!(Tile::C4.is_light());
    /// ```
    pub const fn is_light(&self) -> bool {
        (self.file().0 + self.rank().0) % 2 != 0
    }

    /// Returns `true` if this [`Tile`] is a dark square.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert!(Tile::C5.is_dark());
    /// ```
    pub const fn is_dark(&self) -> bool {
        !self.is_light()
    }

    /// Returns `true` if `self` and `other` lie on the same diagonal.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!(Tile::C5.is_diagonal_to(Tile::A3), true);
    /// assert_eq!(Tile::H1.is_diagonal_to(Tile::A8), true);
    /// assert_eq!(Tile::F7.is_diagonal_to(Tile::F7), true);
    /// assert_eq!(Tile::A1.is_diagonal_to(Tile::B3), false);
    /// assert_eq!(Tile::A4.is_diagonal_to(Tile::H3), false);
    /// assert_eq!(Tile::A4.is_diagonal_to(Tile::H4), false);
    /// ```
    pub const fn is_diagonal_to(&self, other: Self) -> bool {
        if self.0 == other.0 {
            return true;
        }

        (self.0 % 9 == other.0 % 9 || self.0 % 7 == other.0 % 7) // On same diag
            && self.rank().0 != other.rank().0 // Not on same rank
            && self.file().0 != other.file().0 // Not on same file
    }

    /// Returns the [`Color`] of this [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Color, Tile};
    /// assert_eq!(Tile::C5.color(), Color::Black);
    /// assert_eq!(Tile::C4.color(), Color::White);
    /// ```
    pub const fn color(&self) -> Color {
        if self.is_light() {
            Color::White
        } else {
            Color::Black
        }
    }

    /// Creates a [`Tile`] from a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// let c4 = Tile::from_uci("c4");
    /// assert!(c4.is_ok());
    /// assert_eq!(c4.unwrap(), Tile::C4);
    ///
    /// let err = Tile::from_uci("z0");
    /// assert!(err.is_err());
    /// ```
    pub fn from_uci(tile: &str) -> Result<Self> {
        let bytes = tile.as_bytes();
        if tile.len() != 2 {
            bail!("Invalid Tile string: String must contain exactly 2 characters. Got {tile}")
        }
        let file = File::from_char(bytes[0] as char)?;
        let rank = Rank::from_char(bytes[1] as char)?;

        Ok(Self::new(file, rank))
    }

    /// Converts this [`Tile`] to a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!("c4", Tile::C4.to_uci());
    /// ```
    pub fn to_uci(self) -> String {
        format!("{}{}", self.file(), self.rank())
    }

    /// Alias for [`Bitboard::from_tile`].
    pub const fn bitboard(&self) -> Bitboard {
        Bitboard::from_tile(*self)
    }

    /// Computes the distance between `self` and `other`.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!(Tile::C4.distance_to(Tile::C1), 3);
    /// assert_eq!(Tile::A1.distance_to(Tile::A8), 7);
    /// assert_eq!(Tile::D6.distance_to(Tile::D6), 0);
    /// assert_eq!(Tile::E2.distance_to(Tile::C6), 6);
    /// ```
    pub const fn distance_to(&self, other: Self) -> u8 {
        self.file().0.abs_diff(other.file().0) + self.rank().0.abs_diff(other.rank().0)
    }

    /// Computes the distance between `self` and the center of the board.
    ///
    /// The center tiles are E4, E5, D4, and D5.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!(Tile::D5.distance_from_center(), 0);
    /// assert_eq!(Tile::E4.distance_from_center(), 0);
    /// assert_eq!(Tile::A1.distance_from_center(), 6);
    /// ```
    pub fn distance_from_center(&self) -> u8 {
        self.distance_to(Self::E4)
            .min(self.distance_to(Self::E5))
            .min(self.distance_to(Self::D4).min(self.distance_to(Self::D5)))
    }

    /// Attempt to offset this [`Tile`] by the file and rank offsets.
    ///
    /// If `self + offset` would exceed the bounds of this [`File`], then `None` is returned.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::Tile;
    /// assert_eq!(Tile::C4.offset(1, 1), Some(Tile::D5));
    /// assert_eq!(Tile::C4.offset(-1, -1), Some(Tile::B3));
    /// assert_eq!(Tile::A1.offset(-1, -1), None);
    /// ```
    pub fn offset(&self, file_delta: i8, rank_delta: i8) -> Option<Self> {
        let file = self.file().offset(file_delta)?;
        let rank = self.rank().offset(rank_delta)?;

        Some(Self::new(file, rank))
    }

    /// Increments (if `color` is [`Color::White`]) or decrements (if `color` is [`Color::Black`]) the [`Rank`] of this [`Tile`] by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Tile, Color};
    /// assert_eq!(Tile::C4.forward_by(Color::White, 1), Some(Tile::C5));
    /// assert_eq!(Tile::C4.forward_by(Color::Black, 1), Some(Tile::C3));
    /// ```
    pub fn forward_by(&self, color: Color, n: u8) -> Option<Self> {
        self.offset(0, n as i8 * color.negation_multiplier())
    }

    /// Decrements (if `color` is [`Color::White`]) or increments (if `color` is [`Color::Black`]) the [`Rank`] of this [`Tile`] by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Tile, Color};
    /// assert_eq!(Tile::C4.backward_by(Color::White, 1), Some(Tile::C3));
    /// assert_eq!(Tile::C4.backward_by(Color::Black, 1), Some(Tile::C5));
    /// ```
    pub fn backward_by(&self, color: Color, n: u8) -> Option<Self> {
        self.offset(0, n as i8 * color.opponent().negation_multiplier())
    }

    /// Increments (if `color` is [`Color::White`]) or decrements (if `color` is [`Color::Black`]) the [`File`] of this [`Tile`] by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Tile, Color};
    /// assert_eq!(Tile::C4.right_by(Color::White, 1), Some(Tile::D4));
    /// assert_eq!(Tile::C4.right_by(Color::Black, 1), Some(Tile::B4));
    /// ```
    pub fn right_by(&self, color: Color, n: u8) -> Option<Self> {
        self.offset(n as i8 * color.negation_multiplier(), 0)
    }

    /// Decrements (if `color` is [`Color::White`]) or increments (if `color` is [`Color::Black`]) the [`File`] of this [`Tile`] by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use brogle_types::{Tile, Color};
    /// assert_eq!(Tile::C4.left_by(Color::White, 1), Some(Tile::B4));
    /// assert_eq!(Tile::C4.left_by(Color::Black, 1), Some(Tile::D4));
    /// ```
    pub fn left_by(&self, color: Color, n: u8) -> Option<Self> {
        self.offset(n as i8 * color.opponent().negation_multiplier(), 0)
    }
}

impl<T: AsRef<str>> PartialEq<T> for Tile {
    fn eq(&self, other: &T) -> bool {
        self.to_string().eq(other.as_ref())
    }
}

impl FromStr for Tile {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_uci(s)
    }
}

impl TryFrom<&str> for Tile {
    type Error = anyhow::Error;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_uci(value)
    }
}

impl TryFrom<String> for Tile {
    type Error = anyhow::Error;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::from_uci(&value)
    }
}

impl TryFrom<usize> for Tile {
    type Error = anyhow::Error;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Self::from_index(value)
    }
}

impl TryFrom<u8> for Tile {
    type Error = anyhow::Error;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::from_bits(value)
    }
}

impl TryFrom<i32> for Tile {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        Self::from_bits(value as u8)
    }
}

impl Add<Rank> for Tile {
    type Output = Self;
    fn add(self, rhs: Rank) -> Self::Output {
        Self::new(self.file(), self.rank() + rhs)
    }
}

impl Add<File> for Tile {
    type Output = Self;
    fn add(self, rhs: File) -> Self::Output {
        Self::new(self.file() + rhs, self.rank())
    }
}

impl Sub<Rank> for Tile {
    type Output = Self;
    fn sub(self, rhs: Rank) -> Self::Output {
        Self::new(self.file(), self.rank() - rhs)
    }
}

impl Sub<File> for Tile {
    type Output = Self;
    fn sub(self, rhs: File) -> Self::Output {
        Self::new(self.file() - rhs, self.rank())
    }
}

impl AddAssign<Rank> for Tile {
    fn add_assign(&mut self, rhs: Rank) {
        *self = *self + rhs;
    }
}

impl SubAssign<Rank> for Tile {
    fn sub_assign(&mut self, rhs: Rank) {
        *self = *self - rhs;
    }
}

impl AddAssign<File> for Tile {
    fn add_assign(&mut self, rhs: File) {
        *self = *self + rhs;
    }
}

impl SubAssign<File> for Tile {
    fn sub_assign(&mut self, rhs: File) {
        *self = *self - rhs;
    }
}

impl<T> Index<Tile> for [T; Tile::COUNT] {
    type Output = T;
    fn index(&self, index: Tile) -> &Self::Output {
        &self[index.index()]
    }
}

impl<T> IndexMut<Tile> for [T; Tile::COUNT] {
    fn index_mut(&mut self, index: Tile) -> &mut Self::Output {
        &mut self[index.index()]
    }
}

impl<T> Index<Tile> for [[T; File::COUNT]; Rank::COUNT] {
    type Output = T;
    fn index(&self, index: Tile) -> &Self::Output {
        &self[index.file()][index.rank()]
    }
}

impl<T> IndexMut<Tile> for [[T; File::COUNT]; Rank::COUNT] {
    fn index_mut(&mut self, index: Tile) -> &mut Self::Output {
        &mut self[index.file()][index.rank()]
    }
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}

impl fmt::Debug for Tile {
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
    /// # use brogle_types::Rank;
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

    pub const fn index(&self) -> usize {
        self.index_le()
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

    // Index in Little Endian (default)
    pub const fn index_le(&self) -> usize {
        self.0 as usize
    }

    // Index in Big Endian
    pub const fn index_be(&self) -> usize {
        self.index_le() ^ 56
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
    /// # use brogle_types::Rank;
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

impl From<Tile> for Rank {
    fn from(value: Tile) -> Self {
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
    type Output = Tile;
    fn mul(self, file: File) -> Self::Output {
        Tile::new(file, self)
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
    /// # use brogle_types::File;
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

    pub const fn index(&self) -> usize {
        self.index_le()
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

    // Index in Little Endian (default)
    pub const fn index_le(&self) -> usize {
        self.0 as usize
    }

    // Index in Big Endian
    pub const fn index_be(&self) -> usize {
        self.index_le() ^ 7
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
    /// # use brogle_types::File;
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

impl From<Tile> for File {
    fn from(value: Tile) -> Self {
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
    type Output = Tile;
    fn mul(self, rank: Rank) -> Self::Output {
        Tile::new(self, rank)
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
    fn test_tiles() {
        // Test the four corners
        let a1 = Tile::new(File(0), Rank(0));
        assert_eq!(a1.to_string(), "a1");

        let h1 = Tile::new(File(7), Rank(0));
        assert_eq!(h1.to_string(), "h1");

        let a8 = Tile::new(File(0), Rank(7));
        assert_eq!(a8.to_string(), "a8");

        let h8 = Tile::new(File(7), Rank(7));
        assert_eq!(h8.to_string(), "h8");

        // And some arbitrary location near the middle
        let d4 = Tile::new(File(3), Rank(3));
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

        // Now test tiles as a whole
        assert_eq!(Tile::try_from("a1").unwrap(), Tile::A1);
        assert_eq!(Tile::try_from(0).unwrap(), Tile::A1);
        assert_eq!(Tile::try_from("h8").unwrap(), Tile::H8);
        assert_eq!(Tile::try_from(63).unwrap(), Tile::H8);
        assert_eq!(Tile::try_from("d4").unwrap(), Tile::D4);

        assert!(Tile::try_from("a").is_err());
        assert!(Tile::try_from("1").is_err());
        assert!(Tile::try_from("").is_err());
        assert!(Tile::try_from(-1).is_err());
        assert!(Tile::try_from(64).is_err());
    }

    #[test]
    fn test_math() {
        assert_eq!(File::A * Rank::ONE, Tile::A1);
        assert_eq!(Rank::ONE * File::A, Tile::A1);
        assert_eq!(Rank::FOUR * File::G, Tile::G4);

        assert_eq!(Rank::EIGHT - 1, Rank::SEVEN);
        assert_eq!(Rank::FOUR + 2, Rank::SIX);

        assert_eq!(File::A + 3, File::D);
        assert_eq!(File::G - 2, File::E);
    }

    #[test]
    fn test_indexing() {
        let mut board = [0; 64];
        board[Tile::D5] = u8::MAX;
        assert_eq!(board[35], u8::MAX);

        let mut board = [[0; 8]; 8];
        board[Tile::D5] = u8::MAX;
        assert_eq!(board[File::D][Rank::FIVE], u8::MAX);
    }
}
