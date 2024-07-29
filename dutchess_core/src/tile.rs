use std::{
    fmt,
    ops::{Add, AddAssign, Index, IndexMut, Mul, Sub, SubAssign},
    str::FromStr,
};

use anyhow::{bail, Context, Result};

use super::{BitBoard, Color};

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
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    /// # use dutchess_core::Tile;
    /// let mut iter = Tile::iter();
    /// assert_eq!(iter.len(), 64);
    /// assert_eq!(iter.next().unwrap(), Tile::A1);
    /// assert_eq!(iter.last().unwrap(), Tile::H8);
    /// ```
    pub fn iter() -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator<Item = Self> {
        (Self::MIN..=Self::MAX).map(|i| Self(i))
    }

    /// Creates a new [`Tile`] from the provided [`File`] and [`Rank`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Tile, File, Rank};
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
    /// # use dutchess_core::Tile;
    /// let c4 = Tile::from_index(26);
    /// assert!(c4.is_ok());
    /// assert_eq!(c4.unwrap(), Tile::C4);
    /// ```
    pub fn from_index(index: usize) -> Result<Self> {
        Self::from_int(index as u8)
    }

    /// Creates a new [`Tile`] from the provided index value, without error checking.
    ///
    /// # Panics
    ///
    /// If `index` is greater than `63`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// let c4 = Tile::from_index_unchecked(26);
    /// assert_eq!(c4, Tile::C4);
    /// ```
    pub const fn from_index_unchecked(index: usize) -> Self {
        debug_assert!(index < 64, "Index must be between [0,64)");
        Self(index as u8)
    }

    /// Creates a new [`Tile`] from the provided `u8` value.
    ///
    /// The provided `index` must be `[0, 63]` or else an error is returned.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// let c4 = Tile::from_int(26);
    /// assert!(c4.is_ok());
    /// assert_eq!(c4.unwrap(), Tile::C4);
    /// ```
    pub fn from_int(int: u8) -> Result<Self> {
        if int > Self::MAX {
            bail!(
                "Invalid int for Tile: Must be between [{}, {}]. Got {int}",
                Self::MIN,
                Self::MAX
            );
        }
        Ok(Self(int))
    }

    /// Flips this [`Tile`], viewing it from the opponent's perspective.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// assert_eq!(Tile::A1.flipped(), Tile::H8);
    /// assert_eq!(Tile::C4.flipped(), Tile::F5);
    /// ```
    pub const fn flipped(self) -> Self {
        Self(Self::MAX - self.0)
    }

    /// If `color` is Black, flips this [`Tile`].
    /// If `color` is White, does nothing.
    ///
    /// See [`Tile::flipped`] for more.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Color, Tile};
    /// assert_eq!(Tile::C4.relative_to(Color::White), Tile::C4);
    /// assert_eq!(Tile::C4.relative_to(Color::Black), Tile::F5);
    /// ```
    pub const fn relative_to(self, color: Color) -> Self {
        match color {
            Color::White => self,
            Color::Black => self.flipped(),
        }
    }

    /// Fetches the inner index value of the [`Tile`], which represented as a [`u8`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// assert_eq!(Tile::C4.inner(), 26);
    /// ```
    pub const fn inner(&self) -> u8 {
        self.0
    }

    /// Fetches the [`File`] of this [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Tile, File};
    /// assert_eq!(Tile::C4.file(), File::C);
    /// ```
    pub const fn file(&self) -> File {
        File(self.0 & Self::FILE_MASK) // Same as % 8
    }

    /// Fetches the [`Rank`] of this [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Tile, Rank};
    /// assert_eq!(Tile::C4.rank(), Rank::FOUR);
    /// ```
    pub const fn rank(&self) -> Rank {
        Rank(self.0 >> 3) // Same as / 8
    }

    /// Fetches the inner index value of the [`Tile`], casted to a [`usize`].
    ///
    /// Useful when using a [`Tile`] to index into things.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// assert_eq!(Tile::C4.index(), 26);
    /// ```
    pub const fn index(&self) -> usize {
        self.inner() as usize
    }

    /// Converts this [`Tile`] into a [`u64`] mask, primarily for use with [`BitBoard`]s.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// assert_eq!(Tile::C4.to_u64_mask(), 1 << 26); // 1 << 26 = 67108864
    /// ```
    pub const fn to_u64_mask(&self) -> u64 {
        1 << self.0
    }

    /// Fetches the [`File`] and [`Rank`] of this [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Tile, File, Rank};
    /// assert_eq!(Tile::C4.parts(), (File::C, Rank::FOUR));
    /// ```
    pub const fn parts(&self) -> (File, Rank) {
        (self.file(), self.rank())
    }

    // pub fn diag_index(&self) -> usize {
    //     (self.rank().index() - self.file().index()) & 15
    // }

    // pub fn anti_diag_index(&self) -> usize {
    //     (self.rank().index() + self.file().index()) ^ 7
    // }

    /// Returns `true` if this [`Tile`] is a light square.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// assert!(Tile::C4.is_light());
    /// ```
    pub const fn is_light(&self) -> bool {
        (self.file().0 + self.rank().0) % 2 != 0
    }

    /// Returns `true` if this [`Tile`] is a dark square.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// assert!(Tile::C5.is_dark());
    /// ```
    pub const fn is_dark(&self) -> bool {
        !self.is_light()
    }

    /// Returns `true` if `self` and `other` lie on the same diagonal.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
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
    /// # use dutchess_core::{Color, Tile};
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

    pub const fn is_king_start_square(&self, color: Color) -> bool {
        if color.is_white() {
            self.0 == Self::E1.0
        } else {
            self.0 == Self::E8.0
        }
    }

    /// Creates a [`Tile`] from a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
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
    /// # use dutchess_core::Tile;
    /// assert_eq!("c4", Tile::C4.to_uci());
    /// ```
    pub fn to_uci(self) -> String {
        format!("{}{}", self.file(), self.rank())
    }

    /// Alias for [`BitBoard::from_tile`].
    pub const fn bitboard(&self) -> BitBoard {
        BitBoard::from_tile(*self)
    }

    pub fn try_offset(&self, df: i8, dr: i8) -> Result<Self> {
        let file_bits = self.file().0 as i8 + df;
        let file = File::new(file_bits as u8)
            .context(format!("Attempting to offset file {} by {df}", self.file()))?;

        let rank_bits = self.rank().0 as i8 + dr;
        let rank = Rank::new(rank_bits as u8)
            .context(format!("Attempting to offset file {} by {df}", self.rank()))?;

        Ok(Self::new(file, rank))
    }

    /// Increments this [`Tile`] "North" (+rank) by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the maximum rank.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// assert_eq!(Tile::C4.north_by(1), Some(Tile::C5));
    ///
    /// assert_eq!(Tile::C8.north_by(1), None);
    /// ```
    pub const fn north_by(&self, n: u8) -> Option<Self> {
        if let Some(next) = self.rank().increase_by(n) {
            Some(Self::new(self.file(), next))
        } else {
            None
        }
    }

    /// Increments this [`Tile`] "South" (-rank) by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the minimum rank.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// assert_eq!(Tile::C4.south_by(1), Some(Tile::C3));
    ///
    /// assert_eq!(Tile::C1.south_by(1), None);
    /// ```
    pub const fn south_by(&self, n: u8) -> Option<Self> {
        if let Some(next) = self.rank().decrease_by(n) {
            Some(Self::new(self.file(), next))
        } else {
            None
        }
    }

    /// Increments this [`Tile`] "East" (+file) by one, if possible.
    ///
    /// Returns [`None`] if it is already at the maximum file.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// assert_eq!(Tile::C4.east(), Some(Tile::D4));
    ///
    /// assert_eq!(Tile::H4.east(), None);
    /// ```
    pub const fn east(&self) -> Option<Self> {
        if let Some(next) = self.file().increase() {
            Some(Self::new(next, self.rank()))
        } else {
            None
        }
    }

    /// Increments this [`Tile`] "West" (-file) by one, if possible.
    ///
    /// Returns [`None`] if it is already at the minimum file.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Tile;
    /// assert_eq!(Tile::C4.west(), Some(Tile::B4));
    ///
    /// assert_eq!(Tile::A4.west(), None);
    /// ```
    pub const fn west(&self) -> Option<Self> {
        if let Some(next) = self.file().decrease() {
            Some(Self::new(next, self.rank()))
        } else {
            None
        }
    }

    /// Increments (if `color` is [`White`]) or decrements (if `color` is [`Black`]) the [`Rank`] of this [`Tile`] by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Tile, Color};
    /// assert_eq!(Tile::C4.forward_by(Color::White, 1), Some(Tile::C5));
    /// assert_eq!(Tile::C4.forward_by(Color::Black, 1), Some(Tile::C3));
    /// ```
    pub const fn forward_by(&self, color: Color, n: u8) -> Option<Self> {
        match color {
            Color::White => self.north_by(n),
            Color::Black => self.south_by(n),
        }
    }

    /// Decrements (if `color` is [`White`]) or increments (if `color` is [`Black`]) the [`Rank`] of this [`Tile`] by `n`, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Tile, Color};
    /// assert_eq!(Tile::C4.backward_by(Color::White, 1), Some(Tile::C3));
    /// assert_eq!(Tile::C4.backward_by(Color::Black, 1), Some(Tile::C5));
    /// ```
    pub const fn backward_by(&self, color: Color, n: u8) -> Option<Self> {
        match color {
            Color::White => self.south_by(n),
            Color::Black => self.north_by(n),
        }
    }

    /// Increments (if `color` is [`White`]) or decrements (if `color` is [`Black`]) the [`File`] of this [`Tile`] by one, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Tile, Color};
    /// assert_eq!(Tile::C4.right(Color::White), Some(Tile::D4));
    /// assert_eq!(Tile::C4.right(Color::Black), Some(Tile::B4));
    /// ```
    pub const fn right(&self, color: Color) -> Option<Self> {
        match color {
            Color::White => self.east(),
            Color::Black => self.west(),
        }
    }

    /// Decrements (if `color` is [`White`]) or increments (if `color` is [`Black`]) the [`File`] of this [`Tile`] by one, if possible.
    ///
    /// Returns [`None`] if it is already at the edge of the board.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Tile, Color};
    /// assert_eq!(Tile::C4.left(Color::White), Some(Tile::B4));
    /// assert_eq!(Tile::C4.left(Color::Black), Some(Tile::D4));
    /// ```
    pub const fn left(&self, color: Color) -> Option<Self> {
        match color {
            Color::White => self.west(),
            Color::Black => self.east(),
        }
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
        Self::from_int(value)
    }
}

impl TryFrom<i32> for Tile {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        Self::from_int(value as u8)
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

/*
#[macro_export]
macro_rules! do_for_tiles {
    ($id:ident in $code:expr) => {
        do_for_tiles!($id = Tile::A1, $code);
        do_for_tiles!($id = Tile::B1, $code);
        do_for_tiles!($id = Tile::C1, $code);
        do_for_tiles!($id = Tile::D1, $code);
        do_for_tiles!($id = Tile::E1, $code);
        do_for_tiles!($id = Tile::F1, $code);
        do_for_tiles!($id = Tile::G1, $code);
        do_for_tiles!($id = Tile::H1, $code);
        do_for_tiles!($id = Tile::A2, $code);
        do_for_tiles!($id = Tile::B2, $code);
        do_for_tiles!($id = Tile::C2, $code);
        do_for_tiles!($id = Tile::D2, $code);
        do_for_tiles!($id = Tile::E2, $code);
        do_for_tiles!($id = Tile::F2, $code);
        do_for_tiles!($id = Tile::G2, $code);
        do_for_tiles!($id = Tile::H2, $code);
        do_for_tiles!($id = Tile::A3, $code);
        do_for_tiles!($id = Tile::B3, $code);
        do_for_tiles!($id = Tile::C3, $code);
        do_for_tiles!($id = Tile::D3, $code);
        do_for_tiles!($id = Tile::E3, $code);
        do_for_tiles!($id = Tile::F3, $code);
        do_for_tiles!($id = Tile::G3, $code);
        do_for_tiles!($id = Tile::H3, $code);
        do_for_tiles!($id = Tile::A4, $code);
        do_for_tiles!($id = Tile::B4, $code);
        do_for_tiles!($id = Tile::C4, $code);
        do_for_tiles!($id = Tile::D4, $code);
        do_for_tiles!($id = Tile::E4, $code);
        do_for_tiles!($id = Tile::F4, $code);
        do_for_tiles!($id = Tile::G4, $code);
        do_for_tiles!($id = Tile::H4, $code);
        do_for_tiles!($id = Tile::A5, $code);
        do_for_tiles!($id = Tile::B5, $code);
        do_for_tiles!($id = Tile::C5, $code);
        do_for_tiles!($id = Tile::D5, $code);
        do_for_tiles!($id = Tile::E5, $code);
        do_for_tiles!($id = Tile::F5, $code);
        do_for_tiles!($id = Tile::G5, $code);
        do_for_tiles!($id = Tile::H5, $code);
        do_for_tiles!($id = Tile::A6, $code);
        do_for_tiles!($id = Tile::B6, $code);
        do_for_tiles!($id = Tile::C6, $code);
        do_for_tiles!($id = Tile::D6, $code);
        do_for_tiles!($id = Tile::E6, $code);
        do_for_tiles!($id = Tile::F6, $code);
        do_for_tiles!($id = Tile::G6, $code);
        do_for_tiles!($id = Tile::H6, $code);
        do_for_tiles!($id = Tile::A7, $code);
        do_for_tiles!($id = Tile::B7, $code);
        do_for_tiles!($id = Tile::C7, $code);
        do_for_tiles!($id = Tile::D7, $code);
        do_for_tiles!($id = Tile::E7, $code);
        do_for_tiles!($id = Tile::F7, $code);
        do_for_tiles!($id = Tile::G7, $code);
        do_for_tiles!($id = Tile::H7, $code);
        do_for_tiles!($id = Tile::A8, $code);
        do_for_tiles!($id = Tile::B8, $code);
        do_for_tiles!($id = Tile::C8, $code);
        do_for_tiles!($id = Tile::D8, $code);
        do_for_tiles!($id = Tile::E8, $code);
        do_for_tiles!($id = Tile::F8, $code);
        do_for_tiles!($id = Tile::G8, $code);
        do_for_tiles!($id = Tile::H8, $code);
    };
    ($id:ident = $val:expr, $expr:expr) => {
        let $id = $val;
        $expr
    };
}
 */

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    /// # use dutchess_core::Rank;
    /// let mut iter = Rank::iter();
    /// assert_eq!(iter.len(), 8);
    /// assert_eq!(iter.next().unwrap(), Rank::ONE);
    /// assert_eq!(iter.last().unwrap(), Rank::EIGHT);
    /// ```
    pub fn iter() -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator<Item = Self> {
        (Self::MIN..=Self::MAX).map(|i| Self(i))
    }

    pub fn new(rank: u8) -> Result<Self> {
        if rank > Self::MAX {
            bail!(
                "Invalid int for Rank: Must be between [{}, {}]. Got {rank}",
                Self::MIN,
                Self::MAX
            );
        }

        Ok(Self(rank))
    }

    pub const fn new_unchecked(rank: u8) -> Self {
        Self(rank)
    }

    pub const fn home_rank(color: Color) -> Self {
        match color {
            Color::White => Self::ONE,
            Color::Black => Self::EIGHT,
        }
    }

    pub const fn pawn_rank(color: Color) -> Self {
        match color {
            Color::White => Self::TWO,
            Color::Black => Self::SEVEN,
        }
    }

    pub const fn pawn_double_push_rank(color: Color) -> Self {
        match color {
            Color::White => Self::FOUR,
            Color::Black => Self::FIVE,
        }
    }

    pub const fn is_home_rank(&self, color: Color) -> bool {
        self.0 == Self::home_rank(color).0
    }

    pub const fn is_pawn_rank(&self, color: Color) -> bool {
        self.0 == Self::pawn_rank(color).0
    }

    pub const fn is_pawn_double_push_rank(&self, color: Color) -> bool {
        self.0 == Self::pawn_double_push_rank(color).0
    }

    pub fn from_char(rank: char) -> Result<Self> {
        debug_assert!(rank.is_ascii(), "Rank chars must be ASCII!");

        let Some(rank_int) = rank.to_digit(10) else {
            bail!(
                "Invalid char for Rank: Must be between [{}, {}]. Got {rank}",
                '1',
                '8'
            );
        };

        let Some(rank) = rank_int.checked_sub(1) else {
            bail!(
                "Invalid char for Rank: Must be between [{}, {}]. Got {rank}",
                '1',
                '8'
            );
        };

        Self::new(rank as u8)
    }

    pub const fn inner(&self) -> u8 {
        self.0
    }

    pub const fn index(&self) -> usize {
        self.index_le()
    }

    pub const fn char(&self) -> char {
        (self.0 + '1' as u8) as char
    }

    pub const fn str(&self) -> &'static str {
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

    // Index in Little Endian (default)
    pub const fn index_le(&self) -> usize {
        self.0 as usize
    }

    // Index in Big Endian
    pub const fn index_be(&self) -> usize {
        self.index_le() ^ 56
    }

    /// Alias for [`BitBoard::from_rank`].
    pub const fn bitboard(&self) -> BitBoard {
        BitBoard::from_rank(*self)
    }

    /*
    const fn offset(&self, offset: i8) -> Option<Self> {
        let bits = self.0 as i8 + offset;
        if bits.is_negative() || bits > 7 {
            None
        } else {
            Some(Self(bits as u8))
        }
    }
     */

    pub const fn increase(&self) -> Option<Self> {
        self.increase_by(1)
    }

    pub const fn increase_by(&self, n: u8) -> Option<Self> {
        let val = self.0 + n;
        if val > Self::MAX {
            None
        } else {
            Some(Self(val))
        }
    }

    pub const fn decrease(&self) -> Option<Self> {
        self.decrease_by(1)
    }

    pub const fn decrease_by(&self, n: u8) -> Option<Self> {
        if let Some(val) = self.0.checked_sub(n) {
            Some(Self(val))
        } else {
            None
        }
    }

    pub const fn first_rank(&self, color: Color) -> Self {
        match color {
            Color::White => Self::ONE,
            Color::Black => Self::EIGHT,
        }
    }

    pub const fn second_rank(&self, color: Color) -> Self {
        match color {
            Color::White => Self::TWO,
            Color::Black => Self::SEVEN,
        }
    }

    /// Rank just before promoting a pawn
    pub const fn seventh_rank(&self, color: Color) -> Self {
        match color {
            Color::White => Self::SEVEN,
            Color::Black => Self::TWO,
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
        self.str()
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    /// Returns an iterator over all available files.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::File;
    /// let mut iter = File::iter();
    /// assert_eq!(iter.len(), 8);
    /// assert_eq!(iter.next().unwrap(), File::A);
    /// assert_eq!(iter.last().unwrap(), File::H);
    /// ```
    pub fn iter() -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator<Item = Self> {
        (Self::MIN..=Self::MAX).map(|i| Self(i))
    }

    pub fn new(file: u8) -> Result<Self> {
        if file > Self::MAX {
            bail!(
                "Invalid int for File: Must be between [{}, {}]. Got {file}",
                Self::MIN,
                Self::MAX
            );
        }
        Ok(Self(file))
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

    pub const fn inner(&self) -> u8 {
        self.0
    }

    pub const fn index(&self) -> usize {
        self.index_le()
    }

    pub const fn char(&self) -> char {
        (self.0 + 'a' as u8) as char
    }

    pub const fn str(&self) -> &'static str {
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

    /// Alias for [`BitBoard::from_file`].
    pub const fn bitboard(&self) -> BitBoard {
        BitBoard::from_file(*self)
    }

    pub const fn increase(&self) -> Option<Self> {
        self.increase_by(1)
    }

    pub const fn increase_by(&self, n: u8) -> Option<Self> {
        let val = self.0 + n;
        if val > Self::MAX {
            None
        } else {
            Some(Self(val))
        }
    }

    pub const fn decrease(&self) -> Option<Self> {
        self.decrease_by(1)
    }

    pub const fn decrease_by(&self, n: u8) -> Option<Self> {
        if let Some(val) = self.0.checked_sub(n) {
            Some(Self(val))
        } else {
            None
        }
    }
}

impl_binary_ops_with_num!(File);
impl_try_from_num!(File);

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
        self.str()
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
