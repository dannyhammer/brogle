use std::{
    error::Error,
    fmt,
    ops::{Add, AddAssign, Index, IndexMut, Mul, Sub, SubAssign},
    str::FromStr,
};

use crate::Color;

#[derive(Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub enum ChessError {
    OutOfBounds { val: usize, min: usize, max: usize },
    InvalidFileChar { val: char },
    InvalidRankChar { val: char },
    InvalidTileNotation,
    InvalidPieceNotation,
    InvalidPieceChar { val: char },
}

impl fmt::Display for ChessError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OutOfBounds { val, min, max } => {
                write!(f, "value {val} must be within {min}..={max}")
            }
            Self::InvalidFileChar { val } => write!(f, "file chars must be [a, h], found {val}"),
            Self::InvalidRankChar { val } => write!(f, "rank chars must be [1, 8], found {val}"),
            Self::InvalidTileNotation => write!(
                f,
                "tile is not valid notation. notation must be <file><rank>"
            ),
            Self::InvalidPieceNotation => write!(
                f,
                "tile is not valid notation. notation must be <file><rank>"
            ),
            Self::InvalidPieceChar { val } => write!(
                f,
                "pieces must be [p | n | b | r | q | k] or uppercase equivalent. found {val}"
            ),
        }
    }
}

impl Error for ChessError {
    //
}

/// Represents a single tile (or square) on an `8x8` chess board.
///
/// Internally encoded using [Little-Endian Rank-File Mapping](https://www.chessprogramming.org/Square_Mapping_Considerations#Little-Endian_Rank-File_Mapping) (LERF)
///
/// Locations are computed through [Least Significant File Mapping](https://www.chessprogramming.org/Square_Mapping_Considerations#Deduction_on_Files_and_Ranks),
/// so `tile = file + rank * 8`.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Tile(pub(crate) u8);

impl Tile {
    pub const A1: Tile = Self::new(File::A, Rank::ONE);
    pub const A2: Tile = Self::new(File::A, Rank::TWO);
    pub const A3: Tile = Self::new(File::A, Rank::THREE);
    pub const A4: Tile = Self::new(File::A, Rank::FOUR);
    pub const A5: Tile = Self::new(File::A, Rank::FIVE);
    pub const A6: Tile = Self::new(File::A, Rank::SIX);
    pub const A7: Tile = Self::new(File::A, Rank::SEVEN);
    pub const A8: Tile = Self::new(File::A, Rank::EIGHT);

    pub const B1: Tile = Self::new(File::B, Rank::ONE);
    pub const B2: Tile = Self::new(File::B, Rank::TWO);
    pub const B3: Tile = Self::new(File::B, Rank::THREE);
    pub const B4: Tile = Self::new(File::B, Rank::FOUR);
    pub const B5: Tile = Self::new(File::B, Rank::FIVE);
    pub const B6: Tile = Self::new(File::B, Rank::SIX);
    pub const B7: Tile = Self::new(File::B, Rank::SEVEN);
    pub const B8: Tile = Self::new(File::B, Rank::EIGHT);

    pub const C1: Tile = Self::new(File::C, Rank::ONE);
    pub const C2: Tile = Self::new(File::C, Rank::TWO);
    pub const C3: Tile = Self::new(File::C, Rank::THREE);
    pub const C4: Tile = Self::new(File::C, Rank::FOUR);
    pub const C5: Tile = Self::new(File::C, Rank::FIVE);
    pub const C6: Tile = Self::new(File::C, Rank::SIX);
    pub const C7: Tile = Self::new(File::C, Rank::SEVEN);
    pub const C8: Tile = Self::new(File::C, Rank::EIGHT);

    pub const D1: Tile = Self::new(File::D, Rank::ONE);
    pub const D2: Tile = Self::new(File::D, Rank::TWO);
    pub const D3: Tile = Self::new(File::D, Rank::THREE);
    pub const D4: Tile = Self::new(File::D, Rank::FOUR);
    pub const D5: Tile = Self::new(File::D, Rank::FIVE);
    pub const D6: Tile = Self::new(File::D, Rank::SIX);
    pub const D7: Tile = Self::new(File::D, Rank::SEVEN);
    pub const D8: Tile = Self::new(File::D, Rank::EIGHT);

    pub const E1: Tile = Self::new(File::E, Rank::ONE);
    pub const E2: Tile = Self::new(File::E, Rank::TWO);
    pub const E3: Tile = Self::new(File::E, Rank::THREE);
    pub const E4: Tile = Self::new(File::E, Rank::FOUR);
    pub const E5: Tile = Self::new(File::E, Rank::FIVE);
    pub const E6: Tile = Self::new(File::E, Rank::SIX);
    pub const E7: Tile = Self::new(File::E, Rank::SEVEN);
    pub const E8: Tile = Self::new(File::E, Rank::EIGHT);

    pub const F1: Tile = Self::new(File::F, Rank::ONE);
    pub const F2: Tile = Self::new(File::F, Rank::TWO);
    pub const F3: Tile = Self::new(File::F, Rank::THREE);
    pub const F4: Tile = Self::new(File::F, Rank::FOUR);
    pub const F5: Tile = Self::new(File::F, Rank::FIVE);
    pub const F6: Tile = Self::new(File::F, Rank::SIX);
    pub const F7: Tile = Self::new(File::F, Rank::SEVEN);
    pub const F8: Tile = Self::new(File::F, Rank::EIGHT);

    pub const G1: Tile = Self::new(File::G, Rank::ONE);
    pub const G2: Tile = Self::new(File::G, Rank::TWO);
    pub const G3: Tile = Self::new(File::G, Rank::THREE);
    pub const G4: Tile = Self::new(File::G, Rank::FOUR);
    pub const G5: Tile = Self::new(File::G, Rank::FIVE);
    pub const G6: Tile = Self::new(File::G, Rank::SIX);
    pub const G7: Tile = Self::new(File::G, Rank::SEVEN);
    pub const G8: Tile = Self::new(File::G, Rank::EIGHT);

    pub const H1: Tile = Self::new(File::H, Rank::ONE);
    pub const H2: Tile = Self::new(File::H, Rank::TWO);
    pub const H3: Tile = Self::new(File::H, Rank::THREE);
    pub const H4: Tile = Self::new(File::H, Rank::FOUR);
    pub const H5: Tile = Self::new(File::H, Rank::FIVE);
    pub const H6: Tile = Self::new(File::H, Rank::SIX);
    pub const H7: Tile = Self::new(File::H, Rank::SEVEN);
    pub const H8: Tile = Self::new(File::H, Rank::EIGHT);

    pub const MIN: u8 = 0;
    pub const MAX: u8 = 63;

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

    pub const fn new(file: File, rank: Rank) -> Self {
        // least-significant file mapping
        // Self(file.0 + rank.0 * 8)
        Self(file.0 ^ rank.0 << 3)
    }

    pub const fn from_index(index: usize) -> Result<Self, ChessError> {
        Self::from_int(index as u8)
    }

    pub const fn from_index_unchecked(index: usize) -> Self {
        Self(index as u8)
    }

    pub const fn from_int(int: u8) -> Result<Self, ChessError> {
        if int > Self::MAX {
            return Err(ChessError::OutOfBounds {
                val: int as usize,
                min: Self::MIN as usize,
                max: Self::MAX as usize,
            });
        }
        Ok(Self(int))
    }

    pub const fn inner(&self) -> u8 {
        self.0
    }

    pub const fn file(&self) -> File {
        // File(self.0 % 8)
        File(self.0 & 7)
    }

    pub const fn rank(&self) -> Rank {
        // Rank(self.0 / 8)
        Rank(self.0 >> 3)
    }

    pub const fn index(&self) -> usize {
        self.0 as usize
    }

    pub const fn parts(&self) -> (File, Rank) {
        (self.file(), self.rank())
    }

    // pub fn diag_index(&self) -> usize {
    //     (self.rank().index() - self.file().index()) & 15
    // }

    // pub fn anti_diag_index(&self) -> usize {
    //     (self.rank().index() + self.file().index()) ^ 7
    // }

    pub const fn is_light(&self) -> bool {
        (self.file().0 + self.rank().0) % 2 != 0
    }

    pub const fn is_dark(&self) -> bool {
        !self.is_light()
    }

    pub fn from_uci(tile: &str) -> Result<Self, ChessError> {
        let mut chars = tile.chars();

        // If there aren't two chars available, this is an invalid string.
        let (Some(file), Some(rank)) = (chars.next(), chars.next()) else {
            return Err(ChessError::InvalidTileNotation);
        };

        Ok(Self::new(File::from_char(file)?, Rank::from_char(rank)?))
    }

    fn to_uci(self) -> String {
        format!("{}{}", self.file(), self.rank())
    }

    pub const fn north(&self) -> Option<Self> {
        if let Some(next) = self.rank().increase() {
            Some(Self::new(self.file(), next))
        } else {
            None
        }
    }

    pub const fn south(&self) -> Option<Self> {
        if let Some(next) = self.rank().decrease() {
            Some(Self::new(self.file(), next))
        } else {
            None
        }
    }

    pub const fn west(&self) -> Option<Self> {
        if let Some(next) = self.file().decrease() {
            Some(Self::new(next, self.rank()))
        } else {
            None
        }
    }

    pub const fn east(&self) -> Option<Self> {
        if let Some(next) = self.file().increase() {
            Some(Self::new(next, self.rank()))
        } else {
            None
        }
    }

    pub const fn forward(&self, color: Color) -> Option<Self> {
        match color {
            Color::White => self.north(),
            Color::Black => self.south(),
        }
    }

    pub const fn backward(&self, color: Color) -> Option<Self> {
        match color {
            Color::White => self.south(),
            Color::Black => self.north(),
        }
    }

    pub const fn left(&self, color: Color) -> Option<Self> {
        match color {
            Color::White => self.west(),
            Color::Black => self.east(),
        }
    }

    pub const fn right(&self, color: Color) -> Option<Self> {
        match color {
            Color::White => self.east(),
            Color::Black => self.west(),
        }
    }
}

impl FromStr for Tile {
    type Err = ChessError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_uci(s)
    }
}

impl TryFrom<&str> for Tile {
    type Error = ChessError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_uci(value)
    }
}

impl TryFrom<String> for Tile {
    type Error = ChessError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::from_uci(&value)
    }
}

impl TryFrom<usize> for Tile {
    type Error = ChessError;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Self::from_index(value)
    }
}

impl TryFrom<u8> for Tile {
    type Error = ChessError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::from_int(value)
    }
}

impl TryFrom<i32> for Tile {
    type Error = ChessError;
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

impl<T> Index<Tile> for [T; 64] {
    type Output = T;
    fn index(&self, index: Tile) -> &Self::Output {
        &self[index.index()]
    }
}

impl<T> IndexMut<Tile> for [T; 64] {
    fn index_mut(&mut self, index: Tile) -> &mut Self::Output {
        &mut self[index.index()]
    }
}

impl<T> Index<Tile> for [[T; 8]; 8] {
    type Output = T;
    fn index(&self, index: Tile) -> &Self::Output {
        &self[index.file()][index.rank()]
    }
}

impl<T> IndexMut<Tile> for [[T; 8]; 8] {
    fn index_mut(&mut self, index: Tile) -> &mut Self::Output {
        &mut self[index.file()][index.rank()]
    }
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
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

    pub const fn new(rank: u8) -> Result<Self, ChessError> {
        if rank > Self::MAX {
            return Err(ChessError::OutOfBounds {
                val: rank as usize,
                min: Self::MIN as usize,
                max: Self::MAX as usize,
            });
        }

        Ok(Self(rank))
    }

    pub const fn new_unchecked(rank: u8) -> Self {
        Self(rank)
    }

    pub const fn from_char(rank: char) -> Result<Self, ChessError> {
        debug_assert!(rank.is_ascii(), "Rank chars must be ASCII!");

        let Some(rank_int) = rank.to_digit(10) else {
            return Err(ChessError::InvalidRankChar { val: rank });
        };

        let Some(rank) = rank_int.checked_sub(1) else {
            return Err(ChessError::InvalidRankChar { val: rank });
        };

        Self::new(rank as u8)
    }

    pub const fn from_tile_index(index: usize) -> Result<Self, ChessError> {
        debug_assert!(index <= 64, "Rank indices must be [0,64)");
        Self::new(index as u8 % 8)
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

    // Index in Little Endian (default)
    pub const fn index_le(&self) -> usize {
        self.0 as usize
    }

    // Index in Big Endian
    pub const fn index_be(&self) -> usize {
        self.index_le() ^ 56
    }

    pub const fn increase(&self) -> Option<Self> {
        if self.0 == 7 {
            None
        } else {
            Some(Self(self.0 + 1))
        }
    }

    pub const fn decrease(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(Self(self.0 - 1))
        }
    }

    pub const fn wrapping_increase(&self) -> Self {
        Self(self.0.wrapping_add(1))
    }

    pub const fn wrapping_decrease(&self) -> Self {
        Self(self.0.wrapping_sub(1))
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

impl TryFrom<char> for Rank {
    type Error = ChessError;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        Self::from_char(value)
    }
}

impl TryFrom<usize> for Rank {
    type Error = ChessError;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Self::from_tile_index(value)
    }
}

impl TryFrom<u8> for Rank {
    type Error = ChessError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl TryFrom<i32> for Rank {
    type Error = ChessError;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        Self::new(value as u8)
    }
}

impl From<Tile> for Rank {
    fn from(value: Tile) -> Self {
        value.rank()
    }
}

// macro_rules! impl_binary_op {
//     ($trait:tt, $type:ty, $op_name:tt, $op:tt) => {
//         impl $trait for $type {
//             type Output = Self;
//             fn $op_name(self, rhs: Self) -> Self::Output {
//                 Self::new(self.0 $op rhs.0).expect("Attempted binary operation {$op} on {$type} went out of bounds")
//             }
//         }
//     };
// }

// macro_rules! impl_binary_op_assign {
//     ($trait:tt, $type:ty, $op_name:tt, $op:tt) => {
//         impl $trait for $type {
//             fn $op_name(&mut self, rhs: Self) {
//                 *self = *self $op rhs;
//             }
//         }
//     };
// }

// impl_binary_op!(Add, Rank, add, +);
// impl_binary_op!(Sub, Rank, sub, -);
// impl_binary_op_assign!(AddAssign, Rank, add_assign, -);
// impl_binary_op_assign!(SubAssign, Rank, sub_assign, -);

impl Add for Rank {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.0 + rhs.0)
            .expect("Attempted to add {self} and {rhs}, which is beyond Rank's bounds")
    }
}

impl Sub for Rank {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.0 - rhs.0)
            .expect("Attempted to sub {self} and {rhs}, which is beyond Rank's bounds")
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

impl Add<u8> for Rank {
    type Output = Self;
    fn add(self, rhs: u8) -> Self::Output {
        Self::new(self.0 + rhs)
            .expect("Attempted to add {self} and {rhs}, which is beyond Rank's bounds")
    }
}

impl Sub<u8> for Rank {
    type Output = Self;
    fn sub(self, rhs: u8) -> Self::Output {
        Self::new(self.0 - rhs)
            .expect("Attempted to sub {self} and {rhs}, which is beyond Rank's bounds")
    }
}

impl AddAssign<u8> for Rank {
    fn add_assign(&mut self, rhs: u8) {
        *self = *self + rhs;
    }
}

impl SubAssign<u8> for Rank {
    fn sub_assign(&mut self, rhs: u8) {
        *self = *self - rhs;
    }
}

impl Add<i32> for Rank {
    type Output = Self;
    fn add(self, rhs: i32) -> Self::Output {
        Self::new(self.0 + rhs as u8)
            .expect("Attempted to add {self} and {rhs}, which is beyond Rank's bounds")
    }
}

impl Sub<i32> for Rank {
    type Output = Self;
    fn sub(self, rhs: i32) -> Self::Output {
        Self::new(self.0 - rhs as u8)
            .expect("Attempted to sub {self} and {rhs}, which is beyond Rank's bounds")
    }
}

impl AddAssign<i32> for Rank {
    fn add_assign(&mut self, rhs: i32) {
        *self = *self + rhs;
    }
}

impl SubAssign<i32> for Rank {
    fn sub_assign(&mut self, rhs: i32) {
        *self = *self - rhs;
    }
}

impl Add<usize> for Rank {
    type Output = Self;
    fn add(self, rhs: usize) -> Self::Output {
        Self::new(self.0 + rhs as u8)
            .expect("Attempted to add {self} and {rhs}, which is beyond Rank's bounds")
    }
}

impl Sub<usize> for Rank {
    type Output = Self;
    fn sub(self, rhs: usize) -> Self::Output {
        Self::new(self.0 - rhs as u8)
            .expect("Attempted to sub {self} and {rhs}, which is beyond Rank's bounds")
    }
}

impl AddAssign<usize> for Rank {
    fn add_assign(&mut self, rhs: usize) {
        *self = *self + rhs;
    }
}

impl SubAssign<usize> for Rank {
    fn sub_assign(&mut self, rhs: usize) {
        *self = *self - rhs;
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

impl<T> Index<Rank> for [T; 8] {
    type Output = T;
    fn index(&self, index: Rank) -> &Self::Output {
        &self[index.0 as usize]
    }
}

impl<T> IndexMut<Rank> for [T; 8] {
    fn index_mut(&mut self, index: Rank) -> &mut Self::Output {
        &mut self[index.0 as usize]
    }
}

impl fmt::Display for Rank {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
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

    pub const fn new(file: u8) -> Result<Self, ChessError> {
        if file > Self::MAX {
            return Err(ChessError::OutOfBounds {
                val: file as usize,
                min: Self::MIN as usize,
                max: Self::MAX as usize,
            });
        }
        Ok(Self(file))
    }

    pub const fn from_char(file: char) -> Result<Self, ChessError> {
        debug_assert!(file.is_ascii(), "File chars must be ASCII!");

        // Subtract the ASCII value for `a` (or `A`) to zero the number
        let file_int = file as u8 - if file.is_ascii_lowercase() { 'a' } else { 'A' } as u8;

        if file_int > Self::MAX {
            return Err(ChessError::InvalidFileChar { val: file });
        }

        Self::new(file_int)
    }

    pub const fn from_tile_index(index: usize) -> Result<Self, ChessError> {
        debug_assert!(index <= 64, "File indices must be [0,64)");
        Self::new(index as u8 / 8)
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

    // Index in Little Endian (default)
    pub const fn index_le(&self) -> usize {
        self.0 as usize
    }

    // Index in Big Endian
    pub const fn index_be(&self) -> usize {
        self.index_le() ^ 7
    }

    pub const fn increase(&self) -> Option<Self> {
        if self.0 == 7 {
            None
        } else {
            Some(Self(self.0 + 1))
        }
    }

    pub const fn decrease(&self) -> Option<Self> {
        if self.0 == 0 {
            None
        } else {
            Some(Self(self.0 - 1))
        }
    }

    pub const fn wrapping_increase(&self) -> Self {
        Self(self.0.wrapping_add(1))
    }

    pub const fn wrapping_decrease(&self) -> Self {
        Self(self.0.wrapping_sub(1))
    }
}

impl TryFrom<char> for File {
    type Error = ChessError;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        Self::from_char(value)
    }
}

impl TryFrom<usize> for File {
    type Error = ChessError;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Self::from_tile_index(value)
    }
}

impl TryFrom<u8> for File {
    type Error = ChessError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl TryFrom<i32> for File {
    type Error = ChessError;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        Self::new(value as u8)
    }
}

impl From<Tile> for File {
    fn from(value: Tile) -> Self {
        value.file()
    }
}

impl Add for File {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.0 + rhs.0)
            .expect("Attempted to add {self} and {rhs}, which is beyond File's bounds")
    }
}

impl Sub for File {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.0 - rhs.0)
            .expect("Attempted to sub {self} and {rhs}, which is beyond File's bounds")
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

impl Add<u8> for File {
    type Output = Self;
    fn add(self, rhs: u8) -> Self::Output {
        Self::new(self.0 + rhs)
            .expect("Attempted to add {self} and {rhs}, which is beyond File's bounds")
    }
}

impl Sub<u8> for File {
    type Output = Self;
    fn sub(self, rhs: u8) -> Self::Output {
        Self::new(self.0 - rhs)
            .expect("Attempted to sub {self} and {rhs}, which is beyond File's bounds")
    }
}

impl AddAssign<u8> for File {
    fn add_assign(&mut self, rhs: u8) {
        *self = *self + rhs;
    }
}

impl SubAssign<u8> for File {
    fn sub_assign(&mut self, rhs: u8) {
        *self = *self - rhs;
    }
}

impl Add<usize> for File {
    type Output = Self;
    fn add(self, rhs: usize) -> Self::Output {
        Self::new(self.0 + rhs as u8)
            .expect("Attempted to add {self} and {rhs}, which is beyond File's bounds")
    }
}

impl Sub<usize> for File {
    type Output = Self;
    fn sub(self, rhs: usize) -> Self::Output {
        Self::new(self.0 - rhs as u8)
            .expect("Attempted to sub {self} and {rhs}, which is beyond File's bounds")
    }
}

impl AddAssign<usize> for File {
    fn add_assign(&mut self, rhs: usize) {
        *self = *self + rhs;
    }
}

impl SubAssign<usize> for File {
    fn sub_assign(&mut self, rhs: usize) {
        *self = *self - rhs;
    }
}

impl Add<i32> for File {
    type Output = Self;
    fn add(self, rhs: i32) -> Self::Output {
        Self::new(self.0 + rhs as u8)
            .expect("Attempted to add {self} and {rhs}, which is beyond File's bounds")
    }
}

impl Sub<i32> for File {
    type Output = Self;
    fn sub(self, rhs: i32) -> Self::Output {
        Self::new(self.0 - rhs as u8)
            .expect("Attempted to sub {self} and {rhs}, which is beyond File's bounds")
    }
}

impl AddAssign<i32> for File {
    fn add_assign(&mut self, rhs: i32) {
        *self = *self + rhs;
    }
}

impl SubAssign<i32> for File {
    fn sub_assign(&mut self, rhs: i32) {
        *self = *self - rhs;
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

impl<T> Index<File> for [T; 8] {
    type Output = T;
    fn index(&self, index: File) -> &Self::Output {
        &self[index.0 as usize]
    }
}

impl<T> IndexMut<File> for [T; 8] {
    fn index_mut(&mut self, index: File) -> &mut Self::Output {
        &mut self[index.0 as usize]
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.char())
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
