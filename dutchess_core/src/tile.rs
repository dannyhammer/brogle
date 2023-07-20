use std::{
    fmt,
    ops::{Add, AddAssign, Index, IndexMut, Sub, SubAssign},
    str::FromStr,
};

use crate::Color;

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

    pub fn iter() -> impl Iterator<Item = Self> {
        // File::iter()
        //     .map(|file| Rank::iter().map(move |rank| Self::new(file, rank)))
        //     .flatten()
        (0..64).into_iter().map(|bits| Self(bits))
    }

    pub const fn new(file: File, rank: Rank) -> Self {
        // least-significant file mapping
        // Self(file.0 + rank.0 * 8)
        Self(file.0 ^ rank.0 << 3)
    }

    pub fn from_index(index: usize) -> Result<Self, String> {
        if index >= 64 {
            return Err(format!("Valid positions are indices [0,63)"));
        }
        Ok(Self(index as u8))
    }

    pub fn from_index_unchecked(index: usize) -> Self {
        Self(index as u8)
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

    pub fn from_uci(tile: &str) -> Result<Self, String> {
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
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_uci(s)
    }
}

impl TryFrom<usize> for Tile {
    type Error = String;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Self::from_index(value)
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

    pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
        // (Self::min().0..=Self::max().0).map(|i| Self(i))
        (0..8).map(|i| Self(i))
    }

    pub fn new(rank: u8) -> Result<Self, String> {
        if rank > Self::EIGHT.0 {
            return Err(format!(
                "Invalid rank value {rank}. Ranks must be between [0,8)",
            ));
        }
        Ok(Self(rank))
    }

    pub const fn new_unchecked(rank: u8) -> Self {
        Self(rank)
    }

    fn from_char(rank: char) -> Result<Self, String> {
        debug_assert!(
            rank.is_ascii() && rank.is_numeric(),
            "Rank chars must be ASCII!"
        );
        Self::new(rank.to_digit(10).ok_or("Ranks must be a valid number")? as u8 - 1)
    }

    pub fn from_index(index: usize) -> Result<Self, String> {
        debug_assert!(index <= 64, "Rank indices must be [0,64)");
        Self::new(index as u8 % 8)
    }

    pub const fn inner(&self) -> u8 {
        self.0
    }

    pub const fn index(&self) -> usize {
        self.index_le()
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

impl Add<u8> for Rank {
    type Output = Self;
    fn add(self, rhs: u8) -> Self::Output {
        Self::new(self.0 + rhs).expect("Attempted to add beyond Rank's bounds")
    }
}

impl Sub<u8> for Rank {
    type Output = Self;
    fn sub(self, rhs: u8) -> Self::Output {
        Self::new(self.0 - rhs).expect("Attempted to subtract beyond Rank's bounds")
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

impl Add<File> for Rank {
    type Output = Tile;
    fn add(self, file: File) -> Self::Output {
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
        write!(f, "{}", self.0 + 1)
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

    pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
        (0..8).map(|i| Self(i))
    }

    pub fn new(file: u8) -> Result<Self, String> {
        if file > 7 {
            return Err(format!(
                "Invalid file value {file}. Ranks must be between [0,8)",
            ));
        }
        Ok(Self(file))
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

    pub const fn inner(&self) -> u8 {
        self.0
    }

    pub const fn index(&self) -> usize {
        self.index_le()
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

impl Add<u8> for File {
    type Output = Self;
    fn add(self, rhs: u8) -> Self::Output {
        Self::new(self.0 + rhs).expect("Attempted to add beyond File's bounds")
    }
}

impl Sub<u8> for File {
    type Output = Self;
    fn sub(self, rhs: u8) -> Self::Output {
        Self::new(self.0 - rhs).expect("Attempted to subtract beyond File's bounds")
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

impl Add<Rank> for File {
    type Output = Tile;
    fn add(self, rank: Rank) -> Self::Output {
        rank + self
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
        write!(f, "{}", (self.0 + 'a' as u8) as char)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_positions() {
        // for rank in 0..8 {
        //     let rank = Rank(rank);
        //     for file in 0..8 {
        //         let file = File(file);
        //     }
        // }

        let a1 = Tile::new(File(0), Rank(0));
        assert_eq!(a1.to_string(), String::from("a1"));

        let h1 = Tile::new(File(7), Rank(0));
        assert_eq!(h1.to_string(), String::from("h1"));

        let a8 = Tile::new(File(0), Rank(7));
        assert_eq!(a8.to_string(), String::from("a8"));

        let h8 = Tile::new(File(7), Rank(7));
        assert_eq!(h8.to_string(), String::from("h8"));
    }
}
