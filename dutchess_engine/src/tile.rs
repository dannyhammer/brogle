use std::{
    fmt,
    ops::{Add, AddAssign, Index, IndexMut, Sub, SubAssign},
};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
#[repr(transparent)]
pub struct Tile(pub(crate) u8);

impl Tile {
    pub fn iter() -> impl Iterator<Item = Self> {
        File::iter()
            .map(|file| Rank::iter().map(move |rank| Self::new(file, rank)))
            .flatten()
    }

    pub const fn new(file: File, rank: Rank) -> Self {
        // least-significant file mapping
        Self(file.0 + rank.0 * 8)
    }

    pub fn from_index(index: usize) -> Result<Self, String> {
        if index >= 64 {
            return Err(format!("Valid positions are indices [0,63)"));
        }
        Ok(Self(index as u8))
    }

    pub const fn file(&self) -> File {
        File(self.0 % 8)
    }

    pub const fn rank(&self) -> Rank {
        Rank(self.0 / 8)
    }

    pub const fn index(&self) -> usize {
        self.0 as usize
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
    pub fn iter() -> impl Iterator<Item = Self> {
        // (Self::min().0..=Self::max().0).map(|i| Self(i))
        (0..8).map(|i| Self(i))
    }

    pub fn new(rank: u8) -> Result<Self, String> {
        if rank > 7 {
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
        debug_assert!(rank.is_ascii(), "Rank chars must be ASCII!");
        Self::new(rank.to_digit(10).ok_or("Ranks must be a valid number")? as u8)
    }

    pub fn from_index(index: usize) -> Result<Self, String> {
        debug_assert!(index <= 64, "Rank indices must be [0,64)");
        Self::new(index as u8 % 8)
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
    pub fn iter() -> impl Iterator<Item = Self> {
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
