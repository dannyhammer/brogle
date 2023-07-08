use std::{
    fmt,
    ops::{Add, AddAssign, Index, IndexMut, Sub, SubAssign},
};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
#[repr(transparent)]
pub struct Position(pub(crate) u8);

impl Position {
    pub fn iter() -> impl Iterator<Item = Self> {
        File::iter()
            .map(|file| Rank::iter().map(move |rank| Self::new(file, rank)))
            .flatten()
    }

    pub fn new(file: File, rank: Rank) -> Self {
        Self(file.0 + rank.0 * 8)
    }

    pub fn from_index(index: usize) -> Result<Self, String> {
        if index >= 64 {
            return Err(format!("Valid positions are indices [0,63)"));
        }
        Ok(Self(index as u8))
    }

    pub fn file(&self) -> File {
        File(self.0 / 8)
    }

    pub fn rank(&self) -> Rank {
        Rank(self.0 % 8)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }

    pub fn is_light(&self) -> bool {
        (self.file().0 + self.rank().0) % 2 == 0
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

impl TryFrom<usize> for Position {
    type Error = String;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Self::from_index(value)
    }
}

impl Add<Rank> for Position {
    type Output = Self;
    fn add(self, rhs: Rank) -> Self::Output {
        Self::new(self.file(), self.rank() + rhs)
    }
}

impl Add<File> for Position {
    type Output = Self;
    fn add(self, rhs: File) -> Self::Output {
        Self::new(self.file() + rhs, self.rank())
    }
}

impl Sub<Rank> for Position {
    type Output = Self;
    fn sub(self, rhs: Rank) -> Self::Output {
        Self::new(self.file(), self.rank() - rhs)
    }
}

impl Sub<File> for Position {
    type Output = Self;
    fn sub(self, rhs: File) -> Self::Output {
        Self::new(self.file() - rhs, self.rank())
    }
}

impl AddAssign<Rank> for Position {
    fn add_assign(&mut self, rhs: Rank) {
        *self = *self + rhs;
    }
}

impl SubAssign<Rank> for Position {
    fn sub_assign(&mut self, rhs: Rank) {
        *self = *self - rhs;
    }
}

impl AddAssign<File> for Position {
    fn add_assign(&mut self, rhs: File) {
        *self = *self + rhs;
    }
}

impl SubAssign<File> for Position {
    fn sub_assign(&mut self, rhs: File) {
        *self = *self - rhs;
    }
}

impl<T> Index<Position> for [T; 64] {
    type Output = T;
    fn index(&self, index: Position) -> &Self::Output {
        &self[index.index()]
    }
}

impl<T> IndexMut<Position> for [T; 64] {
    fn index_mut(&mut self, index: Position) -> &mut Self::Output {
        &mut self[index.index()]
    }
}

impl<T> Index<Position> for [[T; 8]; 8] {
    type Output = T;
    fn index(&self, index: Position) -> &Self::Output {
        &self[index.rank()][index.file()]
    }
}

impl<T> IndexMut<Position> for [[T; 8]; 8] {
    fn index_mut(&mut self, index: Position) -> &mut Self::Output {
        &mut self[index.rank()][index.file()]
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(transparent)]
pub struct Rank(pub(crate) u8);

impl Rank {
    pub fn iter() -> impl Iterator<Item = Self> {
        (Self::min().0..=Self::max().0).map(|i| Self(i))
    }

    pub fn new(rank: u8) -> Result<Self, String> {
        let rank = Self(rank);
        if rank > Self::max() || rank < Self::min() {
            return Err(format!(
                "Invalid rank value {rank}. Ranks must be between [{},{}]",
                Self::min(),
                Self::max()
            ));
        }
        Ok(rank)
    }

    fn from_char(rank: char) -> Result<Self, String> {
        debug_assert!(rank.is_ascii(), "Rank chars must be ASCII!");
        Self::new(rank.to_digit(10).ok_or("Ranks must be a valid number")? as u8)
    }

    pub fn from_index(index: usize) -> Result<Self, String> {
        debug_assert!(index <= 64, "Rank indices must be [0,64)");
        Self::new(index as u8 % 8)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }

    fn min() -> Self {
        Self(0)
    }

    fn max() -> Self {
        Self(7)
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
    type Output = Position;
    fn add(self, file: File) -> Self::Output {
        Position::new(file, self)
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
        write!(f, "{}", 8 - self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct File(pub(crate) u8);

impl File {
    pub fn iter() -> impl Iterator<Item = Self> {
        (Self::min().0..=Self::max().0).map(|i| Self(i))
    }

    pub fn new(file: u8) -> Result<Self, String> {
        let file = Self(file);
        if file > Self::max() || file < Self::min() {
            return Err(format!(
                "Invalid file value {file}. Files must be between [{},{}]",
                Self::min(),
                Self::max()
            ));
        }
        Ok(file)
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

    pub fn index(self) -> usize {
        self.0 as usize
    }

    fn min() -> Self {
        Self(0)
    }

    fn max() -> Self {
        Self(7)
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
    type Output = Position;
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
