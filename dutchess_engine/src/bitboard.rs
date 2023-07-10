use std::{fmt, ops::Index};

use derive_more::{
    BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, ShlAssign, Shr,
    ShrAssign,
};

use crate::{Color, File, Piece, PieceKind, Rank, Tile};

use crate::utils::*;

#[derive(Clone, PartialEq, Eq, Debug, Hash, Default)]
pub struct ChessBoard {
    /// All tiles occupied by a piece
    occupied: BitBoard,
    /// All unoccupied tiles
    empty: BitBoard,

    /// All tiles occupied by a specific color
    white: BitBoard,
    black: BitBoard,

    /// All tiles occupied by a specific piece kind
    pawn: BitBoard,
    knight: BitBoard,
    bishop: BitBoard,
    rook: BitBoard,
    queen: BitBoard,
    king: BitBoard,
    /*
    /// All tiles attacked by a specific color
    white_attack: BitBoard,
    black_attack: BitBoard,
     */
    piece: BitBoard,
}

impl ChessBoard {
    pub fn get(&self, tile: Tile) -> Option<Piece> {
        let color = self.color_at(tile)?;
        let kind = self.kind_at(tile)?;
        Some(Piece::new(color, kind))
    }

    pub fn set(&mut self, tile: Tile, piece: Piece) {
        let index = tile.index();
        match piece.color() {
            Color::White => self.white.set_index(index),
            Color::Black => self.black.set_index(index),
        }
        // self[piece.color()].set_index(index);

        match piece.kind() {
            PieceKind::Pawn => self.pawn.set_index(index),
            PieceKind::Knight => self.knight.set_index(index),
            PieceKind::Bishop => self.bishop.set_index(index),
            PieceKind::Rook => self.rook.set_index(index),
            PieceKind::Queen => self.queen.set_index(index),
            PieceKind::King => self.king.set_index(index),
        }
        // self[piece.kind()].set_index(index);

        self.occupied.set_index(index);
    }

    pub fn clear(&mut self, tile: Tile) {
        self.white.clear(tile);
        self.black.clear(tile);
        self.pawn.clear(tile);
        self.knight.clear(tile);
        self.bishop.clear(tile);
        self.rook.clear(tile);
        self.queen.clear(tile);
        self.king.clear(tile);
    }

    fn color_at(&self, tile: Tile) -> Option<Color> {
        if !self.occupied.get(tile) {
            return None;
        }

        if self.white.get(tile) {
            Some(Color::White)
        } else {
            Some(Color::Black)
        }
    }

    fn kind_at(&self, tile: Tile) -> Option<PieceKind> {
        if !self.occupied.get(tile) {
            return None;
        }

        if self.pawn.get(tile) {
            Some(PieceKind::Pawn)
        } else if self.knight.get(tile) {
            Some(PieceKind::Knight)
        } else if self.bishop.get(tile) {
            Some(PieceKind::Bishop)
        } else if self.rook.get(tile) {
            Some(PieceKind::Rook)
        } else if self.queen.get(tile) {
            Some(PieceKind::Queen)
        } else {
            Some(PieceKind::King)
        }
    }

    fn get_piece_set(&self, piece: &Piece) -> BitBoard {
        let color = self[piece.color()];
        let kind = self[piece.kind()];
        color & kind
    }

    /// Get a bitboard of all pieces that can be captured by `attacker`
    fn possible_captures(&self, attacker: &Piece) -> BitBoard {
        let attacker_bits = self.get_piece_set(attacker);
        let opponent = self[attacker.color().opponent()];

        attacker_bits & opponent
    }

    pub fn piece(&self, piece: &Piece) -> BitBoard {
        let color = self[piece.color()];
        let kind = self[piece.kind()];
        color & kind
    }

    // fn color(&self, color: Color) -> BitBoard {
    //     self[color]
    // }

    // fn kind(&self, kind: PieceKind) -> BitBoard {
    //     self[kind]
    // }

    // pub fn iter<'a>(&'a self) -> BoardIter<'a> {
    pub fn iter(&self) -> BoardIter<'_> {
        BoardIter {
            board: &self,
            occupancy: self.occupied,
        }
    }
}

impl From<[Option<Piece>; 64]> for ChessBoard {
    fn from(value: [Option<Piece>; 64]) -> Self {
        let mut boards = Self::default();

        for (i, piece) in value.into_iter().enumerate() {
            if let Some(piece) = piece {
                boards.set(Tile::from_index(i).unwrap(), piece)
            } else {
                boards.empty.set_index(i);
            }
        }

        boards
    }
}

impl Index<PieceKind> for ChessBoard {
    type Output = BitBoard;
    fn index(&self, index: PieceKind) -> &Self::Output {
        match index {
            PieceKind::Pawn => &self.pawn,
            PieceKind::Knight => &self.knight,
            PieceKind::Bishop => &self.bishop,
            PieceKind::Rook => &self.rook,
            PieceKind::Queen => &self.queen,
            PieceKind::King => &self.king,
        }
    }
}

impl Index<Color> for ChessBoard {
    type Output = BitBoard;
    fn index(&self, index: Color) -> &Self::Output {
        match index {
            Color::White => &self.white,
            Color::Black => &self.black,
        }
    }
}

/*
impl Index<Piece> for BitBoards {
    type Output = BitBoard;
    fn index(&self, index: Piece) -> &Self::Output {
        let color = self[index.color()];
        let kind = self[index.kind()];

        unsafe {
            // let ptr = self as *const Self;
            // let ptr = ptr as *mut Self;
            // let ptr = &mut *ptr;
            // ptr.piece = color & kind;

            (&mut *((self as *const Self) as *mut Self)).piece = color & kind;
        }

        &self.piece
    }
}
 */

pub struct BoardIter<'a> {
    board: &'a ChessBoard,
    occupancy: BitBoard,
}
impl<'a> Iterator for BoardIter<'a> {
    type Item = Piece;

    fn next(&mut self) -> Option<Self::Item> {
        let lsb = self.occupancy.pop_lsb()?;
        self.board.get(Tile(lsb))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.occupancy.population() as usize;
        (size, Some(size))
    }
}

impl<'a> ExactSizeIterator for BoardIter<'a> {}

/*
impl<'a> IntoIterator for BitBoards {
    type Item = Option<&'a Piece>;
    type IntoIter = BoardIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        BoardIter {
            board: &self,
            current: None,
        }
    }
}
 */

// impl Index<Piece> for BitBoards {
//     type Output = BitBoard;
//     fn index(&self, index: Piece) -> &Self::Output {
//         let color = self[index.color()];
//         let kind = self[index.kind()];
//         color & kind
//     }
// }

/*
fn set_bit(bits: u8, n: u8) -> u8 {
    bits | (1 << n)
}

fn clear_bit(bits: u8, n: u8) -> u8 {
    bits & !(1 << n)
}

fn flip_bit(bits: u8, n: u8) -> u8 {
    bits ^ (1 << n)
}

fn check_bit(bits: u8, n: u8) -> u8 {
    bits & (1 << n)
}
 */

/// A [`BitBoard`] represents the game board as a set of bits.
/// They are used for various computations, such as fetching valid moves or computing move costs.
///
/// The internal representation is a 64-bit binary number, so the values will represent the entire board.
/// They are color-agnostic, with the low order bits representing the "lower" half of the board.
///
/// LERF - Little-Endian Rank-File Mapping
/// https://www.chessprogramming.org/Square_Mapping_Considerations#Little-Endian_Rank-File_Mapping
#[derive(
    Clone,
    Copy,
    PartialEq,
    Eq,
    Debug,
    Hash,
    Default,
    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitXor,
    BitXorAssign,
    Not,
    Shl,
    ShlAssign,
    Shr,
    ShrAssign,
)]
#[repr(transparent)]
pub struct BitBoard(u64);

impl BitBoard {
    pub const FILE_A: Self = Self(0x0101010101010101);
    pub const FILE_H: Self = Self(0x8080808080808080);
    pub const NOT_FILE_A: Self = Self(0xfefefefefefefefe);
    pub const NOT_FILE_H: Self = Self(0x7f7f7f7f7f7f7f7f);
    pub const RANK_1: Self = Self(0x00000000000000FF);
    pub const RANK_8: Self = Self(0xFF00000000000000);
    pub const A1_H8_DIAG: Self = Self(0x8040201008040201);
    pub const H1_A8_DIAG: Self = Self(0x0102040810204080);
    pub const LIGHT_SQUARES: Self = Self(0x55AA55AA55AA55AA);
    pub const DARKS_SQUARES: Self = Self(0xAA55AA55AA55AA55);
    pub const EMPTY_BOARD: Self = Self(0x0000000000000000);
    pub const FULL_BOARD: Self = Self(0xFFFFFFFFFFFFFFFF);

    pub fn new(bits: u64) -> Self {
        Self(bits)
    }

    pub fn from_index(index: usize) -> Result<Self, String> {
        if index < 64 {
            Ok(Self(1 << index))
        } else {
            Err(format!("Index must be between [0,64)"))
        }
    }

    fn from_color<'a>(pieces: impl IntoIterator<Item = &'a Option<Piece>>, color: Color) -> Self {
        Self::from_eq(pieces, |piece| piece.color() == color)
    }

    fn from_kind<'a>(pieces: impl IntoIterator<Item = &'a Option<Piece>>, kind: PieceKind) -> Self {
        Self::from_eq(pieces, |piece| piece.kind() == kind)
    }

    fn from_eq<'a>(
        pieces: impl IntoIterator<Item = &'a Option<Piece>>,
        f: impl FnOnce(&Piece) -> bool + Copy,
    ) -> Self {
        let mut bits = 0;

        for (i, piece) in pieces.into_iter().enumerate() {
            if let Some(piece) = piece {
                if f(&piece) {
                    bits |= 1 << i;
                }
            }
        }

        Self(bits)
    }

    fn from_file(file: File) -> Self {
        // Self(FILE_A << file.0)
        Self::FILE_A << file.0
    }

    fn from_rank(rank: Rank) -> Self {
        // Self(RANK_1 << rank.0 * 8)
        Self::RANK_1 << rank.0 * 8
    }

    // fn diagonal(tile: Tile) -> Self {
    //     let diag_index = tile.rank().index();
    //     // let diag = A1_H8_DIAG >> diag_index;
    //     // let diag = A1_H8_DIAG >> diag_index;

    //     BitBoard::from(tile) | BitBoard(diag)
    // }

    // fn anti_diagonal(tile: Tile) -> Self {
    //     Self(0)
    // }

    fn is_empty(&self) -> bool {
        // self.0 == EMPTY_BOARD
        *self == Self::EMPTY_BOARD
    }

    pub fn set(&mut self, tile: Tile) {
        // self.0 |= 1 << tile.index();
        self.set_index(tile.index())
    }

    pub fn get(&self, tile: Tile) -> bool {
        // (self.0 & 1 << tile.index()) != 0
        self.get_index(tile.index())
    }

    pub fn clear(&mut self, tile: Tile) {
        // self.0 ^= !(1 << tile.index());
        self.clear_index(tile.index())
    }

    /// Remove all tiles from `other` in `self`
    pub fn remove(&mut self, other: &Self) {
        self.0 &= !other.0
    }

    pub fn lsb(&self) -> Option<u8> {
        (!self.is_empty()).then_some(self.0.trailing_zeros() as u8)
    }

    pub fn pop_lsb(&mut self) -> Option<u8> {
        let lsb = self.lsb();
        self.clear_lsb();
        lsb
    }

    /// Clears the lowest non-zero bit from `self`, if there is a square to clear.
    pub fn clear_lsb(&mut self) {
        self.0 &= self.0.wrapping_sub(1);
    }

    fn lsb_tile(&self) -> Option<Tile> {
        self.lsb().map(|lsb| Tile(lsb))
    }

    fn toggle(&mut self, mask: Self) {
        *self ^= mask
    }

    fn set_index(&mut self, index: usize) {
        debug_assert!(index < 64, "Index must be between [0,64)");
        self.0 |= 1 << index;
    }

    fn get_index(&self, index: usize) -> bool {
        debug_assert!(index < 64, "Index must be between [0,64)");
        (self.0 & 1 << index) != 0
    }

    fn clear_index(&mut self, index: usize) {
        debug_assert!(index < 64, "Index must be between [0,64)");
        self.0 ^= !(1 << index);
    }

    /// Returns `true` if `other` is a subset of `self`.
    fn contains_all(&self, other: &Self) -> bool {
        self.0 & other.0 == other.0
    }

    /// Returns `true` if `self` contains any bits of `other`.
    fn contains_any(&self, other: &Self) -> bool {
        // self.0 & other.0 != EMPTY_BOARD
        *self & *other != Self::EMPTY_BOARD
    }

    // Rank up
    pub fn north(self) -> Self {
        Self(self.0 << 8)
    }

    // Rank down
    pub fn south(self) -> Self {
        Self(self.0 >> 8)
    }

    // File up
    pub fn west(self) -> Self {
        // Post-shift mask
        // Self((self.0 >> 1) & NOT_FILE_H)
        (self >> 1) & Self::NOT_FILE_H
    }

    // File down
    pub fn east(self) -> Self {
        // Post-shift mask
        // Self((self.0 << 1) & NOT_FILE_A)
        (self << 1) & Self::NOT_FILE_A
    }

    pub fn northeast(self) -> Self {
        // Post-shift mask
        // Self((self.0 << 9) & NOT_FILE_A)
        (self << 9) & Self::NOT_FILE_A
    }

    pub fn southeast(self) -> Self {
        // Post-shift mask
        // Self((self.0 >> 7) & NOT_FILE_A)
        (self >> 7) & Self::NOT_FILE_A
    }

    pub fn northwest(self) -> Self {
        // Post-shift mask
        // Self((self.0 << 7) & NOT_FILE_H)
        (self << 7) & Self::NOT_FILE_H
    }

    pub fn southwest(self) -> Self {
        // Post-shift mask
        // Self((self.0 >> 9) & NOT_FILE_H)
        (self >> 9) & Self::NOT_FILE_H
    }

    /// Get the value of the bit at `index`
    fn get_bit_at(&self, index: usize) -> bool {
        index <= 64 && self.0 & (1 << index) != 0
    }

    pub fn iter(&self) -> BitBoardIter {
        self.into_iter()
    }

    fn population(&self) -> u8 {
        self.0.count_ones() as u8
    }

    // fn flip_vertical(&self) -> Self {

    // }

    fn north_fill(&self) -> Self {
        let mut bits = self.0;
        bits |= bits << 8;
        bits |= bits << 16;
        bits |= bits << 32;
        Self(bits)
    }

    fn south_fill(&self) -> Self {
        let mut bits = self.0;
        bits |= bits >> 8;
        bits |= bits >> 16;
        bits |= bits >> 32;
        Self(bits)
    }
}

/*
impl<T> Index<Piece> for [T; 12] {
    type Output = T;

    fn index(&self, piece: Piece) -> &Self::Output {
        let idx = match piece.kind() {
            PieceKind::Pawn => 0,
            PieceKind::Knight => 1,
            PieceKind::Bishop => 2,
            PieceKind::Rook => 3,
            PieceKind::Queen => 4,
            PieceKind::King => 5,
        };
        let idx = if piece.is_white() { idx } else { idx + 6 };
        &self[idx]
    }
}
 */

impl From<Tile> for BitBoard {
    fn from(value: Tile) -> Self {
        Self(1 << value.index())
    }
}

impl fmt::Display for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // let bytes = self.0.to_ne_bytes();
        // let bytes = self.0.to_be_bytes();
        let bytes = self.0.to_le_bytes();
        write!(
            f,
            "{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}",
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        )
    }
}

// impl IntoIterator for BitBoard {
//     type Item = Position;
//     type IntoIter = ;
// }

/*
// Formatting
impl fmt::Binary for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:b}", self.0)
    }
}
impl fmt::UpperHex for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:X}", self.0)
    }
}
impl fmt::LowerHex for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x}", self.0)
    }
}
 */

pub struct BitBoardIter {
    bb: BitBoard,
}

impl Iterator for BitBoardIter {
    type Item = Tile;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.bb.lsb_tile()?;
        self.bb.clear_lsb();
        Some(next)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = 0;
        (size, Some(size))
    }
}

impl ExactSizeIterator for BitBoardIter {}

impl IntoIterator for BitBoard {
    type Item = Tile;
    type IntoIter = BitBoardIter;
    fn into_iter(self) -> Self::IntoIter {
        BitBoardIter { bb: self }
    }
}

impl IntoIterator for &BitBoard {
    type Item = Tile;
    type IntoIter = BitBoardIter;
    fn into_iter(self) -> Self::IntoIter {
        BitBoardIter { bb: *self }
    }
}

impl IntoIterator for &mut BitBoard {
    type Item = Tile;
    type IntoIter = BitBoardIter;
    fn into_iter(self) -> Self::IntoIter {
        BitBoardIter { bb: *self }
    }
}
