use std::{
    fmt,
    ops::{Index, Shl, Shr},
};

use derive_more::{
    BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, ShlAssign, Shr,
    ShrAssign,
};

use crate::{ChessError, Color, File, Piece, PieceKind, Rank, Tile};

/// Represents a full chess board at any given state.
///
/// Internally uses a collection of [`BitBoards`] to keep track of piece locations, occupied/empty squares, and attack squares.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
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
    pub fn from_fen_placements(placements: &str) -> Result<Self, ChessError> {
        // Check if the placements string is the correct length
        if placements.matches('/').count() != 7 {
            return Err(ChessError::InvalidFenString);
        }

        let mut board = Self::default();

        // Have to reverse this so that white appears on the bottom
        for (rank, placements) in placements.split('/').rev().enumerate() {
            let mut file = 0;
            let rank = rank as u8;
            for piece_char in placements.chars() {
                // If the next char is a piece, we need to update the relevant BitBoards
                if let Ok(kind) = PieceKind::from_char(piece_char) {
                    // Firstly, create a tile and set the "Occupied" board at this location.
                    let tile = Tile::new(File::new_unchecked(file), Rank::new_unchecked(rank));
                    board.occupied.set(tile);

                    // Based on the color, we either set the "White" or "Black" BitBoards
                    if piece_char.is_ascii_uppercase() {
                        board.white.set(tile);
                    } else {
                        board.black.set(tile);
                    };

                    // Set the appropriate BitBoard based on the piece's kind
                    match kind {
                        PieceKind::Pawn => board.pawn.set(tile),
                        PieceKind::Knight => board.knight.set(tile),
                        PieceKind::Bishop => board.bishop.set(tile),
                        PieceKind::Rook => board.rook.set(tile),
                        PieceKind::Queen => board.queen.set(tile),
                        PieceKind::King => board.king.set(tile),
                    }

                    file += 1;
                } else {
                    // If the next char was not a piece, increment our File counter, checking for errors along the way
                    let Some(empty) = piece_char.to_digit(10) else {
                        return Err(ChessError::InvalidFenString);
                    };
                    file += empty as u8
                }
            }
        }

        // After all pieces have been set, we can compute the "Attacks" boards

        Ok(board)
    }

    /// Gets the [`Piece`] at a given [`Tile`], if there is one present.
    ///
    /// # Example
    ///
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

    pub const fn color(&self, color: Color) -> BitBoard {
        if color.is_white() {
            self.white
        } else {
            self.black
        }
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

    pub fn blockers(&self, blocker_mask: BitBoard) -> BitBoard {
        // All occupied squares within the blocker mask
        self.occupied & blocker_mask
    }

    // pub fn generate_moves(&self) {
    //     //
    // }

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
/// The internal encoding uses [Little-Endian Rank-File Mapping (LERF)](https://www.chessprogramming.org/Square_Mapping_Considerations#Little-Endian_Rank-File_Mapping),
/// so a bitboard of first Rank would look like this in binary:
/// ```text
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 11111111
/// ```
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
pub struct BitBoard(pub(crate) u64);

impl BitBoard {
    pub const FILE_A: Self = Self(0x0101010101010101);
    pub const FILE_B: Self = Self(0x0202020202020202);
    pub const FILE_C: Self = Self(0x0404040404040404);
    pub const FILE_D: Self = Self(0x0808080808080808);
    pub const FILE_E: Self = Self(0x1010101010101010);
    pub const FILE_F: Self = Self(0x2020202020202020);
    pub const FILE_G: Self = Self(0x4040404040404040);
    pub const FILE_H: Self = Self(0x8080808080808080);
    pub const NOT_FILE_A: Self = Self(0xfefefefefefefefe);
    pub const NOT_FILE_H: Self = Self(0x7f7f7f7f7f7f7f7f);
    pub const RANK_1: Self = Self(0x00000000000000FF);
    pub const RANK_2: Self = Self(0x000000000000FF00);
    pub const RANK_3: Self = Self(0x0000000000FF0000);
    pub const RANK_4: Self = Self(0x00000000FF000000);
    pub const RANK_5: Self = Self(0x000000FF00000000);
    pub const RANK_6: Self = Self(0x0000FF0000000000);
    pub const RANK_7: Self = Self(0x00FF000000000000);
    pub const RANK_8: Self = Self(0xFF00000000000000);
    pub const A1_H8_DIAG: Self = Self(0x8040201008040201);
    pub const H1_A8_DIAG: Self = Self(0x0102040810204080);
    pub const LIGHT_SQUARES: Self = Self(0x55AA55AA55AA55AA);
    pub const DARKS_SQUARES: Self = Self(0xAA55AA55AA55AA55);
    pub const EMPTY_BOARD: Self = Self(0x0000000000000000);
    pub const FULL_BOARD: Self = Self(0xFFFFFFFFFFFFFFFF);
    pub const EDGES: Self = Self(0xFF818181818181FF);
    pub const NOT_EDGES: Self = Self(0x007E7E7E7E7E7E00);
    pub const CORNERS: Self = Self(0x8100000000000081);

    /// Constructs a new [`BitBoard`] from the provided bit pattern.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// let board = BitBoard::new(255);
    /// assert_eq!(board.to_hex_string(), "0x00000000000000FF");
    /// ```
    pub const fn new(bits: u64) -> Self {
        Self(bits)
    }

    /// Constructs a new [`BitBoard`] from the provided index.
    ///
    /// The resulting [`BitBoard`] will have only a single bit toggled on.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// let board = BitBoard::from_index(63);
    /// assert_eq!(board.to_hex_string(), "0x8000000000000000");
    /// ```
    pub const fn from_index(index: usize) -> Self {
        debug_assert!(index < 64, "Index must be between [0,64)");
        Self(1 << index)
    }

    /// Constructs a new [`BitBoard`] from the provided [`Tile`].
    ///
    /// The resulting [`BitBoard`] will have only a single bit toggled on.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Tile};
    /// let board = BitBoard::from_tile(Tile::H8);
    /// assert_eq!(board.to_hex_string(), "0x8000000000000000");
    /// ```
    pub const fn from_tile(tile: Tile) -> Self {
        Self::from_index(tile.index())
    }

    /// Constructs a new [`BitBoard`] from the provided [`File`].
    ///
    /// The resulting [`BitBoard`] will have an entire column of bits toggled on.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, File};
    /// let board = BitBoard::from_file(File::F);
    /// assert_eq!(board.to_hex_string(), "0x2020202020202020");
    /// ```
    pub const fn from_file(file: File) -> Self {
        Self::new(Self::FILE_A.0 << file.0)
    }

    /// Constructs a new [`BitBoard`] from the provided [`Rank`].
    ///
    /// The resulting [`BitBoard`] will have an entire row of bits toggled on.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Rank};
    /// let board = BitBoard::from_rank(Rank::SEVEN);
    /// assert_eq!(board.to_hex_string(), "0x00FF000000000000");
    /// ```
    pub const fn from_rank(rank: Rank) -> Self {
        Self::new(Self::RANK_1.0 << rank.0 * 8)
    }

    /// Constructs a new [`BitBoard`] from the provided string.
    ///
    /// The string may be a binary or hexadecimal number, and may be proceeded with `0b` or `0x`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// let board1 = BitBoard::from_str("0x00FF000000000000");
    /// let board2 = BitBoard::from_str("00FF000000000000");
    /// let board3 = BitBoard::from_str("0000000011111111000000000000000000000000000000000000000000000000");
    /// let board4 = BitBoard::from_str("0b0000000011111111000000000000000000000000000000000000000000000000");
    /// assert_eq!(board1, board2);
    /// assert_eq!(board1, board3);
    /// assert_eq!(board1, board4);
    /// assert_eq!(board1.unwrap().to_hex_string(), "0x00FF000000000000");
    /// ```
    pub fn from_str(bits: &str) -> Result<Self, ChessError> {
        let bits = bits.to_lowercase();

        if bits.len() == 64 || bits.len() == 66 {
            let bits = bits.trim_start_matches("0b");
            let bits =
                u64::from_str_radix(bits, 2).map_err(|_| ChessError::InvalidBitBoardString)?;
            Ok(Self::new(bits))
        } else if bits.len() == 16 || bits.len() == 18 {
            let bits = bits.trim_start_matches("0x");
            let bits =
                u64::from_str_radix(bits, 16).map_err(|_| ChessError::InvalidBitBoardString)?;
            Ok(Self::new(bits))
        } else {
            Err(ChessError::InvalidBitBoardString)
        }
    }

    /// Creates a [`Tile`] from this [`BitBoard`] based on the lowest-index bit that is flipped.
    ///
    /// If this [`BitBoard`] contains more than a single flipped bit, it is converted into a [`Tile`]
    /// based on the index of the lowest bit that is flipped.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Tile};
    /// let board = BitBoard::from_index(14);
    /// assert_eq!(board.to_tile(), Tile::G2);
    /// ```
    pub const fn to_tile(&self) -> Tile {
        Tile(self.0.trailing_zeros() as u8)
    }

    /// Reverse this [`BitBoard`], viewing it from the opponent's perspective.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Rank};
    /// let board = BitBoard::from_rank(Rank::SEVEN);
    /// assert_eq!(board.to_hex_string(), "0x00FF000000000000");
    ///
    /// let flipped = board.flipped();
    /// assert_eq!(flipped.to_hex_string(), "0x000000000000FF00");
    /// ```
    pub const fn flipped(&self) -> Self {
        Self(self.0.swap_bytes())
    }

    /// Checks if this [`BitBoard`] is empty, or all zeros.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// let board = BitBoard::new(0x0);
    /// assert!(board.is_empty());
    /// ```
    pub const fn is_empty(&self) -> bool {
        self.0 == 0
    }

    /// Toggles the bit corresponding to the location of the provided [`Tile`] to `1` (on).
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Tile};
    /// let mut board = BitBoard::default();
    /// board.set(Tile::G2);
    /// assert_eq!(board.to_hex_string(), "0x0000000000004000");
    /// ```
    pub fn set(&mut self, tile: Tile) {
        // self.0 |= 1 << tile.index();
        self.set_index(tile.index());
        // *self = *self | Self::from_tile(tile);
    }

    /// Gets the value of the bit corresponding to the location of the provided [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Tile};
    /// let board = BitBoard::FILE_A;
    /// assert!(board.get(Tile::A3));
    /// ```
    pub const fn get(&self, tile: Tile) -> bool {
        // (self.0 & 1 << tile.index()) != 0
        self.get_index(tile.index())
    }

    /// Toggles the bit corresponding to the location of the provided [`Tile`] to `0` (off).
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Tile};
    /// let mut board = BitBoard::RANK_1;
    /// board.clear(Tile::C1);
    /// assert_eq!(board.to_hex_string(), "0x00000000000000FB");
    /// ```
    pub fn clear(&mut self, tile: Tile) {
        // self.0 ^= !(1 << tile.index());
        self.clear_index(tile.index())
    }

    /// Remove all tiles from `other` in `self`
    pub fn remove(&mut self, other: &Self) {
        self.0 &= !other.0
    }

    pub const fn lsb(&self) -> Option<u8> {
        if self.is_empty() {
            None
        } else {
            Some(self.0.trailing_zeros() as u8)
        }
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

    const fn lsb_tile(&self) -> Option<Tile> {
        // self.lsb().map(|lsb| Tile(lsb))
        if let Some(lsb) = self.lsb() {
            Some(Tile(lsb))
        } else {
            None
        }
    }

    pub fn toggle(&mut self, mask: Self) {
        *self ^= mask
    }

    pub fn toggle_tile(&mut self, tile: Tile) {
        self.toggle(Self::from_tile(tile))
    }

    pub fn toggle_index(&mut self, index: usize) {
        self.toggle(Self::from_index(index))
    }

    fn set_index(&mut self, index: usize) {
        debug_assert!(index < 64, "Index must be between [0,64)");
        // self.0 |= 1 << index;
        *self = *self | Self::from_index(index);
        // self.set(Tile::from_index_unchecked(index));
    }

    const fn get_index(&self, index: usize) -> bool {
        debug_assert!(index < 64, "Index must be between [0,64)");
        (self.0 & 1 << index) != 0
    }

    fn clear_index(&mut self, index: usize) {
        debug_assert!(index < 64, "Index must be between [0,64)");
        // self.0 ^= 1 << index;
        *self = *self ^ Self::from_index(index)
    }

    /// Returns `true` if `other` is a subset of `self`.
    pub const fn contains_all(&self, other: &Self) -> bool {
        self.0 & other.0 == other.0
    }

    /// Returns `true` if `self` contains any bits of `other`.
    pub const fn contains_any(&self, other: &Self) -> bool {
        self.0 & other.0 != Self::EMPTY_BOARD.0
        // *self & *other != Self::EMPTY_BOARD
    }

    // Rank up
    pub const fn north(self) -> Self {
        Self(self.0 << 8)
    }

    // Rank down
    pub const fn south(self) -> Self {
        Self(self.0 >> 8)
    }

    // File up
    pub const fn west(self) -> Self {
        // Post-shift mask
        Self((self.0 >> 1) & Self::NOT_FILE_H.0)
        // (self >> 1) & Self::NOT_FILE_H
    }

    // File down
    pub const fn east(self) -> Self {
        // Post-shift mask
        Self((self.0 << 1) & Self::NOT_FILE_A.0)
        // (self << 1) & Self::NOT_FILE_A
    }

    pub const fn northeast(self) -> Self {
        // Post-shift mask
        Self((self.0 << 9) & Self::NOT_FILE_A.0)
        // (self << 9) & Self::NOT_FILE_A
    }

    pub const fn southeast(self) -> Self {
        // Post-shift mask
        Self((self.0 >> 7) & Self::NOT_FILE_A.0)
        // (self >> 7) & Self::NOT_FILE_A
    }

    pub const fn northwest(self) -> Self {
        // Post-shift mask
        Self((self.0 << 7) & Self::NOT_FILE_H.0)
        // (self << 7) & Self::NOT_FILE_H
    }

    pub const fn southwest(self) -> Self {
        // Post-shift mask
        Self((self.0 >> 9) & Self::NOT_FILE_H.0)
        // (self >> 9) & Self::NOT_FILE_H
    }

    const fn north_fill(&self) -> Self {
        let mut bits = self.0;
        bits |= bits << 8;
        bits |= bits << 16;
        bits |= bits << 32;
        Self(bits)
    }

    const fn south_fill(&self) -> Self {
        let mut bits = self.0;
        bits |= bits >> 8;
        bits |= bits >> 16;
        bits |= bits >> 32;
        Self(bits)
    }

    pub fn iter(&self) -> BitBoardIter {
        self.into_iter()
    }

    /// Yields the total number of `1`s in this [`BitBoard`].
    ///
    /// In other words, this function determines how many bits are activated.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// let board = BitBoard::RANK_1;
    /// assert_eq!(board.population(), 8);
    /// ```
    pub const fn population(&self) -> u32 {
        self.0.count_ones()
    }

    pub fn to_hex_string(&self) -> String {
        format!("0x{:0>16X}", self.0)
    }
}

impl Shl<File> for BitBoard {
    type Output = Self;
    fn shl(self, rhs: File) -> Self::Output {
        Self::new(self.0 << rhs.0)
    }
}

impl Shr<File> for BitBoard {
    type Output = Self;
    fn shr(self, rhs: File) -> Self::Output {
        Self::new(self.0 >> rhs.0)
    }
}

impl Shl<Rank> for BitBoard {
    type Output = Self;
    fn shl(self, rhs: Rank) -> Self::Output {
        Self::new(self.0 << rhs.0 * 8)
    }
}

impl Shr<Rank> for BitBoard {
    type Output = Self;
    fn shr(self, rhs: Rank) -> Self::Output {
        Self::new(self.0 >> rhs.0 * 8)
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

impl Index<Tile> for BitBoard {
    type Output = bool;
    fn index(&self, index: Tile) -> &Self::Output {
        if self.get_index(index.index()) {
            &true
        } else {
            &false
        }
    }
}

impl From<Tile> for BitBoard {
    fn from(value: Tile) -> Self {
        Self::from_tile(value)
    }
}

impl From<File> for BitBoard {
    fn from(value: File) -> Self {
        Self::from_file(value)
    }
}

impl From<Rank> for BitBoard {
    fn from(value: Rank) -> Self {
        Self::from_rank(value)
    }
}

impl From<u64> for BitBoard {
    fn from(value: u64) -> Self {
        Self::new(value)
    }
}

impl fmt::UpperHex for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0X{:0>16X}", self.0)
    }
}

impl fmt::LowerHex for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0x{:0>16x}", self.0)
    }
}

impl fmt::Binary for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0b{:0>64b}", self.0)
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
        let size = self.bb.population() as usize;
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bitboard_to_string() {
        let board = BitBoard::A1_H8_DIAG;
        let expected =
            "00000001\n00000010\n00000100\n00001000\n00010000\n00100000\n01000000\n10000000";

        assert_eq!(board.to_string(), expected);
    }

    #[test]
    fn test_bitboard_masking() {
        let file_a = BitBoard::FILE_A;
        let full_board = BitBoard::FULL_BOARD;
        let expected = BitBoard::NOT_FILE_A;

        assert_eq!(file_a ^ full_board, expected);
    }

    #[test]
    fn test_bitboard_from_str() {
        let bits = "0x0101010101010101";
        let board = BitBoard::from_str(&bits).unwrap();
        assert_eq!(board, BitBoard::FILE_A);

        let bits = "0101010101010101";
        let board = BitBoard::from_str(&bits).unwrap();
        assert_eq!(board, BitBoard::FILE_A);

        let bits = "0b0000000100000001000000010000000100000001000000010000000100000001";
        let board = BitBoard::from_str(&bits).unwrap();
        assert_eq!(board, BitBoard::FILE_A);

        let bits = "0000000100000001000000010000000100000001000000010000000100000001";
        let board = BitBoard::from_str(&bits).unwrap();
        assert_eq!(board, BitBoard::FILE_A);

        let bits = "0b0000000200000002000000020000000200000002000000010000000100000001";
        let board = BitBoard::from_str(bits);
        assert!(board.is_err());

        let bits = "0000000200000002000000020000000200000002000000010000000100000001";
        let board = BitBoard::from_str(bits);
        assert!(board.is_err());

        let bits = "x0awdk";
        let board = BitBoard::from_str(bits);
        assert!(board.is_err());

        let bits = "";
        let board = BitBoard::from_str(bits);
        assert!(board.is_err());
    }

    #[test]
    fn test_bitboard_constructors() {
        assert_eq!(BitBoard::RANK_4, BitBoard::from_rank(Rank::FOUR));
    }
}
