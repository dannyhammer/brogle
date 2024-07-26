use std::{
    fmt,
    ops::{Index, IndexMut, Not, Shl, Shr},
};

use super::{
    default_movement_for, ray_between, utils::FEN_STARTPOS, ChessError, Color, File, Piece,
    PieceKind, Rank, Tile, NUM_COLORS, NUM_PIECE_TYPES,
};

/// Represents a full chess board at any given state.
///
/// Internally uses a collection of [`BitBoards`] to keep track of piece locations, occupied/empty squares, and attack squares.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ChessBoard {
    /// All tiles occupied by a piece of any kind or color.
    occupied: BitBoard,

    /// All tiles occupied by a specific color
    colors: [BitBoard; NUM_COLORS],

    /// All tiles occupied by a specific piece kind
    pieces: [BitBoard; NUM_PIECE_TYPES],
}

impl ChessBoard {
    /// Creates a new, empty [`ChessBoard`] containing no pieces.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::ChessBoard;
    /// let board = ChessBoard::new();
    /// assert_eq!(board.fen(), "8/8/8/8/8/8/8/8");
    /// ```
    pub const fn new() -> Self {
        Self {
            occupied: BitBoard::EMPTY_BOARD,
            colors: [BitBoard::EMPTY_BOARD; NUM_COLORS],
            pieces: [BitBoard::EMPTY_BOARD; NUM_PIECE_TYPES],
        }
    }

    /// Sets up this [`ChessBoard`] to the standard, default setup.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::ChessBoard;
    /// let board = ChessBoard::new().with_default_setup();
    /// assert_eq!(board.fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR");
    /// ```
    pub fn with_default_setup(self) -> Self {
        // Safe unwrap because the FEN for startpos is always valid
        self.with_setup(FEN_STARTPOS).unwrap()
    }

    /// Places the supplied [`Piece`] at the provided [`Tile`], returning modified `self`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, Piece, PieceKind, Color, Tile};
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// let board = ChessBoard::new().with_piece(white_knight, Tile::C4);
    /// assert_eq!(board.fen(), "8/8/8/8/2N5/8/8/8");
    /// ```
    pub fn with_piece(mut self, piece: Piece, tile: Tile) -> Self {
        self.set(piece, tile);
        self
    }

    /// Places the supplied [`Piece`]s at the provided [`Tile`]s, returning modified `self`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, Piece, PieceKind, Color, Tile};
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// let black_rook = Piece::new(Color::Black, PieceKind::Rook);
    /// let board = ChessBoard::new().with_pieces([white_knight, black_rook], [Tile::C4, Tile::H7]);
    /// assert_eq!(board.fen(), "8/7r/8/8/2N5/8/8/8");
    /// ```
    pub fn with_pieces(
        mut self,
        pieces: impl IntoIterator<Item = Piece>,
        tiles: impl IntoIterator<Item = Tile>,
    ) -> Self {
        pieces
            .into_iter()
            .zip(tiles)
            .for_each(|(piece, tile)| self = self.with_piece(piece, tile));

        self
    }

    /// Overrides current board state to apply the provided FEN string to the board.
    pub fn with_setup(mut self, fen: &str) -> Result<Self, ChessError> {
        // If this FEN string contains more than just the initial placements, extract the placements
        let placements = if fen.contains(' ') {
            fen.split(' ').next().unwrap()
        } else {
            fen
        };

        // Check if the placements string is the correct length
        if placements.matches('/').count() != 7 {
            return Err(ChessError::InvalidFenString);
        }

        // Need to reverse this so that White pieces are at the "bottom" of the board
        for (rank, placements) in placements.split('/').rev().enumerate() {
            let mut file = 0;
            let rank = rank as u8;

            for piece_char in placements.chars() {
                // If the next char is a piece, we need to update the relevant BitBoards
                if let Ok(piece) = Piece::from_uci(piece_char) {
                    // Firstly, create a tile and set the "Occupied" board at this location.
                    let tile = Tile::new(File::new_unchecked(file), Rank::new_unchecked(rank));

                    self = self.with_piece(piece, tile);

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

        Ok(self)
    }

    /// Alias for chaining [`ChessBoard::new`] with [`ChessBoard::with_setup`].
    pub fn from_fen(fen: &str) -> Result<Self, ChessError> {
        Self::new().with_setup(fen)
    }

    /// Returns `true` if there is a piece at the given [`Tile`], else `false`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, Tile};
    /// let board = ChessBoard::new().with_default_setup();
    /// assert_eq!(board.has(Tile::B1), true);
    /// ```
    pub fn has(&self, tile: Tile) -> bool {
        self.occupied().get(tile)
    }

    /// Gets the [`Piece`] at a given [`Tile`], if there is one present.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, Piece, PieceKind, Color, Tile};
    /// let board = ChessBoard::new().with_default_setup();
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// assert_eq!(board.get(Tile::B1), Some(white_knight));
    /// ```
    pub fn get(&self, tile: Tile) -> Option<Piece> {
        let color = self.color_at(tile)?;
        let kind = self.kind_at(tile)?;
        Some(Piece::new(color, kind))
    }

    /// Places the provided [`Piece`] and the supplied [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, Piece, PieceKind, Color, Tile};
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// let mut board = ChessBoard::new();
    /// board.set(white_knight, Tile::C4);
    /// assert_eq!(board.fen(), "8/8/8/8/2N5/8/8/8");
    /// ```
    pub fn set(&mut self, piece: Piece, tile: Tile) {
        self[piece.color()].set(tile);
        self[piece.kind()].set(tile);
        self.occupied.set(tile);
    }

    /// Clears the supplied [`Tile`] of any pieces.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, Tile};
    /// let mut board = ChessBoard::new().with_setup("8/8/8/8/2N5/8/8/8").unwrap();
    /// board.clear(Tile::C4);
    /// assert_eq!(board.fen(), "8/8/8/8/8/8/8/8");
    /// ```
    pub fn clear(&mut self, tile: Tile) {
        if let Some(piece) = self.get(tile) {
            self[piece.color()].clear(tile);
            self[piece.kind()].clear(tile);
        }
        self.occupied.clear(tile);
    }

    /// Takes the [`Piece`] from a given [`Tile`], if there is one present.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, Piece, PieceKind, Color, Tile};
    /// let mut board = ChessBoard::new().with_setup("8/8/8/8/2N5/8/8/8").unwrap();
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// let taken = board.take(Tile::C4);
    /// assert_eq!(board.fen(), "8/8/8/8/8/8/8/8");
    /// assert_eq!(taken, Some(white_knight));
    /// ```
    pub fn take(&mut self, tile: Tile) -> Option<Piece> {
        let piece = self.get(tile)?;
        self.clear(tile);

        Some(piece)
    }

    /// Clears the entire board, removing all pieces.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::ChessBoard;
    /// let mut board = ChessBoard::new().with_default_setup();
    /// board.clear_all();
    /// assert_eq!(board.fen(), "8/8/8/8/8/8/8/8");
    /// ```
    pub fn clear_all(&mut self) {
        *self = Self::default();
    }

    /// Fetches the [`Color`] of the piece at the provided [`Tile`], if there is one.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, Color, Tile};
    /// let mut board = ChessBoard::new().with_default_setup();
    /// assert_eq!(board.color_at(Tile::A2), Some(Color::White));
    /// assert!(board.color_at(Tile::E4).is_none());
    /// ```
    pub const fn color_at(&self, tile: Tile) -> Option<Color> {
        if !self.occupied().get(tile) {
            return None;
        }

        use Color::*;
        let color = if self.colors[White.index()].get(tile) {
            White
        } else {
            Black
        };

        Some(color)
    }

    /// Fetches the [`PieceKind`] of the piece at the provided [`Tile`], if there is one.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, PieceKind, Tile};
    /// let mut board = ChessBoard::new().with_default_setup();
    /// assert_eq!(board.kind_at(Tile::A2), Some(PieceKind::Pawn));
    /// assert!(board.kind_at(Tile::E4).is_none());
    /// ```
    pub const fn kind_at(&self, tile: Tile) -> Option<PieceKind> {
        if !self.occupied().get(tile) {
            return None;
        }

        use PieceKind::*;
        let kind = if self.pieces[Pawn.index()].get(tile) {
            Pawn
        } else if self.pieces[Knight.index()].get(tile) {
            Knight
        } else if self.pieces[Bishop.index()].get(tile) {
            Bishop
        } else if self.pieces[Rook.index()].get(tile) {
            Rook
        } else if self.pieces[Queen.index()].get(tile) {
            Queen
        } else {
            King
        };

        Some(kind)
    }

    /// Fetches the [`Piece`] of the piece at the provided [`Tile`], if there is one.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, PieceKind, Color, Tile};
    /// let mut board = ChessBoard::new().with_default_setup();
    /// assert_eq!(board.piece_at(Tile::A2).unwrap().kind(), PieceKind::Pawn);
    /// assert_eq!(board.piece_at(Tile::A2).unwrap().color(), Color::White);
    /// assert!(board.piece_at(Tile::E4).is_none());
    /// ```
    pub const fn piece_at(&self, tile: Tile) -> Option<Piece> {
        if let (Some(color), Some(kind)) = (self.color_at(tile), self.kind_at(tile)) {
            Some(Piece::new(color, kind))
        } else {
            None
        }
    }

    /// Fetches the [`BitBoard`] corresponding to the supplied [`PieceKind`].
    ///
    /// The returned [`BitBoard`] will hold the locations of every occurrence of each [`Piece`] matching the supplied [`PieceKind`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, PieceKind, BitBoard};
    /// let board = ChessBoard::new().with_default_setup();
    /// let pawns = board.kind(PieceKind::Pawn);
    /// assert_eq!(pawns, BitBoard::RANK_2 | BitBoard::RANK_7);
    /// ```
    pub const fn kind(&self, kind: PieceKind) -> BitBoard {
        self.pieces[kind.index()]
    }

    /// Fetches the [`BitBoard`] corresponding to the supplied [`Color`].
    ///
    /// The returned [`BitBoard`] will hold the locations of every occurrence each [`Piece`] matching the supplied [`Color`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, Color, Piece, BitBoard};
    /// let board = ChessBoard::new().with_default_setup();
    /// let white_pieces = board.color(Color::White);
    /// assert_eq!(white_pieces, BitBoard::RANK_1 | BitBoard::RANK_2);
    /// ```
    pub const fn color(&self, color: Color) -> BitBoard {
        self.colors[color.index()]
    }

    /// Fetches the [`BitBoard`] corresponding to the supplied [`Piece`].
    ///
    /// The returned [`BitBoard`] will hold the locations of every occurrence of the supplied [`Piece`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{ChessBoard, PieceKind, Color, Piece, BitBoard};
    /// let board = ChessBoard::new().with_default_setup();
    /// let white_pawn = Piece::new(Color::White, PieceKind::Pawn);
    /// let white_pawns = board.piece(white_pawn);
    /// assert_eq!(white_pawns, BitBoard::RANK_2);
    /// ```
    pub const fn piece(&self, piece: Piece) -> BitBoard {
        self.piece_parts(piece.color(), piece.kind())
    }

    pub const fn piece_parts(&self, color: Color, kind: PieceKind) -> BitBoard {
        let color = self.color(color);
        let kind = self.kind(kind);
        color.and(kind)
    }

    pub const fn orthogonal_sliders(&self, color: Color) -> BitBoard {
        (self.pieces[PieceKind::Rook.index()].or(self.pieces[PieceKind::Queen.index()]))
            .and(self.color(color))
    }

    pub const fn diagonal_sliders(&self, color: Color) -> BitBoard {
        (self.pieces[PieceKind::Bishop.index()].or(self.pieces[PieceKind::Queen.index()]))
            .and(self.color(color))
    }

    pub const fn sliders(&self, color: Color) -> BitBoard {
        (self.pieces[PieceKind::Rook.index()]
            .or(self.pieces[PieceKind::Bishop.index()])
            .or(self.pieces[PieceKind::Queen.index()]))
        .and(self.color(color))
    }

    /// Get a bitboard of all pieces that can be captured by `attacker`.
    pub const fn possible_captures(&self, attacker_color: Color, attacks: BitBoard) -> BitBoard {
        let opponent = self.color(attacker_color.opponent());

        attacks.and(opponent)
    }

    pub const fn king(&self, color: Color) -> BitBoard {
        self.piece(Piece::new(color, PieceKind::King))
    }

    pub const fn in_check_by(&self, king_color: Color, attacks: BitBoard) -> bool {
        attacks.and(self.king(king_color)).0 != 0
    }

    pub const fn blockers(&self, blocker_mask: BitBoard) -> BitBoard {
        // All occupied squares within the blocker mask
        self.occupied().and(blocker_mask)
    }

    pub const fn occupied(&self) -> BitBoard {
        self.occupied
        // BitBoard(self.colors[0].0 | self.colors[1].0)
    }

    pub const fn empty(&self) -> BitBoard {
        self.occupied().not()
    }

    /// Returns an instance of this [`ChessBoard`] that has all bits specified by `mask` cleared.
    pub const fn without(&self, mask: BitBoard) -> Self {
        let not_mask = mask.not();
        let occupied = self.occupied().and(not_mask);
        let mut colors = self.colors;
        colors[0] = colors[0].and(not_mask);
        colors[1] = colors[1].and(not_mask);

        let mut pieces = self.pieces;
        pieces[0] = pieces[0].and(not_mask);
        pieces[1] = pieces[1].and(not_mask);
        pieces[2] = pieces[2].and(not_mask);
        pieces[3] = pieces[3].and(not_mask);
        pieces[4] = pieces[4].and(not_mask);
        pieces[5] = pieces[5].and(not_mask);

        Self {
            occupied,
            colors,
            pieces,
        }
    }

    /// Computes a [`BitBoard`] of all of the squares that can be attacked by [`Color`] pieces.
    pub fn squares_attacked_by(&self, color: Color) -> BitBoard {
        let mut attacks = BitBoard::EMPTY_BOARD;

        // All occupied spaces
        let blockers = self.occupied();

        // Get the attack tables for all pieces of this color
        for tile in self.color(color) {
            // Safe unwrap because we're iterating over all pieces of this color
            let piece = self.piece_at(tile).unwrap();
            attacks |= default_movement_for(&piece, tile, blockers);
        }

        attacks
    }

    /// Removes the defending King from this board and computes the attacks of the `attacker_color` pieces,
    /// returning a [`BitBoard`] of all of the squares that can be attacked.
    pub fn king_danger_squares(&self, attacker_color: Color) -> BitBoard {
        let king_bb = self.king(attacker_color.opponent());
        let kingless = self.without(king_bb);
        kingless.squares_attacked_by(attacker_color)
    }

    /*
    pub const fn with_only(&self, piece: Piece) -> Self {
        let piece_board = self.piece(piece);

        let occupied = self.occupied.xor(piece_board);
        let mut colors = self.colors;
        colors[piece.color().index()] = colors[piece.color().index()].xor(piece_board);

        let mut pieces = self.pieces;
        pieces[piece.kind().index()] = pieces[piece.kind().index()].xor(piece_board);

        Self {
            occupied,
            empty: occupied.not(),
            colors,
            pieces,
        }
    }
     */

    /// Get all squares that are either empty or occupied by the enemy
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, ChessBoard, Color};
    /// let board = ChessBoard::new().with_default_setup();
    /// let not_white = board.enemy_or_empty(Color::White);
    /// assert_eq!(not_white.to_hex_string(), "0xFFFFFFFFFFFF0000");
    /// ```
    pub const fn enemy_or_empty(&self, color: Color) -> BitBoard {
        self.color(color).not()
    }

    /// Creates a [`BoardIter`] to iterate over all occupied [`Tile`]s in this [`GameBoard`].
    pub const fn iter<'a>(&'a self) -> BoardIter<'a> {
        BoardIter {
            board: &self,
            occupancy: self.occupied(),
        }
    }

    /// Generates a [FEN](https://www.chess.com/terms/fen-chess) string of this [`ChessBoard`].
    pub fn fen(&self) -> String {
        let mut placements: [String; 8] = Default::default();

        for rank in Rank::iter() {
            let mut empty_spaces = 0;
            for file in File::iter() {
                if let Some(piece) = self.get(file * rank) {
                    if empty_spaces != 0 {
                        placements[rank.index()] += &empty_spaces.to_string();
                        empty_spaces = 0;
                    }
                    placements[rank.index()] += &piece.to_string();
                } else {
                    empty_spaces += 1;
                }
            }

            if empty_spaces != 0 {
                placements[rank.index()] += &empty_spaces.to_string();
            }
        }
        placements.reverse();

        placements.join("/")
    }
}

impl Default for ChessBoard {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ChessBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Allocate just enough capacity
        let mut board = String::with_capacity(198);

        for rank in Rank::iter().rev() {
            board += &format!("{rank}| ");

            for file in File::iter() {
                let tile = Tile::new(file, rank);
                let occupant = if let Some(piece) = self.piece_at(tile) {
                    piece.to_string()
                } else {
                    // String::from(if tile.is_light() { "#" } else { "-" })
                    String::from(".")
                };

                board += &format!("{occupant} ");
            }

            board += "\n"
        }
        board += " +";
        for _ in File::iter() {
            board += &format!("--");
        }
        board += "\n   ";
        for file in File::iter() {
            board += &format!("{file} ");
        }

        write!(f, "{board}")
    }
}

impl From<[Option<Piece>; 64]> for ChessBoard {
    fn from(value: [Option<Piece>; 64]) -> Self {
        let mut board = Self::new();

        for (i, piece) in value.into_iter().enumerate() {
            if let Some(piece) = piece {
                board.set(piece, Tile::from_index(i).unwrap())
            }
        }

        board
    }
}

impl Index<PieceKind> for ChessBoard {
    type Output = BitBoard;
    fn index(&self, index: PieceKind) -> &Self::Output {
        &self.pieces[index]
    }
}

impl IndexMut<PieceKind> for ChessBoard {
    fn index_mut(&mut self, index: PieceKind) -> &mut Self::Output {
        &mut self.pieces[index]
    }
}

impl Index<Color> for ChessBoard {
    type Output = BitBoard;
    fn index(&self, index: Color) -> &Self::Output {
        &self.colors[index]
    }
}

impl IndexMut<Color> for ChessBoard {
    fn index_mut(&mut self, index: Color) -> &mut Self::Output {
        &mut self.colors[index]
    }
}

impl fmt::Debug for ChessBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let format = |to_fmt: &[(BitBoard, &str)]| {
            let strings = to_fmt
                .iter()
                .map(|(b, s)| (b.to_string(), s))
                .collect::<Vec<_>>();

            let splits = strings
                .iter()
                .map(|(b, _)| b.split('\n').collect::<Vec<_>>())
                .collect::<Vec<_>>();

            let labels = strings
                .iter()
                .map(|(_, s)| format!("{s:10}\t\t"))
                .collect::<String>();

            let boards = (0..8)
                .map(|i| {
                    format!(
                        "{}\n",
                        (0..splits.len())
                            .map(|j| format!("{}\t\t", splits[j][i]))
                            .collect::<String>()
                    )
                })
                .collect::<String>();

            format!("{labels}\n{boards}")
        };

        let pieces = format(&[
            (self.pieces[PieceKind::Pawn], "Pawn"),
            (self.pieces[PieceKind::Knight], "Knight"),
            (self.pieces[PieceKind::Bishop], "Bishop"),
            (self.pieces[PieceKind::Rook], "Rook"),
            (self.pieces[PieceKind::Queen], "Queen"),
            (self.pieces[PieceKind::King], "King"),
        ]);

        let metadata = format(&[
            (self.occupied(), "Occupied"),
            (self.empty(), "Empty"),
            (self.colors[Color::White], "White"),
            (self.colors[Color::Black], "Black"),
        ]);

        write!(f, "Game State:\n{pieces}\n\n{metadata}")
    }
}

pub struct BoardIter<'a> {
    board: &'a ChessBoard,
    occupancy: BitBoard,
}

impl<'a> Iterator for BoardIter<'a> {
    type Item = (Tile, Piece);

    fn next(&mut self) -> Option<Self::Item> {
        let lsb = self.occupancy.pop_lsb()?;
        let piece = self.board.get(lsb)?;
        Some((lsb, piece))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.occupancy.population() as usize;
        (size, Some(size))
    }
}

impl<'a> ExactSizeIterator for BoardIter<'a> {}

/// A [`BitBoard`] represents the game board as a set of bits.
/// They are used for various computations, such as fetching valid moves or computing move costs.
///
/// The internal representation is a 64-bit binary number, so the values will represent the entire board.
/// They are color-agnostic, with the low order bits representing the "lower" half of the board.
///
/// Bit index 0 is the least-significant bit (LSB = 2^0)
/// Bit index 63 is the most-significant bit (MSB = 2^63)
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
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
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
    pub const DARK_SQUARES: Self = Self(0xAA55AA55AA55AA55);
    pub const EMPTY_BOARD: Self = Self(0x0000000000000000);
    pub const FULL_BOARD: Self = Self(0xFFFFFFFFFFFFFFFF);
    pub const EDGES: Self = Self(0xFF818181818181FF);
    pub const CORNERS: Self = Self(0x8100000000000081);
    pub const CENTER: Self = Self(0x0000001818000000);

    // const RANK_END_INDICES: [usize; 8] = [7, 15, 23, 31, 39, 47, 55, 63];

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
        Self::from_tile(Tile::from_index_unchecked(index))
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
        Self(1 << tile.index())
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

    /// Returns [`BitBoard::FULL_BOARD`] if `true`, else [`BitBoard::EMPTY_BOARD`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    ///
    /// assert_eq!(BitBoard::from_bool(true), BitBoard::FULL_BOARD);
    /// assert_eq!(BitBoard::from_bool(false), BitBoard::EMPTY_BOARD);
    /// ```
    pub const fn from_bool(value: bool) -> Self {
        Self((value as u64).wrapping_neg() & u64::MAX)
    }

    /// Returns a [`BitBoard`] of this [`Color`]'s first rank.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Color};
    ///
    /// assert_eq!(BitBoard::first_rank(Color::White), BitBoard::RANK_1);
    /// assert_eq!(BitBoard::first_rank(Color::Black), BitBoard::RANK_8);
    /// ```
    pub const fn first_rank(color: Color) -> Self {
        [Self::RANK_1, Self::RANK_8][color.index()]
    }

    /// Returns a [`BitBoard`] of this [`Color`]'s second rank.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Color};
    ///
    /// assert_eq!(BitBoard::second_rank(Color::White), BitBoard::RANK_2);
    /// assert_eq!(BitBoard::second_rank(Color::Black), BitBoard::RANK_7);
    /// ```
    pub const fn second_rank(color: Color) -> Self {
        [Self::RANK_2, Self::RANK_7][color.index()]
    }

    /// Returns a [`BitBoard`] of this [`Color`]'s seventh rank.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Color};
    ///
    /// assert_eq!(BitBoard::seventh_rank(Color::White), BitBoard::RANK_7);
    /// assert_eq!(BitBoard::seventh_rank(Color::Black), BitBoard::RANK_2);
    /// ```
    pub const fn seventh_rank(color: Color) -> Self {
        [Self::RANK_7, Self::RANK_2][color.index()]
    }

    /// Returns a [`BitBoard`] of this [`Color`]'s eighth (last) rank.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Color};
    ///
    /// assert_eq!(BitBoard::eighth_rank(Color::White), BitBoard::RANK_8);
    /// assert_eq!(BitBoard::eighth_rank(Color::Black), BitBoard::RANK_1);
    /// ```
    pub const fn eighth_rank(color: Color) -> Self {
        [Self::RANK_8, Self::RANK_1][color.index()]
    }

    /// Returns the squares between (inclusive) the short (kingside) castling of `color`.
    pub const fn kingside_castle(color: Color) -> Self {
        [
            ray_between(Tile::E1, Tile::G1),
            ray_between(Tile::E8, Tile::G8),
        ][color.index()]
    }

    /// Returns the squares between (inclusive) the long (queenside) castling of `color`.
    pub const fn queenside_castle(color: Color) -> Self {
        [
            ray_between(Tile::E1, Tile::C1),
            ray_between(Tile::E8, Tile::C8),
        ][color.index()]
    }

    pub(crate) const fn queenside_castle_clearance(color: Color) -> Self {
        [
            ray_between(Tile::E1, Tile::B1),
            ray_between(Tile::E8, Tile::B8),
        ][color.index()]
    }

    pub(crate) const fn kingside_castle_clearance(color: Color) -> Self {
        [
            ray_between(Tile::E1, Tile::G1),
            ray_between(Tile::E8, Tile::G8),
        ][color.index()]
    }

    /// Returns the inner `u64` of this [`BitBoard`].
    pub const fn inner(&self) -> u64 {
        self.0
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
    /// assert_eq!(board.to_tile_unchecked(), Tile::G2);
    /// ```
    pub const fn to_tile_unchecked(&self) -> Tile {
        Tile::from_index_unchecked(self.to_index())
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
    /// assert_eq!(board.to_tile(), Some(Tile::G2));
    /// let invalid = BitBoard::RANK_1;
    /// assert_eq!(invalid.to_tile(), None);
    /// ```
    pub const fn to_tile(&self) -> Option<Tile> {
        if self.is_only_one() {
            Some(self.to_tile_unchecked())
        } else {
            None
        }
    }

    pub const fn to_index(&self) -> usize {
        self.0.trailing_zeros() as usize
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

    /// If `color` is Black, flips this [`BitBoard`].
    /// If `color` is White, does nothing.
    ///
    /// See [`BitBoard::flipped`] for more.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Color, BitBoard};
    /// assert_eq!(BitBoard::RANK_2.relative_to(Color::White), BitBoard::RANK_2);
    /// assert_eq!(BitBoard::RANK_2.relative_to(Color::Black), BitBoard::RANK_7);
    /// ```
    pub const fn relative_to(self, color: Color) -> Self {
        match color {
            Color::White => self,
            Color::Black => self.flipped(),
        }
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

    /// Returns `true` if this [`BitBoard`] has at most `n` bits on.
    pub const fn has_at_most(&self, n: u32) -> bool {
        self.population() <= n
    }

    /// Returns `true` if this [`BitBoard`] has at most 1 bit on.
    pub const fn is_only_one(&self) -> bool {
        self.population() == 1
    }

    /// Checks if this [`BitBoard`] contains any of the bits within `other`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// let rank_1 = BitBoard::RANK_1;
    /// let rank_5 = BitBoard::RANK_5;
    /// let file_a = BitBoard::FILE_A;
    /// assert_eq!(rank_1.contains(file_a), true);
    /// assert_eq!(rank_1.contains(rank_5), false);
    /// ```
    pub const fn contains(&self, other: Self) -> bool {
        (self.0 & other.0) != 0
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
        self.set_index(tile.index());
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
        self.clear_index(tile.index())
    }

    /// Remove all tiles from `other` in `self`
    pub fn remove(&mut self, other: &Self) {
        self.0 &= !other.0
    }

    /// Returns the index of the lowest non-zero bit of this [`BitBoard`], as a [`Tile`].
    pub const fn lsb(&self) -> Option<Tile> {
        if self.is_empty() {
            None
        } else {
            Some(Tile(self.0.trailing_zeros() as u8))
        }
    }

    /// Pops and returns the index of the lowest non-zero bit of this [`BitBoard`], as a [`Tile`].
    pub fn pop_lsb(&mut self) -> Option<Tile> {
        let lsb = self.lsb();
        self.clear_lsb();
        lsb
    }

    /// Clears the lowest non-zero bit from `self`, if there is a square to clear.
    pub fn clear_lsb(&mut self) {
        self.0 &= self.0.wrapping_sub(1);
    }

    // https://www.chessprogramming.org/Traversing_Subsets_of_a_Set#All_Subsets_of_any_Set
    pub fn carry_rippler(&mut self, mask: Self) {
        *self = self.to_carry_rippler(mask);
    }

    // https://www.chessprogramming.org/Traversing_Subsets_of_a_Set#All_Subsets_of_any_Set
    pub const fn to_carry_rippler(self, mask: Self) -> Self {
        Self(self.0.wrapping_sub(mask.0) & mask.0)
    }

    /// Toggles the bit corresponding to the specified [`Tile`].
    pub fn toggle_tile(&mut self, tile: Tile) {
        *self ^= Self::from_tile(tile);
    }

    /// Toggles the bit at `index`.
    pub fn toggle_index(&mut self, index: usize) {
        *self ^= Self::from_index(index);
    }

    /// Sets the bit at `index` to `1`.
    ///
    /// # Panics
    /// If `index > 63` with debug assertions enabled.
    fn set_index(&mut self, index: usize) {
        debug_assert!(index < 64, "Index must be between [0,64)");
        // self.0 |= 1 << index;
        *self |= Self::from_index(index);
    }

    /// Returns `true` if the bit at `index` is set, else `false`.
    ///
    /// # Panics
    /// If `index > 63` with debug assertions enabled.
    const fn get_index(&self, index: usize) -> bool {
        debug_assert!(index < 64, "Index must be between [0,64)");
        (self.0 & 1 << index) != 0
    }

    /// Sets the bit at `index` to `0`.
    ///
    /// # Panics
    /// If `index > 63` with debug assertions enabled.
    fn clear_index(&mut self, index: usize) {
        debug_assert!(index < 64, "Index must be between [0,64)");
        // self.0 ^= 1 << index;
        *self ^= Self::from_index(index);
    }

    /// Returns a [`BitBoardIter`] to iterate over all of the set bits as [`Tile`]s.
    pub const fn iter(&self) -> BitBoardIter {
        BitBoardIter { bb: *self }
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

    /// Returns `true` if `other` is a subset of `self`.
    pub const fn contains_all(&self, other: &Self) -> bool {
        self.0 & other.0 == other.0
    }

    /// Returns `true` if `self` contains any bits of `other`.
    pub const fn contains_any(&self, other: &Self) -> bool {
        self.0 & other.0 != Self::EMPTY_BOARD.0
    }

    /// If there are any bits set in `self`, returns [`BitBoard::FULL_BOARD`].
    /// Otherwise, returns `other`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// let a = BitBoard::RANK_3;
    /// let b = BitBoard::FILE_H;
    /// let empty = BitBoard::EMPTY_BOARD;
    /// let full = BitBoard::FULL_BOARD;
    ///
    /// assert_eq!(a.full_if_empty_else_other(b), b);
    /// assert_eq!(b.full_if_empty_else_other(a), a);
    /// assert_eq!(empty.full_if_empty_else_other(a), full);
    /// ```
    pub const fn full_if_empty_else_other(&self, other: Self) -> Self {
        Self::from_bool(self.is_empty()).or(other)
    }

    /// If there are any bits set in `self`, returns [`BitBoard::FULL_BOARD`].
    /// Otherwise, returns `self`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// let a = BitBoard::RANK_1;
    /// let empty = BitBoard::EMPTY_BOARD;
    /// let full = BitBoard::FULL_BOARD;
    ///
    /// assert_eq!(a.full_if_not_empty(), full);
    /// assert_eq!(full.full_if_not_empty(), full);
    /// assert_eq!(empty.full_if_not_empty(), empty);
    /// ```
    pub const fn full_if_not_empty(&self) -> Self {
        Self::from_bool(!self.is_empty()).or(*self)
    }

    /// If there are any bits set in `self`, returns [`BitBoard::FULL_BOARD`].
    /// Otherwise, returns `self`.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// let a = BitBoard::RANK_3;
    /// let empty = BitBoard::EMPTY_BOARD;
    /// let full = BitBoard::FULL_BOARD;
    ///
    /// assert_eq!(a.full_if_empty(), a);
    /// assert_eq!(full.full_if_empty(), full);
    /// assert_eq!(empty.full_if_empty(), full);
    /// ```
    pub const fn full_if_empty(&self) -> Self {
        Self::from_bool(self.is_empty()).or(*self)
    }

    /// Shifts this [`BitBoard`] forward by `n`, according to `color`.
    ///
    /// If `color` is White, this shifts `n` ranks up. If Black, it shifts by `n` rank down.
    ///
    /// Note: This can "wrap" by advancing beyond the end of the board, so be careful!
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Color};
    /// let rank4 = BitBoard::RANK_4;
    /// assert_eq!(rank4.advance_by(Color::White, 1), BitBoard::RANK_5);
    /// assert_eq!(rank4.advance_by(Color::Black, 1), BitBoard::RANK_3);
    /// // Wrapping
    /// assert_eq!(rank4.advance_by(Color::White, 5), BitBoard::RANK_1);
    /// ```
    pub const fn advance_by(self, color: Color, n: u32) -> Self {
        // Black magic: If `color` is White, this rotates left by 8, which is the same as "n ranks up"
        // If `color` is Black, this rotates left by 496, which is the same as rotating right by 8, or "n ranks down"
        Self(self.0.rotate_left(n * 8 * (1 + color as u32 * 62)))
    }

    /// Shifts this [`BitBoard`] backward by `n`, according to `color`.
    ///
    /// If `color` is White, this shifts `n` ranks up. If Black, it shifts by `n` ranks down.
    ///
    /// Note: This can "wrap" by advancing beyond the end of the board, so be careful!
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{BitBoard, Color};
    /// let rank4 = BitBoard::RANK_4;
    /// assert_eq!(rank4.retreat_by(Color::White, 1), BitBoard::RANK_3);
    /// assert_eq!(rank4.retreat_by(Color::Black, 1), BitBoard::RANK_5);
    /// // Wrapping
    /// assert_eq!(rank4.retreat_by(Color::Black, 5), BitBoard::RANK_1);
    /// ```
    pub const fn retreat_by(self, color: Color, n: u32) -> Self {
        // Black magic: If `color` is White, this rotates right by 8, which is the same as "n ranks down"
        // If `color` is Black, this rotates right by 496, which is the same as rotating left by 8, or "n ranks up"
        Self(self.0.rotate_right(n * 8 * (1 + color as u32 * 62)))
    }

    /// Shifts this [`BitBoard`] by one rank up.
    ///
    /// If already at the final rank (8), returns an empty board.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// assert_eq!(BitBoard::RANK_4.north(), BitBoard::RANK_5);
    /// assert_eq!(BitBoard::RANK_8.north(), BitBoard::EMPTY_BOARD);
    /// ```
    pub const fn north(self) -> Self {
        Self(self.0 << 8)
    }

    // Rank down
    /// Shifts this [`BitBoard`] by one rank board.
    ///
    /// If already at the first rank (1), returns an empty board.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// assert_eq!(BitBoard::RANK_4.south(), BitBoard::RANK_3);
    /// assert_eq!(BitBoard::RANK_1.south(), BitBoard::EMPTY_BOARD);
    /// ```
    pub const fn south(self) -> Self {
        Self(self.0 >> 8)
    }

    /// Shifts this [`BitBoard`] by one [`File`] up.
    ///
    /// If already at the first file (a), returns an empty board.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// assert_eq!(BitBoard::FILE_C.east(), BitBoard::FILE_D);
    /// assert_eq!(BitBoard::FILE_H.east(), BitBoard::EMPTY_BOARD);
    /// ```
    pub const fn east(self) -> Self {
        // Post-shift mask
        Self((self.0 << 1) & Self::NOT_FILE_A.0)
    }

    /// Shifts this [`BitBoard`] by one [`File`] down.
    ///
    /// If already at the final file (h), returns an empty board.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::BitBoard;
    /// assert_eq!(BitBoard::FILE_C.west(), BitBoard::FILE_B);
    /// assert_eq!(BitBoard::FILE_A.west(), BitBoard::EMPTY_BOARD);
    /// ```
    pub const fn west(self) -> Self {
        // Post-shift mask
        Self((self.0 >> 1) & Self::NOT_FILE_H.0)
    }

    /// Combination of [`BitBoard::north`] and [`BitBoard::east`].
    ///
    /// If already at the edge, returns an empty board.
    ///
    /// This operation is faster than calling [`BitBoard::north`] and [`BitBoard::east`] separately.
    pub const fn northeast(self) -> Self {
        // Post-shift mask
        Self((self.0 << 9) & Self::NOT_FILE_A.0)
    }

    /// Combination of [`BitBoard::south`] and [`BitBoard::east`].
    ///
    /// If already at the edge, returns an empty board.
    ///
    /// This operation is faster than calling [`BitBoard::south`] and [`BitBoard::east`] separately.
    pub const fn southeast(self) -> Self {
        // Post-shift mask
        Self((self.0 >> 7) & Self::NOT_FILE_A.0)
    }

    /// Combination of [`BitBoard::north`] and [`BitBoard::west`].
    ///
    /// If already at the edge, returns an empty board.
    ///
    /// This operation is faster than calling [`BitBoard::north`] and [`BitBoard::west`] separately.
    pub const fn northwest(self) -> Self {
        // Post-shift mask
        Self((self.0 << 7) & Self::NOT_FILE_H.0)
    }

    /// Combination of [`BitBoard::south`] and [`BitBoard::west`].
    ///
    /// If already at the edge, returns an empty board.
    ///
    /// This operation is faster than calling [`BitBoard::south`] and [`BitBoard::west`] separately.
    pub const fn southwest(self) -> Self {
        // Post-shift mask
        Self((self.0 >> 9) & Self::NOT_FILE_H.0)
    }

    /*
    /// Creates a mask of all squares in front of `tile` (according to `color`) that are either directly in front or on the adjacent files.
    pub fn passed_pawn_mask(tile: Tile, color: Color) -> Self {
        let rank = match color {
            Color::White => tile.rank().increase(),
            Color::Black => tile.rank().decrease(),
        };

        let forward_ranks = Self::FULL_BOARD << rank;

        let file = tile.file();
        let left = Self::from(file.decrease());
        let right = Self::from(file.increase());
        let center = Self::from_file(file);

        let files = center | left | right;

        forward_ranks & files
    }
     */

    /// `const` analog of [`BitAnd::bitand`].
    ///
    /// Returns the bitwise AND (`&`) of `self` and `other`.
    pub const fn and(self, other: Self) -> Self {
        Self(self.0 & other.0)
    }

    /// `const` analog of [`BitOr::bitor`].
    ///
    /// Returns the bitwise OR (`|`) of `self` and `other`.
    pub const fn or(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    /// `const` analog of [`BitXor::bitxor`].
    ///
    /// Returns the bitwise XOR (`^`) of `self` and `other`.
    pub const fn xor(self, other: Self) -> Self {
        Self(self.0 ^ other.0)
    }

    /// `const` analog of [`Not::not`].
    ///
    /// Returns the logical negation (`!`) of `self`, flipping all `1`'s to `0`'s and vice versa.
    pub const fn not(self) -> Self {
        Self(!self.0)
    }

    /// Formats this [`BitBoard`] as a hexadecimal string.
    pub fn to_hex_string(&self) -> String {
        format!("0x{:0>16X}", self.0)
    }
}

macro_rules! impl_bitwise_op {
    // Impl op and op_assign for Self
    ($op:tt, $op_assign:tt, $func:ident, $func_assign:ident, $op_tok:tt) => {
        impl std::ops::$op for BitBoard {
            type Output = Self;
            fn $func(self, rhs: Self) -> Self::Output {
                Self(self.0 $op_tok rhs.0)
            }
        }

        impl std::ops::$op_assign for BitBoard {
            fn $func_assign(&mut self, rhs: Self) {
                *self = *self $op_tok rhs;
            }
        }
    };
}

impl_bitwise_op!(BitAnd, BitAndAssign, bitand, bitand_assign, &);
impl_bitwise_op!(BitOr, BitOrAssign, bitor, bitor_assign, |);
impl_bitwise_op!(BitXor, BitXorAssign, bitxor, bitxor_assign, ^);
impl_bitwise_op!(Shl, ShlAssign, shl, shl_assign, <<);
impl_bitwise_op!(Shr, ShrAssign, shr, shr_assign, >>);

impl Not for BitBoard {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self(!self.0)
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

impl Index<Tile> for BitBoard {
    type Output = bool;
    /// A [`BitBoard`] can be indexed by a [`Tile`] to yield `true` or `false`, if the bit at that index is set.
    fn index(&self, index: Tile) -> &Self::Output {
        if self.get_index(index.index()) {
            &true
        } else {
            &false
        }
    }
}

impl Index<usize> for BitBoard {
    type Output = bool;
    /// A [`BitBoard`] can be indexed by a [`usize`] to yield `true` or `false`, if the bit at that index is set.
    ///
    /// # Panics
    /// If debug assertions are enabled and `index > 63`.
    fn index(&self, index: usize) -> &Self::Output {
        if self.get_index(index) {
            &true
        } else {
            &false
        }
    }
}

impl<T> From<Option<T>> for BitBoard
where
    Self: From<T>,
{
    /// If `value` is `None`, this yields an empty [`BitBoard`].
    fn from(value: Option<T>) -> Self {
        if let Some(t) = value {
            Self::from(t)
        } else {
            Self::default()
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
    /// Formats this [`BitBoard`] as a 16-character uppercase hexadecimal string, not including the `0x` prefix.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0X{:0>16X}", self.0)
    }
}

impl fmt::LowerHex for BitBoard {
    /// Formats this [`BitBoard`] as a 16-character lowercase hexadecimal string, not including the `0x` prefix.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0x{:0>16x}", self.0)
    }
}

impl fmt::Binary for BitBoard {
    /// Formats this [`BitBoard`] as a 64-character binary string, not including the `0b` prefix.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0b{:0>64b}", self.0)
    }
}

impl fmt::Display for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Allocate just enough capacity
        let mut board = String::with_capacity(136);

        for rank in Rank::iter().rev() {
            for file in File::iter() {
                let tile = Tile::new(file, rank);
                let occupant = if self.get(tile) { 'X' } else { '.' };

                board += &format!("{occupant} ");
            }
            board += "\n";
        }

        write!(f, "{board}")
    }
}

impl fmt::Debug for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Allocate just enough capacity
        let mut board = String::with_capacity(198);

        for rank in Rank::iter().rev() {
            board += &format!("{rank}| ");

            for file in File::iter() {
                let tile = Tile::new(file, rank);
                let occupant = if self.get(tile) { 'X' } else { '.' };

                board += &format!("{occupant} ");
            }
            board += "\n";
        }
        board += " +";
        for _ in File::iter() {
            board += "--";
        }
        board += "\n   ";
        for file in File::iter() {
            board += &format!("{file} ");
        }

        write!(f, "{board}")
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
        let next = self.bb.lsb()?;
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
        let expected = ". . . . . . . X\n\
                              . . . . . . X .\n\
                              . . . . . X . .\n\
                              . . . . X . . .\n\
                              . . . X . . . .\n\
                              . . X . . . . .\n\
                              . X . . . . . .\n\
                              X . . . . . . .";
        assert_eq!(BitBoard::A1_H8_DIAG.to_string(), expected);

        let expected = ". . . . . . . .\n\
                              . . . . . . . .\n\
                              . . . . . . . .\n\
                              . . . . . . . .\n\
                              . . . . . . . .\n\
                              . . . . . . . .\n\
                              X X X X X X X X\n\
                              . . . . . . . .";
        assert_eq!(BitBoard::RANK_2.to_string(), expected);

        let board = BitBoard::RANK_2 | BitBoard::FILE_C;
        let expected = ". . X . . . . .\n\
                              . . X . . . . .\n\
                              . . X . . . . .\n\
                              . . X . . . . .\n\
                              . . X . . . . .\n\
                              . . X . . . . .\n\
                              X X X X X X X X\n\
                              . . X . . . . .";
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
