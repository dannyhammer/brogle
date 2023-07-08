use std::{
    fmt,
    ops::{
        BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Index, Not, Shl, ShlAssign,
        Shr, ShrAssign,
    },
};

use crate::{Color, File, Piece, PieceKind, Position, Rank};

#[derive(PartialEq, Eq, Debug, Hash)]
pub enum MoveLegality {
    /// Move is completely legal
    Legal, // TODO: Contain the move?

    /// Can't capture the same color piece
    CaptureSameColor(Color),

    /// King is in check by the pieces at the provided positions
    PutsKingInCheck(Vec<Position>),

    /// That piece doesn't move that way.
    InvalidMovement(PieceKind),

    /// There isn't a piece at the selected tile
    NoPieceAtTile(Position),
}

impl MoveLegality {
    pub fn is_legal(&self) -> bool {
        *self == Self::Legal
    }
}

impl fmt::Display for MoveLegality {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            Self::Legal => format!("Move is legal"),
            Self::CaptureSameColor(color) => {
                format!("Cannot capture a piece with the same color ({color})")
            }
            Self::PutsKingInCheck(attacking) => format!("That move puts your King in check by the pieces at the following positions: {attacking:?}"),
            Self::InvalidMovement(kind) => format!("The {} doesn't move that way...", kind.name()),
            Self::NoPieceAtTile(from) => format!("There's no piece at {from} to be moved"),
        };

        write!(f, "{reason}")
    }
}

/*********************************************************************************
 * Game board
*********************************************************************************/

#[derive(PartialEq, Eq, Debug, Hash)]
pub struct ChessBoard {
    state: ChessBoardState,
    board: BitBoards,
    history: Vec<Move>,
    legal_moves: Vec<Move>,
}

impl ChessBoard {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn fen(&self) -> String {
        self.state.to_fen()
    }

    pub fn piece_at(&self, tile: Position) -> Option<&Piece> {
        self.state.piece(tile).as_ref()
    }

    /// Fetch the piece at the requested position, if it exists.
    fn get(&self, tile: Position) -> Option<Piece> {
        *self.state.piece(tile)
    }

    /// Set the piece at the specified position.
    fn set(&mut self, tile: Position, piece: Piece) {
        self.board.set(tile, piece); // Update the bitboards
        *self.state.piece_mut(tile) = Some(piece); // Update the state
    }

    /// Remove a piece from the specified position, returning it if it exists.
    fn clear(&mut self, tile: Position) -> Option<Piece> {
        self.board.clear(tile); // Update the bitboards
        self.state.piece_mut(tile).take() // Update the state
    }

    /// Returns `true` if the move was made successful, and `false` if it cannot be made.
    pub fn make_move(&mut self, from: Position, to: Position) -> bool {
        let Some(piece) = self.clear(from) else { return false };
        self.set(to, piece);
        true
    }

    /// Returns whether the move is legal to make, given the current game state.
    /*
    pub fn legality(&mut self, from: Position, to: Position) -> MoveLegality {
        // Can't make a move if `from` has no piece!
        let Some(piece) = self.get(from) else { return MoveLegality::NoPieceAtTile(from) };

        if let Some(target) = self.get(to) {
            // Cannot capture your own team's piece
            if piece.color() == target.color() {
                return MoveLegality::CaptureSameColor(piece.color());
            }
        }

        MoveLegality::Legal
    }
     */

    pub fn is_legal(&mut self, from: Position, to: Position) -> bool {
        // TODO: Check if exists in all legal moves.
        self.legal_moves().contains(&Move::new(from, to))
    }

    pub fn legal_moves(&self) -> &[Move] {
        &self.legal_moves
    }

    fn generate_all_legal_moves(&mut self) {
        // self.legal_moves.extend(self.generate_pawn_moves())

        for tile in Position::iter() {
            if let Some(piece) = self.get(tile) {
                // let legal_tiles = match piece.kind() {
                // PieceKind::Pawn => self.generate_pawn_positions(tile),
                // PieceKind::Knight => self.generate_knight_moves(),
                // PieceKind::Bishop => self.generate_bishop_moves(),
                // PieceKind::Rook => self.generate_rook_moves(),
                // PieceKind::Queen => self.generate_queen_moves(),
                // PieceKind::King => self.generate_king_moves(),
                //     _ => todo!(),
                // };
            }
        }

        /*
        for piece in self.pieces() {
        }
         */
    }

    pub fn attacked_by(&self, piece: &Piece) -> Vec<Position> {
        vec![]
    }

    pub fn legal_moves_of(&self, piece: &Piece, tile: Position) -> BitBoard {
        self.board.moves_for(piece, tile)
    }
}

impl Default for ChessBoard {
    fn default() -> Self {
        let state = ChessBoardState::default();
        let board = BitBoards::from(state.pieces);
        let mut s = Self {
            state,
            board,
            history: Vec::with_capacity(64),
            legal_moves: Vec::with_capacity(1024),
        };
        s.generate_all_legal_moves();
        s
    }
}

impl fmt::Display for ChessBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board = String::new();

        for rank in Rank::iter() {
            for _ in File::iter() {
                board += "+---";
            }

            board += "+\n";

            for file in File::iter() {
                let occupant = if let Some(piece) = self.get(file + rank) {
                    piece.to_string()
                } else {
                    String::from(" ")
                };

                board += &format!("| {occupant} ");
            }

            board += "|\n";
        }
        for _ in File::iter() {
            board += "+---";
        }
        board += "+";

        write!(f, "{}", board)
    }
}

impl Index<Position> for ChessBoard {
    type Output = Option<Piece>;
    fn index(&self, index: Position) -> &Self::Output {
        &self.state.pieces[index]
    }
}

/// Represents the current state of the game, including move counters
///
/// Analogous to a FEN string.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ChessBoardState {
    /// Can be indexed by [`Position`]
    pieces: [Option<Piece>; 64],

    current_player: Color,
    can_white_kingside_castle: bool,
    can_white_queenside_castle: bool,
    can_black_kingside_castle: bool,
    can_black_queenside_castle: bool,
    en_passant_tile: Option<Position>,

    /// Used to enforce the fifty-move rule.
    /// Is incremented after each move.
    /// Is reset after a capture or a pawn moves.
    halfmove: u8,

    /// Number of moves since the beginning of the game/
    /// A fullmove is a complete turn by white and then by black.
    fullmove: u8,
}

impl ChessBoardState {
    fn from_fen(fen: &str) -> Result<Self, &'static str> {
        let mut split = fen.split(' ');
        let placements = split.next().ok_or("Empty string is not valid FEN")?;

        if placements.matches('/').count() != 7 {
            return Err(
                "Valid FEN placements must have 8 rows delimited with 7 forward slashes `/`",
            );
        }

        let mut pieces = [None; 64];
        for (rank, placements) in placements.split('/').enumerate() {
            let mut file = 0;
            for piece_char in placements.chars() {
                if let Ok(kind) = PieceKind::from_char(piece_char) {
                    let piece = if piece_char.is_ascii_uppercase() {
                        Piece::new(Color::White, kind)
                    } else {
                        Piece::new(Color::Black, kind)
                    };

                    pieces[rank * 8 + file] = Some(piece);
                    file += 1;
                } else {
                    let Some(empty) = piece_char.to_digit(10) else { return Err("Invalid non-char digit when parsing FEN string") };
                    file += empty as usize
                }
            }
        }

        let active_color = split.next().unwrap_or_else(|| {
            let x = "w";
            eprintln!("Warning: No active color specified; defaulting to {x}");
            x
        });
        let current_player = match active_color {
            "w" | "W" => Color::White,
            "b" | "B" => Color::Black,
            _ => return Err("Active color must be either `w` or `b`"),
        };

        let castling = split.next().unwrap_or_else(|| {
            let x = "KQkq";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let can_white_kingside_castle = castling.contains('K');
        let can_white_queenside_castle = castling.contains('Q');
        let can_black_kingside_castle = castling.contains('k');
        let can_black_queenside_castle = castling.contains('q');

        let en_passant_target = split.next().unwrap_or_else(|| {
            let x = "-";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let en_passant_tile = match en_passant_target {
            "-" => None,
            tile => {
                let Ok(pos) = Position::from_uci(tile) else { return Err("Invalid En Passant target when parsing FEN string") };
                Some(pos)
            }
        };

        let halfmove = split.next().unwrap_or_else(|| {
            let x = "0";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let Ok(halfmove) = halfmove.parse() else { return Err("Invalid halfmove; must be numeric") };

        let fullmove = split.next().unwrap_or_else(|| {
            let x = "1";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let Ok(fullmove) = fullmove.parse() else { return Err("Invalid fullmove; must be numeric") };

        Ok(Self {
            pieces,
            current_player,
            can_white_kingside_castle,
            can_white_queenside_castle,
            can_black_kingside_castle,
            can_black_queenside_castle,
            en_passant_tile,
            halfmove,
            fullmove,
        })
    }

    pub fn to_fen(&self) -> String {
        let mut placements: [String; 8] = Default::default();

        for rank in Rank::iter() {
            let mut empty_spaces = 0;
            for file in File::iter() {
                if let Some(piece) = self.piece(file + rank) {
                    if empty_spaces != 0 {
                        placements[rank.0 as usize] += &empty_spaces.to_string();
                        empty_spaces = 0;
                    }
                    placements[rank.0 as usize] += &piece.to_string();
                } else {
                    empty_spaces += 1;
                }
            }

            if empty_spaces != 0 {
                placements[rank.0 as usize] += &empty_spaces.to_string();
            }
        }
        let placements = placements.join("/");

        let active_color = self.current_player;

        let mut castling = String::with_capacity(4);
        if self.can_white_kingside_castle {
            castling += "K"
        }
        if self.can_white_queenside_castle {
            castling += "Q"
        }
        if self.can_black_kingside_castle {
            castling += "k"
        }
        if self.can_black_queenside_castle {
            castling += "q"
        }
        if castling.is_empty() {
            castling = String::from("-");
        }

        let en_passant_target = if let Some(tile) = self.en_passant_tile {
            tile.to_string()
        } else {
            String::from("-")
        };

        let halfmove = self.halfmove;
        let fullmove = self.fullmove;

        let fen = format!(
            "{placements} {active_color} {castling} {en_passant_target} {halfmove} {fullmove}"
        );

        fen
    }

    fn piece(&self, tile: Position) -> &Option<Piece> {
        &self.pieces[tile]
    }

    fn piece_mut(&mut self, tile: Position) -> &mut Option<Piece> {
        &mut self.pieces[tile]
    }
}

impl Default for ChessBoardState {
    fn default() -> Self {
        // Safe unwrap: Default FEN is always valid
        // Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
        Self::from_fen("8/8/8/2b5/2b5/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
    }
}

impl fmt::Display for ChessBoardState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_fen())
    }
}

/*********************************************************************************
 * Board representations
*********************************************************************************/

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct BitBoards {
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
}

impl BitBoards {
    fn get(&self, tile: Position) -> Option<Piece> {
        let color = self.color_at(tile)?;
        let kind = self.kind_at(tile)?;
        Some(Piece::new(color, kind))
    }

    fn set(&mut self, tile: Position, piece: Piece) {
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

    fn clear(&mut self, tile: Position) {
        self.white.clear(tile);
        self.black.clear(tile);
        self.pawn.clear(tile);
        self.knight.clear(tile);
        self.bishop.clear(tile);
        self.rook.clear(tile);
        self.queen.clear(tile);
        self.king.clear(tile);
    }

    fn color_at(&self, tile: Position) -> Option<Color> {
        if !self.occupied.get(tile) {
            return None;
        }

        if self.white.get(tile) {
            Some(Color::White)
        } else {
            Some(Color::Black)
        }
    }

    fn kind_at(&self, tile: Position) -> Option<PieceKind> {
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

    fn piece(&self, piece: &Piece) -> BitBoard {
        let color = self[piece.color()];
        let kind = self[piece.kind()];
        color & kind
    }

    fn moves_for(&self, piece: &Piece, tile: Position) -> BitBoard {
        let moves = match piece.kind() {
            PieceKind::Pawn => self.pawn_moves(tile, piece.color()),
            PieceKind::Knight => self.knight_moves(tile),
            PieceKind::Bishop => self.bishop_moves(tile),
            PieceKind::Rook => self.rook_moves(tile),
            PieceKind::Queen => self.bishop_moves(tile) | self.rook_moves(tile),
            PieceKind::King => self.king_moves(tile),
        };

        moves & !self[piece.color()]
    }

    fn pawn_moves(&self, tile: Position, color: Color) -> BitBoard {
        let src = BitBoard::from(tile);
        if color.is_white() {
            src.north()
        } else {
            src.south()
        }
    }

    fn knight_moves(&self, tile: Position) -> BitBoard {
        let src = BitBoard::from(tile);
        let n = src.north();
        let s = src.south();
        let e = src.east();
        let w = src.west();

        n.northwest()
            | n.northeast()
            | s.southwest()
            | s.southeast()
            | e.northeast()
            | e.southeast()
            | w.northwest()
            | w.southwest()
    }

    fn bishop_moves(&self, tile: Position) -> BitBoard {
        let src = BitBoard::from(tile);
        todo!()
    }

    fn rook_moves(&self, tile: Position) -> BitBoard {
        let src = BitBoard::from(tile);
        todo!()
    }

    fn king_moves(&self, tile: Position) -> BitBoard {
        let src = BitBoard::from(tile);
        src.north()
            | src.northeast()
            | src.east()
            | src.southeast()
            | src.south()
            | src.southwest()
            | src.west()
            | src.northwest()
    }
}

impl From<[Option<Piece>; 64]> for BitBoards {
    fn from(value: [Option<Piece>; 64]) -> Self {
        let mut boards = Self::default();

        for (i, piece) in value.into_iter().enumerate() {
            if let Some(piece) = piece {
                boards.set(Position::from_index(i).unwrap(), piece)
            } else {
                boards.empty.set_index(i);
            }
        }

        boards
    }
}

impl Index<PieceKind> for BitBoards {
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

impl Index<Color> for BitBoards {
    type Output = BitBoard;
    fn index(&self, index: Color) -> &Self::Output {
        match index {
            Color::White => &self.white,
            Color::Black => &self.black,
        }
    }
}

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
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
#[repr(transparent)]
pub struct BitBoard(u64);

impl BitBoard {
    pub fn new(bits: u64) -> Self {
        Self(bits)
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

    fn is_empty(&self) -> bool {
        self.0 == EMPTY_BOARD
    }

    fn file_mask(file: File) -> Self {
        Self(FILE_A << file.0)
    }

    fn rank_mask(rank: Rank) -> Self {
        Self(RANK_1 << rank.0)
    }

    fn set(&mut self, tile: Position) {
        // self.0 |= 1 << tile.index();
        self.set_index(tile.index())
    }

    pub fn get(&self, tile: Position) -> bool {
        // (self.0 & 1 << tile.index()) != 0
        self.get_index(tile.index())
    }

    fn clear(&mut self, tile: Position) {
        // self.0 ^= !(1 << tile.index());
        self.clear_index(tile.index())
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

    // Rank up
    fn north(self) -> Self {
        Self(self.0 >> 8)
    }

    // Rank down
    fn south(self) -> Self {
        Self(self.0 << 8)
    }

    // File up
    fn west(self) -> Self {
        Self((self.0 & NOT_FILE_H) >> 1)
    }

    // File down
    fn east(self) -> Self {
        Self((self.0 & NOT_FILE_A) << 1)
    }

    fn northeast(self) -> Self {
        // Self((self.0 & NOT_FILE_H) << 9)
        self.north().east()
    }

    fn southeast(self) -> Self {
        // Self((self.0 & NOT_FILE_H) >> 7)
        self.south().east()
    }

    fn northwest(self) -> Self {
        // Self((self.0 & NOT_FILE_A) << 7)
        self.north().west()
    }

    fn southwest(self) -> Self {
        // Self((self.0 & NOT_FILE_A) >> 9)
        self.south().west()
    }

    /// Get the value of the bit at `index`
    fn get_bit_at(&self, index: usize) -> bool {
        index <= 64 && self.0 & (1 << index) != 0
    }

    fn least_significant_one(&self) -> BitBoard {
        *self & !*self
    }

    fn bitscan(&self) -> u8 {
        let mut bits = self.0;
        let mut index = 0;
        loop {
            if bits % 2 == 1 || index == 63 {
                return index;
            } else {
                bits <<= 1;
                index += 1;
            }
        }
    }

    // Brian Kernighan's method
    fn population(&self) -> u8 {
        let mut bits = self.0;
        let mut pop = 0;
        while bits != 0 {
            pop += 1;
            bits &= bits - 1;
        }

        pop
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

impl From<Position> for BitBoard {
    fn from(value: Position) -> Self {
        Self(1 << value.index())
    }
}

impl fmt::Display for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bytes = self.0.to_ne_bytes();
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

// Operations
impl BitAnd for BitBoard {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0.bitand(rhs.0))
    }
}

impl BitAndAssign for BitBoard {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs
    }
}

impl BitOr for BitBoard {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0.bitor(rhs.0))
    }
}

impl BitOrAssign for BitBoard {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs
    }
}

impl BitXor for BitBoard {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.0.bitxor(rhs.0))
    }
}

impl BitXorAssign for BitBoard {
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs
    }
}

impl Not for BitBoard {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self(self.0.not())
    }
}

impl Shl for BitBoard {
    type Output = Self;
    fn shl(self, rhs: Self) -> Self::Output {
        Self(self.0 << rhs.0)
    }
}

impl ShlAssign for BitBoard {
    fn shl_assign(&mut self, rhs: Self) {
        *self = *self << rhs
    }
}

impl Shr for BitBoard {
    type Output = Self;
    fn shr(self, rhs: Self) -> Self::Output {
        Self(self.0 >> rhs.0)
    }
}

impl ShrAssign for BitBoard {
    fn shr_assign(&mut self, rhs: Self) {
        *self = *self >> rhs
    }
}

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

/*********************************************************************************
 * Game logic
*********************************************************************************/

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Move {
    from: Position,
    to: Position,
}

impl Move {
    pub fn new(from: Position, to: Position) -> Self {
        // Self(from | to << 4)
        Self { from, to }
    }

    pub fn from(&self) -> Position {
        self.from
    }

    pub fn to(&self) -> Position {
        self.to
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.from(), self.to())
    }
}

/*********************************************************************************
 * Utility functions
*********************************************************************************/

pub struct MoveGenerator {}

impl MoveGenerator {
    //
}

// fn index(file: File, rank: Rank) -> usize {
//     8 * rank.index() + file.index()
// }

static FILE_A: u64 = 0x0101010101010101;
static FILE_H: u64 = 0x8080808080808080;
static NOT_FILE_A: u64 = 0xfefefefefefefefe; // ~0x0101010101010101
static NOT_FILE_H: u64 = 0x7f7f7f7f7f7f7f7f; // ~0x8080808080808080
static RANK_1: u64 = 0x00000000000000FF;
static RANK_8: u64 = 0xFF00000000000000;
static A1_H8_DIAG: u64 = 0x8040201008040201;
static H1_A8_DIAG: u64 = 0x0102040810204080;
static LIGHT_SQUARES: u64 = 0x55AA55AA55AA55AA;
static DARKS_SQUARES: u64 = 0xAA55AA55AA55AA55;
static EMPTY_BOARD: u64 = 0x0000000000000000;
static FULL_BOARD: u64 = 0xFFFFFFFFFFFFFFFF;
