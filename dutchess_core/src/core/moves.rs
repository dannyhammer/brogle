use std::fmt;

use super::{ChessError, Piece, PieceKind, Position, Tile};

/// Represents the different kinds of moves that can be made during a chess game.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub enum MoveKind {
    /// Involves only a single piece moving from one location to another, and does not change the quantity or kind of any pieces on the board.
    Quiet,

    /// Involves the King and a Rook sliding past each other on the King's side of the board.
    KingsideCastle,

    /// Involves the King and a Rook sliding past each other on the Queen's side of the board.
    QueensideCastle,

    /// A special case on a Pawn's first move, wherein it can advance two squares forward.
    PawnPushTwo,

    /// Involves a piece moving onto a square occupied by an opponent's piece, removing it from the board.
    Capture(Piece),

    /// A special variant of capturing that occurs when a Pawn executes En Passant.
    EnPassantCapture(Piece),

    /// Involves a Pawn reaching the opponent's side of the board (rank 8 for White, rank 1 for Black) and becoming another kind of piece, such as a Knight or Queen.
    Promote(PieceKind),
}

/// Represents a move made on a chess board, including whether a piece is to be promoted.
///
/// Internally encoded using the following bit pattern:
/// ```text
///     0000 000000 000000
///      |     |      |
///      |     |      +- Source tile of the move.
///      |     +- Target tile of the move.
///      +- Special flags for promotion, castling, etc.
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
// #[repr(transparent)]
// pub struct Move(u16);
pub struct Move {
    from: Tile,
    to: Tile,
    kind: MoveKind,
}

impl Move {
    /// Flags fetched from [here](https://www.chessprogramming.org/Encoding_Moves#From-To_Based)
    // const PROMOTE_ROOK: u16 = 0b0001_0000_0000_0000;
    // const PROMOTE_QUEEN: u16 = 0b0011_0000_0000_0000;
    // const PROMOTE_BISHOP: u16 = 0b0111_0000_0000_0000;
    // const PROMOTE_KNIGHT: u16 = 0b1111_0000_0000_0000;
    /*
    const SRC_MASK: u16 = 0b0000_0000_0011_1111;
    const DST_MASK: u16 = 0b0000_1111_1100_0000;
    const FLG_MASK: u16 = 0b1111_0000_0000_0000;

    const FLAG_QUIET_M: u16 = 0 << 12;
    const FLAG_PDOUBLE: u16 = 1 << 12;
    const FLAG_KCASTLE: u16 = 2 << 12;
    const FLAG_QCASTLE: u16 = 3 << 12;
    const FLAG_CAPTURE: u16 = 4 << 12;
    const FLAG_EP_CAPT: u16 = 5 << 12;
    const FLAG_PROMO_N: u16 = 8 << 12;
    const FLAG_PROMO_B: u16 = 9 << 12;
    const FLAG_PROMO_R: u16 = 10 << 12;
    const FLAG_PROMO_Q: u16 = 11 << 12;
    const FLAG_PRO_N_C: u16 = 12 << 12;
    const FLAG_PRO_B_C: u16 = 13 << 12;
    const FLAG_PRO_R_C: u16 = 14 << 12;
    const FLAG_PRO_Q_C: u16 = 15 << 12;
     */

    /*
    /// Creates a new [`Move`] from the given [`Tile`]s and optional [`PieceKind`] for promotion.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Move, Tile, PieceKind};
    /// let e7e8Q = Move::new(Tile::E7, Tile::E8, Some(PieceKind::Queen));
    /// assert_eq!(e7e8Q.to_string(), "e7e8Q");
    /// ```
    pub fn new(from: Tile, to: Tile, promote: Option<PieceKind>) -> Self {
        let from = from.inner() as u16;
        let to = to.inner() as u16;

        // If there is a promotion, fetch that PieceKind's bit value and add 1, because 0 represents no promotion
        let flag = if let Some(kind) = promote {
            kind.bits() as u16 + 1
        } else {
            0
        };

        Self(from | to << 6 | flag << 12)
    }
         */

    /// Creates a new [`Move`] from the given [`Tile`]s and a [`MoveKind`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Move, Tile, MoveKind};
    /// let e2e4 = Move::new(Tile::E2, Tile::E4, MoveKind::PawnPushTwo);
    /// assert_eq!(e2e4.to_string(), "e2e4");
    /// ```
    pub const fn new(from: Tile, to: Tile, kind: MoveKind) -> Self {
        Self { from, to, kind }
    }

    /// Creates a new [`Move`] from the given [`Tile`]s that does promote a piece.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Move, Tile};
    /// let e2e3 = Move::new_quiet(Tile::E2, Tile::E3);
    /// assert_eq!(e2e3.to_string(), "e2e3");
    /// ```
    pub const fn new_quiet(from: Tile, to: Tile) -> Self {
        Self::new(from, to, MoveKind::Quiet)
    }

    /// Creates an "illegal" [`Move`], representing moving a piece to and from the same [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::Move;
    /// let illegal = Move::illegal();
    /// assert_eq!(illegal.to_string(), "a1a1");
    /// ```
    pub const fn illegal() -> Self {
        // Self(0)
        Self::new_quiet(Tile::A1, Tile::A1)
    }

    pub const fn is_capture(&self) -> bool {
        match self.kind() {
            MoveKind::Capture(_) | MoveKind::EnPassantCapture(_) => true,
            _ => false,
        }
    }

    /*
    /// Internal function to fetch the bit pattern of the source (or "from") part of this [`Move`].
    fn src_bits(&self) -> u8 {
        (self.0 & Self::SRC_MASK) as u8
    }

    /// Internal function to fetch the bit pattern of the destination (or "to") part of this [`Move`].
    fn dst_bits(&self) -> u8 {
        ((self.0 & Self::DST_MASK) >> 6) as u8
    }

    /// Internal function to fetch the bit pattern of the special flag (piece promotions) of this [`Move`].
    fn flg_bits(&self) -> u8 {
        ((self.0 & Self::FLG_MASK) >> 12) as u8
    }
     */

    /// Fetches the source (or "from") part of this [`Move`], as a [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Move, Tile, MoveKind};
    /// let e2e4 = Move::new(Tile::E2, Tile::E4, MoveKind::PawnPushTwo);
    /// let from = e2e4.from();
    /// assert_eq!(from, Tile::E2);
    /// ```
    pub const fn from(&self) -> Tile {
        // Tile(self.src_bits())
        self.from
    }

    /// Fetches the destination (or "to") part of this [`Move`], as a [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Move, Tile, MoveKind};
    /// let e2e4 = Move::new(Tile::E2, Tile::E4, MoveKind::PawnPushTwo);
    /// let to = e2e4.to();
    /// assert_eq!(to, Tile::E4);
    /// ```
    pub const fn to(&self) -> Tile {
        // Tile(self.dst_bits())
        self.to
    }

    /// Fetches the [`MoveKind`] part of this [`Move`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Move, MoveKind, PieceKind, Tile};
    /// let e7e8Q = Move::new(Tile::E7, Tile::E8, MoveKind::Promote(PieceKind::Queen));
    /// assert_eq!(e7e8Q.kind(), MoveKind::Promote(PieceKind::Queen));
    /// ```
    pub const fn kind(&self) -> MoveKind {
        // let bits = self.flg_bits();

        // We subtract 1 here because we added 1 in `Self::new`
        // (bits != 0).then(|| PieceKind::from_bits_unchecked(bits - 1))
        self.kind
    }

    /*
    pub fn from_san(san: &str) -> Result<Self, ChessError> {
        todo!()
    }

    /// Pure coordinate notation
    pub fn from_pcn(pcn: &str) -> Result<Self, ChessError> {
        todo!()
    }
     */

    /*
    /// Creates a [`Move`] from a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// Will return a [`ChessError`] if the string is invalid in any way.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Move, Tile, MoveKind, PieceKind};
    /// let e7e8Q = Move::from_uci("e7e8Q");
    /// assert_eq!(e7e8Q, Ok(Move::new(Tile::E7, Tile::E8, MoveKind::Promote(PieceKind::Queen))));
    /// ```
    pub fn from_uci(uci: &str) -> Result<Self, ChessError> {
        let from = uci.get(0..2).ok_or(ChessError::InvalidTileNotation)?;
        let to = uci.get(2..4).ok_or(ChessError::InvalidTileNotation)?;

        let from = Tile::from_uci(from)?;
        let to = Tile::from_uci(to)?;

        let kind = if let Some(promote) = uci.get(4..5) {
            MoveKind::Promote(PieceKind::from_str(promote)?)
        } else if uci == "e1g1" || uci == "e8g8" {
            MoveKind::KingsideCastle
        } else if uci == "e1c1" || uci == "e8c8" {
            MoveKind::QueensideCastle
        } else {
            MoveKind::Quiet
        };

        Ok(Self::new(from, to, kind))
    }
      */

    pub fn from_san(position: &Position, san: &str) -> Result<Self, ChessError> {
        // println!("Parsing SAN: {san}\nPosition: {position}");
        let from = san.get(0..2).ok_or(ChessError::InvalidTileNotation)?;
        let to = san.get(2..4).ok_or(ChessError::InvalidTileNotation)?;

        let from = Tile::from_uci(from)?;
        let to = Tile::from_uci(to)?;

        // Safe unwrap because there MUST be a piece here in order to move
        let piece = position.bitboards().piece_at(from).unwrap();

        let kind = if piece.kind().is_pawn() {
            if let Some(promote) = san.get(4..5) {
                MoveKind::Promote(PieceKind::from_str(promote)?)
            } else if let Some(captured) = position.bitboards().piece_at(to) {
                MoveKind::Capture(captured)
            } else if Some(to) == position.ep_tile() && piece.is_pawn() {
                let captured = position
                    .bitboards()
                    .piece_at(position.ep_tile().unwrap())
                    .unwrap();
                MoveKind::EnPassantCapture(captured)
            } else if from.rank().is_pawn_rank(piece.color())
                && to.rank().is_pawn_double_push_rank(piece.color())
            {
                MoveKind::PawnPushTwo
            } else {
                MoveKind::Quiet
            }
        } else {
            if san == "e1g1" || san == "e8g8" {
                MoveKind::KingsideCastle
            } else if san == "e1c1" || san == "e8c8" {
                MoveKind::QueensideCastle
            } else if let Some(captured) = position.bitboards().piece_at(to) {
                MoveKind::Capture(captured)
            } else {
                MoveKind::Quiet
            }
        };

        // println!("SAN parsed:\n\tfrom: {from:?}\n\tto: {to:?}\n\tkind: {kind:?}");

        Ok(Self::new(from, to, kind))
    }

    /// Converts this [`Move`] to a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// Please note that promotions are capitalized by default.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::{Move, Tile, MoveKind, PieceKind};
    /// let e7e8Q = Move::new(Tile::E7, Tile::E8, MoveKind::Promote(PieceKind::Queen));
    /// assert_eq!(e7e8Q.to_san(), "e7e8Q");
    /// ```
    pub fn to_san(&self) -> String {
        match self.kind() {
            MoveKind::Promote(promote) => format!("{}{}{}", self.from(), self.to(), promote),
            _ => format!("{}{}", self.from(), self.to()),
        }
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_san())
    }
}

impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({:?})", self.to_san(), self.kind())
    }
}
