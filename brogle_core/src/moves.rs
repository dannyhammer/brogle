use std::{fmt, str::FromStr};

use anyhow::{anyhow, Result};

use super::{PieceKind, Position, Rank, Tile};

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
    Capture,

    /// A special variant of capturing that occurs when a Pawn executes En Passant.
    EnPassantCapture,

    /// Involves a Pawn reaching the opponent's side of the board (rank 8 for White, rank 1 for Black) and becoming another kind of piece, such as a Knight or Queen.
    Promote(PieceKind),

    /// Involves a Pawn moving onto a square on the opponent's side of the board that is occupied by an opponent's piece, removing it from the board, and promoting this Pawn to something else.
    PromoCapt(PieceKind),
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
///
/// Flags are fetched directly from the [Chess Programming Wiki](https://www.chessprogramming.org/Encoding_Moves#From-To_Based).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Move(u16);

impl Move {
    /// Mask for the source ("from") bits.
    const SRC_MASK: u16 = 0b0000_0000_0011_1111;
    /// Mask for the destination ("to") bits.
    const DST_MASK: u16 = 0b0000_1111_1100_0000;
    /// Mask for the flag (promotions, captures, etc.) bits.
    const FLG_MASK: u16 = 0b1111_0000_0000_0000;
    /// Start index of destination bits.
    const DST_BITS: u16 = 6;
    /// Start index of flag bits.
    const FLG_BITS: u16 = 12;

    /// Flags fetched from [here](https://www.chessprogramming.org/Encoding_Moves#From-To_Based)
    const FLAG_QUIET: u16 = 0 << Self::FLG_BITS;
    const FLAG_PAWN_DOUBLE: u16 = 1 << Self::FLG_BITS;
    const FLAG_CASTLE_SHORT: u16 = 2 << Self::FLG_BITS;
    const FLAG_CASTLE_LONG: u16 = 3 << Self::FLG_BITS;
    const FLAG_CAPTURE: u16 = 4 << Self::FLG_BITS;
    const FLAG_EP_CAPTURE: u16 = 5 << Self::FLG_BITS;
    const FLAG_PROMO_KNIGHT: u16 = 8 << Self::FLG_BITS;
    const FLAG_PROMO_BISHOP: u16 = 9 << Self::FLG_BITS;
    const FLAG_PROMO_ROOK: u16 = 10 << Self::FLG_BITS;
    const FLAG_PROMO_QUEEN: u16 = 11 << Self::FLG_BITS;
    const FLAG_CAPTURE_PROMO_KNIGHT: u16 = 12 << Self::FLG_BITS;
    const FLAG_CAPTURE_PROMO_BISHOP: u16 = 13 << Self::FLG_BITS;
    const FLAG_CAPTURE_PROMO_ROOK: u16 = 14 << Self::FLG_BITS;
    const FLAG_CAPTURE_PROMO_QUEEN: u16 = 15 << Self::FLG_BITS;

    /// Creates a new [`Move`] from the given [`Tile`]s and a [`MoveKind`].
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, Tile, MoveKind, PieceKind};
    /// let e2e4 = Move::new(Tile::E2, Tile::E4, MoveKind::PawnPushTwo);
    /// assert_eq!(e2e4.to_string(), "e2e4");
    ///
    /// let e7e8n = Move::new(Tile::E7, Tile::E8, MoveKind::Promote(PieceKind::Knight));
    /// assert_eq!(e7e8n.to_string(), "e7e8n");
    /// ```
    pub const fn new(from: Tile, to: Tile, kind: MoveKind) -> Self {
        let from = from.inner() as u16;
        let to = to.inner() as u16;
        let flag = Self::get_bit_flag(kind);

        Self(flag | to << Self::DST_BITS | from)
    }

    /// Creates a new [`Move`] from the given [`Tile`]s that does not promote a piece.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, Tile};
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
    /// # use brogle_core::Move;
    /// let illegal = Move::illegal();
    /// assert_eq!(illegal.to_string(), "a1a1");
    /// ```
    pub const fn illegal() -> Self {
        Self(0)
    }

    /// Fetches the source (or "from") part of this [`Move`], as a [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, Tile, MoveKind};
    /// let e2e4 = Move::new(Tile::E2, Tile::E4, MoveKind::PawnPushTwo);
    /// let from = e2e4.from();
    /// assert_eq!(from, Tile::E2);
    /// ```
    pub const fn from(&self) -> Tile {
        Tile::from_bits_unchecked((self.0 & Self::SRC_MASK) as u8)
    }

    /// Fetches the destination (or "to") part of this [`Move`], as a [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, Tile, MoveKind};
    /// let e2e4 = Move::new(Tile::E2, Tile::E4, MoveKind::PawnPushTwo);
    /// let to = e2e4.to();
    /// assert_eq!(to, Tile::E4);
    /// ```
    pub const fn to(&self) -> Tile {
        Tile::from_bits_unchecked(((self.0 & Self::DST_MASK) >> Self::DST_BITS) as u8)
    }

    /// Fetches the [`MoveKind`] part of this [`Move`].
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, MoveKind, PieceKind, Tile};
    /// let e7e8q = Move::new(Tile::E7, Tile::E8, MoveKind::Promote(PieceKind::Queen));
    /// assert_eq!(e7e8q.kind(), MoveKind::Promote(PieceKind::Queen));
    /// ```
    pub const fn kind(&self) -> MoveKind {
        let bits = self.0 & Self::FLG_MASK;
        match bits {
            Self::FLAG_QUIET => MoveKind::Quiet,
            Self::FLAG_PAWN_DOUBLE => MoveKind::PawnPushTwo,
            Self::FLAG_CASTLE_SHORT => MoveKind::KingsideCastle,
            Self::FLAG_CASTLE_LONG => MoveKind::QueensideCastle,
            Self::FLAG_CAPTURE => MoveKind::Capture,
            Self::FLAG_EP_CAPTURE => MoveKind::EnPassantCapture,
            Self::FLAG_PROMO_QUEEN => MoveKind::Promote(PieceKind::Queen),
            Self::FLAG_PROMO_KNIGHT => MoveKind::Promote(PieceKind::Knight),
            Self::FLAG_PROMO_ROOK => MoveKind::Promote(PieceKind::Rook),
            Self::FLAG_PROMO_BISHOP => MoveKind::Promote(PieceKind::Bishop),
            Self::FLAG_CAPTURE_PROMO_QUEEN => MoveKind::PromoCapt(PieceKind::Queen),
            Self::FLAG_CAPTURE_PROMO_KNIGHT => MoveKind::PromoCapt(PieceKind::Knight),
            Self::FLAG_CAPTURE_PROMO_ROOK => MoveKind::PromoCapt(PieceKind::Rook),
            Self::FLAG_CAPTURE_PROMO_BISHOP => MoveKind::PromoCapt(PieceKind::Bishop),
            _ => unimplemented!(),
        }
    }

    /// Internal function to convert a [`MoveKind`] into a bit flag to encode this move internally.
    const fn get_bit_flag(kind: MoveKind) -> u16 {
        use MoveKind::*;
        match kind {
            Quiet => Self::FLAG_QUIET,
            PawnPushTwo => Self::FLAG_PAWN_DOUBLE,
            Capture => Self::FLAG_CAPTURE,
            EnPassantCapture => Self::FLAG_EP_CAPTURE,
            KingsideCastle => Self::FLAG_CASTLE_SHORT,
            QueensideCastle => Self::FLAG_CASTLE_LONG,
            PromoCapt(promotion) => match promotion {
                PieceKind::Queen => Self::FLAG_CAPTURE_PROMO_QUEEN,
                PieceKind::Knight => Self::FLAG_CAPTURE_PROMO_KNIGHT,
                PieceKind::Rook => Self::FLAG_CAPTURE_PROMO_ROOK,
                PieceKind::Bishop => Self::FLAG_CAPTURE_PROMO_BISHOP,
                _ => unreachable!(),
            },
            Promote(promotion) => match promotion {
                PieceKind::Queen => Self::FLAG_PROMO_QUEEN,
                PieceKind::Knight => Self::FLAG_PROMO_KNIGHT,
                PieceKind::Rook => Self::FLAG_PROMO_ROOK,
                PieceKind::Bishop => Self::FLAG_PROMO_BISHOP,
                _ => unreachable!(),
            },
        }
    }

    /// Fetches the parts of this [`Move`] in a tuple of `(from, to, kind)`.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, MoveKind, PieceKind, Tile};
    /// let e7e8q = Move::new(Tile::E7, Tile::E8, MoveKind::Promote(PieceKind::Queen));
    /// let (from, to, kind) = e7e8q.parts();
    /// assert_eq!(from, Tile::E7);
    /// assert_eq!(to, Tile::E8);
    /// assert_eq!(kind, MoveKind::Promote(PieceKind::Queen));
    /// ```
    pub const fn parts(&self) -> (Tile, Tile, MoveKind) {
        (self.from(), self.to(), self.kind())
    }

    /// Returns `true` if this [`Move`] is a capture of any kind.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, Tile, MoveKind, PieceKind, Position, FEN_KIWIPETE};
    /// let position = Position::from_fen(FEN_KIWIPETE).unwrap();
    /// let e5f7 = Move::from_uci(&position, "e5f7").unwrap();
    /// assert_eq!(e5f7.is_capture(), true);
    /// ```
    pub const fn is_capture(&self) -> bool {
        self.0 & Self::FLAG_CAPTURE != 0
    }

    /// Returns `true` if this [`Move`] is en passant.
    pub const fn is_en_passant(&self) -> bool {
        (self.0 & Self::FLG_MASK) ^ Self::FLAG_EP_CAPTURE == 0
    }

    /// Returns `true` if this [`Move`] is a short (kingside) castle.
    pub const fn is_short_castle(&self) -> bool {
        (self.0 & Self::FLG_MASK) ^ Self::FLAG_CASTLE_SHORT == 0
    }

    /// Returns `true` if this [`Move`] is a long (queenside) castle.
    pub const fn is_long_castle(&self) -> bool {
        (self.0 & Self::FLG_MASK) ^ Self::FLAG_CASTLE_LONG == 0
    }

    /// Returns `true` if this [`Move`] is a short (kingside) or long (queenside) castle.
    pub const fn is_castle(&self) -> bool {
        self.is_short_castle() || self.is_long_castle()
    }

    /// Returns `true` if this [`Move`] is a long (queenside) castle.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, Tile, MoveKind};
    /// let e2e4 = Move::new(Tile::E2, Tile::E4, MoveKind::PawnPushTwo);
    /// assert_eq!(e2e4.is_pawn_double_push(), true);
    /// ```
    pub const fn is_pawn_double_push(&self) -> bool {
        (self.0 & Self::FLG_MASK) ^ Self::FLAG_PAWN_DOUBLE == 0
    }

    /// Returns `true` if this [`Move`] is a promotion of any kind.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, MoveKind, PieceKind, Tile};
    /// let e7e8q = Move::new(Tile::E7, Tile::E8, MoveKind::Promote(PieceKind::Queen));
    /// assert_eq!(e7e8q.is_promotion(), true);
    ///
    /// let e7e8q = Move::new(Tile::E7, Tile::E8, MoveKind::PromoCapt(PieceKind::Queen));
    /// assert_eq!(e7e8q.is_promotion(), true);
    /// ```
    pub const fn is_promotion(&self) -> bool {
        // The flag bit for "promotion" is the most-significant bit.
        // Internally, FLAG_PROMO_KNIGHT has flag bits `1000`, so we can use it as a mask for promotions.
        self.0 & Self::FLAG_PROMO_KNIGHT != 0
    }

    /// Returns `true` if this [`Move`] is a capture of any kind.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, Tile, MoveKind, PieceKind, Position};
    /// // An sample test position for discovering promotion bugs.
    /// let position = Position::from_fen("n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ").unwrap();
    /// let b7c8b = Move::from_uci(&position, "b7c8b").unwrap();
    /// assert_eq!(b7c8b.promotion(), Some(PieceKind::Bishop));
    /// ```
    pub const fn promotion(&self) -> Option<PieceKind> {
        match self.0 & Self::FLG_MASK {
            Self::FLAG_PROMO_QUEEN | Self::FLAG_CAPTURE_PROMO_QUEEN => Some(PieceKind::Queen),
            Self::FLAG_PROMO_KNIGHT | Self::FLAG_CAPTURE_PROMO_KNIGHT => Some(PieceKind::Knight),
            Self::FLAG_PROMO_ROOK | Self::FLAG_CAPTURE_PROMO_ROOK => Some(PieceKind::Rook),
            Self::FLAG_PROMO_BISHOP | Self::FLAG_CAPTURE_PROMO_BISHOP => Some(PieceKind::Bishop),
            _ => None,
        }
    }

    /// Returns `true` if this move is formatted properly according to [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::Move;
    /// assert_eq!(Move::is_uci("b7c8b"), true);
    /// assert_eq!(Move::is_uci("a1a1"), true);
    /// assert_eq!(Move::is_uci("xj9"), false);
    /// ```
    pub fn is_uci(input: &str) -> bool {
        let Some(from) = input.get(0..2) else {
            return false;
        };
        let Some(to) = input.get(2..4) else {
            return false;
        };

        let is_ok = Tile::from_uci(from).is_ok() && Tile::from_uci(to).is_ok();

        if let Some(promote) = input.get(4..5) {
            is_ok && PieceKind::from_str(promote).is_ok()
        } else {
            is_ok
        }
    }
    /// Creates a [`Move`] from a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation, extracting extra info from the provided [`Position`]
    ///
    /// Will return a [`anyhow::Error`] if the string is invalid in any way.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, Tile, MoveKind, PieceKind, Position};
    /// // A sample test position for discovering promotion bugs.
    /// let position = Position::from_fen("n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ").unwrap();
    /// let b7c8b = Move::from_uci(&position, "b7c8b");
    /// assert!(b7c8b.is_ok());
    /// assert_eq!(b7c8b.unwrap(), Move::new(Tile::B7, Tile::C8, MoveKind::PromoCapt(PieceKind::Bishop)));
    /// ```
    pub fn from_uci(position: &Position, uci: &str) -> Result<Self> {
        // Extract the to/from squares
        let from = uci
            .get(0..2)
            .ok_or(anyhow!("Move str must contain a `from` tile."))?;
        let to = uci
            .get(2..4)
            .ok_or(anyhow!("Move str must contain a `to` tile."))?;

        let from = Tile::from_uci(from)?;
        let to = Tile::from_uci(to)?;

        // Extract information about the piece being moved
        let piece = position.board().piece_at(from).ok_or(anyhow!(
            "No piece found at {from} when parsing UCI move string on position {position}"
        ))?;
        let color = piece.color();

        // The MoveKind depends on what kind of piece is being moved and where
        let kind = if piece.is_pawn() {
            // Pawns have a lot of special moves they can do...
            if let Some(promote) = uci.get(4..5) {
                // If this move also captures, it's a capture-promote
                if position.board().has(to) {
                    MoveKind::PromoCapt(PieceKind::from_str(promote)?)
                } else {
                    MoveKind::Promote(PieceKind::from_str(promote)?)
                }
            } else if position.board().has(to) {
                MoveKind::Capture
            } else if Some(to) == position.ep_tile() && piece.is_pawn() {
                MoveKind::EnPassantCapture
            } else if from.rank() == Rank::second(color) && to.rank() == Rank::fourth(color) {
                MoveKind::PawnPushTwo
            } else {
                MoveKind::Quiet
            }
        } else if piece.is_king() {
            // TODO: Support for castling in Chess960
            if uci == "e1g1" || uci == "e8g8" {
                MoveKind::KingsideCastle
            } else if uci == "e1c1" || uci == "e8c8" {
                MoveKind::QueensideCastle
            } else if position.board().has(to) {
                MoveKind::Capture
            } else {
                MoveKind::Quiet
            }
        } else if position.board().has(to) {
            MoveKind::Capture
        } else {
            MoveKind::Quiet
        };

        Ok(Self::new(from, to, kind))
    }

    /// Converts this [`Move`] to a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// Please note that promotions are capitalized by default.
    ///
    /// # Example
    /// ```
    /// # use brogle_core::{Move, Tile, MoveKind, PieceKind};
    /// let e7e8Q = Move::new(Tile::E7, Tile::E8, MoveKind::Promote(PieceKind::Queen));
    /// assert_eq!(e7e8Q.to_uci(), "e7e8q");
    /// ```
    pub fn to_uci(&self) -> String {
        if let Some(promote) = self.promotion() {
            format!("{}{}{}", self.from(), self.to(), promote)
        } else {
            format!("{}{}", self.from(), self.to())
        }
    }
}

impl fmt::Display for Move {
    /// A [`Move`] is displayed in its UCI format.
    ///
    /// See [`Move::to_uci`] for more.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}

impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({:?})", self.to_uci(), self.kind())
    }
}

impl Default for Move {
    /// A "default" move is an illegal move. See [`Move::illegal`]
    ///
    /// This is mostly just to satisfy the compiler, and should never be used in a real scenario.
    fn default() -> Self {
        Self::illegal()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_move_is_capture() {
        let (from, to) = (Tile::A1, Tile::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_capture());
        assert!(!Move::new(from, to, MoveKind::KingsideCastle).is_capture());
        assert!(!Move::new(from, to, MoveKind::QueensideCastle).is_capture());
        assert!(!Move::new(from, to, MoveKind::PawnPushTwo).is_capture());
        assert!(Move::new(from, to, MoveKind::Capture).is_capture());
        assert!(Move::new(from, to, MoveKind::EnPassantCapture).is_capture());
        assert!(!Move::new(from, to, MoveKind::Promote(PieceKind::Queen)).is_capture());
        assert!(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)).is_capture());
    }

    #[test]
    fn test_move_is_en_passant() {
        let (from, to) = (Tile::A1, Tile::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_en_passant());
        assert!(!Move::new(from, to, MoveKind::KingsideCastle).is_en_passant());
        assert!(!Move::new(from, to, MoveKind::QueensideCastle).is_en_passant());
        assert!(!Move::new(from, to, MoveKind::PawnPushTwo).is_en_passant());
        assert!(!Move::new(from, to, MoveKind::Capture).is_en_passant());
        assert!(Move::new(from, to, MoveKind::EnPassantCapture).is_en_passant());
        assert!(!Move::new(from, to, MoveKind::Promote(PieceKind::Queen)).is_en_passant());
        assert!(!Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)).is_en_passant());
    }

    #[test]
    fn test_move_is_short_castle() {
        let (from, to) = (Tile::A1, Tile::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_short_castle());
        assert!(Move::new(from, to, MoveKind::KingsideCastle).is_short_castle());
        assert!(!Move::new(from, to, MoveKind::QueensideCastle).is_short_castle());
        assert!(!Move::new(from, to, MoveKind::PawnPushTwo).is_short_castle());
        assert!(!Move::new(from, to, MoveKind::Capture).is_short_castle());
        assert!(!Move::new(from, to, MoveKind::EnPassantCapture).is_short_castle());
        assert!(!Move::new(from, to, MoveKind::Promote(PieceKind::Queen)).is_short_castle());
        assert!(!Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)).is_short_castle());
    }

    #[test]
    fn test_move_is_long_castle() {
        let (from, to) = (Tile::A1, Tile::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_long_castle());
        assert!(!Move::new(from, to, MoveKind::KingsideCastle).is_long_castle());
        assert!(Move::new(from, to, MoveKind::QueensideCastle).is_long_castle());
        assert!(!Move::new(from, to, MoveKind::PawnPushTwo).is_long_castle());
        assert!(!Move::new(from, to, MoveKind::Capture).is_long_castle());
        assert!(!Move::new(from, to, MoveKind::EnPassantCapture).is_long_castle());
        assert!(!Move::new(from, to, MoveKind::Promote(PieceKind::Queen)).is_long_castle());
        assert!(!Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)).is_long_castle());
    }

    #[test]
    fn test_move_is_castle() {
        let (from, to) = (Tile::A1, Tile::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_castle());
        assert!(Move::new(from, to, MoveKind::KingsideCastle).is_castle());
        assert!(Move::new(from, to, MoveKind::QueensideCastle).is_castle());
        assert!(!Move::new(from, to, MoveKind::PawnPushTwo).is_castle());
        assert!(!Move::new(from, to, MoveKind::Capture).is_castle());
        assert!(!Move::new(from, to, MoveKind::EnPassantCapture).is_castle());
        assert!(!Move::new(from, to, MoveKind::Promote(PieceKind::Queen)).is_castle());
        assert!(!Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)).is_castle());
    }

    #[test]
    fn test_move_is_pawn_double_push() {
        let (from, to) = (Tile::A1, Tile::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_pawn_double_push());
        assert!(!Move::new(from, to, MoveKind::KingsideCastle).is_pawn_double_push());
        assert!(!Move::new(from, to, MoveKind::QueensideCastle).is_pawn_double_push());
        assert!(Move::new(from, to, MoveKind::PawnPushTwo).is_pawn_double_push());
        assert!(!Move::new(from, to, MoveKind::Capture).is_pawn_double_push());
        assert!(!Move::new(from, to, MoveKind::EnPassantCapture).is_pawn_double_push());
        assert!(!Move::new(from, to, MoveKind::Promote(PieceKind::Queen)).is_pawn_double_push());
        assert!(!Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)).is_pawn_double_push());
    }

    #[test]
    fn test_move_is_promotion() {
        let (from, to) = (Tile::A1, Tile::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_promotion());
        assert!(!Move::new(from, to, MoveKind::KingsideCastle).is_promotion());
        assert!(!Move::new(from, to, MoveKind::QueensideCastle).is_promotion());
        assert!(!Move::new(from, to, MoveKind::PawnPushTwo).is_promotion());
        assert!(!Move::new(from, to, MoveKind::Capture).is_promotion());
        assert!(!Move::new(from, to, MoveKind::EnPassantCapture).is_promotion());
        assert!(Move::new(from, to, MoveKind::Promote(PieceKind::Queen)).is_promotion());
        assert!(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)).is_promotion());
    }
}
