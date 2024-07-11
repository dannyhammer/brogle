use std::fmt;

use crate::{ChessError, PieceKind, Tile};

/// Represents which side castling can occur on.
pub enum CastleSide {
    Queenside,
    Kingside,
}

/// Represents the different kinds of moves that can be made during a chess game.
pub enum MoveType {
    /// Involves only a single piece moving from one location to another, and does not change the quantity or kind of any pieces on the board.
    Quiet,

    /// Involves a piece moving onto a square occupied by an opponent's piece, removing it from the board.
    Capture(PieceKind),

    /// A special variant of capturing that occurs when a Pawn executes En Passant.
    EnPassantCapture,

    /// Involves the King and a Rook sliding past each other, either on the King's side or the Queen's side.
    /// See [`CastleSide`] for more.
    Castle(CastleSide),

    /// Involves a Pawn reaching the opponent's side of the board (rank 8 for White, rank 1 for Black) and becoming another kind of piece, such as a Knight or Queen.
    Promote(PieceKind),

    /// A special case on a Pawn's first move, wherein it can advance two squares forward.
    PawnPushTwo,
}

/// Represents a move made on a chess board, including where a piece is to be promoted.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Move {
    from: Tile,
    to: Tile,
    promote: Option<PieceKind>,
}

impl Move {
    /// Creates a new [`Move`] from the given [`Tile`]s and optional [`PieceKind`] for promotion.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Move, Tile, PieceKind};
    /// let e7e8Q = Move::new(Tile::E7, Tile::E8, Some(PieceKind::Queen));
    /// assert_eq!(e7e8Q.to_string(), "e7e8Q");
    /// ```
    pub fn new(from: Tile, to: Tile, promote: Option<PieceKind>) -> Self {
        Self { from, to, promote }
    }

    /// Creates a new [`Move`] from the given [`Tile`]s that does promote a piece.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Move, Tile};
    /// let e2e4 = Move::new_quiet(Tile::E2, Tile::E4);
    /// assert_eq!(e2e4.to_string(), "e2e4");
    /// ```
    pub fn new_quiet(from: Tile, to: Tile) -> Self {
        Self::new(from, to, None)
    }

    /// Creates an "illegal" [`Move`], representing moving a piece to and from the same [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Move;
    /// let illegal = Move::illegal();
    /// assert_eq!(illegal.to_string(), "a1a1");
    /// ```
    pub fn illegal() -> Self {
        Self::new(Tile::A1, Tile::A1, None)
    }

    /// Fetches the source (or "from") part of this [`Move`], as a [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Move, Tile};
    /// let e2e4 = Move::new(Tile::E2, Tile::E4, None);
    /// let from = e2e4.src();
    /// assert_eq!(from, Tile::E2);
    /// ```
    pub fn src(&self) -> Tile {
        self.from
    }

    /// Fetches the destination (or "to") part of this [`Move`], as a [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Move, Tile};
    /// let e2e4 = Move::new(Tile::E2, Tile::E4, None);
    /// let to = e2e4.dst();
    /// assert_eq!(to, Tile::E4);
    /// ```
    pub fn dst(&self) -> Tile {
        self.to
    }

    /// Fetches the destination (or "to") part of this [`Move`], as a [`Tile`].
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Move, Tile, PieceKind};
    /// let e7e8Q = Move::new(Tile::E7, Tile::E8, Some(PieceKind::Queen));
    /// assert_eq!(e7e8Q.promote(), Some(PieceKind::Queen));
    /// ```
    pub fn promote(&self) -> Option<PieceKind> {
        self.promote
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

    /// Creates a [`Move`] from a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// Will return a [`ChessError`] if the string is invalid in any way.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Move, Tile, PieceKind};
    /// let e7e8Q = Move::from_uci("e7e8Q");
    /// assert_eq!(e7e8Q, Ok(Move::new(Tile::E7, Tile::E8, Some(PieceKind::Queen))));
    /// ```
    pub fn from_uci(uci: &str) -> Result<Self, ChessError> {
        let from = uci.get(0..2).ok_or(ChessError::InvalidTileNotation)?;
        let to = uci.get(2..4).ok_or(ChessError::InvalidTileNotation)?;

        let from = Tile::from_uci(from)?;
        let to = Tile::from_uci(to)?;

        let promote = if let Some(promote) = uci.get(4..5) {
            Some(PieceKind::from_str(promote)?)
        } else {
            None
        };

        Ok(Self::new(from, to, promote))
    }

    /// Converts this [`Move`] to a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// Please note that promotions are capitalized by default.
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::{Move, Tile, PieceKind};
    /// let e7e8Q = Move::new(Tile::E7, Tile::E8, Some(PieceKind::Queen));
    /// assert_eq!(e7e8Q.to_uci(), "e7e8Q");
    /// ```
    pub fn to_uci(&self) -> String {
        if let Some(promote) = self.promote() {
            format!("{}{}{}", self.src(), self.dst(), promote)
        } else {
            format!("{}{}", self.src(), self.dst())
        }
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}
