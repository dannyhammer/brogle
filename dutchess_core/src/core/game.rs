use std::fmt;

use crate::core::Piece;

use super::{
    moves_for, utils::DEFAULT_FEN, BitBoard, ChessBoard, ChessError, Color, Move, MoveKind, Tile,
};

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Game {
    init: Position,
    state: Position,
    history: Vec<Move>,
}

impl Game {
    pub fn new(init: Position, moves: impl IntoIterator<Item = Move>) -> Self {
        let mut s = Self::default();
        s.init = init;
        s.state = init;
        s.make_moves(moves);
        s
    }

    pub fn default_setup() -> Self {
        Self::from_fen(DEFAULT_FEN).unwrap()
    }

    pub fn from_fen(fen: &str) -> Result<Self, ChessError> {
        let state = Position::new().from_fen(fen)?;
        Ok(Self::new(state, []))
    }

    pub const fn state(&self) -> &Position {
        &self.state
    }

    pub fn state_mut(&mut self) -> &mut Position {
        &mut self.state
    }

    pub fn history(&self) -> &[Move] {
        &self.history
    }

    pub fn make_move(&mut self, chessmove: Move) {
        // Remove the piece from it's previous location, exiting early if there is no piece there
        let Some(mut piece) = self.state.board.take(chessmove.from()) else {
            return;
        };

        // Handle special cases like promotions, castling, and en passant
        match chessmove.kind() {
            MoveKind::Promote(promotion) => piece.promote(promotion),
            MoveKind::EnPassantCapture => {
                // In En Passant, we need to remove the piece from one rank behind
                // Safe unwrap because the pawn is always guaranteed to be in front of this location
                let captured = chessmove.to().backward_by(piece.color(), 1).unwrap();
                self.state.board.clear(captured)
            }
            _ => {}
        }

        // Place the piece in it's new position
        self.state.board.set(piece, chessmove.to());

        // Record the move in history
        self.history.push(chessmove);

        // Increment move counters
        self.state.halfmove += 1;
        self.state.fullmove = self.state.halfmove / 2;

        // Next player's turn
        self.state.toggle_current_player();
    }

    pub fn unmake_move(&mut self) {
        let Some(chessmove) = self.history.pop() else {
            return;
        };

        // Safe unwrap because there is guaranteed to be a piece at the destination of a move.
        let piece = self.state.board.take(chessmove.to()).unwrap();

        // Return the piece to it's original tile
        self.state.board.set(piece, chessmove.from());

        match chessmove.kind() {
            MoveKind::Quiet => {}
            MoveKind::Capture(kind) => {
                // Put the captured piece back
                let captured = Piece::new(piece.color().opponent(), kind);
                self.state.board.set(captured, chessmove.to())
            }
            _ => todo!(),
        }

        // Decrement move counters
        self.state.halfmove -= 1;
        self.state.fullmove = self.state.halfmove / 2;

        // Previous player's turn
        self.state.toggle_current_player();
    }

    pub fn make_moves(&mut self, moves: impl IntoIterator<Item = Move>) {
        moves
            .into_iter()
            .for_each(|chessmove| self.make_move(chessmove))
    }

    pub fn unmake_moves(&mut self, count: usize) {
        (0..count).for_each(|_| self.unmake_move());
    }

    pub fn get_legal_moves(&self) -> impl IntoIterator<Item = Move> {
        let moves = self.legal_moves();

        moves
            .into_iter()
            .enumerate()
            .map(|(from_index, bitboard)| {
                let from = Tile::from_index_unchecked(from_index);
                bitboard
                    .into_iter()
                    .map(move |to| Move::new_quiet(from, to))
            })
            .flatten()
    }

    pub fn legal_moves(&self) -> [BitBoard; Tile::COUNT] {
        let mut moves = [BitBoard::default(); Tile::COUNT];

        let current_player_pieces = self.state.board().color(self.state.current_player());
        for tile in current_player_pieces {
            moves[tile] = self.legal_moves_at(tile);
        }

        moves
    }

    pub fn legal_moves_at(&self, tile: Tile) -> BitBoard {
        let Some(piece) = self.state.board.piece_at(tile) else {
            return BitBoard::default();
        };

        // TODO: I don't like this
        moves_for(&piece, tile, &self.state)
    }
}

impl Default for Game {
    fn default() -> Self {
        Self {
            state: Position::default(),
            init: Position::default(),
            history: Vec::with_capacity(128),
        }
    }
}

impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let title =
            "+---+---+---+---+---+---+---+---+ GAME STATE +---+---+---+---+---+---+---+---+";
        let current = self.state.current_player();
        let board = self.state.board();
        let en_passant = if let Some(tile) = self.state.ep_tile() {
            tile.to_string()
        } else {
            String::from("None")
        };

        let fen = self.state.to_fen();

        write!(
            f,
            "{title}\n\n\tCurrent Player: {current}\n\n\tEn Passant? {en_passant}\n\n{board}\n\nFEN: {fen}"
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct CastlingRights {
    white_kingside: bool,
    white_queenside: bool,
    black_kingside: bool,
    black_queenside: bool,
}

impl CastlingRights {
    fn from_uci(castling: &str) -> Result<Self, ChessError> {
        if castling.is_empty() {
            Err(ChessError::InvalidCastlingRights)
        } else {
            Ok(Self {
                white_kingside: castling.contains('K'),
                white_queenside: castling.contains('Q'),
                black_kingside: castling.contains('k'),
                black_queenside: castling.contains('q'),
            })
        }
    }

    fn to_uci(&self) -> String {
        let mut castling = String::with_capacity(4);

        if self.white_kingside {
            castling.push_str("K");
        }
        if self.white_queenside {
            castling.push_str("Q");
        }
        if self.black_kingside {
            castling.push_str("k");
        }
        if self.black_queenside {
            castling.push_str("q")
        }

        if castling.is_empty() {
            String::from("-")
        } else {
            castling
        }
    }
}

/// Represents the current state of the game, including move counters
///
/// Analogous to a FEN string.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Position {
    /// Mailbox representation of game board
    // pieces: [Option<Piece>; Tile::COUNT],

    /// BitBoard representation of the game board.
    board: ChessBoard,

    current_player: Color,
    castling_rights: CastlingRights,
    ep_tile: Option<Tile>,

    /// Used to enforce the fifty-move rule.
    /// Is incremented after each move.
    /// Is reset after a capture or a pawn moves.
    halfmove: usize,

    /// Number of moves since the beginning of the game.
    /// A fullmove is a complete turn by white and then by black.
    fullmove: usize,
}

impl Position {
    /// Creates a new, empty [`Position`] with the following properties:
    /// * No pieces on the board
    /// * White moves first
    /// * No castling rights
    /// * No en passant tile available
    /// * Halfmove and fullmove counters set to 0
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::Position;
    /// let state = Position::new();
    /// assert_eq!(state.to_fen(), "8/8/8/8/8/8/8/8 w - - 0 0");
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new [`Position`] with the standard chess setup.
    /// * Pieces placed in standard positions
    /// * White moves first
    /// * All castling rights
    /// * No en passant tile available
    /// * Halfmove and fullmove counters set to 0
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::Position;
    /// let state = Position::new().with_default_setup();
    /// assert_eq!(state.to_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// ```
    pub fn with_default_setup(self) -> Self {
        self.from_fen(DEFAULT_FEN).unwrap()
    }

    // pub fn with_pieces(mut self, placements: impl IntoIterator<Item = (Tile, Piece)>) -> Self {
    //     for (tile, piece) in placements {
    //         self.pieces[tile] = Some(piece);
    //     }
    //     self
    // }

    pub fn with_current_player(mut self, color: Color) -> Self {
        self.current_player = color;
        self
    }

    pub fn with_castling_rights(mut self, castling_rights: CastlingRights) -> Self {
        self.castling_rights = castling_rights;
        self
    }

    pub fn with_ep_tile(mut self, ep_tile: Tile) -> Self {
        self.ep_tile = Some(ep_tile);
        self
    }

    pub fn with_halfmove(mut self, halfmove: usize) -> Self {
        self.halfmove = halfmove;
        self
    }

    pub fn with_fullmove(mut self, fullmove: usize) -> Self {
        self.fullmove = fullmove;
        self
    }

    pub fn from_fen(mut self, fen: &str) -> Result<Self, ChessError> {
        let mut split = fen.split(' ');
        let placements = split.next().ok_or(ChessError::InvalidFenString)?;
        self.board = ChessBoard::from_fen(placements)?;

        let active_color = split.next().unwrap_or_else(|| {
            let x = "w";
            eprintln!("Warning: No active color specified; defaulting to {x}");
            x
        });
        self.current_player = Color::from_str(active_color)?;

        let castling = split.next().unwrap_or_else(|| {
            let x = "KQkq";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        self.castling_rights = CastlingRights::from_uci(castling)?;

        let en_passant_target = split.next().unwrap_or_else(|| {
            let x = "-";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        self.ep_tile = match en_passant_target {
            "-" => None,
            tile => Some(Tile::from_uci(tile)?),
        };

        let halfmove = split.next().unwrap_or_else(|| {
            let x = "0";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        self.halfmove = halfmove.parse().or(Err(ChessError::InvalidFenString))?;

        let fullmove = split.next().unwrap_or_else(|| {
            let x = "1";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        self.fullmove = fullmove.parse().or(Err(ChessError::InvalidFenString))?;

        Ok(self)
    }

    /// Generates a FEN string from this [`Position`].
    pub fn to_fen(&self) -> String {
        let placements = self.board.fen();
        let active_color = self.current_player;
        let castling = self.castling_rights.to_uci();

        let en_passant_target = if let Some(tile) = self.ep_tile {
            tile.to_string()
        } else {
            String::from("-")
        };

        let halfmove = self.halfmove;
        let fullmove = self.fullmove;

        format!("{placements} {active_color} {castling} {en_passant_target} {halfmove} {fullmove}")
    }

    /*
    pub const fn get(&self, tile: Tile) -> Option<Piece> {
        self.board.piece_at(tile)
    }

    /// Set the piece at the specified position.
    pub fn set(&mut self, piece: Piece, tile: Tile) {
        self.board.set(piece, tile); // Update the bitboards

        // *self.piece_mut(tile) = Some(piece); // Update the state
    }

    /// Remove a piece from the specified position, returning it if it exists.
    pub fn clear(&mut self, tile: Tile) -> Option<Piece> {
        self.board.clear(tile); // Update the bitboards

        // self.piece_mut(tile).take() // Update the state
        // self.piece(tile).take();
        None
    }
     */

    /*
    pub const fn piece(&self, tile: Tile) -> &Option<Piece> {
        // &self.board.piece_at(tile)
        self.board.piece_at(tile)
    }
     */

    /*
    pub fn piece_mut(&mut self, tile: Tile) -> &mut Option<Piece> {
        &mut self.pieces[tile.index()]
        self.board.piece_at(tile)
    }
     */

    pub const fn current_player(&self) -> Color {
        self.current_player
    }

    pub const fn board(&self) -> &ChessBoard {
        &self.board
    }

    pub const fn ep_tile(&self) -> Option<Tile> {
        self.ep_tile
    }

    pub fn toggle_current_player(&mut self) {
        // println!("Current player is now {}", self.current_player.opponent());
        self.current_player = self.current_player.opponent();
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            // pieces: [None; Tile::COUNT],
            board: ChessBoard::default(),
            current_player: Color::White,
            castling_rights: CastlingRights::default(),
            ep_tile: None,
            halfmove: 0,
            fullmove: 0,
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_fen())
    }
}
