use std::fmt;

use crate::core::{utils::DEFAULT_FEN, ChessBoard, ChessError, Color, Move, Tile};

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Game {
    init: GameState,
    state: GameState,
    history: Vec<Move>,
}

impl Game {
    pub fn new(init: GameState, moves: impl IntoIterator<Item = Move>) -> Self {
        let mut s = Self::default();
        s.init = init;
        s.state = init;
        s.make_moves(moves);
        s
    }

    pub fn from_fen(fen: &str) -> Result<Self, ChessError> {
        let state = GameState::new().from_fen(fen)?;
        Ok(Self::new(state, []))
    }

    pub const fn state(&self) -> &GameState {
        &self.state
    }

    pub fn state_mut(&mut self) -> &mut GameState {
        &mut self.state
    }

    pub fn history(&self) -> &[Move] {
        &self.history
    }

    /// Returns `true` if the move was made successful, and `false` if it cannot be made.
    pub fn make_move(&mut self, chessmove: Move) {
        // Remove the piece from it's previous location
        let Some(mut piece) = self.state.board.take(chessmove.src()) else {
            return;
        };

        // Perform promotion, if possible.
        if let Some(promotion) = chessmove.promote() {
            piece.promote(promotion)
        }

        // Place the piece in it's new position
        self.state.board.set(piece, chessmove.dst());

        // Record the move in history
        self.history.push(chessmove);

        // Next player's turn
        self.state.toggle_current_player();
    }

    pub fn make_moves(&mut self, moves: impl IntoIterator<Item = Move>) {
        moves
            .into_iter()
            .for_each(|chessmove| self.make_move(chessmove))
    }

    pub fn unmake_move(&mut self) {
        let Some(_chessmove) = self.history.pop() else {
            return;
        };
    }

    pub fn unmake_moves(&mut self, count: usize) {
        for _ in 0..count {
            self.unmake_move();
        }
    }
}

impl Default for Game {
    fn default() -> Self {
        Self {
            state: GameState::default(),
            init: GameState::default(),
            history: Vec::with_capacity(128),
        }
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
pub struct GameState {
    /// BitBoard representation of the game board.
    board: ChessBoard,

    current_player: Color,
    castling_rights: CastlingRights,
    en_passant_tile: Option<Tile>,

    /// Used to enforce the fifty-move rule.
    /// Is incremented after each move.
    /// Is reset after a capture or a pawn moves.
    halfmove: u8,

    /// Number of moves since the beginning of the game.
    /// A fullmove is a complete turn by white and then by black.
    fullmove: u8,
}

impl GameState {
    /// Creates a new, empty [`GameState`] with the following properties:
    /// * No pieces on the board
    /// * White moves first
    /// * No castling rights
    /// * No en passant tile available
    /// * Halfmove and fullmove counters set to 0
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::GameState;
    /// let state = GameState::new();
    /// assert_eq!(state.to_fen(), "8/8/8/8/8/8/8/8 w - - 0 0");
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new [`GameState`] with the standard chess setup.
    /// * Pieces placed in standard positions
    /// * White moves first
    /// * All castling rights
    /// * No en passant tile available
    /// * Halfmove and fullmove counters set to 0
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::core::GameState;
    /// let state = GameState::new().with_default_setup();
    /// assert_eq!(state.to_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// ```
    pub fn with_default_setup(self) -> Self {
        self.from_fen(DEFAULT_FEN).unwrap()
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
        self.en_passant_tile = match en_passant_target {
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

    /// Generates a FEN string from this [`GameState`].
    pub fn to_fen(&self) -> String {
        let placements = self.board.fen();
        let active_color = self.current_player;
        let castling = self.castling_rights.to_uci();

        let en_passant_target = if let Some(tile) = self.en_passant_tile {
            tile.to_string()
        } else {
            String::from("-")
        };

        let halfmove = self.halfmove;
        let fullmove = self.fullmove;

        format!("{placements} {active_color} {castling} {en_passant_target} {halfmove} {fullmove}")
    }

    /// Applies a [`Move`] to this [`GameState`], updating metadata along the way.
    fn make_move(&mut self, chessmove: Move) {
        // Remove the piece from it's previous location, exiting early if there is no piece there
        let Some(mut piece) = self.board.take(chessmove.src()) else {
            return;
        };

        // Perform promotion, if necessary.
        if let Some(promotion) = chessmove.promote() {
            piece.promote(promotion)
        }

        // Place the piece in it's new position
        self.board.set(piece, chessmove.dst());

        // Next player's turn
        self.toggle_current_player();
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

    pub fn current_player(&self) -> Color {
        self.current_player
    }

    pub fn board(&self) -> &ChessBoard {
        &self.board
    }

    pub fn toggle_current_player(&mut self) {
        // println!("Current player is now {}", self.current_player.opponent());
        self.current_player = self.current_player.opponent();
    }

    pub fn legal_moves(&self) -> impl Iterator<Item = Move> {
        // let fen = self.to_fen();
        // println!("FEN: {fen}");
        // let board = Board::from_str(&fen).unwrap();

        // println!("{board}");
        // println!("{board:?}");

        /*
        MoveGen::new_legal(&board).map(|legal| {
            let src = Tile::from_index_unchecked(legal.get_source().to_index());
            let dst = Tile::from_index_unchecked(legal.get_dest().to_index());
            let promotion = legal.get_promotion().map(|piece| {
                use chess::Piece::*;
                match piece {
                    Pawn => PieceKind::Pawn,
                    Knight => PieceKind::Knight,
                    Bishop => PieceKind::Bishop,
                    Rook => PieceKind::Rook,
                    Queen => PieceKind::Queen,
                    King => PieceKind::King,
                }
            });

            Move::new(src, dst, promotion)
        })
         */

        todo!();
        [].into_iter()
    }
}

/*
impl Index<Tile> for GameState {
    type Output = Option<Piece>;
    fn index(&self, index: Tile) -> &Self::Output {
        // &self.pieces[index]
        self.board.piece_at(index)
    }
}
 */

/*
impl IndexMut<Tile> for GameState {
    fn index_mut(&mut self, index: Tile) -> &mut Self::Output {
        &mut self.pieces[index]
    }
}
 */

impl Default for GameState {
    fn default() -> Self {
        Self {
            board: ChessBoard::default(),
            current_player: Color::White,
            castling_rights: CastlingRights::default(),
            en_passant_tile: None,
            halfmove: 0,
            fullmove: 0,
        }
    }
}

impl fmt::Display for GameState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let title =
            "+---+---+---+---+---+---+---+---+ GAME STATE +---+---+---+---+---+---+---+---+";
        let current = self.current_player;
        let board = self.board;
        let en_passant = if let Some(tile) = self.en_passant_tile {
            tile.to_string()
        } else {
            String::from("None")
        };

        let fen = self.to_fen();

        write!(
            f,
            "{title}\n\n\tCurrent Player: {current}\n\n\tEn Passant? {en_passant}\n\n{board}\n\nFEN: {fen}"
        )
    }
}
