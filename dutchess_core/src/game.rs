use std::fmt;

use crate::utils::DEFAULT_FEN;
use crate::{ChessBoard, ChessError, Color, File, Move, Piece, PieceKind, Rank, Tile};

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
        let state = GameState::from_fen(fen)?;
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
        let Some(mut piece) = self.state.clear(chessmove.src()) else {
            return;
        };

        // Perform promotion, if possible.
        if let Some(promotion) = chessmove.promote() {
            piece.promote(promotion)
        }

        // Place the piece in it's new position
        self.state.set(piece, chessmove.dst());

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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
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
        let wk = if self.white_kingside { 'K' } else { ' ' };
        let wq = if self.white_queenside { 'Q' } else { ' ' };
        let bk = if self.black_kingside { 'k' } else { ' ' };
        let bq = if self.black_queenside { 'q' } else { ' ' };
        let castling = format!("{wk}{wq}{bk}{bq}");

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
    /// Can be indexed by [`Tile`]
    // pieces: [Option<Piece>; 64],

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
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_fen(fen: &str) -> Result<Self, ChessError> {
        let mut split = fen.split(' ');
        let placements = split.next().ok_or(ChessError::InvalidFenString)?;
        let board = ChessBoard::from_fen(placements)?;

        let active_color = split.next().unwrap_or_else(|| {
            let x = "w";
            eprintln!("Warning: No active color specified; defaulting to {x}");
            x
        });
        let current_player = Color::from_str(active_color)?;

        let castling = split.next().unwrap_or_else(|| {
            let x = "KQkq";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let castling_rights = CastlingRights::from_uci(castling)?;

        let en_passant_target = split.next().unwrap_or_else(|| {
            let x = "-";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let en_passant_tile = match en_passant_target {
            "-" => None,
            tile => Some(Tile::from_uci(tile)?),
        };

        let halfmove = split.next().unwrap_or_else(|| {
            let x = "0";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let halfmove = halfmove.parse().or(Err(ChessError::InvalidFenString))?;

        let fullmove = split.next().unwrap_or_else(|| {
            let x = "1";
            eprintln!("Warning: No castling availability specified; defaulting to {x}");
            x
        });
        let fullmove = fullmove.parse().or(Err(ChessError::InvalidFenString))?;

        Ok(Self {
            // pieces,
            board,
            current_player,
            castling_rights,
            en_passant_tile,
            halfmove,
            fullmove,
        })

        // Ok(state)
    }

    pub fn to_fen(&self) -> String {
        let mut placements: [String; 8] = Default::default();

        for rank in Rank::iter() {
            let mut empty_spaces = 0;
            for file in File::iter() {
                if let Some(piece) = self.board.piece_at(file * rank) {
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
        let placements = placements.join("/");

        let active_color = self.current_player;

        let mut castling = self.castling_rights.to_uci();
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
        // Safe unwrap: Default FEN is always valid
        Self::from_fen(DEFAULT_FEN).unwrap()
    }
}

impl fmt::Display for GameState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let title =
            "+---+---+---+---+---+---+---+---+ GAME STATE +---+---+---+---+---+---+---+---+";
        let current = self.current_player;
        let board = self.board;
        let castling_helper = |can_castle| if can_castle { "Yes" } else { "No" };
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

/*
impl From<chess::Board> for GameState {
    fn from(value: chess::Board) -> Self {


        Self {
            pieces,
            board,
            current_player: (),
            can_white_kingside_castle: (),
            can_white_queenside_castle: (),
            can_black_kingside_castle: (),
            can_black_queenside_castle: (),
            en_passant_tile: (),
            halfmove: (),
            fullmove: (),
        }
    }
}
 */
