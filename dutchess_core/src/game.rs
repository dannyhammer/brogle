use std::fmt;
use std::ops::{Index, IndexMut};
use std::str::FromStr;

use chess::{Board, MoveGen};

use crate::utils::DEFAULT_FEN;
use crate::{ChessBoard, Color, File, Move, Piece, PieceKind, Rank, Tile};

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
        s.apply_moves(moves);
        s
    }

    pub fn from_fen(fen: &str) -> Result<Self, String> {
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
    pub fn apply_move(&mut self, chessmove: Move) {
        // Remove the piece from it's previous location
        let Some(mut piece) = self.state.clear(chessmove.src()) else { return; };

        // Perform promotion, if possible.
        if let Some(promotion) = chessmove.promote() {
            piece.promote(promotion)
        }

        // Place the piece in it's new position
        self.state.set(chessmove.dst(), piece);

        // Record the move in history
        self.history.push(chessmove);

        // Next player's turn
        self.state.toggle_current_player();
    }

    pub fn apply_moves(&mut self, moves: impl IntoIterator<Item = Move>) {
        moves
            .into_iter()
            .for_each(|chessmove| self.apply_move(chessmove))
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

/// Represents the current state of the game, including move counters
///
/// Analogous to a FEN string.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct GameState {
    /// Can be indexed by [`Position`]
    pieces: [Option<Piece>; 64],

    /// BitBoard representation of the game board.
    board: ChessBoard,

    current_player: Color,
    can_white_kingside_castle: bool,
    can_white_queenside_castle: bool,
    can_black_kingside_castle: bool,
    can_black_queenside_castle: bool,
    en_passant_tile: Option<Tile>,

    /// Used to enforce the fifty-move rule.
    /// Is incremented after each move.
    /// Is reset after a capture or a pawn moves.
    halfmove: u8,

    /// Number of moves since the beginning of the game/
    /// A fullmove is a complete turn by white and then by black.
    fullmove: u8,
}

impl GameState {
    pub fn from_fen(fen: &str) -> Result<Self, &'static str> {
        let mut split = fen.split(' ');
        let placements = split.next().ok_or("Empty string is not valid FEN")?;

        if placements.matches('/').count() != 7 {
            return Err(
                "Valid FEN placements must have 8 rows delimited with 7 forward slashes `/`",
            );
        }

        let mut pieces = [None; 64];
        // Have to reverse this so that white appears on the bottom
        for (rank, placements) in placements.split('/').rev().enumerate() {
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

        let board = ChessBoard::from(pieces);

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
                let Ok(pos) = Tile::from_uci(tile) else { return Err("Invalid En Passant target when parsing FEN string") };
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
            board,
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

    /// Fetch the piece at the requested position, if it exists.
    pub const fn get(&self, tile: Tile) -> Option<Piece> {
        *self.piece(tile)
    }

    /// Set the piece at the specified position.
    pub fn set(&mut self, tile: Tile, piece: Piece) {
        self.board.set(tile, piece); // Update the bitboards
        *self.piece_mut(tile) = Some(piece); // Update the state
    }

    /// Remove a piece from the specified position, returning it if it exists.
    pub fn clear(&mut self, tile: Tile) -> Option<Piece> {
        self.board.clear(tile); // Update the bitboards
        self.piece_mut(tile).take() // Update the state
    }

    pub const fn piece(&self, tile: Tile) -> &Option<Piece> {
        &self.pieces[tile.index()]
    }

    pub fn piece_mut(&mut self, tile: Tile) -> &mut Option<Piece> {
        &mut self.pieces[tile.index()]
    }

    pub fn current_player(&self) -> Color {
        self.current_player
    }

    pub fn toggle_current_player(&mut self) {
        println!("Current player is now {}", self.current_player.opponent());
        self.current_player = self.current_player.opponent();
    }

    pub fn legal_moves(&self) -> impl Iterator<Item = Move> {
        let fen = self.to_fen();
        println!("FEN: {fen}");
        let board = Board::from_str(&fen).unwrap();

        println!("{board}");
        println!("{board:?}");

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
    }
}

impl Index<Tile> for GameState {
    type Output = Option<Piece>;
    fn index(&self, index: Tile) -> &Self::Output {
        &self.pieces[index]
    }
}

impl IndexMut<Tile> for GameState {
    fn index_mut(&mut self, index: Tile) -> &mut Self::Output {
        &mut self.pieces[index]
    }
}

impl Default for GameState {
    fn default() -> Self {
        // Safe unwrap: Default FEN is always valid
        Self::from_fen(DEFAULT_FEN).unwrap()
    }
}

impl fmt::Display for GameState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board = String::with_capacity(577);

        for rank in Rank::iter() {
            for _ in File::iter() {
                board += "+---";
            }

            board += "+\n";

            for file in File::iter() {
                let occupant = if let Some(piece) = self.piece(file + rank) {
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

        write!(f, "{board}")
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
