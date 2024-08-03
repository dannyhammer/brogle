use std::{
    fmt,
    ops::{Deref, Index, IndexMut},
};

use anyhow::{anyhow, bail, Result};

use crate::MoveGenerator;

use super::{
    utils::FEN_STARTPOS, BitBoard, Color, File, Move, MoveKind, Piece, PieceKind, Rank, Tile,
    NUM_COLORS, NUM_PIECE_TYPES,
};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum GameState {
    Playing,
    Stalemate,
    Checkmate,
    Check,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum GameResult {
    Resign(Color),
    Checkmate(Color),
    Stalemate,
    Draw,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Game {
    movegen: MoveGenerator,
    history: Vec<Position>,
}

impl Game {
    pub fn new() -> Self {
        Self {
            movegen: MoveGenerator::new_legal(Position::new()),
            history: Vec::with_capacity(128),
        }
    }

    pub fn from_fen(fen: &str) -> Result<Self> {
        let position = Position::from_fen(fen)?;
        Ok(Self {
            movegen: MoveGenerator::new_legal(position),
            history: Vec::with_capacity(128),
        })
    }

    pub fn with_move_made(&self, chessmove: Move) -> Self {
        let mut new = self.clone();
        new.make_move(chessmove);
        new
    }

    pub fn is_repetition(&self) -> bool {
        for prev_pos in self.history.iter().rev().skip(1).step_by(2) {
            if prev_pos == self.position() {
                return true;
            }
            // if prev_pos.halfmove == 0 {
            //     return false;
            // }
        }
        return false;
    }

    pub fn movegen(&self) -> &MoveGenerator {
        &self.movegen
    }

    pub fn movegen_mut(&mut self) -> &mut MoveGenerator {
        &mut self.movegen
    }

    pub fn make_move(&mut self, chessmove: Move) {
        self.history.push(self.position.clone());
        let new_pos = self.position().clone().with_move_made(chessmove);
        self.movegen = MoveGenerator::new_legal(new_pos);
    }

    pub fn make_moves(&mut self, moves: impl IntoIterator<Item = Move>) {
        for chessmove in moves {
            self.make_move(chessmove);
        }
    }
}

impl Deref for Game {
    type Target = MoveGenerator;
    fn deref(&self) -> &Self::Target {
        self.movegen()
    }
}

/// Represents the castling rights of both players
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct CastlingRights {
    pub(crate) kingside: [bool; 2],
    pub(crate) queenside: [bool; 2],
}

impl CastlingRights {
    const fn new() -> Self {
        Self {
            kingside: [false; 2],
            queenside: [false; 2],
        }
    }

    fn from_uci(castling: &str) -> Result<Self> {
        if castling.is_empty() {
            bail!("Invalid castling rights: Got empty string.");
        } else {
            let mut kingside = [false; 2];
            let mut queenside = [false; 2];
            kingside[Color::White] = castling.contains('K');
            queenside[Color::White] = castling.contains('Q');
            kingside[Color::Black] = castling.contains('k');
            queenside[Color::Black] = castling.contains('q');
            Ok(Self {
                kingside,
                queenside,
            })
        }
    }

    fn to_uci(&self) -> String {
        let mut castling = String::with_capacity(4);

        if self.kingside[Color::White] {
            castling.push_str("K");
        }
        if self.queenside[Color::White] {
            castling.push_str("Q");
        }
        if self.kingside[Color::Black] {
            castling.push_str("k");
        }
        if self.queenside[Color::Black] {
            castling.push_str("q")
        }

        if castling.is_empty() {
            String::from("-")
        } else {
            castling
        }
    }
}

impl fmt::Display for CastlingRights {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_uci())
    }
}

/// Represents the current state of the game, including move counters
///
/// Analogous to a FEN string.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Position {
    /// BitBoard representation of the game board.
    bitboards: ChessBoard,

    /// Side to move.
    current_player: Color,

    /// Castling rights for each player.
    castling_rights: CastlingRights,

    /// Optional attack square for en passant.
    ep_tile: Option<Tile>,

    /// Used to enforce the fifty-move rule.
    /// - Is incremented after each move.
    /// - Is reset after a capture or a pawn moves.
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
    /// * Halfmove counter set to 0
    /// * Fullmove counter set to 1
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Position;
    /// let state = Position::new();
    /// assert_eq!(state.to_fen(), "8/8/8/8/8/8/8/8 w - - 0 1");
    /// ```
    pub fn new() -> Self {
        Self {
            bitboards: ChessBoard::new(),
            current_player: Color::White,
            castling_rights: CastlingRights::new(),
            ep_tile: None,
            halfmove: 0,
            fullmove: 1,
        }
    }

    /// Creates a new [`Position`] with the standard chess setup.
    /// * Pieces placed in standard positions
    /// * White moves first
    /// * All castling rights
    /// * No en passant tile available
    /// * Halfmove counter set to 0
    /// * Fullmove counter set to 1
    ///
    /// # Example
    /// ```
    /// # use dutchess_core::Position;
    /// let state = Position::standard_setup();
    /// assert_eq!(state.to_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// ```
    pub fn standard_setup() -> Self {
        Self::from_fen(FEN_STARTPOS).unwrap()
    }

    pub fn from_fen(fen: &str) -> Result<Self> {
        let mut pos = Self::new();
        let mut split = fen.trim().split(' ');
        let placements = split.next().ok_or(anyhow!(
            "Invalid FEN string: FEN string must have piece placements."
        ))?;
        pos.bitboards = ChessBoard::from_fen(placements)?;

        let active_color = split.next().unwrap_or_else(|| "w");
        pos.current_player = Color::from_str(active_color)?;

        let castling = split.next().unwrap_or_else(|| "KQkq");
        pos.castling_rights = CastlingRights::from_uci(castling)?;

        let en_passant_target = split.next().unwrap_or_else(|| "-");
        pos.ep_tile = match en_passant_target {
            "-" => None,
            tile => Some(Tile::from_uci(tile)?),
        };

        let halfmove = split.next().unwrap_or_else(|| "0");
        pos.halfmove = halfmove.parse().or(Err(anyhow!(
            "Invalid FEN string: FEN string must have valid halfmove counter. Got {halfmove}"
        )))?;

        let fullmove = split.next().unwrap_or_else(|| "1");
        pos.fullmove = fullmove.parse().or(Err(anyhow!(
            "Invalid FEN string: FEN string must have valid fullmove counter. Got {fullmove}"
        )))?;

        Ok(pos)
    }

    pub fn with_move_made(mut self, chessmove: Move) -> Self {
        self.make_move(chessmove);
        self
    }

    /// Generates a FEN string from this [`Position`].
    pub fn to_fen(&self) -> String {
        let placements = self.board().fen();
        let active_color = self.current_player();
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

    pub const fn current_player(&self) -> Color {
        self.current_player
    }

    pub const fn ep_tile(&self) -> Option<Tile> {
        self.ep_tile
    }

    pub fn ep_target_tile(&self) -> Option<Tile> {
        self.ep_tile()
            .map(|ep_tile| ep_tile.backward_by(self.current_player(), 1).unwrap())
    }

    pub const fn castling_rights(&self) -> &CastlingRights {
        &self.castling_rights
    }

    pub const fn halfmove(&self) -> usize {
        self.halfmove
    }

    pub const fn fullmove(&self) -> usize {
        self.fullmove
    }

    pub fn toggle_current_player(&mut self) {
        self.current_player = self.current_player.opponent();
    }

    pub const fn board(&self) -> &ChessBoard {
        &self.bitboards
    }

    pub fn board_mut(&mut self) -> &mut ChessBoard {
        &mut self.bitboards
    }

    pub const fn can_castle(&self, color: Color) -> bool {
        self.castling_rights().kingside[color.index()]
            || self.castling_rights().queenside[color.index()]
    }

    pub const fn times_castled(&self) -> usize {
        let [wk, bk] = self.castling_rights.kingside;
        let [wq, bq] = self.castling_rights.queenside;

        !wk as usize + !wq as usize + !bk as usize + !bq as usize
    }

    // Checks if the provided move is legal to perform
    pub fn is_legal(&self, chessmove: Move) -> (bool, &str) {
        let (from, to, kind) = chessmove.parts();

        // If there's no piece here, illegal move
        let Some(piece) = self.board().piece_at(from) else {
            return (false, "No piece here to move");
        };

        // If it's not this piece's color's turn, illegal move
        if piece.color() != self.current_player() {
            return (false, "Tried to move a piece that wasn't yours");
        }

        // If this move captures a piece, handle those cases
        if let Some(to_capture) = self.board().piece_at(to) {
            // Can't capture own pieces
            if to_capture.color() == piece.color() {
                return (false, "Tried to capture your own piece");
            }

            // Can't capture king
            if to_capture.is_king() {
                return (false, "Tried to capture enemy king");
            }

            // Ensure that the move is a capture or en passant, and that it captures the correct piece
            if !chessmove.is_capture() {
                return (false, "Captured on a non-capture move");
            }
        }

        match kind {
            // If the move is pawn-specific, ensure it's a pawn moving
            MoveKind::EnPassantCapture | MoveKind::PawnPushTwo | MoveKind::Promote(_) => {
                if !piece.is_pawn() {
                    return (false, "Tried to do a pawn move (EP, Push 2, Promote) with a piece that isn't a pawn");
                }
            }
            // If castling, ensure we have the right to
            MoveKind::KingsideCastle => {
                if self.castling_rights.kingside[piece.color()] == false {
                    return (false, "Tried to castle (kingside) without rights");
                }
            }
            // If castling, ensure we have the right to
            MoveKind::QueensideCastle => {
                if self.castling_rights.queenside[piece.color()] == false {
                    return (false, "Tried to castle (queenside) without rights");
                }
            }
            // Quiet moves are fine
            _ => {}
        }

        (true, "")
    }

    pub fn make_move_checked(&mut self, chessmove: Move) -> Result<(), String> {
        let (is_legal, reason) = self.is_legal(chessmove);
        if is_legal {
            Ok(self.make_move(chessmove))
        } else {
            Err(format!("{reason}"))
        }
    }

    pub fn make_moves(&mut self, moves: impl IntoIterator<Item = Move>) {
        for chessmove in moves {
            self.make_move(chessmove);
        }
    }

    /// Applies the move. No enforcement of legality
    pub fn make_move(&mut self, chessmove: Move) {
        // Remove the piece from it's previous location, exiting early if there is no piece there
        let Some(mut piece) = self.board_mut().take(chessmove.from()) else {
            return;
        };

        let color = piece.color();
        let to = chessmove.to();
        let from = chessmove.from();

        // Clear the EP tile from the last move
        self.ep_tile = None;

        // Increment move counters
        self.halfmove += 1; // This is reset if a capture occurs or a pawn moves
        self.fullmove += self.current_player().index();

        // First, deal with special cases like captures and castling
        if chessmove.is_capture() {
            // If this move was en passant, the piece we captured isn't at `to`, it's one square behind
            let captured_tile = if chessmove.is_en_passant() {
                to.backward_by(color, 1).unwrap()
            } else {
                to
            };

            let captured = self.board_mut().take(captured_tile).unwrap();
            let captured_color = captured.color();

            // If the capture was on a rook's starting square, disable that side's castling.
            // Either a rook was captured, or there wasn't a rook there, in which case castling on that side is already disabled
            let can_queenside = to != [Tile::A1, Tile::A8][captured_color];
            let can_kingside = to != [Tile::H1, Tile::H8][captured_color];

            self.castling_rights.queenside[captured_color] &= can_queenside;
            self.castling_rights.kingside[captured_color] &= can_kingside;

            // Reset halfmove counter, since a capture occurred
            self.halfmove = 0;
        } else if chessmove.is_pawn_double_push() {
            // Double pawn push, so set the EP square
            self.ep_tile = from.forward_by(color, 1);
        } else if chessmove.is_castle() {
            let castle_index = chessmove.is_short_castle() as usize;
            let old_rook_tile = [Tile::A1, Tile::H1][castle_index].rank_relative_to(color);
            let new_rook_tile = [Tile::D1, Tile::F1][castle_index].rank_relative_to(color);

            // Move the rook. The King is already handled before and after this match statement.
            let rook = self.board_mut().take(old_rook_tile).unwrap();
            self.board_mut().set(rook, new_rook_tile);

            // Disable castling
            self.castling_rights.kingside[color] = false;
            self.castling_rights.queenside[color] = false;
        }

        // Next, handle special cases for pieces like the King
        match piece.kind() {
            PieceKind::Pawn => self.halfmove = 0,
            PieceKind::Rook => {
                // Disable castling if a rook moved
                self.castling_rights.queenside[color] &= from != Tile::A1.rank_relative_to(color);
                self.castling_rights.kingside[color] &= from != Tile::H1.rank_relative_to(color);
            }
            PieceKind::King => {
                // Disable all castling
                self.castling_rights.kingside[color] = false;
                self.castling_rights.queenside[color] = false;
            }
            _ => {}
        }

        // Now we check for promotions, since all special cases for Pawns and Rooks have been dealt with
        if let Some(promotion) = chessmove.promotion() {
            piece = piece.promoted(promotion);
        }

        // Place the piece in it's new position
        self.board_mut().set(piece, to);

        // Next player's turn
        self.toggle_current_player();
    }

    /*
    pub fn unmake_move(&mut self, chessmove: Move) {
        //
    }
    */

    /*
    pub fn unmake_move(&mut self, chessmove: Move) {
        // Safe unwrap because there is guaranteed to be a piece at the destination of a move.
        let mut piece = self.bitboards_mut().take(chessmove.to()).unwrap();

        let color = piece.color();

        // Undo any special cases, like castling and en passant
        match chessmove.kind() {
            MoveKind::Quiet => {
                match piece.kind() {
                    PieceKind::Rook => {
                        // Disable this side's castling
                        let (queenside_rook_tile, kingside_rook_tile) = if color.is_white() {
                            (Tile::A1, Tile::H1)
                        } else {
                            (Tile::A8, Tile::H8)
                        };

                        if chessmove.from() == queenside_rook_tile {
                            // println!("{chessmove} moves a rook. Removing all queenside rights from {color}");
                            self.castling_rights.queenside[color] = false;
                        } else if chessmove.from() == kingside_rook_tile {
                            // println!("{chessmove} moves a rook. Removing all kingside rights from {color}");
                            self.castling_rights.kingside[color] = false;
                        }
                    }
                    PieceKind::King => {
                        // Disable all castling
                        // println!(
                        //     "{chessmove} moves a rook. Removing all castling rights from {color}"
                        // );
                        self.castling_rights.kingside[color] = false;
                        self.castling_rights.queenside[color] = false;
                    }
                    _ => {}
                }
            }
            // Put the captured piece back
            MoveKind::Capture => unimplemented!(), // self.bitboards_mut().set(captured, chessmove.to()),
            MoveKind::KingsideCastle => {
                let (new_king_tile, old_king_tile, new_rook_tile, old_rook_tile) =
                    if piece.color().is_white() {
                        (Tile::E1, Tile::G1, Tile::H1, Tile::F1)
                    } else {
                        (Tile::E8, Tile::G8, Tile::H8, Tile::F8)
                    };

                // Swap king and rook
                let king = self.bitboards_mut().take(old_king_tile).unwrap();
                let rook = self.bitboards_mut().take(old_rook_tile).unwrap();
                self.bitboards_mut().set(king, new_king_tile);
                self.bitboards_mut().set(rook, new_rook_tile);

                // Re-enable castling
                self.castling_rights.kingside[piece.color()] = true;
            }
            MoveKind::QueensideCastle => {
                let (new_king_tile, old_king_tile, new_rook_tile, old_rook_tile) =
                    if piece.color().is_white() {
                        (Tile::E1, Tile::C1, Tile::A1, Tile::D1)
                    } else {
                        (Tile::E8, Tile::C8, Tile::A8, Tile::D8)
                    };

                // Swap king and rook
                let king = self.bitboards_mut().take(old_king_tile).unwrap();
                let rook = self.bitboards_mut().take(old_rook_tile).unwrap();
                self.bitboards_mut().set(king, new_king_tile);
                self.bitboards_mut().set(rook, new_rook_tile);

                // Re-enable castling
                self.castling_rights.queenside[piece.color()] = true;
            }
            // A piece was removed from directly behind the pawn
            MoveKind::EnPassantCapture => {
                // Safe unwrap because the pawn is always guaranteed to be in front of this location
                let ep_tile = chessmove.to();
                let captured_tile = ep_tile.backward_by(piece.color(), 1).unwrap();
                self.bitboards_mut().set(
                    Piece::new(piece.color().opponent(), PieceKind::Pawn),
                    captured_tile,
                );
                self.ep_tile = Some(ep_tile);
            }
            MoveKind::Promote(_) => piece = piece.demoted(),
            MoveKind::PawnPushTwo => self.ep_tile = None,
            _ => unimplemented!(""),
        }

        // Return the piece to it's original tile
        self.bitboards_mut().set(piece, chessmove.from());

        // Decrement move counters
        self.halfmove -= 1;
        self.fullmove -= self.current_player().index();

        // Previous player's turn
        self.toggle_current_player();

        self.history.pop();
    }
     */

    // pub fn game_state(&self) -> GameState {
    //     if self.is_check() {

    //     } else {
    //         GameState::Playing
    //     }
    // }
}

impl Deref for Position {
    type Target = ChessBoard;
    fn deref(&self) -> &Self::Target {
        self.board()
    }
}

impl Default for Position {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_fen())
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ranks = Rank::iter().rev();

        let mut board_str = String::with_capacity(10);
        for rank in ranks {
            board_str += &format!("{rank}|");
            for file in File::iter() {
                let piece = self.board().piece_at(file * rank);
                let piece_char = piece.map(|p| p.char()).unwrap_or('.');
                board_str += &format!(" {piece_char}");
            }

            if rank == Rank(6) {
                board_str += &format!("           FEN: {}", self.to_fen());
            } else if rank == Rank(5) {
                board_str += &format!("          Side: {}", self.current_player());
            } else if rank == Rank(4) {
                board_str += &format!("      Castling: {}", self.castling_rights());
            } else if rank == Rank(3) {
                let ep = self
                    .ep_tile()
                    .map(|t| t.to_uci())
                    .unwrap_or(String::from("-"));
                board_str += &format!("            EP: {ep}",);
            } else if rank == Rank(2) {
                board_str += &format!("     Half-move: {}", self.halfmove());
            } else if rank == Rank(1) {
                board_str += &format!("     Full-move: {}", self.fullmove());
            }
            board_str += "\n";
        }
        board_str += " +";
        for _ in File::iter() {
            board_str += "--";
        }
        board_str += "\n   ";
        for file in File::iter() {
            board_str += &format!("{file} ");
        }

        write!(f, "{board_str}")
    }
}

/// Represents all pieces and their locations on a chess board.
///
/// Has no knowledge of castling rights, en passant, or move counters.
///
/// Internally uses a collection of [`BitBoard`]s to keep track of piece/color locations.
#[derive(Clone, PartialEq, Eq, Hash)]
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
    /// let board = ChessBoard::with_standard_setup();
    /// assert_eq!(board.fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR");
    /// ```
    pub fn with_standard_setup() -> Self {
        // Safe unwrap because the FEN for startpos is always valid
        Self::from_fen(FEN_STARTPOS).unwrap()
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
        for (piece, tile) in pieces.into_iter().zip(tiles) {
            self = self.with_piece(piece, tile);
        }

        self
    }

    /// Constructs a [`ChessBoard`] from the provided FEN string, ignoring castling/ep/move counters.
    pub fn from_fen(fen: &str) -> Result<Self> {
        let mut board = Self::new();

        // If this FEN string contains more than just the initial placements, extract the placements
        let placements = if fen.contains(' ') {
            fen.split(' ').next().unwrap()
        } else {
            fen
        };
        let mut has_both_kings = [false, false];

        // Check if the placements string is the correct length
        if placements.matches('/').count() != 7 {
            bail!("Invalid FEN string: Missing placements for all 8 ranks.");
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

                    board = board.with_piece(piece, tile);

                    file += 1;

                    if piece.is_king() {
                        has_both_kings[piece.color()] = true;
                    }
                } else {
                    // If the next char was not a piece, increment our File counter, checking for errors along the way
                    let Some(empty) = piece_char.to_digit(10) else {
                        bail!("Invalid FEN string: Found non-piece, non-numeric char {piece_char} when parsing FEN.");
                    };
                    file += empty as u8
                }
            }
        }

        if !(has_both_kings[Color::White] && has_both_kings[Color::Black]) {
            bail!("Invalid FEN string: A FEN must have valid placements for both Kings.");
        }

        Ok(board)
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

    /// Returns an instance of this [`ChessBoard`] that has the additional bits specified by `mask` set, according to the [`Piece`] supplied.
    ///
    /// If `mask` contains only 1 tile, use [`ChessBoard::with_piece`] instead, as it is likely to be faster.
    pub const fn with(&self, mask: BitBoard, piece: Piece) -> Self {
        let occupied = self.occupied().or(mask);
        let (color, kind) = piece.parts();

        let mut colors = self.colors;
        colors[color.index()] = colors[color.index()].or(mask);

        let mut pieces = self.pieces;
        pieces[kind.index()] = pieces[kind.index()].or(mask);

        Self {
            occupied,
            colors,
            pieces,
        }
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
    /// let mut board = ChessBoard::new().with_setup("k7/8/8/8/2N5/8/8/7K").unwrap();
    /// board.clear(Tile::C4);
    /// assert_eq!(board.fen(), "k7/8/8/8/8/8/8/7K");
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
    /// let mut board = ChessBoard::new().with_setup("k7/8/8/8/2N5/8/8/7K").unwrap();
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// let taken = board.take(Tile::C4);
    /// assert_eq!(board.fen(), "k7/8/8/8/8/8/8/7K");
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

    /// Fetches a [`BitBoard`] of all occupied squares on the board.
    pub const fn occupied(&self) -> BitBoard {
        self.occupied
    }

    /// Fetches a [`BitBoard`] of all non-occupied squares on the board.
    pub const fn empty(&self) -> BitBoard {
        self.occupied().not()
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

    /// Analogous to [`ChessBoard::piece`] with a [`Piece`]'s individual components
    pub const fn piece_parts(&self, color: Color, kind: PieceKind) -> BitBoard {
        let color = self.color(color);
        let kind = self.kind(kind);
        color.and(kind)
    }

    /// Fetches a [`BitBoard`] containing the locations of all orthogonal sliding pieces (Rook, Queen).
    pub const fn orthogonal_sliders(&self, color: Color) -> BitBoard {
        (self.pieces[PieceKind::Rook.index()].or(self.pieces[PieceKind::Queen.index()]))
            .and(self.color(color))
    }

    /// Fetches a [`BitBoard`] containing the locations of all diagonal sliding pieces (Bishop, Queen).
    pub const fn diagonal_sliders(&self, color: Color) -> BitBoard {
        (self.pieces[PieceKind::Bishop.index()].or(self.pieces[PieceKind::Queen.index()]))
            .and(self.color(color))
    }

    /// Fetches a [`BitBoard`] containing the locations of all sliding pieces (Rook, Bishop, Queen).
    pub const fn sliders(&self, color: Color) -> BitBoard {
        (self.pieces[PieceKind::Rook.index()]
            .or(self.pieces[PieceKind::Bishop.index()])
            .or(self.pieces[PieceKind::Queen.index()]))
        .and(self.color(color))
    }

    /// Fetches the [`BitBoard`] for the King of the provided color.
    pub const fn king(&self, color: Color) -> BitBoard {
        self.piece_parts(color, PieceKind::King)
    }

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
