use std::{
    fmt,
    ops::{Index, IndexMut},
};

use anyhow::{anyhow, bail, Result};

use super::{
    bishop_attacks, compute_squares_attacked_by, default_attacks_for, king_attacks,
    king_danger_squares, knight_attacks, pawn_attacks, pawn_pushes, queen_moves, ray_between,
    ray_containing, rook_attacks, utils::FEN_STARTPOS, BitBoard, Color, File, Move, MoveKind,
    Piece, PieceKind, Rank, Tile, MAX_NUM_MOVES, NUM_COLORS, NUM_PIECE_TYPES,
};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Action {
    Move(Move),
    OfferDraw(Color),
    AcceptDraw,
    DeclineDraw,
    Resign(Color),
}

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

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Game {
    init_position: Position,
    position: Position,
    history: Vec<Move>,
}

impl Game {
    pub fn new(init: Position, moves: impl IntoIterator<Item = Move>) -> Self {
        let mut s = Self::default();
        s.init_position = init.clone();
        s.position = init;
        s.make_moves(moves);
        s
    }

    pub fn default_setup() -> Self {
        Self::from_fen(FEN_STARTPOS).unwrap()
    }

    pub fn from_fen(fen: &str) -> Result<Self> {
        let state = Position::new().from_fen(fen)?;
        Ok(Self::new(state, []))
    }

    pub const fn position(&self) -> &Position {
        &self.position
    }

    pub fn position_mut(&mut self) -> &mut Position {
        &mut self.position
    }

    pub fn history(&self) -> &[Move] {
        &self.history
    }

    pub fn make_move(&mut self, chessmove: Move) {
        self.position.make_move(chessmove);
        self.history.push(chessmove);
    }

    /*
    pub fn unmake_move(&mut self) {
        let Some(chessmove) = self.history.pop() else {
            return;
        };

        self.position.unmake_move(chessmove);
    }
     */

    pub fn make_moves(&mut self, moves: impl IntoIterator<Item = Move>) {
        moves
            .into_iter()
            .for_each(|chessmove| self.make_move(chessmove))
    }

    /*
    pub fn unmake_moves(&mut self, count: usize) {
        (0..count).for_each(|_| self.unmake_move());
    }
     */
}

impl Default for Game {
    fn default() -> Self {
        Self {
            position: Position::default(),
            init_position: Position::default(),
            history: Vec::with_capacity(128),
        }
    }
}

impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let current = self.position.current_player();
        let board = self.position.bitboards();
        let fen = self.position.to_fen();

        write!(f, "{board}\nFEN: {fen}\nSide to Move: {current}")
    }
}

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
#[derive(Clone, PartialEq, Eq, Hash, Copy)]
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

    /// A cache for all of the legal moves in the current board state.
    pub legal_moves: [Move; MAX_NUM_MOVES],

    /// The total number of legal moves in the current board state.
    pub num_legal_moves: usize,
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
            // attacks: [BitBoard::EMPTY_BOARD; Tile::COUNT],
            legal_moves: [Move::illegal(); MAX_NUM_MOVES],
            num_legal_moves: 0,
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
    /// let state = Position::new().with_default_setup();
    /// assert_eq!(state.to_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// ```
    pub fn with_default_setup(self) -> Self {
        self.from_fen(FEN_STARTPOS).unwrap()
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

    pub fn from_fen(mut self, fen: &str) -> Result<Self> {
        let mut split = fen.trim().split(' ');
        let placements = split.next().ok_or(anyhow!(
            "Invalid FEN string: FEN string must have piece placements."
        ))?;
        self.bitboards = ChessBoard::from_fen(placements)?;

        let active_color = split.next().unwrap_or_else(|| "w");
        self.current_player = Color::from_str(active_color)?;

        let castling = split.next().unwrap_or_else(|| "KQkq");
        self.castling_rights = CastlingRights::from_uci(castling)?;

        let en_passant_target = split.next().unwrap_or_else(|| "-");
        self.ep_tile = match en_passant_target {
            "-" => None,
            tile => Some(Tile::from_uci(tile)?),
        };

        let halfmove = split.next().unwrap_or_else(|| "0");
        self.halfmove = halfmove.parse().or(Err(anyhow!(
            "Invalid FEN string: FEN string must have valid halfmove counter. Got {halfmove}"
        )))?;

        let fullmove = split.next().unwrap_or_else(|| "1");
        self.fullmove = fullmove.parse().or(Err(anyhow!(
            "Invalid FEN string: FEN string must have valid fullmove counter. Got {fullmove}"
        )))?;

        self.compute_legal_moves_for(self.current_player());

        Ok(self)
    }

    /// Generates a FEN string from this [`Position`].
    pub fn to_fen(&self) -> String {
        let placements = self.bitboards().fen();
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
        if let Some(ep) = self.ep_tile() {
            ep.backward_by(self.current_player(), 1)
        } else {
            None
        }
    }

    pub const fn castling_rights(&self) -> CastlingRights {
        self.castling_rights
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

    pub const fn bitboards(&self) -> &ChessBoard {
        &self.bitboards
    }

    pub fn bitboards_mut(&mut self) -> &mut ChessBoard {
        &mut self.bitboards
    }

    // pub fn pseudo_legal_moves(&self) -> &[Move] {}

    pub fn legal_moves(&self) -> &[Move] {
        &self.legal_moves[..self.num_legal_moves]
    }

    pub fn legal_moves_mut(&mut self) -> &mut [Move] {
        &mut self.legal_moves[..self.num_legal_moves]
    }

    pub fn legal_captures(&self) -> impl Iterator<Item = Move> {
        self.legal_moves.into_iter().filter(|mv| mv.is_capture())
    }

    fn compute_legal_moves_for(&mut self, color: Color) {
        self.num_legal_moves = 0;

        let mobility = self.compute_legal_mobility_for(color);
        for from in self.bitboards().color(color) {
            // If there are no legal moves at this location, move on to the next tile
            let moves_bb = mobility[from];
            if moves_bb.is_empty() {
                continue;
            }
            let piece = self.bitboards().piece_at(from).unwrap();

            for to in moves_bb {
                // By default, this move is either quiet or a capture, depending on whether its destination contains a piece
                let mut kind = if self.bitboards().has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                // The King has some special cases around castling
                // If the King is moving for the first time, check if he's castling
                if piece.is_king() && from == Tile::KING_START_SQUARES[color] {
                    if to == Tile::KINGSIDE_CASTLE_SQUARES[color]
                        && self.castling_rights.kingside[color]
                    {
                        kind = MoveKind::KingsideCastle;
                    } else if to == Tile::QUEENSIDE_CASTLE_SQUARES[color]
                        && self.castling_rights.queenside[color]
                    {
                        kind = MoveKind::QueensideCastle;
                    }
                } else if piece.is_pawn() {
                    // Special pawn cases
                    if Some(to) == from.forward_by(color, 2) {
                        kind = MoveKind::PawnPushTwo;
                    } else if to.file() != from.file() && self.bitboards().piece_at(to).is_none() {
                        // A piece was NOT at the captured spot, so this was en passant
                        kind = MoveKind::EnPassantCapture;
                    }

                    // Regardless of whether this was a capture or quiet, it may be a promotion
                    if to.rank() == Rank::eighth(color) {
                        if let MoveKind::Capture = kind {
                            // The pawn can reach the enemy's home rank and become promoted
                            self.legal_moves[self.num_legal_moves] =
                                Move::new(from, to, MoveKind::CaptureAndPromote(PieceKind::Knight));
                            self.num_legal_moves += 1;
                            self.legal_moves[self.num_legal_moves] =
                                Move::new(from, to, MoveKind::CaptureAndPromote(PieceKind::Rook));
                            self.num_legal_moves += 1;
                            self.legal_moves[self.num_legal_moves] =
                                Move::new(from, to, MoveKind::CaptureAndPromote(PieceKind::Bishop));
                            self.num_legal_moves += 1;
                            // This gets pushed to the move list after this if-else chain
                            kind = MoveKind::CaptureAndPromote(PieceKind::Queen);
                        } else {
                            // The pawn can reach the enemy's home rank and become promoted
                            self.legal_moves[self.num_legal_moves] =
                                Move::new(from, to, MoveKind::Promote(PieceKind::Knight));
                            self.num_legal_moves += 1;
                            self.legal_moves[self.num_legal_moves] =
                                Move::new(from, to, MoveKind::Promote(PieceKind::Rook));
                            self.num_legal_moves += 1;
                            self.legal_moves[self.num_legal_moves] =
                                Move::new(from, to, MoveKind::Promote(PieceKind::Bishop));
                            self.num_legal_moves += 1;
                            // This gets pushed to the move list after this if-else chain
                            kind = MoveKind::Promote(PieceKind::Queen);
                        }
                    }
                }

                // Everyone else is normal
                self.legal_moves[self.num_legal_moves] = Move::new(from, to, kind);
                self.num_legal_moves += 1;
            }
        }

        // (moves, num_moves)
    }

    pub fn pinmasks(&self, color: Color) -> (BitBoard, BitBoard) {
        let mut pinmask_hv = BitBoard::EMPTY_BOARD;
        let mut pinmask_diag = BitBoard::EMPTY_BOARD;

        let king_bb = self.bitboards().king(color);
        let king_tile = king_bb.to_tile_unchecked();
        let friendlies = self.bitboards().color(color);
        let enemies = self.bitboards().color(color.opponent());

        // By treating the King like a rook/bishop that can attack "through" anything, we can find all of the possible attacks *to* the King by these enemy pieces, including possible pins
        let orthogonal_attacks = rook_attacks(king_tile, BitBoard::EMPTY_BOARD);
        let diagonal_attacks = bishop_attacks(king_tile, BitBoard::EMPTY_BOARD);

        let enemy_orthogonal_sliders = self.bitboards().orthogonal_sliders(color.opponent());
        let enemy_diagonal_sliders = self.bitboards().diagonal_sliders(color.opponent());

        // println!("ORTHO ATTACKS:\n{orthogonal_attacks:?}");
        // println!("DIAG ATTACKS:\n{diagonal_attacks:?}");

        // If an orthogonal slider is reachable from the King, then it is attacking the King
        let orthogonal_overlap = orthogonal_attacks & enemy_orthogonal_sliders;
        for tile in orthogonal_overlap {
            let ray = ray_between(king_tile, tile);
            if (ray & friendlies).population() <= 2 && (ray & enemies).population() <= 1 {
                pinmask_hv |= ray;
            }
        }

        let diagonal_overlap = diagonal_attacks & enemy_diagonal_sliders;
        for tile in diagonal_overlap {
            let ray = ray_between(king_tile, tile);
            if (ray & friendlies).population() <= 2 && (ray & enemies).population() <= 1 {
                pinmask_diag |= ray;
            }
        }

        (pinmask_hv, pinmask_diag)
    }

    fn compute_legal_mobility_for(&self, color: Color) -> [BitBoard; Tile::COUNT] {
        let mut moves = [BitBoard::EMPTY_BOARD; Tile::COUNT];

        // If the king is in check, move generation is much more strict
        let checkers = self.compute_checkers_for(color);
        let checkmask = if checkers.is_empty() {
            // Not in check; checkmask is irrelevant
            BitBoard::FULL_BOARD
        } else {
            // In check, so handle cases of single check or multi check
            let king_tile = self.bitboards().king(color).to_tile_unchecked();

            // If the number of checkers is one, we define a ray between the King and the checker
            let num_checkers = checkers.population();
            if num_checkers == 1 {
                // If there's only one checker, the checkmask is the path between that piece and the King
                ray_between(king_tile, checkers.to_tile_unchecked()) | checkers // Need to OR so that the attacking piece appears in the checkmask (Knights)
            } else {
                // eprintln!("Double-check. King must escape");
                // Otherwise, only the King can move, so move him somewhere not attacked
                let unsafe_squares =
                    compute_squares_attacked_by(self.bitboards(), color.opponent()); // TODO: Compute this after removing the King?
                let king_attacks = king_attacks(king_tile);
                let enemy_or_empty = self.bitboards().enemy_or_empty(color);

                // These are the attack lines of the pieces checking the King. Don't move along them!
                let mut discoverable_checks = BitBoard::EMPTY_BOARD;
                for checker in checkers {
                    let attacking_piece = self.bitboards().piece_at(checker).unwrap();
                    if attacking_piece.is_pawn() {
                        discoverable_checks |=
                            ray_between(king_tile, checker) & !checker.bitboard();
                    } else {
                        discoverable_checks |=
                            ray_containing(king_tile, checker) & !checker.bitboard();
                    }
                }

                // eprintln!("DISCOVERABLE CHECKS:\n{discoverable_checks:?}");
                // eprintln!("UNSAFE SQUARES:\n{unsafe_squares:?}");

                // Castling is illegal when in check, so just capture or evade
                moves[king_tile] =
                    king_attacks & enemy_or_empty & !(unsafe_squares | discoverable_checks);

                return moves;
            }
        };
        // eprintln!("CHECKERS:\n{checkers:?}");
        // eprintln!("CHECKMASK:\n{checkmask:?}");

        // Some pieces may be pinned, preventing them from moving freely without endangering the king
        let (pinmask_hv, pinmask_diag) = self.pinmasks(color);

        // eprintln!("PINMASK_HV:\n{pinmask_hv:?}");
        // eprintln!("PINMASK_DIAG:\n{pinmask_diag:?}");
        // eprintln!("PINMASK:\n{:?}", pinmask_hv | pinmask_diag);

        // let ep_bb = BitBoard::from_option_tile(self.ep_tile());
        // println!("EN PASSANT:\n{ep_bb}\n---------------");

        let not_enemy_king = !self.bitboards().king(color.opponent());

        // Assign legal moves to each piece
        for tile in self.bitboards().color(color) {
            let piece = self.bitboards().get(tile).unwrap();

            moves[tile.index()] =
                self.compute_legal_mobility_at(&piece, tile, checkmask, pinmask_hv, pinmask_diag)
                    & not_enemy_king;
        }

        moves
    }

    fn compute_legal_mobility_at(
        &self,
        piece: &Piece,
        tile: Tile,
        checkmask: BitBoard,
        pinmask_hv: BitBoard,
        pinmask_diag: BitBoard,
    ) -> BitBoard {
        let color = piece.color();

        // These are not yet pseudo-legal; they are just BitBoards of the default movement behavior for each piece
        let attacks = default_attacks_for(piece, tile, self.bitboards().occupied());

        // Anything that isn't a friendly piece
        let enemy_or_empty = self.bitboards().enemy_or_empty(color);

        // A BitBoard of this piece
        let piece_bb = tile.bitboard();

        // A BitBoard of our King
        let king_bb = self.bitboards().king(color);

        // The File / Rank / Diagonal containing our King and this Piece
        let pinning_ray = ray_containing(tile, king_bb.to_tile_unchecked());

        // Check if this piece is pinned along any of the pinmasks
        let is_pinned = (pinmask_hv | pinmask_diag).contains(piece_bb);
        let pinmask = BitBoard::from_bool(!is_pinned) | pinning_ray;
        // println!("PINMASK ({is_pinned}):\n{pinmask:?}");

        match piece.kind() {
            PieceKind::Pawn => {
                // A pinned pawn's movement depends on its pin:
                //  - If pinned on a file, it can only push
                //  - If pinned on a rank, it cannot do anything
                //  - If pinned on a diagonal, it can only capture, and only along that diagonal

                // By default, a pawn can push forward two on it's starting rank, and one elsewhere
                let unblocked_pushes = pawn_pushes(tile, color);

                // If there is a piece in front of this pawn, we cannot push two
                let pushes = if unblocked_pushes.contains(self.bitboards().occupied()) {
                    piece_bb.advance_by(color, 1) // Piece in front; Can only push one
                } else {
                    unblocked_pushes
                };

                // By default, a pawn can attack diagonally in front of it, if there's an enemy piece
                let attacks = pawn_attacks(tile, color);
                let empty = self.bitboards().empty();
                let enemy = self.bitboards().color(color.opponent());

                let ep_bb = if let Some(ep_tile) = self.ep_tile() {
                    // println!("{piece} at {tile} CAN capture EP at {ep_tile}. Should it?");
                    // Construct a board without the EP target and EP capturer
                    let ep_bb = ep_tile.bitboard();
                    // println!("EP BB:\n{ep_bb}");
                    let ep_pawn = ep_bb.advance_by(color.opponent(), 1);
                    // println!("EP PAWN:\n{ep_pawn}");
                    let board_after_ep = self
                        .bitboards()
                        .without(piece_bb | ep_pawn)
                        .with_piece(*piece, ep_tile);
                    // println!("AFTER EP:\n{board_after_ep}");

                    // Get all enemy attacks on the board without these pieces
                    let enemy_attacks =
                        compute_squares_attacked_by(&board_after_ep, color.opponent());
                    // println!("Enemy attacks:\n{enemy_attacks}");

                    // If the enemy could now attack our King, en passant is not legal
                    if enemy_attacks.contains(king_bb) {
                        // println!("EN PASSANT IS NOT SAFE");
                        BitBoard::EMPTY_BOARD
                    } else {
                        // println!("EN PASSANT IS SAFE:\n{ep_bb:?}");
                        // println!("EN PASSANT IS SAFE");
                        // Otherwise, en passant is safe
                        ep_bb
                    }

                    // TODO this is equivalent to the above, just a one-liner
                    // BitBoard::from_bool(!enemy_attacks.contains(king_bb)) & ep_bb
                } else {
                    BitBoard::EMPTY_BOARD
                };

                let pseudo_legal = (pushes & empty) | (attacks & (enemy | ep_bb));

                // Note that we must OR with the checkmask just in case the king is being checked by a pawn that can be captured by EP
                pseudo_legal & (checkmask | ep_bb) & pinmask
            }
            PieceKind::King => {
                let friendlies = self.bitboards().color(color) & !king_bb;
                let enemies = self.bitboards().color(color.opponent());

                // A king can move anywhere that isn't attacked by the enemy
                let enemy_attacks = king_danger_squares(self.bitboards(), color.opponent());
                // println!("ENEMY ATTACKS:\n{enemy_attacks}");

                let kingside = if self.castling_rights.kingside[color] {
                    // These squares must not be attacked by the enemy
                    let rook_tile = [Tile::G1, Tile::G8][color];
                    let castling = ray_between(tile, rook_tile);

                    // These squares must be empty
                    let squares_between = ray_between(tile, rook_tile);

                    if (enemy_attacks & castling).is_empty()
                        && (squares_between & (friendlies | enemies)).is_empty()
                    {
                        castling
                    } else {
                        BitBoard::EMPTY_BOARD
                    }
                } else {
                    BitBoard::EMPTY_BOARD
                };
                // println!("kingside:\n{kingside}\n");

                let queenside = if self.castling_rights.queenside[color] {
                    // These squares must not be attacked by the enemy
                    let rook_tile = [Tile::C1, Tile::C8][color];
                    let castling = ray_between(tile, rook_tile);

                    // These squares must be empty
                    let dst_tile = [Tile::B1, Tile::B8][color];
                    let squares_between = ray_between(tile, dst_tile);

                    if (enemy_attacks & castling).is_empty()
                        && (squares_between & (friendlies | enemies)).is_empty()
                    {
                        castling
                    } else {
                        BitBoard::EMPTY_BOARD
                    }
                } else {
                    BitBoard::EMPTY_BOARD
                };
                // println!("queenside:\n{queenside}\n");

                let castling = kingside | queenside;

                (attacks | castling) & enemy_or_empty & !enemy_attacks
            }
            PieceKind::Knight | PieceKind::Rook | PieceKind::Bishop | PieceKind::Queen => {
                let pseudo_legal = attacks & enemy_or_empty;

                pseudo_legal & checkmask & pinmask
            }
        }
    }

    pub fn attacks_by(&self, color: Color) -> BitBoard {
        let board = self.bitboards();
        let occupied = board.occupied();

        let mut attacks = BitBoard::EMPTY_BOARD;

        for tile in board.color(color) {
            let piece = board.piece_at(tile).unwrap();
            attacks |= default_attacks_for(&piece, tile, occupied);
        }

        attacks
    }

    // https://www.chessprogramming.org/Square_Attacked_By#By_all_Pieces
    pub const fn attacks_to(&self, tile: Tile, attacker_color: Color) -> BitBoard {
        let board = self.bitboards();
        let occupied = board.occupied();

        let pawns = board.piece_parts(attacker_color, PieceKind::Pawn);
        let knights = board.piece_parts(attacker_color, PieceKind::Knight);
        let bishops = board.piece_parts(attacker_color, PieceKind::Bishop);
        let rooks = board.piece_parts(attacker_color, PieceKind::Rook);
        let queens = board.piece_parts(attacker_color, PieceKind::Queen);

        let mut attacks = BitBoard::EMPTY_BOARD;

        let bb = tile.bitboard();
        let pawn_moves = bb
            .retreat_by(attacker_color, 1)
            .east()
            .or(bb.retreat_by(attacker_color, 1).west());

        attacks = attacks.or(pawn_moves.and(pawns));
        attacks = attacks.or(knight_attacks(tile).and(knights));
        attacks = attacks.or(bishop_attacks(tile, occupied).and(bishops));
        attacks = attacks.or(rook_attacks(tile, occupied).and(rooks));
        attacks = attacks.or(queen_moves(tile, occupied).and(queens));

        attacks
    }

    pub const fn compute_checkers_for(&self, color: Color) -> BitBoard {
        let king = self.bitboards().king(color);
        let tile = king.to_tile_unchecked();

        self.attacks_to(tile, color.opponent())
    }

    pub const fn is_in_check(&self, color: Color) -> bool {
        !self.compute_checkers_for(color).is_empty()
    }

    pub const fn is_check(&self) -> bool {
        self.is_in_check(self.current_player())
    }

    pub fn is_in_checkmate(&self, color: Color) -> bool {
        // self.is_in_check(color) && self.compute_legal_moves_for(color).1 == 0
        self.is_in_check(color) && self.num_legal_moves == 0
    }

    pub fn is_checkmate(&self) -> bool {
        self.is_in_checkmate(self.current_player())
    }

    pub const fn is_in_double_check(&self, color: Color) -> bool {
        let checkers = self.compute_checkers_for(color);
        checkers.population() > 1
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
        let Some(piece) = self.bitboards().piece_at(from) else {
            return (false, "No piece here to move");
        };

        // If it's not this piece's color's turn, illegal move
        if piece.color() != self.current_player() {
            return (false, "Tried to move a piece that wasn't yours");
        }

        // If this move captures a piece, handle those cases
        if let Some(to_capture) = self.bitboards().piece_at(to) {
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

            /*
            let Some(kind) = chessmove.captured() else {
                panic!("{chessmove} captured {} {kind:?}, but isn't classified as a capture move internally", to_capture.color())
            };
             */

            /*
            if kind != to_capture.kind() {
                return (false, "Captured wrong piece");
            }
             */
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

    pub fn make_moves(&mut self, moves: impl IntoIterator<Item = Move>) {
        for chessmove in moves {
            self.make_move(chessmove);
        }
    }

    pub fn make_move_checked(&mut self, chessmove: Move) -> Result<(), String> {
        let (is_legal, reason) = self.is_legal(chessmove);
        if is_legal {
            Ok(self.make_move(chessmove))
        } else {
            Err(format!("{reason}"))
        }
    }

    /// Applies the move. No enforcement of legality
    pub fn make_move(&mut self, chessmove: Move) {
        // Remove the piece from it's previous location, exiting early if there is no piece there
        let Some(mut piece) = self.bitboards_mut().take(chessmove.from()) else {
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

        if chessmove.is_capture() {
            if chessmove.is_en_passant() {
                let captured_tile = to.backward_by(color, 1).unwrap();
                self.bitboards_mut().clear(captured_tile);
            } else {
                let captured = self.bitboards_mut().take(to).unwrap();

                // Disable castling, if necessary
                if captured.is_rook() && to.rank() == Rank::first(captured.color()) {
                    // Disable castling if an unmoved rook was captured
                    let can_queenside = to != [Tile::A1, Tile::A8][captured.color()];
                    let can_kingside = to != [Tile::H1, Tile::H8][captured.color()];

                    self.castling_rights.queenside[captured.color()] &= can_queenside;
                    self.castling_rights.kingside[captured.color()] &= can_kingside;
                }
            };

            // Reset halfmove counter, since a capture occurred
            self.halfmove = 0;
        } else if chessmove.is_pawn_double_push() {
            self.ep_tile = from.forward_by(color, 1);
        } else if chessmove.is_castle() {
            let castle_index = chessmove.is_short_castle() as usize;
            let old_rook_tile = [[Tile::A1, Tile::A8], [Tile::H1, Tile::H8]][castle_index][color];
            let new_rook_tile = [[Tile::D1, Tile::D8], [Tile::F1, Tile::F8]][castle_index][color];

            // Move the rook. The King is already handled before and after this match statement.
            let rook = self.bitboards_mut().take(old_rook_tile).unwrap();
            self.bitboards_mut().set(rook, new_rook_tile);

            // Disable castling
            self.castling_rights.kingside[color] = false;
            self.castling_rights.queenside[color] = false;
        }

        match piece.kind() {
            PieceKind::Pawn => self.halfmove = 0,
            PieceKind::Rook => {
                // Disable castling if a rook moved
                let queenside_rook_tile = [Tile::A1, Tile::A8][color];
                let kingside_rook_tile = [Tile::H1, Tile::H8][color];

                self.castling_rights.queenside[color] &= from != queenside_rook_tile;
                self.castling_rights.kingside[color] &= from != kingside_rook_tile;
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
        self.bitboards_mut().set(piece, to);

        // Next player's turn
        self.toggle_current_player();
        self.compute_legal_moves_for(self.current_player());
    }

    pub fn with_move_made(&self, chessmove: Move) -> Self {
        let mut new_pos = *self;
        new_pos.make_move(chessmove);
        new_pos
    }

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
                let piece = self.bitboards().piece_at(file * rank);
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
    pub fn with_setup(mut self, fen: &str) -> Result<Self> {
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

                    self = self.with_piece(piece, tile);

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

        Ok(self)
    }

    /// Alias for chaining [`ChessBoard::new`] with [`ChessBoard::with_setup`].
    pub fn from_fen(fen: &str) -> Result<Self> {
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
