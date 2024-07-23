use std::fmt;

use crate::MAX_NUM_MOVES;

use super::{
    bishop_moves, default_movement_for, king_moves, knight_moves, pawn_attacks, pawn_pushes,
    queen_moves, ray_between, ray_containing, rook_moves, utils::FEN_STARTPOS, BitBoard,
    ChessBoard, ChessError, Color, Move, MoveKind, Piece, PieceKind, Tile,
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

    pub fn from_fen(fen: &str) -> Result<Self, ChessError> {
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
    kingside: [bool; 2],
    queenside: [bool; 2],
}

impl CastlingRights {
    const fn new() -> Self {
        Self {
            kingside: [false; 2],
            queenside: [false; 2],
        }
    }

    fn from_uci(castling: &str) -> Result<Self, ChessError> {
        if castling.is_empty() {
            Err(ChessError::InvalidCastlingRights)
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

    // /// Every tile attacked by the piece at the respective index
    // attacks: [BitBoard; Tile::COUNT],
    history: Vec<Move>,
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
            history: Vec::with_capacity(256), // TODO: GET RID OF THIS IN POSITION
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

    pub fn from_fen(mut self, fen: &str) -> Result<Self, ChessError> {
        let mut split = fen.split(' ');
        let placements = split.next().ok_or(ChessError::InvalidFenString)?;
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
        self.halfmove = halfmove.parse().or(Err(ChessError::InvalidFenString))?;

        let fullmove = split.next().unwrap_or_else(|| "1");
        self.fullmove = fullmove.parse().or(Err(ChessError::InvalidFenString))?;

        Ok(self)
    }

    /// Generates a FEN string from this [`Position`].
    pub fn to_fen(&self) -> String {
        let placements = self.bitboards().fen();
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

    pub const fn current_player(&self) -> Color {
        self.current_player
    }

    pub const fn bitboards(&self) -> &ChessBoard {
        &self.bitboards
    }

    pub fn bitboards_mut(&mut self) -> &mut ChessBoard {
        &mut self.bitboards
    }

    pub const fn ep_tile(&self) -> Option<Tile> {
        self.ep_tile
    }

    pub fn toggle_current_player(&mut self) {
        self.current_player = self.current_player.opponent();
    }

    pub fn legal_moves(&self) -> Vec<Move> {
        self.legal_moves_for(self.current_player())
    }

    pub fn legal_moves_for(&self, color: Color) -> Vec<Move> {
        let mut moves = Vec::with_capacity(MAX_NUM_MOVES);

        let mobility = self.compute_legal_for(color);
        for (i, moves_bb) in mobility.into_iter().enumerate() {
            if moves_bb.is_empty() {
                continue;
            }
            let from = Tile::from_index_unchecked(i);
            let piece = self.bitboards().piece_at(from).unwrap();

            for to in moves_bb {
                // By default, this move is either quiet or a capture, depending on whether its destination contains a piece
                let mut kind = if self.bitboards().has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                // The King has some special cases around castling
                if piece.is_king() {
                    // If the King is moving for the first time, check if he's castling
                    if from == Tile::KING_START_SQUARES[color] {
                        if to == Tile::KINGSIDE_CASTLE_SQUARES[color]
                            && self.castling_rights.kingside[color]
                        {
                            kind = MoveKind::KingsideCastle;
                        } else if to == Tile::QUEENSIDE_CASTLE_SQUARES[color]
                            && self.castling_rights.queenside[color]
                        {
                            kind = MoveKind::QueensideCastle;
                        }
                    }
                } else if piece.is_pawn() {
                    // Special pawn cases
                    if Some(to) == from.forward_by(color, 2) {
                        kind = MoveKind::PawnPushTwo;
                    } else if to.file() != from.file() {
                        // A capture is occurring, so check if it's en passant
                        if self.bitboards().piece_at(to).is_none() {
                            // A piece was NOT at the captured spot, so this was en passant
                            kind = MoveKind::EnPassantCapture;
                        }
                    }

                    // Regardless of whether this was a capture or quiet, it may be a promotion
                    if to.rank().is_home_rank(color.opponent()) {
                        if let MoveKind::Capture = kind {
                            // The pawn can reach the enemy's home rank and become promoted
                            moves.push(Move::new(
                                from,
                                to,
                                MoveKind::CaptureAndPromote(PieceKind::Knight),
                            ));
                            moves.push(Move::new(
                                from,
                                to,
                                MoveKind::CaptureAndPromote(PieceKind::Rook),
                            ));
                            moves.push(Move::new(
                                from,
                                to,
                                MoveKind::CaptureAndPromote(PieceKind::Bishop),
                            ));
                            // This gets pushed to the move list after this if-else chain
                            kind = MoveKind::CaptureAndPromote(PieceKind::Queen);
                        } else {
                            // The pawn can reach the enemy's home rank and become promoted
                            moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Knight)));
                            moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Rook)));
                            moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Bishop)));
                            // This gets pushed to the move list after this if-else chain
                            kind = MoveKind::Promote(PieceKind::Queen);
                        }
                    }
                }

                // Everyone else is normal
                moves.push(Move::new(from, to, kind));
            }
        }

        moves
    }

    pub fn pinmasks(&self, color: Color) -> (BitBoard, BitBoard) {
        let mut pinmask_hv = BitBoard::EMPTY_BOARD;
        let mut pinmask_diag = BitBoard::EMPTY_BOARD;

        let king_bb = self.bitboards().king(color);
        let king_tile = king_bb.to_tile_unchecked();
        let friendlies = self.bitboards().color(color);
        let enemies = self.bitboards().color(color.opponent());

        // By treating the King like a rook/bishop that can attack "through" anything, we can find all of the possible attacks *to* the King by these enemy pieces, including possible pins
        let orthogonal_attacks = rook_moves(king_tile, BitBoard::EMPTY_BOARD);
        let diagonal_attacks = bishop_moves(king_tile, BitBoard::EMPTY_BOARD);

        let enemy_orthogonal_sliders = self.bitboards().orthogonal_sliders(color.opponent());
        let enemy_diagonal_sliders = self.bitboards().diagonal_sliders(color.opponent());

        // println!("ORTHO ATTACKS:\n{orthogonal_attacks:?}");
        // println!("DIAG ATTACKS:\n{diagonal_attacks:?}");

        // If an orthogonal slider is reachable from the King, then it is attacking the King
        let orthogonal_overlap = orthogonal_attacks & enemy_orthogonal_sliders;
        if !orthogonal_overlap.is_empty() {
            for tile in orthogonal_overlap {
                let ray = ray_between(king_tile, tile);
                if (ray & friendlies).population() <= 2 && (ray & enemies).population() <= 1 {
                    pinmask_hv |= ray;
                }
            }
        }

        let diagonal_overlap = diagonal_attacks & enemy_diagonal_sliders;
        if !diagonal_overlap.is_empty() {
            for tile in diagonal_overlap {
                let ray = ray_between(king_tile, tile);
                if (ray & friendlies).population() <= 2 && (ray & enemies).population() <= 1 {
                    pinmask_diag |= ray;
                }
            }
        }

        (pinmask_hv, pinmask_diag)
    }

    pub fn compute_legal_for(&self, color: Color) -> [BitBoard; Tile::COUNT] {
        let mut moves = [BitBoard::EMPTY_BOARD; Tile::COUNT];

        let not_enemy_king = !self.bitboards().king(color.opponent());

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
                let unsafe_squares = self.bitboards().squares_attacked_by(color.opponent()); // TODO: Compute this after removing the King?
                let king_attacks = king_moves(king_tile);
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

        // Assign legal moves to each piece
        for tile in self.bitboards().color(color) {
            let piece = self.bitboards().get(tile).unwrap_or_else(|| {
                panic!(
                    "Tried to get {color} piece at {tile}. Board:\n{}\nHistory: {:?}\n{:?}",
                    self.bitboards(),
                    self.history,
                    self.bitboards(),
                )
            });

            moves[tile.index()] =
                self.compute_legal_at(&piece, tile, checkmask, pinmask_hv, pinmask_diag)
                    & not_enemy_king;
        }

        moves
    }

    fn compute_legal_at(
        &self,
        piece: &Piece,
        tile: Tile,
        checkmask: BitBoard,
        pinmask_hv: BitBoard,
        pinmask_diag: BitBoard,
    ) -> BitBoard {
        let color = piece.color();

        // These are not yet pseudo-legal; they are just BitBoards of the default movement behavior for each piece
        let attacks = default_movement_for(piece, tile, self.bitboards().occupied());

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
                    let enemy_attacks = board_after_ep.squares_attacked_by(color.opponent());
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
                let enemy_attacks = self.bitboards().king_danger_squares(color.opponent());
                // println!("ENEMY ATTACKS:\n{enemy_attacks}");

                let kingside = if self.castling_rights.kingside[color] {
                    // These squares must not be attacked by the enemy
                    let castling = BitBoard::kingside_castle(color);

                    // These squares must be empty
                    let squares_between = BitBoard::kingside_castle_clearance(color);

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
                    let castling = BitBoard::queenside_castle(color);

                    // These squares must be empty
                    let squares_between = BitBoard::queenside_castle_clearance(color);

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
            PieceKind::Knight => {
                // A knight can move to any non-friendly space within the checkmask.
                // Unless it is pinned, in which case it cannot move
                let pseudo_legal = attacks & enemy_or_empty;
                pseudo_legal & checkmask & pinmask
            }
            PieceKind::Rook | PieceKind::Bishop | PieceKind::Queen => {
                let pseudo_legal = attacks & enemy_or_empty;

                pseudo_legal & checkmask & pinmask
            }
        }
    }

    // https://www.chessprogramming.org/Square_Attacked_By#By_all_Pieces
    /*
    const fn attacks_to(&self, tile: Tile, color: Color) -> BitBoard {
        let bb = tile.bitboard();
        let board = self.bitboards();
        let occupied = board.occupied();

        let pawns = board.kind(PieceKind::Pawn);
        let knights = board.kind(PieceKind::Knight);
        let bishops = board.kind(PieceKind::Bishop);
        let rooks = board.kind(PieceKind::Rook);
        let queens = board.kind(PieceKind::Queen);

        let mut attackers = BitBoard::EMPTY_BOARD;
        let pawn_moves = bb
            .advance_by(color, 1)
            .east()
            .or(bb.advance_by(color, 1).west());

        attackers = attackers.or(pawn_moves.and(pawns));
        attackers = attackers.or(knight_moves(tile).and(knights));
        attackers = attackers.or(bishop_moves(tile, occupied).and(bishops));
        attackers = attackers.or(rook_moves(tile, occupied).and(rooks));
        attackers = attackers.or(queen_moves(tile, occupied).and(queens));

        attackers
    }
     */

    const fn compute_checkers_for(&self, color: Color) -> BitBoard {
        let king = self.bitboards().king(color);
        let tile = king.to_tile_unchecked();
        let board = self.bitboards();
        let occupied = board.occupied();

        let pawns = board.piece_parts(color.opponent(), PieceKind::Pawn);
        let knights = board.piece_parts(color.opponent(), PieceKind::Knight);
        let bishops = board.piece_parts(color.opponent(), PieceKind::Bishop);
        let rooks = board.piece_parts(color.opponent(), PieceKind::Rook);
        let queens = board.piece_parts(color.opponent(), PieceKind::Queen);

        let mut checkers = BitBoard::EMPTY_BOARD;
        let pawn_moves = king
            .advance_by(color, 1)
            .east()
            .or(king.advance_by(color, 1).west());

        checkers = checkers.or(pawn_moves.and(pawns));
        checkers = checkers.or(knight_moves(tile).and(knights));
        checkers = checkers.or(bishop_moves(tile, occupied).and(bishops));
        checkers = checkers.or(rook_moves(tile, occupied).and(rooks));
        checkers = checkers.or(queen_moves(tile, occupied).and(queens));

        checkers
    }

    pub const fn is_in_check(&self, color: Color) -> bool {
        !self.compute_checkers_for(color).is_empty()
    }

    pub const fn is_check(&self) -> bool {
        self.is_in_check(self.current_player())
    }

    pub fn is_in_checkmate(&self, color: Color) -> bool {
        self.is_in_check(color) && self.legal_moves_for(color).is_empty()
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
        let (from, to, kind) = (chessmove.from(), chessmove.to(), chessmove.kind());

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

    /// Applies the move. No enforcement of legality
    pub fn make_move(&mut self, chessmove: Move) {
        // println!("Making {chessmove} on:\n{:?}", self.bitboards());
        // let (is_legal, msg) = self.is_legal(chessmove);
        // if !is_legal {
        //     panic!(
        //         "Move \"{chessmove:?}\" is not legal in current board state: {self}\n{:?}\nReason: {msg}\nHistory: {:?}", self, self.history
        //     );
        // }

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
                if captured.is_rook() && to.rank().is_home_rank(captured.color()) {
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

        // TODO: REMOVE HISTORY FROM POSITION
        self.history.push(chessmove);
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
        write!(f, "{}", self.bitboards())
    }
}
