use std::fmt;

use super::{
    attacks_for, bishop_moves, knight_moves, queen_moves, rook_moves, utils::DEFAULT_FEN, BitBoard,
    ChessBoard, ChessError, Color, Move, MoveKind, Piece, PieceKind, Tile,
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
        self.state.make_move(chessmove);
        self.history.push(chessmove);
    }

    pub fn unmake_move(&mut self) {
        let Some(chessmove) = self.history.pop() else {
            return;
        };

        self.state.unmake_move(chessmove);
    }

    pub fn make_moves(&mut self, moves: impl IntoIterator<Item = Move>) {
        moves
            .into_iter()
            .for_each(|chessmove| self.make_move(chessmove))
    }

    pub fn unmake_moves(&mut self, count: usize) {
        (0..count).for_each(|_| self.unmake_move());
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
    const fn new() -> Self {
        Self {
            white_kingside: false,
            white_queenside: false,
            black_kingside: false,
            black_queenside: false,
        }
    }

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
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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
    // /// Every tile attacked by the piece at the respective index
    // attacks: [BitBoard; Tile::COUNT],
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
    pub const fn new() -> Self {
        Self {
            // pieces: [None; Tile::COUNT],
            board: ChessBoard::new(),
            current_player: Color::White,
            castling_rights: CastlingRights::new(),
            ep_tile: None,
            halfmove: 0,
            fullmove: 0,
            // attacks: [BitBoard::EMPTY_BOARD; Tile::COUNT],
        }
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

        // self.compute_attacks();

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

    /*
    pub const fn attacks(&self, tile: Tile) -> BitBoard {
        self.attacks[tile.index()]
    }
     */

    /*
    fn compute_attacks(&mut self) {
        self.compute_attacks_for(self.current_player());
        self.compute_attacks_for(self.current_player().opponent());
    }
     */

    /*
    fn compute_attacks_for(&mut self, color: Color) {
        for tile in self.board().color(color) {
            let piece = self.board().piece_at(tile).unwrap();
            self.attacks[tile] = attacks_for(&piece, tile, self);
        }
    }
     */

    /// Computes a [`BitBoard`] of all of the squares that can be attacked by [`Color`] pieces.
    fn squares_attacked_by(&self, color: Color) -> BitBoard {
        let mut attacks = BitBoard::EMPTY_BOARD;

        for tile in self.board().color(color) {
            let Some(piece) = self.board().piece_at(tile) else {
                unreachable!()
            };

            attacks |= self.compute_pseudo_legal_for(&piece, tile);
        }

        attacks
    }

    // If `color` is WHITE, then this removes BLACK's king from the board and computes attacks
    fn king_danger_squares(&self, color: Color) -> BitBoard {
        let mut attacks = BitBoard::EMPTY_BOARD;
        // Fetch a board without the opponent's king
        let kingless = self.board().kingless(color.opponent());

        for tile in kingless.color(color) {
            let Some(piece) = self.board().piece_at(tile) else {
                unreachable!()
            };

            attacks |= self.compute_pseudo_legal_for(&piece, tile);
        }

        attacks
    }

    const fn compute_pseudo_legal_for(&self, piece: &Piece, tile: Tile) -> BitBoard {
        // These are not yet pseudo-legal; they are just BitBoards of the default movement behavior for each piece
        let attacks = attacks_for(piece, tile, self);

        let enemy_or_empty = self.board().enemy_or_empty(piece.color());

        attacks.and(enemy_or_empty)
    }

    fn compute_legal_for(&self, piece: &Piece, tile: Tile) -> BitBoard {
        let pseudo = self.compute_pseudo_legal_for(piece, tile);

        let checkmask = if self.is_in_check(piece.color()) {
            BitBoard::EMPTY_BOARD
        } else {
            BitBoard::FULL_BOARD
        };

        pseudo.and(checkmask)
    }

    /*
    // https://www.chessprogramming.org/Square_Attacked_By#By_all_Pieces
    const fn attacks_to(&self, tile: Tile) -> BitBoard {
        let board = self.board();
        let occupied = board.occupied();

        let pawns = board.kind(PieceKind::Pawn);
        let knights = board.kind(PieceKind::Knight);
        let bishops = board.kind(PieceKind::Bishop);
        let rooks = board.kind(PieceKind::Rook);
        let queens = board.kind(PieceKind::Queen);

        let mut attackers = BitBoard::EMPTY_BOARD;
        // let pawn_moves = tile
        //     .forward_by(color, 1)
        //     .unwrap()
        //     .east()
        //     .or(tile.forward_by(color, 1).unwrap().west())
        //     .unwrap();

        // checkers = checkers.or(pawn_moves.and(pawns));
        attackers = attackers.or(knight_moves(tile).and(knights));
        attackers = attackers.or(bishop_moves(tile, occupied).and(bishops));
        attackers = attackers.or(rook_moves(tile, occupied).and(rooks));
        attackers = attackers.or(queen_moves(tile, occupied).and(queens));

        attackers
    }
     */

    const fn compute_checkers_for(&self, color: Color) -> BitBoard {
        let king = self.board().king(color);
        let tile = king.to_tile_unchecked();
        let board = self.board();
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

    const fn is_in_check(&self, color: Color) -> bool {
        !self.compute_checkers_for(color).is_empty()
    }

    const fn is_in_double_check(&self, color: Color) -> bool {
        let checkers = self.compute_checkers_for(color);
        checkers.population() > 1
    }

    pub fn mobility(&self, color: Color) -> [BitBoard; Tile::COUNT] {
        let mut moves = [BitBoard::EMPTY_BOARD; Tile::COUNT];

        for from in self.board().color(color) {
            let Some(piece) = self.board().piece_at(from) else {
                break;
            };

            if piece.color() != color {
                break;
            }

            moves[from.index()] |= self.compute_legal_for(&piece, from);
        }

        moves
    }

    /// Applies the move. No enforcement of legality
    pub fn make_move(&mut self, chessmove: Move) {
        // Remove the piece from it's previous location, exiting early if there is no piece there
        let Some(mut piece) = self.board.take(chessmove.from()) else {
            return;
        };

        // Handle special cases like promotions, castling, and en passant
        match chessmove.kind() {
            MoveKind::Quiet => {}
            // Remove captured piece from board
            MoveKind::Capture(_captured) => self.board.clear(chessmove.to()),
            MoveKind::KingsideCastle => {}
            MoveKind::QueensideCastle => {}
            // In En Passant, we need to remove the piece from one rank behind
            // Safe unwrap because the pawn is always guaranteed to be in front of this location
            MoveKind::EnPassantCapture(_captured) => {
                let captured_tile = chessmove.to().backward_by(piece.color(), 1).unwrap();
                self.board.clear(captured_tile);
            }
            MoveKind::Promote(promotion) => piece = piece.promoted(promotion),
            MoveKind::PawnPushTwo => {}
        }

        // Place the piece in it's new position
        self.board.set(piece, chessmove.to());

        // Increment move counters
        self.halfmove += 1;
        self.fullmove = self.halfmove / 2;

        // Next player's turn
        self.toggle_current_player();
        // self.compute_attacks();
    }

    pub fn unmake_move(&mut self, chessmove: Move) {
        // Safe unwrap because there is guaranteed to be a piece at the destination of a move.
        let mut piece = self.board.take(chessmove.to()).unwrap();

        // Undo any special cases, like castling and en passant
        match chessmove.kind() {
            MoveKind::Quiet => {}
            // Put the captured piece back
            MoveKind::Capture(captured) => self.board.set(captured, chessmove.to()),
            MoveKind::KingsideCastle => {}
            MoveKind::QueensideCastle => {}
            // A piece was removed from directly behind the pawn
            MoveKind::EnPassantCapture(captured) => {
                // Safe unwrap because the pawn is always guaranteed to be in front of this location
                let captured_tile = chessmove.to().backward_by(piece.color(), 1).unwrap();
                self.board.set(captured, captured_tile);
            }
            MoveKind::Promote(_) => piece = piece.demoted(),
            MoveKind::PawnPushTwo => {}
        }

        // Return the piece to it's original tile
        self.board.set(piece, chessmove.from());

        // Decrement move counters
        self.halfmove -= 1;
        self.fullmove = self.halfmove / 2;

        // Previous player's turn
        self.toggle_current_player();
        // self.compute_attacks();
    }

    /*
    pub fn get_legal_moves(&self) -> Vec<Move> {
        let player = self.current_player();
        let opponent = self.current_player().opponent();
        let player_tiles = self.board().color(player);

        println!("Generating legal moves for {player}");

        let mut legal_moves = Vec::with_capacity(218);

        for from in player_tiles {
            // Break if there is no piece to move
            let Some(piece) = self.board().piece_at(from) else {
                break;
            };

            if piece.color() != player {
                break;
            }

            // Loop over every possible move this piece can make
            for to in self.legal_moves_for(from) {
                // If the destination is on the enemy's home rank and this piece is a Pawn, promotions are possible
                match piece.kind() {
                    // Pawns require special cases for promotion and en passant
                    PieceKind::Pawn => {
                        // If the destination is the enemy's home rank, create moves for all possible promotions
                        if to.rank().is_home_rank(opponent) {
                            for promotion in PieceKind::promotions() {
                                legal_moves.push(Move::new(from, to, MoveKind::Promote(promotion)));
                            }
                        } else if from.rank().is_pawn_rank(player) {
                            // If a pawn is on it's first move, it can double push
                            legal_moves.push(Move::new(from, to, MoveKind::PawnPushTwo));
                        } else if to.file() != from.file() {
                            // If changing files, this is a capture, so check for en passant
                            if let Some(captured) = self.board().piece_at(to) {
                                legal_moves.push(Move::new(from, to, MoveKind::Capture(captured)));
                            } else {
                                let captured =
                                    self.board().piece_at(self.ep_tile().unwrap()).unwrap();
                                // If there was no piece at the destination tile, then this was en passant
                                legal_moves.push(Move::new(
                                    from,
                                    to,
                                    MoveKind::EnPassantCapture(captured),
                                ));
                            }
                        } else {
                            // If nothing else, this is a quiet, single push
                            legal_moves.push(Move::new_quiet(from, to));
                        }
                    }
                    _ => {
                        // For all other pieces, behavior is normal
                        legal_moves.push(Move::new_quiet(from, to));
                    }
                }
            }
        }

        legal_moves
    }
     */

    /*
    fn legal_moves_for(&self, tile: Tile) -> BitBoard {
        self.attacks(tile) & !self.board().color(self.current_player())
    }
     */

    /*
    fn legal_moves(&self) -> [BitBoard; Tile::COUNT] {
        let mut moves = [BitBoard::default(); Tile::COUNT];

        let current_player_pieces = self.board().color(self.current_player());
        for tile in current_player_pieces {
            let Some(piece) = self.board.piece_at(tile) else {
                break;
            };

            // TODO: I don't like this
            moves[tile] = generate_pseudo_legals_for(&piece, tile, self);
            println!("{tile} has {} moves", moves[tile].population());
        }

        moves
    }
     */
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
        write!(f, "{:?}", self.board())
    }
}
