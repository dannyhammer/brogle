use std::ops::Deref;

use arrayvec::ArrayVec;

use super::{
    Bitboard, Board, Color, Move, MoveKind, Piece, PieceKind, Position, Rank, Tile,
    MAX_NUM_MOVES,
};

include!("blobs/magics.rs"); // TODO: Make these into blobs

/*
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct MoveGen {
    pub(crate) position: Position,
    moves: ArrayVec<Move, MAX_NUM_MOVES>,
    to_mask: Bitboard,
    from_mask: Bitboard,
    current: Tile,
    checkers: Bitboard,
    checkmask: Bitboard,
    pinmask_ortho: Bitboard,
    pinmask_diag: Bitboard,
}

impl MoveGen {
    pub fn new(position: Position) -> Self {
        let color = position.current_player();
        let king_tile = position.king(color).to_tile_unchecked();

        let mut discoverable_checks = Bitboard::EMPTY_BOARD;
        let checkers = self.compute_attacks_to(&position, king_tile, color.opponent());
        let pinmasks = self.compute_pinmasks_for(&position, king_tile, color);

        // These are the rays containing the King and his Checkers
        // They are used to prevent the King from retreating along a line he is checked on!
        // Note: A pawn can't generate a discoverable check, as it can only capture 1 square away.
        for checker in checkers & !position.kind(PieceKind::Pawn) {
            discoverable_checks |= ray_containing(king_tile, checker) ^ checker.bitboard();
        }

        Self {
            position,
            moves: ArrayVec::default(),
            to_mask: Bitboard::FULL_BOARD,
            from_mask: Bitboard::FULL_BOARD,
            current: Tile::A1,
            checkers: Bitboard::EMPTY_BOARD,
            checkmask: Bitboard::FULL_BOARD,
            pinmask_ortho: Bitboard::EMPTY_BOARD,
            pinmask_diag: Bitboard::EMPTY_BOARD,
        }
    }

    pub fn generate_moves_from(&mut self, tile: Tile) -> Option<Move> {
        let _piece = self.position.piece_at(tile)?;

        None
    }
}

impl Iterator for MoveGen {
    type Item = Move;
    fn next(&mut self) -> Option<Self::Item> {
        self.current = self.current.next()?;
        self.generate_moves_from(self.current)
    }
}

impl Deref for MoveGen {
    type Target = Position;
    fn deref(&self) -> &Self::Target {
        &self.position
    }
}
 */

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MoveGenerator {
    pub(crate) position: Position,
    attacks_by_color: [Bitboard; Color::COUNT],
    attacks_by_tile: [Bitboard; Tile::COUNT],
    pinmasks: (Bitboard, Bitboard),
    checkers: Bitboard,
    discoverable_checks: Bitboard,
    legal_mobility: [Bitboard; Tile::COUNT],
}

impl MoveGenerator {
    pub fn new(position: Position) -> Self {
        let mut movegen = Self {
            position,
            attacks_by_color: [Bitboard::default(); Color::COUNT],
            pinmasks: (Bitboard::default(), Bitboard::default()),
            checkers: Bitboard::default(),
            discoverable_checks: Bitboard::default(),
            attacks_by_tile: [Bitboard::default(); Tile::COUNT],
            legal_mobility: [Bitboard::default(); Tile::COUNT],
        };

        movegen.generate_pseudo_legal();

        movegen
    }

    pub fn generate_pseudo_legal(&mut self) {
        let blockers = self.occupied();

        for tile in blockers {
            let piece = self.piece_at(tile).unwrap();
            let color = piece.color();

            let default_attacks = pseudo_legal_movement_for(&piece, tile, blockers);
            self.attacks_by_tile[tile] = default_attacks;
            self.attacks_by_color[color] |= default_attacks;
        }
    }

    pub fn new_legal(position: Position) -> Self {
        let mut movegen = Self::new(position);
        movegen.generate_legal();
        movegen
    }

    pub const fn position(&self) -> &Position {
        &self.position
    }

    pub const fn attacks_by_color(&self, color: Color) -> Bitboard {
        self.attacks_by_color[color.index()]
    }

    pub const fn attacks_by_tile(&self, tile: Tile) -> Bitboard {
        self.attacks_by_tile[tile.index()]
    }

    pub const fn pinmasks(&self) -> (Bitboard, Bitboard) {
        self.pinmasks
    }

    pub const fn pinmask(&self) -> Bitboard {
        self.pinmask_ortho().or(self.pinmask_diag())
    }

    pub const fn pinmask_ortho(&self) -> Bitboard {
        self.pinmasks().0
    }

    pub const fn pinmask_diag(&self) -> Bitboard {
        self.pinmasks().1
    }

    pub const fn checkers(&self) -> Bitboard {
        self.checkers
    }

    /// Returns `true` if `tile` is attacked by `color`.
    pub fn is_attacked_by(&self, tile: Tile, color: Color) -> bool {
        (self.attacks_by_color(color.opponent()) & tile.bitboard()).is_nonempty()
    }

    pub const fn is_in_check(&self) -> bool {
        self.checkers().is_nonempty()
    }

    pub const fn is_in_double_check(&self) -> bool {
        self.checkers().population() > 1
    }

    pub fn legal_captures(&self) -> ArrayVec<Move, MAX_NUM_MOVES> {
        self.legal_moves()
            .into_iter()
            .filter(|mv| mv.is_capture())
            .collect()
    }

    pub fn legal_moves(&self) -> ArrayVec<Move, MAX_NUM_MOVES> {
        let mut moves = ArrayVec::new();

        let color = self.current_player();
        let king_tile = self.king(color).to_tile_unchecked();
        let enemy_or_empty = self.enemy_or_empty(color);
        let checkmask = match self.checkers.population() {
            // Not in check; checkmask is irrelevant
            0 => Bitboard::FULL_BOARD,
            // In single-check, so something must capture or block the check, or the king must leave check
            1 => {
                // Need to OR so that the attacking piece appears in the checkmask (Knights)
                ray_between_inclusive(king_tile, self.checkers.to_tile_unchecked()) | self.checkers
            }

            // In double-check, so only the King can move. Move him somewhere not attacked.
            _ => {
                let enemy_attacks = self.attacks_by_color(color.opponent());
                let attacks = self.attacks_by_tile(king_tile);
                let safe_squares = !(enemy_attacks | self.discoverable_checks);

                // Castling is illegal when in check, so just capture or evade
                for to in attacks & enemy_or_empty & safe_squares {
                    let kind = if self.has(to) {
                        MoveKind::Capture
                    } else {
                        MoveKind::Quiet
                    };

                    moves.push(Move::new(king_tile, to, kind));
                }

                return moves;
            }
        };

        // let pinmask = self.pinmask();
        // eprintln!("PINMASK:\n{pinmask:?}");
        // eprintln!("CHECKMASK:\n{checkmask:?}");
        // eprintln!("ENEMY_OR_EMPTY:\n{enemy_or_empty:?}");

        // Pawns are... weird
        self.compute_pawn_moves(color, checkmask, &mut moves);

        // For sliding pieces, we need a blocker mask to compute pseudo-legal moves
        self.compute_normal_piece_moves(color, king_tile, checkmask, &mut moves);
        self.compute_king_moves(color, self.occupied(), &mut moves); // TODO: legal_mask for King?

        moves
    }

    /*
    // TODO: https://github.com/dannyhammer/brogle/issues/9
    fn compute_pawn_moves(
        &self,
        color: Color,
        checkmask: Bitboard,
        moves: &mut ArrayVec<Move, MAX_NUM_MOVES>,
    ) {
        // Fetch all pinned and unpinned pawns
        let pinned_pawns = self.pawns(color) & self.pinmask();
        let unpinned_pawns = self.pawns(color) & !self.pinmask();
        // eprintln!("PINNED PAWNS:\n{pinned_pawns:?}");
        // eprintln!("UNPINNED PAWNS:\n{unpinned_pawns:?}");

        // Pinned pawns may push along their pin ray
        let pinned_pushes = pinned_pawns.advance_by(color, 1) & self.pinmask();
        // Unpinned pawns may push normally
        let unpinned_pushes = unpinned_pawns.advance_by(color, 1);
        let pushes = pinned_pushes | unpinned_pushes;
        // eprintln!("PUSHES:\n{pushes:?}");

        // Cannot push outside of checkmask or into an occupied spot
        let legal_push_mask = !self.occupied() & checkmask;
        let single_pushes = pushes & legal_push_mask;
        // If it can push once, check if it's on the third rank. If so, it can push again.
        let third_rank = Bitboard::third_rank(color);
        let double_pushes = (single_pushes & third_rank).advance_by(color, 1) & legal_push_mask;

        // eprintln!("DOUBLE PUSHES:\n{double_pushes:?}");

        // Cannot capture outside of checkmask or into an empty or friendly spot
        let legal_enemies = self.color(color.opponent()) & checkmask;
        let east_captures = self.pawns(color).advance_by(color, 1).east() & legal_enemies;
        let west_captures = self.pawns(color).advance_by(color, 1).west() & legal_enemies;

        // Now generate the moves for these
        for to in single_pushes {
            let from = to.backward_by(color, 1).unwrap();
            if to.rank() == Rank::eighth(color) {
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Knight)));
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Bishop)));
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Rook)));
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Queen)));
            } else {
                moves.push(Move::new(from, to, MoveKind::Quiet));
            }
        }

        for to in double_pushes {
            let from = to.backward_by(color, 2).unwrap();
            moves.push(Move::new(from, to, MoveKind::Quiet));
        }

        for to in west_captures {
            let from = to.backward_by(color, 1).unwrap().left_by(color, 1).unwrap();

            if to.rank() == Rank::eighth(color) {
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Knight)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Bishop)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Rook)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)));
            } else {
                moves.push(Move::new(from, to, MoveKind::Quiet));
            }
        }

        for to in east_captures {
            let from = to
                .backward_by(color, 1)
                .unwrap()
                .right_by(color, 1)
                .unwrap();
            if to.rank() == Rank::eighth(color) {
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Knight)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Bishop)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Rook)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)));
            } else {
                moves.push(Move::new(from, to, MoveKind::Quiet));
            }
        }
    }
     */

    fn compute_pawn_moves(
        &self,
        color: Color,
        checkmask: Bitboard,
        moves: &mut ArrayVec<Move, MAX_NUM_MOVES>,
    ) {
        // A Bitboard and Tile of our King
        let king_bb = self.king(color);
        let king_tile = king_bb.to_tile_unchecked();

        // Enemy sliders; used for checking if EP is legal
        let enemy_rooks = self.orthogonal_sliders(color.opponent());
        let enemy_bishops = self.diagonal_sliders(color.opponent());

        for from in self.piece_parts(color, PieceKind::Pawn) {
            // The File / Rank / Diagonal containing our King and this Piece
            let pinning_ray = ray_containing(from, king_bb.to_tile_unchecked());

            // Check if this piece is pinned along any of the pinmasks
            let is_pinned = self.pinmask().get(from);
            let pinmask = Bitboard::from_bool(!is_pinned) | pinning_ray;
            // println!("PAWN PINMASK ({is_pinned}):\n{pinmask:?}");

            // A pinned pawn's movement depends on its pin:
            //  - If pinned on a file, it can only push
            //  - If pinned on a rank, it cannot do anything
            //  - If pinned on a diagonal, it can only capture, and only along that diagonal

            // A Bitboard of this piece
            let piece_bb = from.bitboard();
            // All pseudo-legal attacks for this Pawn
            let attacks = pawn_attacks(from, color);

            // En passant happens so rarely, so we have an expensive check for its legality
            let ep_bb = if let Some(ep_tile) = self.position().ep_tile() {
                // Compute a blockers bitboard as if EP was performed.
                let ep_bb = ep_tile.bitboard();
                let ep_target_bb = ep_bb.advance_by(color.opponent(), 1);
                let board_after_ep = (self.occupied() ^ ep_target_bb ^ piece_bb) | ep_bb;

                // If enemy sliders can attack our King, then EP is not legal to perform
                let checkers = (rook_attacks(king_tile, board_after_ep) & enemy_rooks)
                    | (bishop_attacks(king_tile, board_after_ep) & enemy_bishops);

                Bitboard::from_bool(checkers.is_empty()) & ep_bb
            } else {
                // eprintln!("EP IS NOT SAFE");
                Bitboard::default()
            };

            // eprintln!("EP:\n{ep_bb}");

            let enemy = self.color(color.opponent());
            // Check if this piece is pinned along any of the pinmasks

            // By default, a pawn can push forward two on it's starting rank, and one elsewhere
            // If there is a piece in front of this pawn, we cannot push two
            let all_but_this_pawn = self.occupied() ^ piece_bb;
            let shift_mask = all_but_this_pawn | all_but_this_pawn.advance_by(color, 1);
            let pushes = pawn_pushes(from, color) & !shift_mask;

            let pseudo_legal = (pushes & self.empty()) | (attacks & (enemy | ep_bb)); // Original

            // Note that we must OR with the checkmask just in case the king is being checked by a pawn that can be captured by EP
            let legal = pseudo_legal & (checkmask | ep_bb) & pinmask;

            for to in legal {
                let mut kind = if self.position().board().has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                // Special pawn cases
                if Some(to) == from.forward_by(color, 2) {
                    kind = MoveKind::PawnPushTwo;
                } else if to.file() != from.file() && self.position().board().piece_at(to).is_none()
                {
                    // A piece was NOT at the captured spot, so this was en passant
                    kind = MoveKind::EnPassantCapture;
                }

                // Regardless of whether this was a capture or quiet, it may be a promotion
                if to.rank() == Rank::eighth(color) {
                    if let MoveKind::Capture = kind {
                        // The pawn can reach the enemy's home rank and become promoted
                        kind = MoveKind::PromoCapt(PieceKind::Knight);
                        moves.push(Move::new(from, to, kind));
                        kind = MoveKind::PromoCapt(PieceKind::Bishop);
                        moves.push(Move::new(from, to, kind));
                        kind = MoveKind::PromoCapt(PieceKind::Rook);
                        moves.push(Move::new(from, to, kind));
                        // This gets pushed to the move list after this if-else chain
                        kind = MoveKind::PromoCapt(PieceKind::Queen);
                    } else {
                        // The pawn can reach the enemy's home rank and become promoted
                        kind = MoveKind::Promote(PieceKind::Knight);
                        moves.push(Move::new(from, to, kind));
                        kind = MoveKind::Promote(PieceKind::Bishop);
                        moves.push(Move::new(from, to, kind));
                        kind = MoveKind::Promote(PieceKind::Rook);
                        moves.push(Move::new(from, to, kind));
                        // This gets pushed to the move list after this if-else chain
                        kind = MoveKind::Promote(PieceKind::Queen);
                    }
                }

                moves.push(Move::new(from, to, kind));
            }
        }
    }

    fn compute_normal_piece_moves(
        &self,
        color: Color,
        king_tile: Tile,
        checkmask: Bitboard,
        moves: &mut ArrayVec<Move, MAX_NUM_MOVES>,
    ) {
        // Loop over every tile containing this piece
        let normal_pieces = self.color(color) ^ self.king(color) ^ self.pawns(color);

        // for from in self.piece_parts(color, PieceKind::Knight) {
        for from in normal_pieces {
            let pseudo_legal = self.attacks_by_tile(from);

            // Check if this piece is pinned along any of the pinmasks
            let is_pinned = self.pinmask().get(from);
            let pinmask = Bitboard::from_bool(!is_pinned) | ray_containing(from, king_tile);

            for to in pseudo_legal & checkmask & pinmask & self.enemy_or_empty(color) {
                let kind = if self.position().board().has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                moves.push(Move::new(from, to, kind));
            }
        }
    }

    fn compute_king_moves(
        &self,
        color: Color,
        blockers: Bitboard,
        moves: &mut ArrayVec<Move, MAX_NUM_MOVES>,
    ) {
        // Loop over every tile containing this piece
        for from in self.piece_parts(color, PieceKind::King) {
            let pseudo_legal = self.attacks_by_tile(from);

            // A king can move anywhere that isn't attacked by the enemy
            let enemy_attacks = self.attacks_by_color(color.opponent());

            let castling_availability = |side: [Option<Tile>; Color::COUNT], dst_tile: Tile| {
                // Check if we can castle at all on this side
                if let Some(rook_tile) = side[color] {
                    // No squares between the King and his destination may be under attack
                    let must_be_safe = ray_between_inclusive(from, dst_tile);
                    let is_safe = (must_be_safe & enemy_attacks).is_empty();

                    // All squares between the King and Rook must be empty
                    let must_be_clear = ray_between_exclusive(from, rook_tile);
                    let is_clear = (must_be_clear & blockers).is_empty();

                    if is_safe && is_clear {
                        Bitboard::from_tile(dst_tile)
                    } else {
                        Bitboard::EMPTY_BOARD
                    }
                } else {
                    Bitboard::EMPTY_BOARD
                }
            };

            let kingside = castling_availability(
                self.position().castling_rights().kingside,
                Tile::G1.rank_relative_to(color),
            );
            // eprintln!("kingside:\n{kingside}\n");

            let queenside = castling_availability(
                self.position().castling_rights().queenside,
                Tile::C1.rank_relative_to(color),
            );
            // eprintln!("queenside:\n{queenside}\n");

            let attack_or_castle = pseudo_legal | kingside | queenside; // Any attacks or castling
            let safe_squares = !(enemy_attacks | self.discoverable_checks); // Not attacked by the enemy (even after King retreats)

            let enemy_or_empty = self.enemy_or_empty(color);
            let legal = attack_or_castle & enemy_or_empty & safe_squares;

            for to in legal {
                let mut kind = if self.position().board().has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                if from == Tile::KING_START_SQUARES[color] {
                    if to == Tile::KINGSIDE_CASTLE_SQUARES[color]
                        && self.position().castling_rights().kingside[color].is_some()
                    {
                        kind = MoveKind::KingsideCastle;
                    } else if to == Tile::QUEENSIDE_CASTLE_SQUARES[color]
                        && self.position().castling_rights().queenside[color].is_some()
                    {
                        kind = MoveKind::QueensideCastle;
                    }
                }

                moves.push(Move::new(from, to, kind));
            }
        }
    }

    pub fn generate_legal(&mut self) {
        let color = self.position().current_player();
        let king_tile = self.position().board().king(color).to_tile_unchecked();

        self.checkers =
            self.compute_attacks_to(self.position().board(), king_tile, color.opponent());
        self.pinmasks = self.compute_pinmasks_for(self.position().board(), king_tile, color);

        // These are the rays containing the King and his Checkers
        // They are used to prevent the King from retreating along a line he is checked on!
        // Note: A pawn can't generate a discoverable check, as it can only capture 1 square away.
        for checker in self.checkers & !self.position().board().kind(PieceKind::Pawn) {
            self.discoverable_checks |= ray_containing(king_tile, checker) ^ checker.bitboard();
        }
    }

    /// Computes a [`Bitboard`] of all the pieces that attack the provided [`Tile`].
    const fn compute_attacks_to(
        &self,
        board: &Board,
        tile: Tile,
        attacker_color: Color,
    ) -> Bitboard {
        let pawns = board.piece_parts(attacker_color, PieceKind::Pawn);
        let knights = board.piece_parts(attacker_color, PieceKind::Knight);
        let bishops = board.piece_parts(attacker_color, PieceKind::Bishop);
        let rooks = board.piece_parts(attacker_color, PieceKind::Rook);
        let queens = board.piece_parts(attacker_color, PieceKind::Queen);

        let occupied = board.occupied();
        let mut attacks = pawn_attacks(tile, attacker_color.opponent()).and(pawns);
        attacks = attacks.or(knight_attacks(tile).and(knights));
        attacks = attacks.or(bishop_attacks(tile, occupied).and(bishops));
        attacks = attacks.or(rook_attacks(tile, occupied).and(rooks));
        attacks = attacks.or(queen_attacks(tile, occupied).and(queens));

        attacks
    }

    fn compute_pinmasks_for(
        &self,
        board: &Board,
        tile: Tile,
        color: Color,
    ) -> (Bitboard, Bitboard) {
        let mut pinmask_ortho = Bitboard::default();
        let mut pinmask_diag = Bitboard::default();
        let opponent = color.opponent();
        let occupied = board.occupied();

        // By treating this tile like a rook/bishop that can attack "through" anything, we can find all of the possible attacks *to* this tile by these enemy pieces, including possible pins
        let orthogonal_attacks = ROOK_ATTACKS[tile];
        let enemy_orthogonal_sliders = board.orthogonal_sliders(opponent);

        // If an orthogonal slider is reachable from this tile, then it is attacking this tile
        for attacker_tile in orthogonal_attacks & enemy_orthogonal_sliders {
            // Get a ray between this tile and the attacker tile, excluding both pieces
            let ray = ray_between_exclusive(tile, attacker_tile);

            // A ray is a pin if there is only one piece along it
            if (ray & occupied).population() == 1 {
                pinmask_ortho |= ray;
            }
        }

        // Repeat the process with diagonal sliders
        let diagonal_attacks = BISHOP_ATTACKS[tile];
        let enemy_diagonal_sliders = board.diagonal_sliders(opponent);

        for attacker_tile in diagonal_attacks & enemy_diagonal_sliders {
            let ray = ray_between_exclusive(tile, attacker_tile);
            if (ray & occupied).population() == 1 {
                pinmask_diag |= ray;
            }
        }

        (pinmask_ortho, pinmask_diag)
    }
}

impl Deref for MoveGenerator {
    type Target = Position;
    fn deref(&self) -> &Self::Target {
        &self.position
    }
}

impl Default for MoveGenerator {
    fn default() -> Self {
        Self::new(Position::default())
    }
}

const RAY_BETWEEN_EXCLUSIVE: [[Bitboard; Tile::COUNT]; Tile::COUNT] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/ray_between_exclusive.dat"
    )))
};

const RAY_BETWEEN_INCLUSIVE: [[Bitboard; Tile::COUNT]; Tile::COUNT] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/ray_between_inclusive.dat"
    )))
};

const RAY_CONTAINING: [[Bitboard; Tile::COUNT]; Tile::COUNT] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/ray_containing.dat"
    )))
};

const KNIGHT_ATTACKS: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/knight_attacks.dat"
    )))
};

// const QUEEN_ATTACKS: [Bitboard; 64] =
//     unsafe { std::mem::transmute(*include_bytes!("blobs/queen_mobility.blob")) };

const ROOK_ATTACKS: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/rook_attacks.dat"
    )))
};

const BISHOP_ATTACKS: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/bishop_attacks.dat"
    )))
};

const KING_ATTACKS: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/king_attacks.dat"
    )))
};

const WHITE_PAWN_PUSHES: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/white_pawn_pushes.dat"
    )))
};

const BLACK_PAWN_PUSHES: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/black_pawn_pushes.dat"
    )))
};

const WHITE_PAWN_ATTACKS: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/white_pawn_attacks.dat"
    )))
};

const BLACK_PAWN_ATTACKS: [Bitboard; 64] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/black_pawn_attacks.dat"
    )))
};

struct MagicEntry {
    mask: u64,
    magic: u64,
    shift: u8,
    offset: u32,
}

pub fn pseudo_legal_movement_for(piece: &Piece, tile: Tile, blockers: Bitboard) -> Bitboard {
    MOVE_HELPERS[piece.kind().index()](tile, blockers, piece.color())
}

const MOVE_HELPERS: [fn(Tile, Bitboard, Color) -> Bitboard; PieceKind::COUNT] = [
    pawn_move_helper,
    knight_move_helper,
    bishop_move_helper,
    rook_move_helper,
    queen_move_helper,
    king_move_helper,
];

const fn pawn_move_helper(tile: Tile, _blockers: Bitboard, color: Color) -> Bitboard {
    pawn_attacks(tile, color)
    // pawn_moves(tile, color, blockers)
}

const fn knight_move_helper(tile: Tile, _: Bitboard, _: Color) -> Bitboard {
    knight_attacks(tile)
}

const fn bishop_move_helper(tile: Tile, blockers: Bitboard, _: Color) -> Bitboard {
    bishop_attacks(tile, blockers)
}

const fn rook_move_helper(tile: Tile, blockers: Bitboard, _: Color) -> Bitboard {
    rook_attacks(tile, blockers)
}

const fn queen_move_helper(tile: Tile, blockers: Bitboard, _: Color) -> Bitboard {
    rook_attacks(tile, blockers).or(bishop_attacks(tile, blockers))
}

const fn king_move_helper(tile: Tile, _: Bitboard, _: Color) -> Bitboard {
    king_attacks(tile)
}

const fn magic_index(entry: &MagicEntry, blockers: Bitboard) -> usize {
    let blockers = blockers.inner() & entry.mask;
    let hash = blockers.wrapping_mul(entry.magic);
    let index = (hash >> entry.shift) as usize;
    entry.offset as usize + index
}

/// Fetches a [`Bitboard`] with all of the bits along the ray between `from` and `to` (inclusive) set to `1`.
pub const fn ray_between_inclusive(from: Tile, to: Tile) -> Bitboard {
    RAY_BETWEEN_INCLUSIVE[from.index()][to.index()]
}

/// Fetches a [`Bitboard`] with all of the bits along the ray between `from` and `to` (exclusive) set to `1`.
pub const fn ray_between_exclusive(from: Tile, to: Tile) -> Bitboard {
    RAY_BETWEEN_EXCLUSIVE[from.index()][to.index()]
}

/// Fetches a [`Bitboard`] with all of the bits along the ray containing `from` and `to` set to `1`.
pub const fn ray_containing(from: Tile, to: Tile) -> Bitboard {
    RAY_CONTAINING[from.index()][to.index()]
}

/// Computes the possible moves for a Rook at a given [`Tile`] with the provided blockers.
///
/// This will yield a [`Bitboard`] that allows the Rook to capture the first blocker.
pub const fn rook_attacks(tile: Tile, blockers: Bitboard) -> Bitboard {
    let magic = &ROOK_MAGICS[tile.index()];
    Bitboard::new(ROOK_MOVES[magic_index(magic, blockers)])
}

/// Computes the possible moves for a Bishop at a given [`Tile`] with the provided blockers.
///
/// This will yield a [`Bitboard`] that allows the Bishop to capture the first blocker.
pub const fn bishop_attacks(tile: Tile, blockers: Bitboard) -> Bitboard {
    let magic = &BISHOP_MAGICS[tile.index()];
    Bitboard::new(BISHOP_MOVES[magic_index(magic, blockers)])
}

/// Computes the possible moves for a Queen at a given [`Tile`] with the provided blockers.
///
/// This will yield a [`Bitboard`] that allows the Queen to capture the first blocker.
pub const fn queen_attacks(tile: Tile, blockers: Bitboard) -> Bitboard {
    rook_attacks(tile, blockers).or(bishop_attacks(tile, blockers))
}

/// Fetch the raw, unblocked attacks for a knight on the provided tile.
pub const fn knight_attacks(tile: Tile) -> Bitboard {
    KNIGHT_ATTACKS[tile.index()]
}

/// Fetch the raw, unblocked attacks for a king on the provided tile.
pub const fn king_attacks(tile: Tile) -> Bitboard {
    KING_ATTACKS[tile.index()]
}

pub const fn pawn_moves(tile: Tile, color: Color, blockers: Bitboard) -> Bitboard {
    // By default, a pawn can push forward two on it's starting rank, and one elsewhere
    // We get a mask of all the blockers (minus this piece) and shift it twice forward
    // So, if this pawn *could* move forward twice, but there was a piece directly in front of it, it now cannot move
    let all_but_this_pawn = blockers.xor(tile.bitboard());
    let shift_mask = all_but_this_pawn.or(all_but_this_pawn.advance_by(color, 1));

    // If there is a piece in front of this pawn, we cannot push two
    let pushes = pawn_pushes(tile, color).and(shift_mask.not());

    // By default, a pawn can only attack if there is a piece available to take
    let attacks = pawn_attacks(tile, color).and(blockers);

    pushes.or(attacks)
}

/// Fetch the raw, unblocked pushes for a pawn of the provided color on the provided tile.
pub const fn pawn_pushes(tile: Tile, color: Color) -> Bitboard {
    [
        WHITE_PAWN_PUSHES[tile.index()],
        BLACK_PAWN_PUSHES[tile.index()],
    ][color.index()]
}

/// Fetch the raw, unblocked attacks for a pawn of the provided color on the provided tile.
pub const fn pawn_attacks(tile: Tile, color: Color) -> Bitboard {
    [
        WHITE_PAWN_ATTACKS[tile.index()],
        BLACK_PAWN_ATTACKS[tile.index()],
    ][color.index()]
}

#[cfg(test)]
mod test {
    use super::*;

    /// Checks if `moves` and `legal_moves` contain all the same elements, ignoring order
    fn lists_match(moves: Bitboard, legal_moves: &[Tile]) {
        assert_eq!(
            moves.population() as usize,
            legal_moves.len(),
            "\nMoves: {:?}\nLegal: {:?}",
            moves.iter().collect::<Vec<_>>(),
            legal_moves
        );

        for mv in moves {
            assert!(
                legal_moves.contains(&mv),
                "{} not found in {:?}",
                mv,
                legal_moves
            );
        }
    }

    #[test]
    fn rook_blockers() {
        let legal_moves = [
            Tile::D2,
            Tile::D3,
            Tile::D5,
            Tile::D6,
            Tile::A4,
            Tile::B4,
            Tile::C4,
            Tile::E4,
            Tile::F4,
            Tile::G4,
            Tile::H4,
        ];

        // . . . X . . . X
        // . . . . . . . .
        // . . . X . . . .
        // . . . . . . . .
        // . . . . . . . X
        // . . X . . . . .
        // . . . X . X . .
        // . . . . . . . .
        let blockers =
            Bitboard::new(0b1000100000000000000010000000000010000000000001000010100000000000);

        let moves = rook_attacks(Tile::D4, blockers);

        lists_match(moves, &legal_moves);
    }
}
