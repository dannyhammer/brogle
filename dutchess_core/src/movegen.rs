use std::ops::Deref;

use arrayvec::ArrayVec;

use crate::MAX_NUM_MOVES;

use super::{
    BitBoard, ChessBoard, Color, Move, MoveKind, Piece, PieceKind, Position, Rank, Tile,
    NUM_COLORS, NUM_PIECE_TYPES,
};

include!("blobs/magics.rs"); // TODO: Make these into blobs

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MoveGenerator {
    pub(crate) position: Position,
    attacks_by_color: [BitBoard; NUM_COLORS],
    attacks_by_tile: [BitBoard; Tile::COUNT],
    pinmasks: (BitBoard, BitBoard),
    checkers: BitBoard,
    discoverable_checks: BitBoard,
    legal_mobility: [BitBoard; Tile::COUNT],
}

impl MoveGenerator {
    pub fn new(position: Position) -> Self {
        let mut movegen = Self {
            position,
            attacks_by_color: [BitBoard::default(); NUM_COLORS],
            pinmasks: (BitBoard::default(), BitBoard::default()),
            checkers: BitBoard::default(),
            discoverable_checks: BitBoard::default(),
            attacks_by_tile: [BitBoard::default(); Tile::COUNT],
            legal_mobility: [BitBoard::default(); Tile::COUNT],
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

    pub const fn attacks_by_color(&self, color: Color) -> BitBoard {
        self.attacks_by_color[color.index()]
    }

    pub const fn attacks_by_tile(&self, tile: Tile) -> BitBoard {
        self.attacks_by_tile[tile.index()]
    }

    pub const fn pinmasks(&self) -> (BitBoard, BitBoard) {
        self.pinmasks
    }

    pub const fn pinmask(&self) -> BitBoard {
        self.pinmask_ortho().or(self.pinmask_diag())
    }

    pub const fn pinmask_ortho(&self) -> BitBoard {
        self.pinmasks().0
    }

    pub const fn pinmask_diag(&self) -> BitBoard {
        self.pinmasks().1
    }

    pub const fn checkers(&self) -> BitBoard {
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

    // pub fn is_in_checkmate(&self) -> bool {
    //     self.is_in_check() && self.num_legal_moves == 0
    // }

    // pub fn order_moves<K: Ord>(&mut self, f: impl FnMut(&Move) -> K) {
    //     self.legal_moves[..self.num_legal_moves].sort_by_cached_key(f);
    // }

    // pub fn num_legal_moves(&self) -> usize {
    //     self.num_legal_moves
    // }

    // pub fn legal_moves(&self) -> &[Move] {
    //     &self.legal_moves[..self.num_legal_moves]
    // }

    // pub fn legal_moves_mut(&mut self) -> &mut [Move] {
    //     &mut self.legal_moves[..self.num_legal_moves]
    // }

    // pub fn legal_captures(&self) -> impl Iterator<Item = Move> {
    //     self.legal_moves.into_iter().filter(|mv| mv.is_capture())
    // }

    pub fn legal_moves(&self) -> ArrayVec<Move, MAX_NUM_MOVES> {
        let mut moves = ArrayVec::new();

        let color = self.current_player();
        let king_tile = self.king(color).to_tile_unchecked();
        let enemy_or_empty = self.enemy_or_empty(color);
        let checkmask = match self.checkers.population() {
            // Not in check; checkmask is irrelevant
            0 => BitBoard::FULL_BOARD,
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

        // For sliding pieces, we need a blocker mask to compute pseudo-legal moves
        let blockers = self.occupied();

        self.compute_normal_piece_moves(color, king_tile, checkmask, &mut moves);
        self.compute_king_moves(color, blockers, &mut moves); // TODO: legal_mask for King?

        // Pawns are... weird
        self.compute_pawn_moves(color, checkmask, &mut moves);

        moves
    }

    /*
    fn compute_pawn_moves(
        &self,
        color: Color,
        checkmask: BitBoard,
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
        let third_rank = BitBoard::third_rank(color);
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
        checkmask: BitBoard,
        moves: &mut ArrayVec<Move, MAX_NUM_MOVES>,
    ) {
        // A BitBoard of our King
        let king_bb = self.king(color);

        for from in self.piece_parts(color, PieceKind::Pawn) {
            // The File / Rank / Diagonal containing our King and this Piece
            let pinning_ray = ray_containing(from, king_bb.to_tile_unchecked());

            // Check if this piece is pinned along any of the pinmasks
            let is_pinned = self.pinmask().get(from);
            let pinmask = BitBoard::from_bool(!is_pinned) | pinning_ray;
            // println!("PAWN PINMASK ({is_pinned}):\n{pinmask:?}");

            //
            // A pinned pawn's movement depends on its pin:
            //  - If pinned on a file, it can only push
            //  - If pinned on a rank, it cannot do anything
            //  - If pinned on a diagonal, it can only capture, and only along that diagonal

            // A BitBoard of this piece
            let piece_bb = from.bitboard();
            // All pseudo-legal attacks for this Pawn
            let attacks = pawn_attacks(from, color);
            // let attacks = self.attacks_by_tile(from);

            // En passant happens so rarely, so we have an expensive check for its legality
            let ep_bb = if let Some(ep_tile) = self.position().ep_tile() {
                // Construct a board without the EP target and EP capturer
                let ep_bb = ep_tile.bitboard();
                let ep_target = ep_bb.advance_by(color.opponent(), 1);
                let board_after_ep = self
                    .board()
                    .without(piece_bb | ep_target)
                    .with_piece(Piece::new(color, PieceKind::Pawn), ep_tile);

                // Get all enemy attacks on the board without these pieces.
                // This is expensive, but EP is rare, so it's fine to pay this price on occasion.
                let enemy_attacks =
                    self.compute_squares_attacked_by(&board_after_ep, color.opponent());

                // If the enemy could now attack our King, en passant is not legal
                // eprintln!("EP IS SAFE");
                BitBoard::from_bool(!enemy_attacks.contains(&king_bb)) & ep_bb
            } else {
                // eprintln!("EP IS NOT SAFE");
                BitBoard::default()
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
        checkmask: BitBoard,
        moves: &mut ArrayVec<Move, MAX_NUM_MOVES>,
    ) {
        // Loop over every tile containing this piece
        let normal_pieces = self.color(color) ^ self.king(color) ^ self.pawns(color);
        // for from in self.piece_parts(color, PieceKind::Knight) {
        for from in normal_pieces {
            let pseudo_legal = self.attacks_by_tile(from);

            // Check if this piece is pinned along any of the pinmasks
            let is_pinned = self.pinmask().get(from);
            let pinmask = BitBoard::from_bool(!is_pinned) | ray_containing(from, king_tile);

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
        blockers: BitBoard,
        moves: &mut ArrayVec<Move, MAX_NUM_MOVES>,
    ) {
        // Loop over every tile containing this piece
        for from in self.piece_parts(color, PieceKind::King) {
            let pseudo_legal = self.attacks_by_tile(from);

            // A king can move anywhere that isn't attacked by the enemy
            let enemy_attacks = self.attacks_by_color(color.opponent());

            let castling_availability = |side: [bool; 2], rook_tile, dst_tile| {
                // Check if we can castle at all on this side
                let can_castle = BitBoard::from_bool(side[color]);

                // No squares between the Rook and King may be under attack by the enemy
                let castling = can_castle & ray_between_inclusive(from, rook_tile);

                // All squares within the castling range must be empty
                let squares_between = ray_between_inclusive(from, dst_tile);

                let not_attacked = (enemy_attacks & castling).is_empty();
                // There can be at most one piece in this ray (the King)
                let is_clear = (squares_between & blockers).population() <= 1;

                if not_attacked && is_clear {
                    castling
                } else {
                    BitBoard::EMPTY_BOARD
                }
            };

            let kingside = castling_availability(
                self.position().castling_rights().kingside,
                Tile::G1.rank_relative_to(color),
                Tile::G1.rank_relative_to(color),
            );
            // println!("kingside:\n{kingside}\n");

            let queenside = castling_availability(
                self.position().castling_rights().queenside,
                Tile::C1.rank_relative_to(color),
                Tile::B1.rank_relative_to(color),
            );
            // println!("queenside:\n{queenside}\n");

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
                        && self.position().castling_rights().kingside[color]
                    {
                        kind = MoveKind::KingsideCastle;
                    } else if to == Tile::QUEENSIDE_CASTLE_SQUARES[color]
                        && self.position().castling_rights().queenside[color]
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

    /// Computes a [`BitBoard`] of all of the squares that can be attacked by [`Color`] pieces.
    fn compute_squares_attacked_by(&self, board: &ChessBoard, color: Color) -> BitBoard {
        let mut attacks = BitBoard::EMPTY_BOARD;

        // All occupied spaces
        let blockers = board.occupied();

        // Get the attack tables for all pieces of this color
        for tile in board.color(color) {
            // Safe unwrap because we're iterating over all pieces of this color
            let piece = board.piece_at(tile).unwrap();
            attacks |= pseudo_legal_movement_for(&piece, tile, blockers);
        }

        attacks
    }

    /// Computes a [`BitBoard`] of all the pieces that attack the provided [`Tile`].
    const fn compute_attacks_to(
        &self,
        board: &ChessBoard,
        tile: Tile,
        attacker_color: Color,
    ) -> BitBoard {
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
        board: &ChessBoard,
        tile: Tile,
        color: Color,
    ) -> (BitBoard, BitBoard) {
        let mut pinmask_ortho = BitBoard::default();
        let mut pinmask_diag = BitBoard::default();
        let opponent = color.opponent();
        let occupied = board.occupied();

        // By treating this tile like a rook/bishop that can attack "through" anything, we can find all of the possible attacks *to* this tile by these enemy pieces, including possible pins
        let orthogonal_attacks = ROOK_MOBILITY[tile];
        let enemy_orthogonal_sliders = board.orthogonal_sliders(opponent);

        // If an orthogonal slider is reachable from this tile, then it is attacking this tile
        for attacker_tile in orthogonal_attacks & enemy_orthogonal_sliders {
            // Get a ray between this tile and the attacker tile, excluding both pieces
            let ray = ray_between_exclusive(tile, attacker_tile);

            // A ray is a pin if there is only one piece along it
            if (ray & occupied).is_only_one() {
                pinmask_ortho |= ray;
            }
        }

        // Repeat the process with diagonal sliders
        let diagonal_attacks = BISHOP_MOBILITY[tile];
        let enemy_diagonal_sliders = board.diagonal_sliders(opponent);

        for attacker_tile in diagonal_attacks & enemy_diagonal_sliders {
            let ray = ray_between_exclusive(tile, attacker_tile);
            if (ray & occupied).is_only_one() {
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

/*
fn compute_pseudo_legal_mobility(board: &ChessBoard, color: Color) -> [BitBoard; Tile::COUNT] {
    let mut pseudo_legal_mobility = [BitBoard::EMPTY_BOARD; Tile::COUNT];

    let blockers = board.occupied();

    for tile in board.color(color) {
        // Safe unwrap because we're iterating over all pieces of this color
        let piece = board.piece_at(tile).unwrap();
        pseudo_legal_mobility[tile.index()] = default_attacks_for(&piece, tile, blockers);
    }

    pseudo_legal_mobility
}
 */

const RAY_BETWEEN_EXCLUSIVE: [[BitBoard; Tile::COUNT]; Tile::COUNT] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/ray_between_exclusive.blob")) };

const RAY_BETWEEN_INCLUSIVE: [[BitBoard; Tile::COUNT]; Tile::COUNT] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/ray_between_inclusive.blob")) };

const RAY_CONTAINING: [[BitBoard; Tile::COUNT]; Tile::COUNT] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/ray_containing.blob")) };

const KNIGHT_ATTACKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/knight_mobility.blob")) };

// const QUEEN_ATTACKS: [BitBoard; 64] =
//     unsafe { std::mem::transmute(*include_bytes!("blobs/queen_mobility.blob")) };

const ROOK_MOBILITY: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/rook_mobility.blob")) };

const BISHOP_MOBILITY: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/bishop_mobility.blob")) };

const KING_ATTACKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/king_mobility.blob")) };

const WHITE_PAWN_PUSHES: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/white_pawn_push_mobility.blob")) };

const BLACK_PAWN_PUSHES: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/black_pawn_push_mobility.blob")) };

const WHITE_PAWN_ATTACKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/white_pawn_attack_mobility.blob")) };

const BLACK_PAWN_ATTACKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/black_pawn_attack_mobility.blob")) };

struct MagicEntry {
    mask: u64,
    magic: u64,
    shift: u8,
    offset: u32,
}

pub fn pseudo_legal_movement_for(piece: &Piece, tile: Tile, blockers: BitBoard) -> BitBoard {
    MOVE_HELPERS[piece.kind().index()](tile, blockers, piece.color())
}

const MOVE_HELPERS: [fn(Tile, BitBoard, Color) -> BitBoard; NUM_PIECE_TYPES] = [
    pawn_move_helper,
    knight_move_helper,
    bishop_move_helper,
    rook_move_helper,
    queen_move_helper,
    king_move_helper,
];

const fn pawn_move_helper(tile: Tile, _blockers: BitBoard, color: Color) -> BitBoard {
    pawn_attacks(tile, color)
    // pawn_moves(tile, color, blockers)
}

const fn knight_move_helper(tile: Tile, _: BitBoard, _: Color) -> BitBoard {
    knight_attacks(tile)
}

const fn bishop_move_helper(tile: Tile, blockers: BitBoard, _: Color) -> BitBoard {
    bishop_attacks(tile, blockers)
}

const fn rook_move_helper(tile: Tile, blockers: BitBoard, _: Color) -> BitBoard {
    rook_attacks(tile, blockers)
}

const fn queen_move_helper(tile: Tile, blockers: BitBoard, _: Color) -> BitBoard {
    rook_attacks(tile, blockers).or(bishop_attacks(tile, blockers))
}

const fn king_move_helper(tile: Tile, _: BitBoard, _: Color) -> BitBoard {
    king_attacks(tile)
}

const fn magic_index(entry: &MagicEntry, blockers: BitBoard) -> usize {
    let blockers = blockers.0 & entry.mask;
    let hash = blockers.wrapping_mul(entry.magic);
    let index = (hash >> entry.shift) as usize;
    entry.offset as usize + index
}

/// Fetches a [`BitBoard`] with all of the bits along the ray between `from` and `to` (inclusive) set to `1`.
pub const fn ray_between_inclusive(from: Tile, to: Tile) -> BitBoard {
    RAY_BETWEEN_INCLUSIVE[from.index()][to.index()]
}

/// Fetches a [`BitBoard`] with all of the bits along the ray between `from` and `to` (exclusive) set to `1`.
pub const fn ray_between_exclusive(from: Tile, to: Tile) -> BitBoard {
    RAY_BETWEEN_EXCLUSIVE[from.index()][to.index()]
}

/// Fetches a [`BitBoard`] with all of the bits along the ray containing `from` and `to` set to `1`.
pub const fn ray_containing(from: Tile, to: Tile) -> BitBoard {
    RAY_CONTAINING[from.index()][to.index()]
}

/// Computes the possible moves for a Rook at a given [`Tile`] with the provided blockers.
///
/// This will yield a [`BitBoard`] that allows the Rook to capture the first blocker.
pub const fn rook_attacks(tile: Tile, blockers: BitBoard) -> BitBoard {
    let magic = &ROOK_MAGICS[tile.index()];
    BitBoard(ROOK_MOVES[magic_index(magic, blockers)])
}

/// Computes the possible moves for a Bishop at a given [`Tile`] with the provided blockers.
///
/// This will yield a [`BitBoard`] that allows the Bishop to capture the first blocker.
pub const fn bishop_attacks(tile: Tile, blockers: BitBoard) -> BitBoard {
    let magic = &BISHOP_MAGICS[tile.index()];
    BitBoard(BISHOP_MOVES[magic_index(magic, blockers)])
}

/// Computes the possible moves for a Queen at a given [`Tile`] with the provided blockers.
///
/// This will yield a [`BitBoard`] that allows the Queen to capture the first blocker.
pub const fn queen_attacks(tile: Tile, blockers: BitBoard) -> BitBoard {
    rook_attacks(tile, blockers).or(bishop_attacks(tile, blockers))
}

pub const fn knight_attacks(tile: Tile) -> BitBoard {
    KNIGHT_ATTACKS[tile.index()]
}

pub const fn king_attacks(tile: Tile) -> BitBoard {
    KING_ATTACKS[tile.index()]
}

pub const fn pawn_moves(tile: Tile, color: Color, blockers: BitBoard) -> BitBoard {
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

pub const fn pawn_pushes(tile: Tile, color: Color) -> BitBoard {
    [
        WHITE_PAWN_PUSHES[tile.index()],
        BLACK_PAWN_PUSHES[tile.index()],
    ][color.index()]
}

pub const fn pawn_attacks(tile: Tile, color: Color) -> BitBoard {
    [
        WHITE_PAWN_ATTACKS[tile.index()],
        BLACK_PAWN_ATTACKS[tile.index()],
    ][color.index()]
}

#[cfg(test)]
mod test {
    use super::*;

    /// Checks if `moves` and `legal_moves` contain all the same elements, ignoring order
    fn lists_match(moves: BitBoard, legal_moves: &[Tile]) {
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
            BitBoard::new(0b1000100000000000000010000000000010000000000001000010100000000000);

        let moves = rook_attacks(Tile::D4, blockers);

        lists_match(moves, &legal_moves);
    }
}
