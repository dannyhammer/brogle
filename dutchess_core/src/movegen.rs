use crate::{ChessBoard, Rank, MAX_NUM_MOVES, NUM_COLORS};

use super::{BitBoard, Color, Move, MoveKind, Piece, PieceKind, Position, Tile};

include!("blobs/rays_between.rs");
include!("blobs/rays.rs");
include!("blobs/magics.rs");

pub struct MoveGenerator<'a> {
    position: &'a Position,
    attacks: [BitBoard; NUM_COLORS],
    pinmasks: (BitBoard, BitBoard),
    checkers: BitBoard,
    // pseudo_legal_mobility: [BitBoard; Tile::COUNT],
    legal_mobility: [BitBoard; Tile::COUNT],
    legal_moves: [Move; MAX_NUM_MOVES],
    num_legal_moves: usize,
}

impl<'a> MoveGenerator<'a> {
    pub fn new(position: &'a Position) -> Self {
        let mut attacks = [BitBoard::default(); NUM_COLORS];
        // let mut pseudo_legal_mobility = [BitBoard::default(); Tile::COUNT];
        let legal_mobility = [BitBoard::default(); Tile::COUNT];
        let checkers = BitBoard::default();
        let pinmasks = (BitBoard::default(), BitBoard::default());

        let blockers = position.board().occupied();

        for tile in position.board().occupied() {
            let piece = position.board().piece_at(tile).unwrap();
            let color = piece.color();

            // pseudo_legal_mobility[tile] = default_attacks_for(&piece, tile, blockers);
            attacks[color] |= default_attacks_for(&piece, tile, blockers);
        }

        Self {
            position,
            attacks,
            pinmasks,
            checkers,
            // pseudo_legal_mobility,
            legal_mobility,
            legal_moves: [Move::default(); MAX_NUM_MOVES],
            num_legal_moves: 0,
        }
    }

    pub fn new_legal(position: &'a Position) -> Self {
        let mut movegen = Self::new(position);
        let color = position.current_player();
        let king_tile = position.board().king(color).to_tile_unchecked();
        movegen.pinmasks = movegen.compute_pinmasks_for(position.board(), king_tile, color);
        movegen.checkers = movegen.compute_checkers_for(position.board(), color);

        let legal_mobility = movegen.compute_legal_mobility(&position);
        movegen.generate_moves_from_mobility(&legal_mobility);
        movegen.legal_mobility = legal_mobility;
        movegen
    }

    pub const fn position(&self) -> &Position {
        &self.position
    }

    pub const fn attacks(&self, color: Color) -> BitBoard {
        self.attacks[color.index()]
    }

    pub const fn pinmasks(&self) -> (BitBoard, BitBoard) {
        self.pinmasks
    }

    pub const fn orthogonal_pinmasks(&self) -> BitBoard {
        self.pinmasks.0
    }

    pub const fn diagonal_pinmasks(&self) -> BitBoard {
        self.pinmasks.1
    }

    pub const fn checkers(&self) -> BitBoard {
        self.checkers
    }

    /// Returns `true` if `tile` is attacked by `color`.
    pub fn is_attacked_by(&self, tile: Tile, color: Color) -> bool {
        (self.attacks(color.opponent()) & tile.bitboard()).is_nonempty()
    }

    pub const fn is_in_check(&self) -> bool {
        self.checkers().is_nonempty()
    }

    pub const fn is_in_double_check(&self) -> bool {
        self.checkers().population() > 1
    }

    pub fn is_in_checkmate(&self) -> bool {
        self.is_in_check() && self.num_legal_moves == 0
    }

    pub fn order_moves<K: Ord>(&mut self, f: impl FnMut(&Move) -> K) {
        self.legal_moves[..self.num_legal_moves].sort_by_cached_key(f);
    }

    pub fn legal_moves(&self) -> &[Move] {
        &self.legal_moves[..self.num_legal_moves]
    }

    pub fn legal_moves_mut(&mut self) -> &mut [Move] {
        &mut self.legal_moves[..self.num_legal_moves]
    }

    pub fn legal_captures(&self) -> impl Iterator<Item = Move> {
        self.legal_moves.into_iter().filter(|mv| mv.is_capture())
    }

    fn generate_moves_from_mobility(&mut self, mobility: &[BitBoard; Tile::COUNT]) {
        let color = self.position().current_player();
        self.num_legal_moves = 0;

        // Loop over all tiles relevant to the current player
        for from in self.position().board().color(color) {
            // If there are no legal moves at this location, move on to the next tile
            let moves_bb = mobility[from];
            if moves_bb.is_empty() {
                continue;
            }
            // Safe unwrap because we're iterating over tiles with pieces
            let piece = self.position().board().piece_at(from).unwrap();

            for to in moves_bb {
                // By default, this move is either quiet or a capture, depending on whether its destination contains a piece
                let mut kind = if self.position().board().has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                // If the King is moving for the first time, check if he's castling
                if piece.is_king() && from == Tile::KING_START_SQUARES[color] {
                    if to == Tile::KINGSIDE_CASTLE_SQUARES[color]
                        && self.position().castling_rights().kingside[color]
                    {
                        kind = MoveKind::KingsideCastle;
                    } else if to == Tile::QUEENSIDE_CASTLE_SQUARES[color]
                        && self.position().castling_rights().queenside[color]
                    {
                        kind = MoveKind::QueensideCastle;
                    }
                } else if piece.is_pawn() {
                    // Special pawn cases
                    if Some(to) == from.forward_by(color, 2) {
                        kind = MoveKind::PawnPushTwo;
                    } else if to.file() != from.file()
                        && self.position().board().piece_at(to).is_none()
                    {
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
    }

    fn compute_legal_mobility(&self, position: &Position) -> [BitBoard; Tile::COUNT] {
        let color = position.current_player();
        let mut moves = [BitBoard::default(); Tile::COUNT];
        let king_tile = position.board().king(color).to_tile_unchecked();

        // If the king is in check, move generation is much more strict
        let checkers = self.compute_checkers_for(position.board(), color);
        let checkmask = match checkers.population() {
            // Not in check; checkmask is irrelevant
            0 => BitBoard::FULL_BOARD,
            // In single-check, so something must capture or block the check, or the king must leave check
            1 => ray_between(king_tile, checkers.to_tile_unchecked()) | checkers, // Need to OR so that the attacking piece appears in the checkmask (Knights)

            // In double-check, so only the King can move, so move him somewhere not attacked
            _ => {
                let unsafe_squares =
                    self.compute_squares_attacked_by(position.board(), color.opponent());
                let king_attacks = king_attacks(king_tile);
                let enemy_or_empty = position.board().enemy_or_empty(color);

                // These are the attack lines of the pieces checking the King. Don't move along them!
                let mut discoverable_checks = BitBoard::EMPTY_BOARD;
                for checker in checkers {
                    let attacking_piece = position.board().piece_at(checker).unwrap();
                    if attacking_piece.is_pawn() {
                        discoverable_checks |=
                            ray_between(king_tile, checker) & !checker.bitboard();
                    } else {
                        discoverable_checks |=
                            ray_containing(king_tile, checker) & !checker.bitboard();
                    }
                }

                // Castling is illegal when in check, so just capture or evade
                moves[king_tile] =
                    king_attacks & enemy_or_empty & !(unsafe_squares | discoverable_checks);

                return moves;
            }
        };

        // Some pieces may be pinned, preventing them from moving freely without endangering the king
        // let (pinmask_hv, pinmask_diag) = compute_pinmasks_for(position.board(), king_tile, color);
        let (pinmask_hv, pinmask_diag) = self.pinmasks;

        let not_enemy_king = !position.board().king(color.opponent());

        // Assign legal moves to each piece
        for tile in position.board().color(color) {
            let piece = position.board().get(tile).unwrap();

            moves[tile.index()] = self.compute_legal_mobility_at(
                position,
                &piece,
                tile,
                checkmask,
                pinmask_hv,
                pinmask_diag,
            ) & not_enemy_king;
        }

        moves
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
            attacks |= default_attacks_for(&piece, tile, blockers);
        }

        attacks
    }

    /// Removes the defending King from this board and computes the attacks of the `attacker_color` pieces,
    /// returning a [`BitBoard`] of all of the squares that can be attacked.
    fn king_danger_squares(&self, board: &ChessBoard, attacker_color: Color) -> BitBoard {
        let king_bb = board.king(attacker_color.opponent());
        let kingless = board.without(king_bb);
        self.compute_squares_attacked_by(&kingless, attacker_color)
    }

    /// Computes a [`BitBoard`] of all the pieces that attack the provided [`Tile`].
    fn compute_attacks_to(
        &self,
        board: &ChessBoard,
        tile: Tile,
        attacker_color: Color,
    ) -> BitBoard {
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

    fn compute_pinmasks_for(
        &self,
        board: &ChessBoard,
        tile: Tile,
        color: Color,
    ) -> (BitBoard, BitBoard) {
        let mut pinmask_hv = BitBoard::EMPTY_BOARD;
        let mut pinmask_diag = BitBoard::EMPTY_BOARD;

        let friendlies = board.color(color);
        let enemies = board.color(color.opponent());

        // By treating this tile like a rook/bishop that can attack "through" anything, we can find all of the possible attacks *to* this tile by these enemy pieces, including possible pins
        let orthogonal_attacks = rook_attacks(tile, BitBoard::EMPTY_BOARD);
        let diagonal_attacks = bishop_attacks(tile, BitBoard::EMPTY_BOARD);

        let enemy_orthogonal_sliders = board.orthogonal_sliders(color.opponent());
        let enemy_diagonal_sliders = board.diagonal_sliders(color.opponent());

        // If an orthogonal slider is reachable from this tile, then it is attacking this tile
        let orthogonal_overlap = orthogonal_attacks & enemy_orthogonal_sliders;
        for attacker_tile in orthogonal_overlap {
            let ray = ray_between(tile, attacker_tile);
            if (ray & friendlies).population() <= 2 && (ray & enemies).population() <= 1 {
                pinmask_hv |= ray;
            }
        }

        let diagonal_overlap = diagonal_attacks & enemy_diagonal_sliders;
        for attacker_tile in diagonal_overlap {
            let ray = ray_between(tile, attacker_tile);
            if (ray & friendlies).population() <= 2 && (ray & enemies).population() <= 1 {
                pinmask_diag |= ray;
            }
        }

        (pinmask_hv, pinmask_diag)
    }

    fn compute_checkers_for(&self, board: &ChessBoard, color: Color) -> BitBoard {
        let king = board.king(color);
        let tile = king.to_tile_unchecked();

        self.compute_attacks_to(board, tile, color.opponent())
    }

    fn compute_legal_mobility_at(
        &self,
        position: &Position,
        piece: &Piece,
        tile: Tile,
        checkmask: BitBoard,
        pinmask_hv: BitBoard,
        pinmask_diag: BitBoard,
    ) -> BitBoard {
        let color = piece.color();

        // These are not yet pseudo-legal; they are just BitBoards of the default movement behavior for each piece
        let attacks = default_attacks_for(piece, tile, position.board().occupied());

        // Anything that isn't a friendly piece
        let enemy_or_empty = position.board().enemy_or_empty(color);

        // A BitBoard of this piece
        let piece_bb = tile.bitboard();

        // A BitBoard of our King
        let king_bb = position.board().king(color);

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
                let pushes = if unblocked_pushes.contains(position.board().occupied()) {
                    piece_bb.advance_by(color, 1) // Piece in front; Can only push one
                } else {
                    unblocked_pushes
                };

                // By default, a pawn can attack diagonally in front of it, if there's an enemy piece
                let attacks = pawn_attacks(tile, color);
                let empty = position.board().empty();
                let enemy = position.board().color(color.opponent());

                let ep_bb = if let Some(ep_tile) = position.ep_tile() {
                    // println!("{piece} at {tile} CAN capture EP at {ep_tile}. Should it?");
                    // Construct a board without the EP target and EP capturer
                    let ep_bb = ep_tile.bitboard();
                    // println!("EP BB:\n{ep_bb}");
                    let ep_pawn = ep_bb.advance_by(color.opponent(), 1);
                    // println!("EP PAWN:\n{ep_pawn}");
                    let board_after_ep = position
                        .board()
                        .without(piece_bb | ep_pawn)
                        .with_piece(*piece, ep_tile);
                    // println!("AFTER EP:\n{board_after_ep}");

                    // Get all enemy attacks on the board without these pieces
                    let enemy_attacks =
                        self.compute_squares_attacked_by(&board_after_ep, color.opponent());
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
                let friendlies = position.board().color(color) & !king_bb;
                let enemies = position.board().color(color.opponent());

                // A king can move anywhere that isn't attacked by the enemy
                let enemy_attacks = self.king_danger_squares(position.board(), color.opponent());
                // println!("ENEMY ATTACKS:\n{enemy_attacks}");

                let kingside = if position.castling_rights().kingside[color] {
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

                let queenside = if position.castling_rights().queenside[color] {
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

const KNIGHT_ATTACKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/knight_mobility.blob")) };

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

pub const fn default_attacks_for(piece: &Piece, tile: Tile, blockers: BitBoard) -> BitBoard {
    // These are not yet pseudo-legal; they are just BitBoards of the default movement behavior for each piece
    match piece.kind() {
        PieceKind::Pawn => pawn_attacks(tile, piece.color()),
        PieceKind::Knight => knight_attacks(tile),
        PieceKind::Bishop => bishop_attacks(tile, blockers),
        PieceKind::Rook => rook_attacks(tile, blockers),
        PieceKind::Queen => rook_attacks(tile, blockers).or(bishop_attacks(tile, blockers)),
        PieceKind::King => king_attacks(tile),
    }
}

const fn magic_index(entry: &MagicEntry, blockers: BitBoard) -> usize {
    let blockers = blockers.0 & entry.mask;
    let hash = blockers.wrapping_mul(entry.magic);
    let index = (hash >> entry.shift) as usize;
    entry.offset as usize + index
}

pub const fn ray_between(from: Tile, to: Tile) -> BitBoard {
    RAY_BETWEEN[from.index()][to.index()]
}

pub const fn ray_containing(from: Tile, to: Tile) -> BitBoard {
    RAYS[from.index()][to.index()]
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
pub const fn queen_moves(tile: Tile, blockers: BitBoard) -> BitBoard {
    rook_attacks(tile, blockers).or(bishop_attacks(tile, blockers))
}

pub const fn knight_attacks(tile: Tile) -> BitBoard {
    KNIGHT_ATTACKS[tile.index()]
}

pub const fn king_attacks(tile: Tile) -> BitBoard {
    KING_ATTACKS[tile.index()]
}

pub const fn pawn_moves(tile: Tile, color: Color) -> BitBoard {
    pawn_pushes(tile, color).or(pawn_attacks(tile, color))
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
