use arrayvec::ArrayVec;

use crate::Board;

use super::{Bitboard, Color, Move, Piece, PieceKind, Square, MAX_NUM_MOVES};

include!("magics.rs");
// include!(concat!(env!("OUT_DIR"), "/rook_magics.rs"));

/// An alias for an [`ArrayVec`] containing at most [`MAX_NUM_MOVES`] moves.
pub type MoveList = ArrayVec<Move, MAX_NUM_MOVES>;

const RAY_BETWEEN_EXCLUSIVE: [[Bitboard; Square::COUNT]; Square::COUNT] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/ray_between_exclusive.dat"
    )))
};

const RAY_BETWEEN_INCLUSIVE: [[Bitboard; Square::COUNT]; Square::COUNT] = unsafe {
    std::mem::transmute(*include_bytes!(concat!(
        env!("OUT_DIR"),
        "/ray_between_inclusive.dat"
    )))
};

const RAY_CONTAINING: [[Bitboard; Square::COUNT]; Square::COUNT] = unsafe {
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

/*
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
 */

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

/// Computes a [`Bitboard`] of all the pieces that attack the provided [`Square`].
pub const fn compute_attacks_to(board: &Board, square: Square, attacker_color: Color) -> Bitboard {
    let pawns = board.piece_parts(attacker_color, PieceKind::Pawn);
    let knights = board.piece_parts(attacker_color, PieceKind::Knight);
    let bishops = board.piece_parts(attacker_color, PieceKind::Bishop);
    let rooks = board.piece_parts(attacker_color, PieceKind::Rook);
    let queens = board.piece_parts(attacker_color, PieceKind::Queen);

    let occupied = board.occupied();
    let mut attacks = pawn_attacks(square, attacker_color.opponent()).and(pawns);
    attacks = attacks.or(knight_attacks(square).and(knights));
    attacks = attacks.or(bishop_attacks(square, occupied).and(bishops));
    attacks = attacks.or(rook_attacks(square, occupied).and(rooks));
    attacks = attacks.or(queen_attacks(square, occupied).and(queens));

    attacks
}

pub fn compute_pinmasks_for(board: &Board, square: Square, color: Color) -> (Bitboard, Bitboard) {
    let mut pinmask_ortho = Bitboard::default();
    let mut pinmask_diag = Bitboard::default();
    let opponent = color.opponent();
    let occupied = board.occupied();

    // By treating this square like a rook/bishop that can attack "through" anything, we can find all of the possible attacks *to* this square by these enemy pieces, including possible pins
    let orthogonal_attacks = rook_attacks(square, Bitboard::EMPTY_BOARD);
    // let orthogonal_attacks = ROOK_ATTACKS[square];
    let enemy_orthogonal_sliders = board.orthogonal_sliders(opponent);

    // If an orthogonal slider is reachable from this square, then it is attacking this square
    for attacker_square in orthogonal_attacks & enemy_orthogonal_sliders {
        // Get a ray between this square and the attacker square, excluding both pieces
        let ray = ray_between_exclusive(square, attacker_square);

        // A ray is a pin if there is only one piece along it
        if (ray & occupied).population() == 1 {
            pinmask_ortho |= ray;
        }
    }

    // Repeat the process with diagonal sliders
    let diagonal_attacks = bishop_attacks(square, Bitboard::EMPTY_BOARD);
    // let diagonal_attacks = BISHOP_ATTACKS[square];
    let enemy_diagonal_sliders = board.diagonal_sliders(opponent);

    for attacker_square in diagonal_attacks & enemy_diagonal_sliders {
        let ray = ray_between_exclusive(square, attacker_square);
        if (ray & occupied).population() == 1 {
            pinmask_diag |= ray;
        }
    }

    (pinmask_ortho, pinmask_diag)
}

/// Fetch the default, pseudo-legal attacks for `piece` at `square`, given `blockers`.
///
/// Note: For Pawns, this retrieves only the Pawn's _attacks_, not their pushes.
/// This is because we call this function internally when generating legal moves to create
/// an "attack map" of all attacks possible, indexable by a [`Square`].
/// This map is then used to determine if a square is "safe" for the King to move to.
/// If Pawn pushes were included in this "attack map", then the King would not be able to
/// move to squares otherwise be safe, as the move generator would think that a Pawn's
/// threat of pushing could check the King.
pub fn attacks_for(piece: &Piece, square: Square, blockers: Bitboard) -> Bitboard {
    match piece.kind() {
        PieceKind::Pawn => pawn_attacks(square, piece.color()),
        PieceKind::Knight => knight_attacks(square),
        PieceKind::Bishop => bishop_attacks(square, blockers),
        PieceKind::Rook => rook_attacks(square, blockers),
        PieceKind::Queen => queen_attacks(square, blockers),
        PieceKind::King => king_attacks(square),
    }
}

/// Fetches a [`Bitboard`] with all of the bits along the ray between `from` and `to` (inclusive) set to `1`.
pub const fn ray_between_inclusive(from: Square, to: Square) -> Bitboard {
    RAY_BETWEEN_INCLUSIVE[from.index()][to.index()]
}

/// Fetches a [`Bitboard`] with all of the bits along the ray between `from` and `to` (exclusive) set to `1`.
pub const fn ray_between_exclusive(from: Square, to: Square) -> Bitboard {
    RAY_BETWEEN_EXCLUSIVE[from.index()][to.index()]
}

/// Fetches a [`Bitboard`] with all of the bits along the ray containing `from` and `to` set to `1`.
pub const fn ray_containing(from: Square, to: Square) -> Bitboard {
    RAY_CONTAINING[from.index()][to.index()]
}

/// Computes the possible moves for a Rook at a given [`Square`] with the provided blockers.
///
/// This will yield a [`Bitboard`] that allows the Rook to capture the first blocker.
pub const fn rook_attacks(square: Square, blockers: Bitboard) -> Bitboard {
    let magic = &ROOK_MAGICS[square.index()];
    Bitboard::new(ROOK_MOVES[magic_index(magic, blockers)])
}

/// Computes the possible moves for a Bishop at a given [`Square`] with the provided blockers.
///
/// This will yield a [`Bitboard`] that allows the Bishop to capture the first blocker.
pub const fn bishop_attacks(square: Square, blockers: Bitboard) -> Bitboard {
    let magic = &BISHOP_MAGICS[square.index()];
    Bitboard::new(BISHOP_MOVES[magic_index(magic, blockers)])
}

/// Computes the possible moves for a Queen at a given [`Square`] with the provided blockers.
///
/// This will yield a [`Bitboard`] that allows the Queen to capture the first blocker.
pub const fn queen_attacks(square: Square, blockers: Bitboard) -> Bitboard {
    rook_attacks(square, blockers).or(bishop_attacks(square, blockers))
}

/// Fetch the raw, unblocked attacks for a knight on the provided square.
pub const fn knight_attacks(square: Square) -> Bitboard {
    KNIGHT_ATTACKS[square.index()]
}

/// Fetch the raw, unblocked attacks for a king on the provided square.
pub const fn king_attacks(square: Square) -> Bitboard {
    KING_ATTACKS[square.index()]
}

pub const fn pawn_moves(square: Square, color: Color, blockers: Bitboard) -> Bitboard {
    // By default, a pawn can push forward two on it's starting rank, and one elsewhere
    // We get a mask of all the blockers (minus this piece) and shift it twice forward
    // So, if this pawn *could* move forward twice, but there was a piece directly in front of it, it now cannot move
    let all_but_this_pawn = blockers.xor(square.bitboard());
    let shift_mask = all_but_this_pawn.or(all_but_this_pawn.advance_by(color, 1));

    // If there is a piece in front of this pawn, we cannot push two
    let pushes = pawn_pushes(square, color).and(shift_mask.not());

    // By default, a pawn can only attack if there is a piece available to take
    let attacks = pawn_attacks(square, color).and(blockers);

    pushes.or(attacks)
}

/// Fetch the raw, unblocked pushes for a pawn of the provided color on the provided square.
pub const fn pawn_pushes(square: Square, color: Color) -> Bitboard {
    [
        WHITE_PAWN_PUSHES[square.index()],
        BLACK_PAWN_PUSHES[square.index()],
    ][color.index()]
}

/// Fetch the raw, unblocked attacks for a pawn of the provided color on the provided square.
pub const fn pawn_attacks(square: Square, color: Color) -> Bitboard {
    [
        WHITE_PAWN_ATTACKS[square.index()],
        BLACK_PAWN_ATTACKS[square.index()],
    ][color.index()]
}

struct MagicEntry {
    mask: u64,
    magic: u64,
    shift: u8,
    offset: u32,
}

const fn magic_index(entry: &MagicEntry, blockers: Bitboard) -> usize {
    let blockers = blockers.inner() & entry.mask;
    let hash = blockers.wrapping_mul(entry.magic);
    let index = (hash >> entry.shift) as usize;
    entry.offset as usize + index
}

#[cfg(test)]
mod test {
    use super::*;

    /// Checks if `moves` and `legal_moves` contain all the same elements, ignoring order
    fn lists_match(moves: Bitboard, legal_moves: &[Square]) {
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
            Square::D2,
            Square::D3,
            Square::D5,
            Square::D6,
            Square::A4,
            Square::B4,
            Square::C4,
            Square::E4,
            Square::F4,
            Square::G4,
            Square::H4,
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

        let moves = rook_attacks(Square::D4, blockers);

        lists_match(moves, &legal_moves);
    }
}
