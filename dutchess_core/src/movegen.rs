use super::{BitBoard, Color, Piece, PieceKind, Tile};

include!("blobs/rays_between.rs");
include!("blobs/rays.rs");
include!("blobs/magics.rs");
struct MagicEntry {
    mask: u64,
    magic: u64,
    shift: u8,
    offset: u32,
}

const KNIGHT_MOVES: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/knight_mobility.blob")) };

const KING_MOVES: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/king_mobility.blob")) };

const WHITE_PAWN_PUSHES: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/white_pawn_push_mobility.blob")) };

const BLACK_PAWN_PUSHES: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/black_pawn_push_mobility.blob")) };

const WHITE_PAWN_ATTACKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/white_pawn_attack_mobility.blob")) };

const BLACK_PAWN_ATTACKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/black_pawn_attack_mobility.blob")) };

pub const fn default_attacks_for(piece: &Piece, tile: Tile, blockers: BitBoard) -> BitBoard {
    // These are not yet pseudo-legal; they are just BitBoards of the default movement behavior for each piece
    match piece.kind() {
        PieceKind::Pawn => pawn_attacks(tile, piece.color()),
        PieceKind::Knight => knight_moves(tile),
        PieceKind::Bishop => bishop_moves(tile, blockers),
        PieceKind::Rook => rook_moves(tile, blockers),
        PieceKind::Queen => rook_moves(tile, blockers).or(bishop_moves(tile, blockers)),
        PieceKind::King => king_moves(tile),
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
pub const fn rook_moves(tile: Tile, blockers: BitBoard) -> BitBoard {
    let magic = &ROOK_MAGICS[tile.index()];
    BitBoard(ROOK_MOVES[magic_index(magic, blockers)])
}

/// Computes the possible moves for a Bishop at a given [`Tile`] with the provided blockers.
///
/// This will yield a [`BitBoard`] that allows the Bishop to capture the first blocker.
pub const fn bishop_moves(tile: Tile, blockers: BitBoard) -> BitBoard {
    let magic = &BISHOP_MAGICS[tile.index()];
    BitBoard(BISHOP_MOVES[magic_index(magic, blockers)])
}

/// Computes the possible moves for a Queen at a given [`Tile`] with the provided blockers.
///
/// This will yield a [`BitBoard`] that allows the Queen to capture the first blocker.
pub const fn queen_moves(tile: Tile, blockers: BitBoard) -> BitBoard {
    rook_moves(tile, blockers).or(bishop_moves(tile, blockers))
}

pub const fn knight_moves(tile: Tile) -> BitBoard {
    KNIGHT_MOVES[tile.index()]
}

pub const fn king_moves(tile: Tile) -> BitBoard {
    KING_MOVES[tile.index()]
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

/*
// http://pradu.us/old/Nov27_2008/Buzz/research/magic/Bitboards.pdf
/// `key` - Sparsely populated input key
///
/// `magic` - Magic constant used to hash `key`
///
/// `num_bits` - Number of bits in the index
///
/// `data` - Data to index into
const fn magic_hash<T>(key: u64, magic: u64, num_bits: u8, data: &[T]) -> &T {
    &data[(key.wrapping_mul(magic) >> (64 - num_bits)) as usize]
}
 */

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

        let moves = rook_moves(Tile::D4, blockers);

        lists_match(moves, &legal_moves);
    }
}
