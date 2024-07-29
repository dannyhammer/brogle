use crate::{ChessBoard, NUM_COLORS};

use super::{BitBoard, Color, Move, MoveKind, Piece, PieceKind, Position, Tile};

include!("blobs/rays_between.rs");
include!("blobs/rays.rs");
include!("blobs/magics.rs");

pub struct MoveGenerator<'a> {
    position: &'a Position,
    attacks: [BitBoard; NUM_COLORS],
}

impl<'a> MoveGenerator<'a> {
    pub const fn new(position: &'a Position) -> Self {
        let attacks = [BitBoard::EMPTY_BOARD; NUM_COLORS];
        Self { position, attacks }
    }

    pub const fn position(&self) -> &'a Position {
        &self.position
    }
}

fn generate_pseudo_legal_mobility(board: &ChessBoard, color: Color) -> [BitBoard; Tile::COUNT] {
    let mut pseudo_legal_mobility = [BitBoard::EMPTY_BOARD; Tile::COUNT];

    let blockers = board.occupied();

    for tile in board.color(color) {
        // Safe unwrap because we're iterating over all pieces of a color
        let piece = board.piece_at(tile).unwrap();
        pseudo_legal_mobility[tile.index()] = default_attacks_for(&piece, tile, blockers);
    }

    pseudo_legal_mobility
}

/// Computes a [`BitBoard`] of all of the squares that can be attacked by [`Color`] pieces.
pub fn squares_attacked_by(board: &ChessBoard, color: Color) -> BitBoard {
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
