use super::{BitBoard, Color, Piece, PieceKind, Position, Tile};

include!("rays.rs");
include!("pregenerated_magics.rs");
struct MagicEntry {
    mask: u64,
    magic: u64,
    shift: u8,
    offset: u32,
}

const KNIGHT_MOVES: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/knight_masks.blob")) };

const KING_MOVES: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/king_masks.blob")) };

const WHITE_PAWN_PUSHES: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/white_pawn_push.blob")) };

const BLACK_PAWN_PUSHES: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/black_pawn_push.blob")) };

const WHITE_PAWN_ATTACKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/white_pawn_attack.blob")) };

const BLACK_PAWN_ATTACKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/black_pawn_attack.blob")) };

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
/// Represents all possible locations that can be moved to from a single [`Tile`], including whether the movement can result in a promotion.
pub(crate) struct Mobility {
    pub(crate) origin: Tile,
    pub(crate) destinations: BitBoard,
    pub(crate) possible_promotions: u8,
}

impl Mobility {
    // const fn new(origin: Tile) -> Self {
    //     Self {
    //         origin,
    //         destinations: BitBoard::EMPTY_BOARD,
    //         possible_promotions: 0,
    //     }
    // }
}

pub const fn default_movement_for(piece: &Piece, tile: Tile, blockers: BitBoard) -> BitBoard {
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
    match color {
        Color::White => WHITE_PAWN_PUSHES[tile.index()],
        Color::Black => BLACK_PAWN_PUSHES[tile.index()],
    }
}

pub const fn pawn_attacks(tile: Tile, color: Color) -> BitBoard {
    match color {
        Color::White => WHITE_PAWN_ATTACKS[tile.index()],
        Color::Black => BLACK_PAWN_ATTACKS[tile.index()],
    }
}

/// Computes the attacks and pushes for a pawn at the provided [`Tile`].
///
/// This serves as a "mask" of all the pawn's available moves, regardless of legality (check).
pub const fn pseudo_legal_pawn_moves(tile: Tile, position: &Position, color: Color) -> BitBoard {
    let can_double_push = tile.rank().is_pawn_rank(color);
    let enemies = position.bitboards().color(color.opponent());

    // If there is an en passant tile, add that to the pawn's movement options
    // let en_passant = BitBoard::from(position.ep_tile());
    let ep = if let Some(en_passant) = position.ep_tile() {
        let en_passant = BitBoard::from_tile(en_passant);
        let east = tile.file().bitboard().east();
        let west = tile.file().bitboard().west();
        // But only allow it if it's on the pawn's adjacent files
        let valid_en_passant_files = east.or(west);
        en_passant.and(valid_en_passant_files)
    } else {
        BitBoard::EMPTY_BOARD
    };

    let pushes = pawn_push_masks(tile, color, can_double_push);
    let attacks = pawn_attack_masks(tile, color);

    // Can only attack spaces occupied by enemies, or it can do en passant
    let valid_attacks = attacks.and(enemies.or(ep));

    pushes.or(valid_attacks)
}

/// Computes the space(s) in front of the pawn.
///
/// If `can_double_push` is `true`, two spaces are calculated.
const fn pawn_push_masks(tile: Tile, color: Color, can_double_push: bool) -> BitBoard {
    // By default, pawns can move one space forward one space
    let push = BitBoard::from_tile(tile).advance_by(color, 1);

    // If it's this pawn's first move, it can move forward two spaces.
    let double_push = if can_double_push {
        push.advance_by(color, 1)
    } else {
        BitBoard::EMPTY_BOARD
    };

    // This mask ensures we're not "teleporting" by moving past the first/final rank
    let not_home_rank = BitBoard::home_rank(color).not();

    not_home_rank.and(push.or(double_push))
}

/// Computes the diagonals of the pawn's position, for attacking
///
/// Does NOT compute en passant attacks.
const fn pawn_attack_masks(tile: Tile, color: Color) -> BitBoard {
    let push = BitBoard::from_tile(tile).advance_by(color, 1);

    push.east().or(push.west())
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
    use crate::core::DEFAULT_FEN;

    // Sets up a game from the provided FEN
    fn setup_game(fen: &str) -> Position {
        Position::new().from_fen(fen).unwrap()
    }

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
    fn pawn_initial_setup() {
        let position = setup_game(&DEFAULT_FEN);

        let test_setup_for = |color| {
            for pawn_pos in BitBoard::pawn_rank(color) {
                let legal_moves = [
                    pawn_pos.forward_by(color, 1).unwrap(),
                    pawn_pos.forward_by(color, 2).unwrap(),
                ];
                let moves = pseudo_legal_pawn_moves(pawn_pos, &position, color);

                lists_match(moves, &legal_moves);
            }
        };

        // test_setup_for(Color::White);
        test_setup_for(Color::Black);
    }

    #[test]
    fn pawn_captures() {
        // White
        // Can push two and capture on both diagonals
        let position = setup_game("8/8/8/8/8/2r1n3/3P4/8 w - - 0 1");
        let legal_moves = [Tile::D3, Tile::D4, Tile::C3, Tile::E3];
        let moves = pseudo_legal_pawn_moves(Tile::D2, &position, Color::White);
        lists_match(moves, &legal_moves);

        // Black
        // Can push two and capture on both diagonals
        let position = setup_game("8/5p2/4Q1B1/8/8/8/8/8 b - - 0 1");
        let legal_moves = [Tile::F6, Tile::F5, Tile::E6, Tile::G6];
        let moves = pseudo_legal_pawn_moves(Tile::F7, &position, Color::Black);
        lists_match(moves, &legal_moves);

        // Edge case
        // Black can move forward and capture one diagonal
        let position = setup_game("8/8/8/8/7p/n5B1/8/8 b - - 0 1");
        let legal_moves = [Tile::G3, Tile::H3];
        let moves = pseudo_legal_pawn_moves(Tile::H4, &position, Color::Black);
        lists_match(moves, &legal_moves);

        // Impossible case
        // White pawn at enemy home rank and not promoted; cannot move
        let position = setup_game("3P4/8/8/8/8/8/8/8 w - - 0 1");
        let legal_moves = [];
        let moves = pseudo_legal_pawn_moves(Tile::D8, &position, Color::White);
        lists_match(moves, &legal_moves);
    }

    #[test]
    fn pawn_en_passant() {
        // White pawn can move forward or en passant
        let position = setup_game("8/8/8/3pP3/8/8/8/8 w - d6 0 1");
        let legal_moves = [Tile::E6, Tile::D6];
        let moves = pseudo_legal_pawn_moves(Tile::E5, &position, Color::White);
        lists_match(moves, &legal_moves);

        // White pawn can move forward, but NOT en passant
        let position = setup_game("8/8/8/3p3P/8/8/8/8 w - d6 0 1");
        let legal_moves = [Tile::H6];
        let moves = pseudo_legal_pawn_moves(Tile::H5, &position, Color::White);
        lists_match(moves, &legal_moves);
    }

    #[test]
    fn rook_blockers() {
        // let position = setup_game("8/3n4/3Q4/8/1npR2N1/8/8/8 w - - 0 1");

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