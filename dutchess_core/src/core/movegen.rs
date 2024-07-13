use std::collections::HashMap;

use super::{
    // utils::{BISHOP_INDEX_BITS, BISHOP_MAGICS, ROOK_INDEX_BITS, ROOK_MAGICS},
    BitBoard,
    Color,
    GameState,
    Piece,
    PieceKind,
    Rank,
    Tile,
};

// include!(concat!(env!("OUT_DIR"), "/magics.rs"));
include!("pregenerated_magics.rs");
struct MagicEntry {
    mask: u64,
    magic: u64,
    shift: u8,
    offset: u32,
}

// const BISHOP_MASKS: [BitBoard; 64] =
//     unsafe { std::mem::transmute(*include_bytes!("blobs/bishop_masks.blob")) };

// const ROOK_MASKS: [BitBoard; 64] =
//     unsafe { std::mem::transmute(*include_bytes!("blobs/rook_masks.blob")) };

const KNIGHT_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/knight_masks.blob")) };

const KING_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/king_masks.blob")) };

const QUEEN_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("blobs/queen_masks.blob")) };

// const BISHOP_ATTACKS: [[BitBoard; 512]; 64] =
//     unsafe { std::mem::transmute(*include_bytes!("masks/bishop_attacks.blob")) };

// const ROOK_ATTACKS: [[BitBoard; 4096]; 64] =
//     unsafe { std::mem::transmute(*include_bytes!("masks/rook_attacks.blob")) };

pub fn moves_for(piece: &Piece, tile: Tile, state: &GameState) -> BitBoard {
    println!("Computing moves for {piece} at {tile}");

    let blockers = state.board().blockers(BitBoard::FULL_BOARD) & !BitBoard::EDGES;

    // These are not yet pseudo-legal; they are just BitBoards of the default movement behavior for each piece
    let default_moves = match piece.kind() {
        PieceKind::Pawn => pawn_masks(tile, state, piece.color()),
        PieceKind::Knight => knight_masks(tile),
        // PieceKind::Bishop => bishop_masks(tile),
        PieceKind::Rook => get_rook_moves(tile, blockers),
        PieceKind::Queen => queen_masks(tile),
        PieceKind::King => king_masks(tile),
        _ => unimplemented!(),
    };

    // All squares that can block a piece, excluding edge squares
    // let blocker_mask = piece_mask & !BitBoard::EDGES;

    // Includes friendly and enemy pieces, and is a subset of the blocker mask
    // let blocker_board = board.blockers(blocker_mask);

    // Will result in a board that can capture your own pieces

    // moves & !board[piece.color()]
    default_moves
    //& state.board().enemy_or_empty(piece.color())
    // & checkmask(piece.color(), state)
    // & !(pinmask(piece.color(), state))
}

/*
fn blocker_bitboards(default_moves: BitBoard) -> Vec<BitBoard> {
    let mut move_square_indices = Vec::with_capacity(64);
    for i in 0..64 {
        if ((default_moves >> i).0 & 1) == 1 {
            move_square_indices.push(i);
        }
    }

    let capacity = 1 << move_square_indices.len();
    let mut blockers = vec![BitBoard::default(); capacity];

    for pattern_index in 0..capacity {
        for bit_index in 0..move_square_indices.len() {
            let bit = (pattern_index >> bit_index) & 1;
            blockers[pattern_index] |= BitBoard((bit as u64) << move_square_indices[bit_index]);
        }
    }

    blockers
}

fn create_rook_lookup_table() -> HashMap<(Tile, BitBoard), BitBoard> {
    let mut map = HashMap::with_capacity(64);

    for tile in Tile::iter() {
        let movement_mask = rook_masks(tile);
        let blockers = blocker_bitboards(movement_mask);

        for blocker in blockers {
            // TOOD: CreateRookLegalMoveBitBoard(tile, blocker);
            let legal_moves = BitBoard::default();
            map.insert((tile, blocker), legal_moves);
        }
    }

    map
}
 */

fn magic_index(entry: &MagicEntry, blockers: BitBoard) -> usize {
    let blockers = blockers.0 & entry.mask;
    let hash = blockers.wrapping_mul(entry.magic);
    let index = (hash >> entry.shift) as usize;
    entry.offset as usize + index
    // index
}
pub fn get_rook_moves(tile: Tile, blockers: BitBoard) -> BitBoard {
    let magic = &ROOK_MAGICS[tile];
    BitBoard(ROOK_MOVES[magic_index(magic, blockers)])
}

pub fn get_bishop_moves(tile: Tile, blockers: BitBoard) -> BitBoard {
    let magic = &BISHOP_MAGICS[tile];
    BitBoard(BISHOP_MOVES[magic_index(magic, blockers)])
}

/*
const fn checkmask(color: Color, state: &GameState) -> BitBoard {
    // todo!()
    BitBoard::FULL_BOARD
}

const fn pinmask(color: Color, state: &GameState) -> BitBoard {
    // Horizontal/Vertical and then both diags
    // todo!()
    BitBoard::FULL_BOARD
}
 */

fn pawn_masks(tile: Tile, state: &GameState, color: Color) -> BitBoard {
    let can_double_push = tile.rank() == Rank::pawn_rank(color);
    let enemies = state.board().color(color.opponent());

    // If there is an en passant tile, add that to the pawn's movement options
    let en_passant = BitBoard::from(state.ep_tile());
    // But only allow it if it's on the pawn's adjacent files
    let valid_en_passant_files =
        BitBoard::from(tile.file().decrease()) | BitBoard::from(tile.file().increase());
    let ep = en_passant & valid_en_passant_files;

    //
    let pushes = pawn_push_masks(tile, color, can_double_push);
    let attacks = pawn_attack_masks(tile, color);

    // Can only attack spaces occupied by enemies
    let valid_attacks = attacks & enemies;

    pushes | valid_attacks | ep
}

fn pawn_push_masks(tile: Tile, color: Color, can_double_push: bool) -> BitBoard {
    // By default, pawns can move one space forward one space
    let push = BitBoard::from_tile(tile).advance_by(color, 1);

    // If it's this pawn's first move, it can move forward two spaces.
    let double_push = if can_double_push {
        push.advance_by(color, 1)
    } else {
        BitBoard::EMPTY_BOARD
    };

    // This mask ensures we're not "teleporting" by moving past the first/final rank
    let not_home_rank = !BitBoard::home_rank(color);

    (push | double_push) & not_home_rank
}

fn pawn_attack_masks(tile: Tile, color: Color) -> BitBoard {
    let push = BitBoard::from_tile(tile).advance_by(color, 1);

    push.east() | push.west()
}

const fn knight_masks(tile: Tile) -> BitBoard {
    KNIGHT_MASKS[tile.index()]
}

// const fn bishop_masks(tile: Tile) -> BitBoard {
//     BISHOP_MASKS[tile.index()]
// }

// const fn rook_masks(tile: Tile) -> BitBoard {
//     ROOK_MASKS[tile.index()]
// }

const fn queen_masks(tile: Tile) -> BitBoard {
    QUEEN_MASKS[tile.index()]
}

const fn king_masks(tile: Tile) -> BitBoard {
    KING_MASKS[tile.index()]
}

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

// fn get_move(tile: Tile, occupancy: BitBoard) -> BitBoard {}

/*
// https://analog-hors.github.io/site/magic-bitboards/
#[derive(Clone, Copy, Default)]
struct MagicEntry {
    mask: BitBoard,
    magic: u64,
    index_bits: u8,
}
// const ROOK_MAGICS: &[MagicEntry; 64] = todo!();
// const BISHOP_MAGICS: &[MagicEntry; 64] = todo!();

// const ROOK_MOVES: &[&[BitBoard]; 64] = todo!();
// const BISHOP_MOVES: &[&[BitBoard]; 64] = todo!();
fn magic_index(entry: &MagicEntry, occupied: BitBoard) -> usize {
    let blockers = occupied & entry.mask;
    let hash = blockers.0.wrapping_mul(entry.magic);
    (hash >> (64 - entry.index_bits)) as usize
    // ((blockers & entry.mask).0.wrapping_mul(entry.magic) >> (64 - entry.index_bits)) as usize
}
fn get_rook_moves(tile: Tile, blockers: BitBoard) -> BitBoard {
    // let magic = &ROOK_MAGICS[tile.index()];
    // let moves = &ROOK_MOVES[tile.index()];

    let magic = ROOK_MAGICS[tile.index()];
    let index_bits = ROOK_INDEX_BITS[tile.index()];
    let mask = ROOK_MASKS[tile.index()];
    let entry = MagicEntry {
        magic,
        mask,
        index_bits,
    };

    ROOK_MASKS[magic_index(&entry, blockers)]
}
fn get_bishop_moves(tile: Tile, blockers: BitBoard) -> BitBoard {
    //     let magic = &BISHOP_MAGICS[tile.index()];
    //     let moves = &BISHOP_MOVES[tile.index()];
    //     moves[magic_index(magic, blockers)]
    let magic = BISHOP_MAGICS[tile.index()];
    let index_bits = BISHOP_INDEX_BITS[tile.index()];
    let mask = BISHOP_MASKS[tile.index()];
    let entry = MagicEntry {
        magic,
        mask,
        index_bits,
    };

    BISHOP_MASKS[magic_index(&entry, blockers)]
}
 */

/*
fn perft(depth: usize) -> u64 {
    if depth == 0 {
        return 1;
    }

    /*
    u64 Perft(int depth)
    {
      MOVE move_list[256];
      int n_moves, i;
      u64 nodes = 0;

      if (depth == 0)
        return 1ULL;

      n_moves = GenerateLegalMoves(move_list);
      for (i = 0; i < n_moves; i++) {
        MakeMove(move_list[i]);
        nodes += Perft(depth - 1);
        UndoMove(move_list[i]);
      }
      return nodes;
    }

         */

    todo!()
}
 */

#[cfg(test)]
mod test {
    use super::*;
    use crate::core::DEFAULT_FEN;

    // Sets up a game from the provided FEN
    fn setup_game(fen: &str) -> GameState {
        GameState::new().from_fen(fen).unwrap()
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
        let state = setup_game(&DEFAULT_FEN);

        let test_setup_for = |color| {
            for pawn_pos in BitBoard::pawn_rank(color) {
                let legal_moves = [
                    pawn_pos.forward_by(color, 1).unwrap(),
                    pawn_pos.forward_by(color, 2).unwrap(),
                ];
                let moves = pawn_masks(pawn_pos, &state, color);

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
        let state = setup_game("8/8/8/8/8/2r1n3/3P4/8 w - - 0 1");
        let legal_moves = [Tile::D3, Tile::D4, Tile::C3, Tile::E3];
        let moves = pawn_masks(Tile::D2, &state, Color::White);
        lists_match(moves, &legal_moves);

        // Black
        // Can push two and capture on both diagonals
        let state = setup_game("8/5p2/4Q1B1/8/8/8/8/8 b - - 0 1");
        let legal_moves = [Tile::F6, Tile::F5, Tile::E6, Tile::G6];
        let moves = pawn_masks(Tile::F7, &state, Color::Black);
        lists_match(moves, &legal_moves);

        // Edge case
        // Black can move forward and capture one diagonal
        let state = setup_game("8/8/8/8/7p/n5B1/8/8 b - - 0 1");
        let legal_moves = [Tile::G3, Tile::H3];
        let moves = pawn_masks(Tile::H4, &state, Color::Black);
        lists_match(moves, &legal_moves);

        // Impossible case
        // White pawn at enemy home rank and not promoted; cannot move
        let state = setup_game("3P4/8/8/8/8/8/8/8 w - - 0 1");
        let legal_moves = [];
        let moves = pawn_masks(Tile::D8, &state, Color::White);
        lists_match(moves, &legal_moves);
    }

    #[test]
    fn en_passant() {
        // White pawn can move forward or en passant
        let state = setup_game("8/8/8/3pP3/8/8/8/8 w - d6 0 1");
        let legal_moves = [Tile::E6, Tile::D6];
        let moves = pawn_masks(Tile::E5, &state, Color::White);
        lists_match(moves, &legal_moves);

        // White pawn can move forward, but NOT en passant
        let state = setup_game("8/8/8/3p3P/8/8/8/8 w - d6 0 1");
        let legal_moves = [Tile::H6];
        let moves = pawn_masks(Tile::H5, &state, Color::White);
        lists_match(moves, &legal_moves);
    }

    #[test]
    fn rook_blockers() {
        // let state = setup_game("8/3n4/3Q4/8/1npR2N1/8/8/8 w - - 0 1");

        let legal_moves = [
            Tile::D1,
            Tile::D2,
            Tile::D3,
            Tile::D5,
            Tile::C4,
            Tile::E4,
            Tile::F4,
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
        let moves = get_rook_moves(Tile::D4, blockers);
        lists_match(moves, &legal_moves);
    }
}
