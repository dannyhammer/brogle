use crate::{
    // utils::{BISHOP_INDEX_BITS, BISHOP_MAGICS, ROOK_INDEX_BITS, ROOK_MAGICS},
    BitBoard,
    ChessBoard,
    Color,
    Piece,
    PieceKind,
    Tile,
};

const BISHOP_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("masks/bishop_masks.blob")) };

const ROOK_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("masks/rook_masks.blob")) };

const KNIGHT_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("masks/knight_masks.blob")) };

const KING_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("masks/king_masks.blob")) };

const QUEEN_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("masks/queen_masks.blob")) };

// const BISHOP_ATTACKS: [[BitBoard; 512]; 64] =
//     unsafe { std::mem::transmute(*include_bytes!("masks/bishop_attacks.blob")) };

// const ROOK_ATTACKS: [[BitBoard; 4096]; 64] =
//     unsafe { std::mem::transmute(*include_bytes!("masks/rook_attacks.blob")) };

pub fn moves_for(piece: &Piece, tile: Tile, board: &ChessBoard) -> BitBoard {
    use PieceKind::*;

    // println!("Computing moves for {piece} at {tile}");

    let moves = match piece.kind() {
        Pawn => pawn_masks(tile, piece.color(), board),
        Knight => knight_masks(tile),
        Bishop => bishop_masks(tile),
        Rook => rook_masks(tile),
        Queen => queen_masks(tile),
        King => king_masks(tile),
    };

    // All squares that can block a piece, excluding edge squares
    // let blocker_mask = piece_mask & !BitBoard::EDGES;

    // Includes friendly and enemy pieces, and is a subset of the blocker mask
    // let blocker_board = board.blockers(blocker_mask);

    // Will result in a board that can capture your own pieces
    // let move_board = BitBoard::default();

    // moves & !board[piece.color()]
    // moves
    //     & enemy_or_empty(piece.color(), board)
    //     & checkmask(piece.color(), board)
    //     & !(pinmask(piece.color(), board))

    // moves.and(board.color(piece.color()).not())
    moves
}

/// Get all squares that are either empty or occupied by the enemy
const fn enemy_or_empty(color: Color, board: &ChessBoard) -> BitBoard {
    board.color(color).not()
}

const fn checkmask(color: Color, board: &ChessBoard) -> BitBoard {
    todo!()
}

const fn pinmask(color: Color, board: &ChessBoard) -> BitBoard {
    // Horizontal/Vertical and then both diags
    todo!()
}

const fn pawn_masks(tile: Tile, color: Color, board: &ChessBoard) -> BitBoard {
    let src = BitBoard::from_tile(tile);
    if color.is_white() {
        src.north()
    } else {
        src.south()
    }
}

const fn knight_masks(tile: Tile) -> BitBoard {
    KNIGHT_MASKS[tile.index()]
}

const fn bishop_masks(tile: Tile) -> BitBoard {
    BISHOP_MASKS[tile.index()]
}

const fn rook_masks(tile: Tile) -> BitBoard {
    ROOK_MASKS[tile.index()]
}

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

#[cfg(test)]
mod test {

    #[test]
    fn test_perft() {
        //
    }
}
