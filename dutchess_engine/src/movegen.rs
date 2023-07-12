use crate::{BitBoard, ChessBoard, Color, Piece, PieceKind, Tile};

pub const BISHOP_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("masks/bishop_masks.blob")) };

pub const ROOK_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("masks/rook_masks.blob")) };

pub const KNIGHT_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("masks/knight_masks.blob")) };

pub const KING_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("masks/king_masks.blob")) };

pub const QUEEN_MASKS: [BitBoard; 64] =
    unsafe { std::mem::transmute(*include_bytes!("masks/queen_masks.blob")) };

pub fn moves_for(piece: &Piece, tile: Tile, board: &ChessBoard) -> BitBoard {
    use PieceKind::*;

    // println!("Computing moves for {piece} at {tile}");
    let moves = match piece.kind() {
        Pawn => pawn_masks(tile, piece.color()),
        Knight => knight_masks(tile),
        Bishop => bishop_masks(tile),
        Rook => rook_masks(tile),
        Queen => queen_masks(tile),
        King => king_masks(tile),
    };

    // moves & !board[piece.color()]
    // moves
    //     & enemy_or_empty(piece.color(), board)
    //     & checkmask(piece.color(), board)
    //     & !(pinmask(piece.color(), board))

    moves.and(board.color(piece.color()).not())
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

const fn pawn_masks(tile: Tile, color: Color) -> BitBoard {
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

/*
fn blocker_mask_rook(tile: Tile) -> BitBoard {
    todo!()
}

fn blocker_mask_bishop(tile: Tile) -> BitBoard {
    todo!()
}

fn moveboard_rook(tile: Tile) -> BitBoard {
    todo!()
}

fn moveboard_bishop(tile: Tile) -> BitBoard {
    todo!()
}

fn blockerboard(tile: Tile) -> BitBoard {
    // blockers = occupancy & blockermask
    todo!()
}

fn magic_move_rook(tile: Tile, mut occupancy: BitBoard) -> BitBoard {
    /*
    /* Remove occupants that aren't in the blocker mask for this square. */
    occupancy &= Rook.blockmask[square];
    /* Calculate the magic move index. */
    int index = (occupancy*Rook.magic[square]) >> (64-Rook.bits[square]);
    /* Return the pre-calculated move board. */
    return Rook.moveboard[square][index];
    */

    todo!()
}
 */
