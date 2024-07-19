use super::{BitBoard, Tile};

pub const DEFAULT_FEN: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

/// https://www.chessprogramming.org/Chess_Position#cite_note-4
pub const MAX_NUM_MOVES: usize = 218;

pub const NUM_PIECE_TYPES: usize = 6;

pub const NUM_COLORS: usize = 2;

pub const QUEEN_DELTAS: [(i8, i8); 8] = [
    /* Rook */
    (1, 0),
    (0, -1),
    (-1, 0),
    (0, 1),
    /* Bishop */
    (1, 1),
    (1, -1),
    (-1, -1),
    (-1, 1),
];

pub const ROOK_DELTAS: [(i8, i8); 4] = [
    QUEEN_DELTAS[0],
    QUEEN_DELTAS[1],
    QUEEN_DELTAS[2],
    QUEEN_DELTAS[3],
];

pub const BISHOP_DELTAS: [(i8, i8); 4] = [
    QUEEN_DELTAS[4],
    QUEEN_DELTAS[5],
    QUEEN_DELTAS[6],
    QUEEN_DELTAS[7],
];

#[allow(dead_code)]
fn generate_ray_table() -> [[BitBoard; Tile::COUNT]; Tile::COUNT] {
    let mut rays = [[BitBoard::EMPTY_BOARD; Tile::COUNT]; Tile::COUNT];

    // let mut orthogonal_count = 0;
    // let mut diagonal_count = 0;
    for from in Tile::iter() {
        for (df, dr) in ROOK_DELTAS {
            let mut ray = from.bitboard();
            let mut to = from;
            while let Ok(shifted) = to.try_offset(df, dr) {
                ray.set(shifted);
                to = shifted;
                rays[from][to] = ray;
                // orthogonal_count += 1;
                // println!("{from} -> {to}\n{ray}\n---------------");
            }
        }
        for (df, dr) in BISHOP_DELTAS {
            let mut ray = from.bitboard();
            let mut to = from;
            while let Ok(shifted) = to.try_offset(df, dr) {
                ray.set(shifted);
                to = shifted;
                rays[from][to] = ray;
                // diagonal_count += 1;
                // println!("{from} -> {to}\n{ray}\n---------------");
            }
        }
    }
    // println!("Wrote {orthogonal_count} orthogonal rays and {diagonal_count} diagonals");

    // println!("#[rustfmt::skip]\nconst RAYS: [[BitBoard; 64]; 64] = {rays:?};");
    rays
}
