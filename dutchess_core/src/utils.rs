use std::path::{Path, PathBuf};

use super::{BitBoard, Color, Tile};

pub const FEN_STARTPOS: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
pub const FEN_KIWIPETE: &'static str =
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -";

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

pub const KNIGHT_DELTAS: [(i8, i8); 8] = [
    (1, 2),
    (1, -2),
    (2, 1),
    (2, -1),
    (-1, 2),
    (-1, -2),
    (-2, 1),
    (-2, -1),
];

pub fn generate_ray_between_table() -> [[BitBoard; Tile::COUNT]; Tile::COUNT] {
    let mut ray_between = [[BitBoard::EMPTY_BOARD; Tile::COUNT]; Tile::COUNT];

    // let mut orthogonal_count = 0;
    // let mut diagonal_count = 0;
    for from in Tile::iter() {
        for (df, dr) in ROOK_DELTAS {
            let mut ray = from.bitboard();
            let mut to = from;
            while let Ok(shifted) = to.try_offset(df, dr) {
                ray.set(shifted);
                to = shifted;
                ray_between[from][to] = ray;
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
                ray_between[from][to] = ray;
                // diagonal_count += 1;
                // println!("{from} -> {to}\n{ray}\n---------------");
            }
        }
    }
    // println!("Wrote {orthogonal_count} orthogonal rays and {diagonal_count} diagonals");

    // println!("#[rustfmt::skip]\nconst RAYS: [[BitBoard; 64]; 64] = {rays:?};");
    ray_between
}

pub fn generate_ray_table() -> [[BitBoard; Tile::COUNT]; Tile::COUNT] {
    let mut rays = [[BitBoard::EMPTY_BOARD; Tile::COUNT]; Tile::COUNT];

    // let mut orthogonal_count = 0;
    // let mut diagonal_count = 0;
    for from in Tile::iter() {
        let fr = from.rank();
        let ff = from.file();
        for to in Tile::iter() {
            let tr = to.rank();
            let tf = to.file();
            if from == to {
                rays[from][to] = BitBoard::from_tile(from);
            } else if fr == tr {
                rays[from][to] = BitBoard::from_rank(fr);
            } else if ff == tf {
                rays[from][to] = BitBoard::from_file(ff);
            } else {
                // To check if these lie on a diagonal, compute (y1 - y2) / (x1 - x2)
                let file_diff = from.file().inner() as i32 - to.file().inner() as i32;
                let rank_diff = from.rank().inner() as i32 - to.rank().inner() as i32;

                // Checked division
                let diff = if rank_diff != 0 {
                    file_diff / rank_diff
                } else {
                    0
                };

                if diff == 1 {
                    rays[from][to] |= from.bitboard();
                    // I'm too lazy to figure out the proper math, so I'm just going to cast rays in the diagonals
                    let mut tmp = from;
                    // First ray goes Northeast
                    while let Ok(shifted) = tmp.try_offset(1, 1) {
                        rays[from][to] |= shifted.bitboard();
                        tmp = shifted;
                    }
                    tmp = from;
                    // Second ray goes Southwest
                    // I'm intentionally not resetting tmp here, so that the tile for `from` gets OR'd in
                    while let Ok(shifted) = tmp.try_offset(-1, -1) {
                        rays[from][to] |= shifted.bitboard();
                        tmp = shifted;
                    }
                } else if diff == -1 {
                    rays[from][to] |= from.bitboard();
                    // Do it again, in the Northwest / Southeast directions
                    let mut tmp = from;
                    while let Ok(shifted) = tmp.try_offset(-1, 1) {
                        rays[from][to] |= shifted.bitboard();
                        tmp = shifted;
                    }
                    tmp = from;
                    while let Ok(shifted) = tmp.try_offset(1, -1) {
                        rays[from][to] |= shifted.bitboard();
                        tmp = shifted;
                    }
                }
            }
        }
    }

    print!("#[rustfmt::skip]\nconst RAYS: [[BitBoard; 64]; 64] = {rays:?};");
    rays
}

/// Generates the default mobility for each of the pieces of standard chess, and writes the mobility to new files created in `outdir`.
///
/// This will produce the following 9 files located in `outdir`:
///     * `queen_mobility.blob`
///     * `rook_mobility.blob`
///     * `bishop_mobility.blob`
///     * `knight_mobility.blob`
///     * `king_mobility.blob`
///     * `white_pawn_push_mobility.blob`
///     * `black_pawn_push_mobility.blob`
///     * `white_pawn_attack_mobility.blob`
///     * `black_pawn_attack_mobility.blob`
///
/// You can use `include_bytes!()` to read from these blobs like so:
/// ```compile_fail
/// const KNIGHT_MOVES: [BitBoard; 64] = unsafe { std::mem::transmute(*include_bytes!("knight_mobility.blob")) };
/// ```
pub fn generate_mobility_blobs<P: AsRef<Path>>(outdir: P) -> std::io::Result<()> {
    // Generate the blobs
    let bishop: [u8; 512] = unsafe { std::mem::transmute(generate_bishop_mobility()) };
    let rook: [u8; 512] = unsafe { std::mem::transmute(generate_rook_mobility()) };
    let knight: [u8; 512] = unsafe { std::mem::transmute(generate_knight_mobility()) };
    let king: [u8; 512] = unsafe { std::mem::transmute(generate_king_mobility()) };
    let queen: [u8; 512] = unsafe { std::mem::transmute(generate_queen_mobility()) };
    let wpp: [u8; 512] = unsafe { std::mem::transmute(generate_pawn_pushes(Color::White)) };
    let bpp: [u8; 512] = unsafe { std::mem::transmute(generate_pawn_pushes(Color::Black)) };
    let wpa: [u8; 512] = unsafe { std::mem::transmute(generate_pawn_attacks(Color::White)) };
    let bpa: [u8; 512] = unsafe { std::mem::transmute(generate_pawn_attacks(Color::Black)) };

    // Write the blobs
    let path = |name| {
        let outfile = format!("{name}_mobility.blob");
        PathBuf::from(outdir.as_ref()).join(outfile)
    };
    std::fs::write(path("bishop"), bishop)?;
    std::fs::write(path("rook"), rook)?;
    std::fs::write(path("knight"), knight)?;
    std::fs::write(path("king"), king)?;
    std::fs::write(path("queen"), queen)?;
    std::fs::write(path("white_pawn_push"), wpp)?;
    std::fs::write(path("black_pawn_push"), bpp)?;
    std::fs::write(path("white_pawn_attack"), wpa)?;
    std::fs::write(path("black_pawn_attack"), bpa)?;

    Ok(())
}

/// Generates the default push mobility for Pawns.
///
/// Pawns, by default, may push forward by one, except when pushing from their starting rank (rank 2 for White, rank 7 for Black), in which case they may push forward by two.
fn generate_pawn_pushes(color: Color) -> [BitBoard; 64] {
    let mut boards = [BitBoard::default(); Tile::COUNT];
    for tile in Tile::iter() {
        let bb = BitBoard::from_tile(tile);

        if tile.rank().is_pawn_rank(color) {
            boards[tile] = bb.advance_by(color, 1) | bb.advance_by(color, 2);
        } else {
            boards[tile] = bb.advance_by(color, 1);
        }
    }
    boards
}

/// Generates the default attack mobility for Pawns.
///
/// Pawns, by default, may capture diagonally forward by one.
fn generate_pawn_attacks(color: Color) -> [BitBoard; 64] {
    let mut boards = [BitBoard::default(); Tile::COUNT];
    for tile in Tile::iter() {
        let bb = BitBoard::from_tile(tile);

        boards[tile] = bb.advance_by(color, 1).east() | bb.advance_by(color, 1).west();
    }
    boards
}

/// Generates the moves from every location for the "Leaper" pieces.
/// Leapers may "leap" or "jump" to a square a specified distance away.
///
/// In standard chess, the Leapers are the King and Knight.
fn generate_leaper_mobility(deltas: &[(i8, i8)]) -> [BitBoard; Tile::COUNT] {
    // Represents all locations this piece can reach from that tile/index.
    let mut mobility = [BitBoard::default(); Tile::COUNT];

    for tile in Tile::iter() {
        // All reachable locations from `tile`.
        // This is empty because we cannot "move to" the tile where we are currently.
        let mut movement = BitBoard::default();

        // Loop over every pair of deltas
        for (df, dr) in deltas {
            // If shifting this location by the delta results in a valid position, add it to the movement mask.
            if let Ok(shifted) = tile.try_offset(*df, *dr) {
                movement.set(shifted);
            }
        }

        // Store the mobility from this tile.
        mobility[tile] = movement;
    }

    mobility
}

/// Generates the moves from every location for the "Rider" pieces.
/// Riders may "ride" or "slide" an unlimited number of squares in a direction.
///
/// In standard chess, the Riders are the Rook, Bishop, and Queen.
fn generate_rider_mobility(deltas: &[(i8, i8)]) -> [BitBoard; Tile::COUNT] {
    // Represents all locations this piece can reach from that tile/index.
    let mut mobility = [BitBoard::default(); Tile::COUNT];

    for tile in Tile::iter() {
        // All reachable locations from `tile`.
        // This is empty because we cannot "move to" the tile where we are currently.
        let mut movement = BitBoard::default();

        // Loop over every pair of deltas
        for (df, dr) in deltas {
            // Create a "ray" that represents movement in this direction.
            let mut ray = tile;

            // Shift the ray and append it's movement, until we reach the end of the board.
            while let Ok(shifted) = ray.try_offset(*df, *dr) {
                movement.set(shifted);
                ray = shifted;
            }
        }

        // Store the mobility from this tile.
        mobility[tile] = movement;
    }

    mobility
}

/// Generates the default mobility for the King.
fn generate_king_mobility() -> [BitBoard; 64] {
    generate_leaper_mobility(&QUEEN_DELTAS)
}

/// Generates the default mobility for the Knight.
fn generate_knight_mobility() -> [BitBoard; 64] {
    generate_leaper_mobility(&KNIGHT_DELTAS)
}

/// Generates the default mobility for the Rook.
fn generate_rook_mobility() -> [BitBoard; Tile::COUNT] {
    generate_rider_mobility(&ROOK_DELTAS)
}

/// Generates the default mobility for the Bishop.
fn generate_bishop_mobility() -> [BitBoard; Tile::COUNT] {
    generate_rider_mobility(&BISHOP_DELTAS)
}

/// Generates the default mobility for the Queen.
fn generate_queen_mobility() -> [BitBoard; Tile::COUNT] {
    generate_rider_mobility(&QUEEN_DELTAS)
}
