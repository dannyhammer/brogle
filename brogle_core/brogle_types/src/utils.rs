use std::path::Path;

use super::{Bitboard, Color, Rank, Tile};

pub const FEN_STARTPOS: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
pub const FEN_KIWIPETE: &str = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -";

/// <https://www.chessprogramming.org/Chess_Position#cite_note-4>
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

pub fn generate_ray_table_datfiles<P: AsRef<Path>>(outdir: P) -> std::io::Result<()> {
    // 64 * 64 * 8 = 32,768
    // 2D Bitboard array being cast to u8 (8 u8 in a u64)

    // Generate the blobs
    let ray_between_inclusive: [u8; 32_768] =
        unsafe { std::mem::transmute(generate_ray_between_inclusive_table()) };
    let ray_between_exclusive: [u8; 32_768] =
        unsafe { std::mem::transmute(generate_ray_between_exclusive_table()) };
    let ray_containing: [u8; 32_768] =
        unsafe { std::mem::transmute(generate_ray_containing_table()) };

    // Write the blobs
    let path = |name| Path::new(outdir.as_ref()).join(name);

    std::fs::write(path("ray_between_inclusive.dat"), ray_between_inclusive)?;
    std::fs::write(path("ray_between_exclusive.dat"), ray_between_exclusive)?;
    std::fs::write(path("ray_containing.dat"), ray_containing)?;

    Ok(())
}

fn generate_ray_between_inclusive_table() -> [[Bitboard; Tile::COUNT]; Tile::COUNT] {
    let mut rays = [[Bitboard::EMPTY_BOARD; Tile::COUNT]; Tile::COUNT];

    for from in Tile::iter() {
        for (df, dr) in QUEEN_DELTAS {
            let mut ray = from.bitboard(); // Include `from`
            let mut to = from;
            while let Some(shifted) = to.offset(df, dr) {
                ray.set(shifted);
                to = shifted;
                rays[from][to] = ray;
            }
        }
    }

    rays
}

fn generate_ray_between_exclusive_table() -> [[Bitboard; Tile::COUNT]; Tile::COUNT] {
    let mut rays = [[Bitboard::EMPTY_BOARD; Tile::COUNT]; Tile::COUNT];

    for from in Tile::iter() {
        for (df, dr) in QUEEN_DELTAS {
            let mut ray = Bitboard::default(); // Do not include `from`
            let mut to = from;
            while let Some(shifted) = to.offset(df, dr) {
                ray.set(shifted);
                to = shifted;
                rays[from][to] = ray ^ to.bitboard(); // Do not include `to`
            }
        }
    }

    rays
}

fn generate_ray_containing_table() -> [[Bitboard; Tile::COUNT]; Tile::COUNT] {
    let mut rays = [[Bitboard::EMPTY_BOARD; Tile::COUNT]; Tile::COUNT];

    for from in Tile::iter() {
        let fr = from.rank();
        let ff = from.file();
        for to in Tile::iter() {
            let tr = to.rank();
            let tf = to.file();
            if from == to {
                rays[from][to] = Bitboard::from_tile(from);
            } else if fr == tr {
                rays[from][to] = Bitboard::from_rank(fr);
            } else if ff == tf {
                rays[from][to] = Bitboard::from_file(ff);
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
                    while let Some(shifted) = tmp.offset(1, 1) {
                        rays[from][to] |= shifted.bitboard();
                        tmp = shifted;
                    }
                    tmp = from;
                    // Second ray goes Southwest
                    // I'm intentionally not resetting tmp here, so that the tile for `from` gets OR'd in
                    while let Some(shifted) = tmp.offset(-1, -1) {
                        rays[from][to] |= shifted.bitboard();
                        tmp = shifted;
                    }
                } else if diff == -1 {
                    rays[from][to] |= from.bitboard();
                    // Do it again, in the Northwest / Southeast directions
                    let mut tmp = from;
                    while let Some(shifted) = tmp.offset(-1, 1) {
                        rays[from][to] |= shifted.bitboard();
                        tmp = shifted;
                    }
                    tmp = from;
                    while let Some(shifted) = tmp.offset(1, -1) {
                        rays[from][to] |= shifted.bitboard();
                        tmp = shifted;
                    }
                }
            }
        }
    }

    rays
}

/// Generates the default mobility for each of the pieces of standard chess, and writes the mobility to new files created in `outdir`.
///
/// This will produce the following 9 files located in `outdir`:
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
/// const KNIGHT_MOVES: [Bitboard; 64] = unsafe { std::mem::transmute(*include_bytes!("knight_mobility.blob")) };
/// ```
pub fn generate_piece_attack_datfiles<P: AsRef<Path>>(outdir: P) -> std::io::Result<()> {
    // Generate the blobs
    let bishop: [u8; 512] = unsafe { std::mem::transmute(generate_bishop_mobility()) };
    let rook: [u8; 512] = unsafe { std::mem::transmute(generate_rook_mobility()) };
    let knight: [u8; 512] = unsafe { std::mem::transmute(generate_knight_mobility()) };
    let king: [u8; 512] = unsafe { std::mem::transmute(generate_king_mobility()) };
    let wpp: [u8; 512] = unsafe { std::mem::transmute(generate_pawn_pushes(Color::White)) };
    let bpp: [u8; 512] = unsafe { std::mem::transmute(generate_pawn_pushes(Color::Black)) };
    let wpa: [u8; 512] = unsafe { std::mem::transmute(generate_pawn_attacks(Color::White)) };
    let bpa: [u8; 512] = unsafe { std::mem::transmute(generate_pawn_attacks(Color::Black)) };

    // Write the blobs
    let path = |name| Path::new(outdir.as_ref()).join(name);
    std::fs::write(path("bishop_attacks.dat"), bishop)?;
    std::fs::write(path("rook_attacks.dat"), rook)?;
    std::fs::write(path("knight_attacks.dat"), knight)?;
    std::fs::write(path("king_attacks.dat"), king)?;
    std::fs::write(path("white_pawn_pushes.dat"), wpp)?;
    std::fs::write(path("black_pawn_pushes.dat"), bpp)?;
    std::fs::write(path("white_pawn_attacks.dat"), wpa)?;
    std::fs::write(path("black_pawn_attacks.dat"), bpa)?;

    Ok(())
}

/// Generates the default push mobility for Pawns.
///
/// Pawns, by default, may push forward by one, except when pushing from their starting rank (rank 2 for White, rank 7 for Black), in which case they may push forward by two.
fn generate_pawn_pushes(color: Color) -> [Bitboard; 64] {
    let mut boards = [Bitboard::default(); Tile::COUNT];
    for tile in Tile::iter() {
        let bb = Bitboard::from_tile(tile);

        if tile.rank() == Rank::second(color) {
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
fn generate_pawn_attacks(color: Color) -> [Bitboard; 64] {
    let mut boards = [Bitboard::default(); Tile::COUNT];
    for tile in Tile::iter() {
        let bb = Bitboard::from_tile(tile);

        boards[tile] = bb.advance_by(color, 1).east() | bb.advance_by(color, 1).west();
    }
    boards
}

/// Generates the moves from every location for the "Leaper" pieces.
/// Leapers may "leap" or "jump" to a square a specified distance away.
///
/// In standard chess, the Leapers are the King and Knight.
fn generate_leaper_mobility(deltas: &[(i8, i8)]) -> [Bitboard; Tile::COUNT] {
    // Represents all locations this piece can reach from that tile/index.
    let mut mobility = [Bitboard::default(); Tile::COUNT];

    for tile in Tile::iter() {
        // All reachable locations from `tile`.
        // This is empty because we cannot "move to" the tile where we are currently.
        let mut movement = Bitboard::default();

        // Loop over every pair of deltas
        for (df, dr) in deltas {
            // If shifting this location by the delta results in a valid position, add it to the movement mask.
            if let Some(shifted) = tile.offset(*df, *dr) {
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
fn generate_rider_mobility(deltas: &[(i8, i8)]) -> [Bitboard; Tile::COUNT] {
    // Represents all locations this piece can reach from that tile/index.
    let mut mobility = [Bitboard::default(); Tile::COUNT];

    for tile in Tile::iter() {
        // All reachable locations from `tile`.
        // This is empty because we cannot "move to" the tile where we are currently.
        let mut movement = Bitboard::default();

        // Loop over every pair of deltas
        for (df, dr) in deltas {
            // Create a "ray" that represents movement in this direction.
            let mut ray = tile;

            // Shift the ray and append it's movement, until we reach the end of the board.
            while let Some(shifted) = ray.offset(*df, *dr) {
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
fn generate_king_mobility() -> [Bitboard; 64] {
    generate_leaper_mobility(&QUEEN_DELTAS)
}

/// Generates the default mobility for the Knight.
fn generate_knight_mobility() -> [Bitboard; 64] {
    generate_leaper_mobility(&KNIGHT_DELTAS)
}

/// Generates the default mobility for the Rook.
fn generate_rook_mobility() -> [Bitboard; Tile::COUNT] {
    generate_rider_mobility(&ROOK_DELTAS)
}

/// Generates the default mobility for the Bishop.
fn generate_bishop_mobility() -> [Bitboard; Tile::COUNT] {
    generate_rider_mobility(&BISHOP_DELTAS)
}

/*
/// Generates the default mobility for the Queen.
fn generate_queen_mobility() -> [Bitboard; Tile::COUNT] {
    generate_rider_mobility(&QUEEN_DELTAS)
}

/// Generates the default mobility for the Dragon (Queen + Knight).
fn generate_dragon_mobility() -> [Bitboard; Tile::COUNT] {
    let mut dragon = generate_rider_mobility(&QUEEN_DELTAS);
    let knight = generate_leaper_mobility(&KNIGHT_DELTAS);

    for tile in Tile::iter() {
        dragon[tile] |= knight[tile];
    }

    dragon
}
 */
