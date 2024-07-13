use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use std::path::PathBuf;

use crate::core::magics::*;
use crate::core::{BitBoard, Tile};

fn magic_index(entry: &MagicEntry, blockers: BitBoard) -> usize {
    let blockers = blockers.0 & entry.mask;
    let hash = blockers.wrapping_mul(entry.magic);
    let index = (hash >> entry.shift) as usize;
    entry.offset as usize + index
}

fn slider_moves(slider_deltas: &[(i8, i8)], tile: Tile, blockers: BitBoard) -> BitBoard {
    let mut moves = BitBoard::EMPTY_BOARD;
    for &(df, dr) in slider_deltas {
        let mut ray = tile;
        while !blockers.get(ray) {
            if let Ok(shifted) = ray.try_offset(df, dr) {
                ray = shifted;
                moves |= ray.bitboard();
            } else {
                break;
            }
        }
    }
    moves
}

fn make_table(
    table_size: usize,
    slider_deltas: &[(i8, i8)],
    magics: &[MagicEntry; Tile::COUNT],
) -> Vec<BitBoard> {
    let mut table = vec![BitBoard::EMPTY_BOARD; table_size];
    for tile in Tile::iter() {
        let magic_entry = &magics[tile];
        let mask = BitBoard(magic_entry.mask);

        let mut blockers = BitBoard::EMPTY_BOARD;
        loop {
            let moves = slider_moves(slider_deltas, tile, blockers);
            table[magic_index(magic_entry, blockers)] = moves;

            // Carry-Rippler trick that enumerates all subsets of the mask, getting us all blockers.
            // https://www.chessprogramming.org/Traversing_Subsets_of_a_Set#All_Subsets_of_any_Set
            blockers.0 = blockers.0.wrapping_sub(mask.0) & mask.0;
            if blockers.is_empty() {
                break;
            }
        }
    }
    table
}

fn write_table(name: &str, table: &[BitBoard], out: &mut impl Write) -> std::io::Result<()> {
    write!(out, "const {}_MOVES: &[u64; {}] = &[", name, table.len())?;
    for entry in table {
        write!(out, "{},", entry.0)?;
    }
    write!(out, "];")?;
    Ok(())
}

fn write_magics(
    name: &str,
    magics: &[MagicEntry; Tile::COUNT],
    out: &mut impl Write,
) -> std::io::Result<()> {
    write!(
        out,
        "const {}_MAGICS: &[MagicEntry; {}] = &[",
        name,
        Tile::COUNT
    )?;
    for entry in magics {
        write!(
            out,
            "MagicEntry {{ mask: {}, magic: {}, shift: {}, offset: {} }},",
            entry.mask, entry.magic, entry.shift, entry.offset
        )?;
    }
    write!(out, "];")?;
    Ok(())
}

pub fn write_moves() {
    let rook_table = make_table(
        ROOK_TABLE_SIZE,
        &[(1, 0), (0, -1), (-1, 0), (0, 1)],
        ROOK_MAGICS,
    );
    let bishop_table = make_table(
        BISHOP_TABLE_SIZE,
        &[(1, 1), (1, -1), (-1, -1), (-1, 1)],
        BISHOP_MAGICS,
    );

    // let mut out: PathBuf = std::env::var("OUT_DIR").unwrap().into();
    let mut out = PathBuf::from("src/core");
    out.push("pregenerated_magics.rs");
    let mut out = BufWriter::new(File::create(out).unwrap());

    write_magics("ROOK", ROOK_MAGICS, &mut out).unwrap();
    write_magics("BISHOP", BISHOP_MAGICS, &mut out).unwrap();
    write_table("ROOK", &rook_table, &mut out).unwrap();
    write_table("BISHOP", &bishop_table, &mut out).unwrap();
}
