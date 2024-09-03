use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use std::path::PathBuf;

use chessie::{Bitboard, Tile};
mod magics;
use crate::magics::*;

fn magic_index(entry: &MagicEntry, blockers: Bitboard) -> usize {
    let blockers = blockers.inner() & entry.mask;
    let hash = blockers.wrapping_mul(entry.magic);
    let index = (hash >> entry.shift) as usize;
    entry.offset + index
}

fn slider_moves(slider_deltas: &[(i8, i8)], tile: Tile, blockers: Bitboard) -> Bitboard {
    let mut moves = Bitboard::EMPTY_BOARD;
    for &(df, dr) in slider_deltas {
        let mut ray = tile;
        while !blockers.get(ray) {
            if let Some(shifted) = ray.offset(df, dr) {
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
) -> Vec<Bitboard> {
    let mut table = vec![Bitboard::EMPTY_BOARD; table_size];
    for tile in Tile::iter() {
        let magic_entry = &magics[tile];
        let mask = Bitboard::new(magic_entry.mask);

        let mut blockers = Bitboard::EMPTY_BOARD;
        loop {
            let moves = slider_moves(slider_deltas, tile, blockers);
            table[magic_index(magic_entry, blockers)] = moves;

            blockers = blockers.carry_rippler(mask);
            if blockers.is_empty() {
                break;
            }
        }
    }
    table
}

fn write_table(name: &str, table: &[Bitboard], out: &mut impl Write) -> std::io::Result<()> {
    write!(out, "const {}_MOVES: &[u64; {}] = &[", name, table.len())?;
    for entry in table {
        write!(out, "{},", entry.inner())?;
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

pub fn main() {
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
    let out = PathBuf::from("pregenerated_magics.rs");
    let mut out = BufWriter::new(File::create(out).unwrap());

    write_magics("ROOK", ROOK_MAGICS, &mut out).unwrap();
    write_magics("BISHOP", BISHOP_MAGICS, &mut out).unwrap();
    write_table("ROOK", &rook_table, &mut out).unwrap();
    write_table("BISHOP", &bishop_table, &mut out).unwrap();
}
