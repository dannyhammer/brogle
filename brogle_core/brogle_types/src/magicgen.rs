use rand::random;

use super::{Bitboard, Tile};

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
struct MagicEntry {
    mask: u64,
    magic: u64,
    shift: u8,
    // pub(crate) offset: usize,
}

fn magic_index(entry: &MagicEntry, blockers: Bitboard) -> usize {
    let blockers = blockers.0 & entry.mask;
    let hash = blockers.wrapping_mul(entry.magic);
    // let index = (hash >> entry.shift) as usize;
    // entry.offset + index
    // index
    (hash >> entry.shift) as usize
}

// #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct SlidingPiece {
    deltas: [(i8, i8); 4],
}

impl SlidingPiece {
    const ROOK: Self = Self {
        deltas: [(1, 0), (0, -1), (-1, 0), (0, 1)],
    };

    const BISHOP: Self = Self {
        deltas: [(1, 1), (1, -1), (-1, -1), (-1, 1)],
    };

    fn blockers(&self, tile: Tile) -> Bitboard {
        let mut blockers = Bitboard::default();
        for (df, dr) in self.deltas {
            let mut ray = tile;
            while let Some(shifted) = ray.offset(df, dr) {
                blockers |= ray.bitboard();
                ray = shifted;
            }
        }

        blockers &= !tile.bitboard();
        blockers
    }

    fn moves(&self, tile: Tile, blockers: Bitboard) -> Bitboard {
        let mut moves = Bitboard::EMPTY_BOARD;

        for (df, dr) in self.deltas {
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
}

fn find_magic(slider: &SlidingPiece, tile: Tile, index_bits: u8) -> (MagicEntry, Vec<Bitboard>) {
    let mask = slider.blockers(tile).0;
    let shift = 64 - index_bits;
    loop {
        let magic = random::<u64>() & random::<u64>() & random::<u64>();
        let magic_entry = MagicEntry { mask, magic, shift };
        if let Ok(table) = try_make_table(slider, tile, &magic_entry) {
            return (magic_entry, table);
        }
    }
}
fn try_make_table(
    slider: &SlidingPiece,
    tile: Tile,
    magic_entry: &MagicEntry,
) -> Result<Vec<Bitboard>, &'static str> {
    let index_bits = 64 - magic_entry.shift;
    let mut table = vec![Bitboard::EMPTY_BOARD; 1 << index_bits];

    let blockers = Bitboard::EMPTY_BOARD;
    loop {
        let moves = slider.moves(tile, blockers);
        let table_entry = &mut table[magic_index(magic_entry, blockers)];

        if table_entry.is_empty() {
            *table_entry = moves;
        } else if *table_entry != moves {
            return Err("Table Fill Error");
        }

        blockers.carry_rippler(Bitboard(magic_entry.mask));
        if blockers.is_empty() {
            break;
        }
    }
    Ok(table)
}
fn find_and_print_all_magics(slider: &SlidingPiece, piece_name: &str) {
    println!(
        "pub const {piece_name}_MAGICS: &[MagicEntry; {}] = &[",
        Tile::COUNT
    );

    let mut table_size = 0;

    for tile in Tile::iter() {
        let index_bits = slider.blockers(tile).population();
        let (entry, table) = find_magic(slider, tile, index_bits);

        println!(
            "  MagicEntry {{ mask: 0x{:016X}, magic: 0x{:016X}, shift: {}, offset: {} }},",
            entry.mask, entry.magic, entry.shift, table_size
        );

        table_size += table.len();
    }

    println!("];");
    println!("pub const {piece_name}_TABLE_SIZE: usize = {table_size};");
}

pub fn print_magics() {
    find_and_print_all_magics(&SlidingPiece::ROOK, "ROOK");
    find_and_print_all_magics(&SlidingPiece::BISHOP, "BISHOP");
}

////////////////////// Below is for move generation, and requires magics to be generated

/*
fn make_table(slider: &SlidingPiece,size: usize, magics: &[MagicEntry; Tile::COUNT]) -> Vec<Bitboard> {
    let mut table = vec![Bitboard::default(); size];

    for tile in Tile::iter() {
        let magic_entry = &magics[tile];
        let mask = Bitboard::new(magic_entry.mask);

        let mut blockers = Bitboard::EMPTY_BOARD;
        loop {
        let moves = slider.moves(tile, blockers);
        table[magic_index(magic_entry, blockers)] = moves;


        blockers.carry_rippler(Bitboard(magic_entry.mask));
        if blockers.is_empty() {
            break;
        }

        }
    }

    table
}

fn find_magics(slider: &SlidingPiece) -> ([MagicEntry; Tile::COUNT], usize) {
    let mut table_size = 0;
    let mut magics = [MagicEntry::default(); Tile::COUNT];

    for tile in Tile::iter() {
        let index_bits = slider.blockers(tile).population() as u8;
        let (entry, table) = find_magic(slider, tile, index_bits, table_size);
        magics[tile] = entry;
        table_size += table.len();
    }

    (magics, table_size)
}

fn write_magics(
    name: &str,
    magics: &[MagicEntry; Tile::COUNT],
    out: &mut impl Write,
) -> io::Result<()> {
    write!(
        out,
        "pub(crate) const {name}_MAGICS: &[MagicEntry; Tile::COUNT] = &["
    )?;

    for entry in magics {

        write!(out,
            "  MagicEntry {{ mask: Bitboard(0x{:016X}), magic: 0x{:016X}, shift: {}, offset: {} }},",
            entry.mask,entry.magic, entry.shift, entry.offset)?;
    }

    write!(out, "];")?;

    Ok(())
}

pub fn magics_gen() {
    // let mut out = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    let mut out = PathBuf::from("src/core");
    out.push("magics.rs");
    let mut out = BufWriter::new(File::create(out).unwrap());
    write!(out, "use super::{{Bitboard, MagicEntry, Tile}};").unwrap();

    let (rook_magics, rook_table_size) = find_magics(&SlidingPiece::ROOK);
    let (bishop_magics, bishop_table_size) = find_magics(&SlidingPiece::ROOK);

    write_magics("ROOK", &rook_magics, &mut out).unwrap();
    write_magics("BISHOP", &bishop_magics, &mut out).unwrap();
}
 */
