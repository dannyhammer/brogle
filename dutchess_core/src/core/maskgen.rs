use super::BitBoard;

pub fn generate_blobs() {
    let bishop: [u8; 512] = unsafe { std::mem::transmute(bishop_moves()) };
    let rook: [u8; 512] = unsafe { std::mem::transmute(rook_moves()) };
    let knight: [u8; 512] = unsafe { std::mem::transmute(knight_moves()) };
    let king: [u8; 512] = unsafe { std::mem::transmute(king_moves()) };
    let queen: [u8; 512] = unsafe { std::mem::transmute(queen_moves()) };

    std::fs::write("dutchess_engine/src/masks/bishop_masks.blob", bishop).unwrap();
    std::fs::write("dutchess_engine/src/masks/rook_masks.blob", rook).unwrap();
    std::fs::write("dutchess_engine/src/masks/knight_masks.blob", knight).unwrap();
    std::fs::write("dutchess_engine/src/masks/king_masks.blob", king).unwrap();
    std::fs::write("dutchess_engine/src/masks/queen_masks.blob", queen).unwrap();

    // let rook_attacks = rook_attacks();
    // println!("{:?}", rook_attacks);
    // let rook_attacks: [u8; 2097152] = unsafe { std::mem::transmute(rook_attacks()) };
    // let bishop_attacks: [u8; 262144] = unsafe { std::mem::transmute(bishop_attacks()) };
    // std::fs::write("dutchess_engine/src/masks/rook_attacks.blob", rook_attacks).unwrap();
}

fn king_moves() -> [BitBoard; 64] {
    let mut boards = [BitBoard::default(); 64];

    for i in 0..64 {
        let bb = BitBoard::from_index(i);

        boards[i as usize] = bb.north()
            | bb.northeast()
            | bb.east()
            | bb.southeast()
            | bb.south()
            | bb.southwest()
            | bb.west()
            | bb.northwest();
    }

    boards
}

fn knight_moves() -> [BitBoard; 64] {
    let mut boards = [BitBoard::default(); 64];

    for i in 0..64 {
        let bb = BitBoard::from_index(i);

        let n = bb.north();
        let s = bb.south();
        let e = bb.east();
        let w = bb.west();

        boards[i as usize] = n.northwest()
            | n.northeast()
            | s.southwest()
            | s.southeast()
            | e.northeast()
            | e.southeast()
            | w.northwest()
            | w.southwest();
    }

    boards
}

fn rook_moves() -> [BitBoard; 64] {
    let mut boards = [BitBoard::default(); 64];

    for i in 0..64 {
        let bb = BitBoard::from_index(i);

        let mut n = bb.north();
        let mut s = bb.south();
        let mut e = bb.east();
        let mut w = bb.west();
        for _ in 0..6 {
            n |= n.north();
            s |= s.south();
            e |= e.east();
            w |= w.west();
        }

        boards[i as usize] = n | s | e | w;
    }

    boards
}

fn bishop_moves() -> [BitBoard; 64] {
    let mut boards = [BitBoard::default(); 64];

    for i in 0..64 {
        let bb = BitBoard::from_index(i);

        let mut ne = bb.northeast();
        let mut nw = bb.northwest();
        let mut se = bb.southeast();
        let mut sw = bb.southwest();
        for _ in 0..6 {
            ne |= ne.northeast();
            nw |= nw.northwest();
            se |= se.southeast();
            sw |= sw.southwest();
        }

        boards[i as usize] = ne | nw | se | sw;
    }

    boards
}

fn queen_moves() -> [BitBoard; 64] {
    let mut boards = [BitBoard::default(); 64];
    let bishop = bishop_moves();
    let rook = rook_moves();

    for i in 0..64 {
        boards[i] = bishop[i] | rook[i];
    }

    boards
}

/*
// https://github.com/nkarve/surge/blob/master/src/tables.cpp#L138
fn rook_attacks() -> [[BitBoard; 4096]; 64] {
    let mut attacks = [[BitBoard::default(); 4096]; 64];
    let mut attack_masks = [BitBoard::default(); 64];
    let mut attack_shifts = [0; 64];

    for tile in Tile::iter() {
        // for i in 0..64 {
        let rank_mask = BitBoard::from_rank(tile.rank());
        let file_mask = BitBoard::from_file(tile.file());

        // Exclude edges unless the piece is located on an edge
        let edges = BitBoard::EDGES & !rank_mask & !file_mask;

        attack_masks[tile] = (rank_mask ^ file_mask) & !edges;

        attack_shifts[tile] = 64 - attack_masks[tile].population();

        let mut subset = tile.index() as u64;
        while subset != 0 {
            let mut index = subset;
            index = index * ROOK_MAGICS[tile];
            index = index >> attack_shifts[tile];

            attacks[tile][index as usize] = {
                //
                let occupancy = BitBoard::new(subset);
                let mut atk = BitBoard::from_tile(tile);

                let mut t = tile;
                while let Some(next) = t.north() {
                    t = next;
                    atk &= BitBoard::from_tile(t);

                    if occupancy.get(t) {
                        break;
                    }
                }

                let mut t = tile;
                while let Some(next) = t.south() {
                    t = next;
                    atk &= BitBoard::from_tile(t);

                    if occupancy.get(t) {
                        break;
                    }
                }

                let mut t = tile;
                while let Some(next) = t.east() {
                    t = next;
                    atk &= BitBoard::from_tile(t);

                    if occupancy.get(t) {
                        break;
                    }
                }

                let mut t = tile;
                while let Some(next) = t.west() {
                    t = next;
                    atk &= BitBoard::from_tile(t);

                    if occupancy.get(t) {
                        break;
                    }
                }

                atk
            };
            subset = (subset - attack_masks[tile].0) & attack_masks[tile].0;
        }
    }

    attacks
}

fn bishop_attacks() -> [[BitBoard; 512]; 64] {
    todo!()
}

 */
