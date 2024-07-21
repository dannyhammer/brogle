use super::{BitBoard, Color, Tile};

pub fn generate_blobs() {
    let bishop: [u8; 512] = unsafe { std::mem::transmute(bishop_moves()) };
    let rook: [u8; 512] = unsafe { std::mem::transmute(rook_moves()) };
    let knight: [u8; 512] = unsafe { std::mem::transmute(knight_moves()) };
    let king: [u8; 512] = unsafe { std::mem::transmute(king_moves()) };
    let queen: [u8; 512] = unsafe { std::mem::transmute(queen_moves()) };
    let wpp: [u8; 512] = unsafe { std::mem::transmute(pawn_push(Color::White)) };
    let bpp: [u8; 512] = unsafe { std::mem::transmute(pawn_push(Color::Black)) };
    let wpa: [u8; 512] = unsafe { std::mem::transmute(pawn_attack(Color::White)) };
    let bpa: [u8; 512] = unsafe { std::mem::transmute(pawn_attack(Color::Black)) };

    std::fs::write("src/core/blobs/bishop_masks.blob", bishop).unwrap();
    std::fs::write("src/core/blobs/rook_masks.blob", rook).unwrap();
    std::fs::write("src/core/blobs/knight_masks.blob", knight).unwrap();
    std::fs::write("src/core/blobs/king_masks.blob", king).unwrap();
    std::fs::write("src/core/blobs/queen_masks.blob", queen).unwrap();
    std::fs::write("src/core/blobs/white_pawn_push.blob", wpp).unwrap();
    std::fs::write("src/core/blobs/black_pawn_push.blob", bpp).unwrap();
    std::fs::write("src/core/blobs/white_pawn_attack.blob", wpa).unwrap();
    std::fs::write("src/core/blobs/black_pawn_attack.blob", bpa).unwrap();
}

fn pawn_push(color: Color) -> [BitBoard; 64] {
    let mut boards = [BitBoard::default(); 64];
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

fn pawn_attack(color: Color) -> [BitBoard; 64] {
    let mut boards = [BitBoard::default(); 64];
    for tile in Tile::iter() {
        let bb = BitBoard::from_tile(tile);

        boards[tile] = bb.advance_by(color, 1).east() | bb.advance_by(color, 1).west();
    }
    boards
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
