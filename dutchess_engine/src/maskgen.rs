fn generate_blobs() {
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
}

fn king_moves() -> [BitBoard; 64] {
    let mut boards = [BitBoard::default(); 64];

    for i in 0..64 {
        let bb = BitBoard::from_index(i).unwrap();

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
        let bb = BitBoard::from_index(i).unwrap();

        let mut n = bb.north();
        let mut s = bb.south();
        let mut e = bb.east();
        let mut w = bb.west();

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
        let bb = BitBoard::from_index(i).unwrap();

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
        let bb = BitBoard::from_index(i).unwrap();

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
