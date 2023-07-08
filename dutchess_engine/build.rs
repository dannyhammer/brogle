use std::path::Path;
use std::{env, fs};

fn main() {
    let moves = precompute_moves();
    // let attacks = precompute_attacks();

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let outfile = Path::new(&out_dir).join("precomputed_moves.rs");

    fs::write(outfile, moves).unwrap();
}

fn precompute_moves() -> String {
    let val: [[u64; 64]; 4] = [[0; 64]; 4];
    format!("const ORTHOGONAL_RAYS: [[u64; 64]; 4] = {:?}", val)
}

/*
fn bishop_moves() -> [u64; 64] {
    let mut boards = [0; 64];

    for i in 0..64 {
        let bb = 1 << i;

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

        // boards[i as usize] = ne | nw | se | sw;
        // boards[i] = nw;
        bb = ne | nw | se | sw;
    }

    boards
}
 */
