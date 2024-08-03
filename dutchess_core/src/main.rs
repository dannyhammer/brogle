use dutchess_core::*;
// TODO: Get rid of this file once library is published

fn main() {
    // let fen = FEN_STARTPOS;
    let fen = "k6K/8/2P5/8/6n1/2nn4/P2P2P1/8 w - - 0 1";
    let position = Position::from_fen(fen).unwrap();
    println!("{position:?}");
    // let movegen = MoveGenerator::new_legal(position);
    // for mv in movegen.legal_moves() {
    //     println!("{mv}");
    // }

    /*
    generate_ray_table_blobs("dutchess_core/src/blobs").unwrap();

    for from in Tile::iter() {
        for to in Tile::iter() {
            let ray = ray_between_inclusive(from, to);
            if ray.is_nonempty() {
                println!("{from} -> {to} (inclusive)\n{ray:?}");
            }

            let ray = ray_between_exclusive(from, to);
            if ray.is_nonempty() {
                println!("{from} -> {to} (exclusive)\n{ray:?}");
            }

            let ray = ray_containing(from, to);
            if ray.is_nonempty() {
                println!("{from} -> {to} (containing)\n{ray:?}");
            }
        }
    }
     */
}
