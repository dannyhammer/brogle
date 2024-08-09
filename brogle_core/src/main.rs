use brogle_core::*;
// TODO: Get rid of this file once library is published

fn main() {
    // let fen = FEN_STARTPOS;
    // let fen = "8/8/8/4P3/2k5/K7/P1P1P1P1/8 w - - 0 1";
    // let fen = "K3b2k/3P4/8/5Pp1/8/1n1n4/2P5/8 w - g6 0 1";
    // let fen = "3q3k/8/8/8/b7/1P6/3P2P1/3K1PrP w - - 0 1";
    let fen = "7k/8/8/8/8/8/8/K7 w - - 0 1";
    let mut game = Game::from_fen(fen).unwrap();
    println!("{:?}", game.position());
    // let moves = game.legal_moves();
    game.make_move(Move::from_uci(&game, "a1b1").unwrap());
    println!("{}", game.is_repetition());
    game.make_move(Move::from_uci(&game, "h8h7").unwrap());
    println!("{}", game.is_repetition());
    game.make_move(Move::from_uci(&game, "b1a1").unwrap());
    println!("{}", game.is_repetition());
    game.make_move(Move::from_uci(&game, "h7h8").unwrap());
    println!("{}", game.is_repetition());
    // for mv in moves {
    //     if game.piece_at(mv.from()).unwrap().is_pawn() {
    //         println!("{mv}");
    //     }
    // }

    /*
    generate_ray_table_blobs("brogle_core/src/blobs").unwrap();

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
