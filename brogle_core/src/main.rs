use brogle_core::*;

fn main() {
    // let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N4Q/PPPBBPPP/R3K2R b KQkq - 0 1"; // e8g8 is legal
    // let fen = "r3k2r/p1pNqpb1/bn2pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1"; // e8c8 is legal
    let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/5Q1p/PPPBBPPP/RN2K2R w KQkq - 0 1"; // e1c1 is NOT legal
    let game = Game::from_fen(fen).unwrap();

    for mv in game.legal_moves() {
        if mv.from() == Tile::E1 {
            println!("{mv}");
        }
    }

    /*
    let mut game = Game::default();
    game.make_move(Move::from_uci(&game, "b1a3").unwrap());
    println!("pos: {}\nkey: {}", game.position(), game.zobrist_key());
    game.make_move(Move::from_uci(&game, "b8a6").unwrap());
    println!("pos: {}\nkey: {}", game.position(), game.zobrist_key());
    game.make_move(Move::from_uci(&game, "a3b1").unwrap());
    println!("pos: {}\nkey: {}", game.position(), game.zobrist_key());
    game.make_move(Move::from_uci(&game, "a6b8").unwrap());
    println!("pos: {}\nkey: {}", game.position(), game.zobrist_key());
     */
}
