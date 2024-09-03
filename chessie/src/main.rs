use chessie::*;

/// All contents of this file should be ignored. I just use this `main` to test small things in `chessie`.
fn main() {
    let mut game = Game::default();
    println!("pos: {}\nkey: {}", game.position(), game.key());
    game.make_move(Move::from_uci(&game, "b1a3").unwrap());
    println!("pos: {}\nkey: {}", game.position(), game.key());
    game.make_move(Move::from_uci(&game, "b8a6").unwrap());
    println!("pos: {}\nkey: {}", game.position(), game.key());
    game.make_move(Move::from_uci(&game, "a3b1").unwrap());
    println!("pos: {}\nkey: {}", game.position(), game.key());
    game.make_move(Move::from_uci(&game, "a6b8").unwrap());
    println!("pos: {}\nkey: {}", game.position(), game.key());

    // let fen = FEN_STARTPOS;
    // let moves = ["b1a3", "b8a6", "a3b1", "a6b8"];
    // let fen = "k7/8/8/8/3p4/8/4P3/K7 w - - 0 1"; // Testing hash keys with en passant
    // let moves = ["e2e4", "d4e3"];
    // let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
    // let moves = ["e1g1"];

    // let fen = FEN_KIWIPETE;
    // let pos = Position::from_fen(fen).unwrap();

    // let movegen = MoveGen::new(pos);

    // for mv in movegen {
    //     println!("{mv}");
    // }

    // let mut game = Game::default();
    // game.make_move(Move::from_uci(&game, "b1a3").unwrap());
    // println!("repetition? {}", game.is_repetition());
    // game.make_move(Move::from_uci(&game, "b8a6").unwrap());
    // println!("repetition? {}", game.is_repetition());
    // game.make_move(Move::from_uci(&game, "a3b1").unwrap());
    // println!("repetition? {}", game.is_repetition());
    // game.make_move(Move::from_uci(&game, "a6b8").unwrap());
    // println!("repetition? {}", game.is_repetition());
}
