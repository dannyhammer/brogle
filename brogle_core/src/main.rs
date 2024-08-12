use brogle_core::*;

/// All contents of this file should be ignored. I just use this `main` to test small things in `brogle_core`.
fn main() {
    /*
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
     */

    let mut game = Game::default();
    game.make_move(Move::from_uci(&game, "b1a3").unwrap());
    println!("repetition? {}", game.is_repetition());
    game.make_move(Move::from_uci(&game, "b8a6").unwrap());
    println!("repetition? {}", game.is_repetition());
    game.make_move(Move::from_uci(&game, "a3b1").unwrap());
    println!("repetition? {}", game.is_repetition());
    game.make_move(Move::from_uci(&game, "a6b8").unwrap());
    println!("repetition? {}", game.is_repetition());
}
