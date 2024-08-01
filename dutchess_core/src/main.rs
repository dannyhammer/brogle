use dutchess_core::*;
// TODO: Get rid of this file once library is published

fn main() {
    let fen = FEN_STARTPOS;
    let game = Game::from_fen(fen).unwrap();
    println!("{game}\n\n");
}
