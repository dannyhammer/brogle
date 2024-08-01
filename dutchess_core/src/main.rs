use dutchess_core::*;
// TODO: Get rid of this file once library is published

fn main() {
    let fen = FEN_STARTPOS;
    let position = Position::from_fen(fen).unwrap();
    println!("{position}");
}
