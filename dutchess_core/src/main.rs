use std::str::FromStr;

use chess::Board;

fn main() {
    let ok = "r1bqkbnr/ppp1p1pp/3p4/5p2/1n1PPPPP/8/PPP5/RNBQKBNR b KQkq - 0 1";
    println!("{:?}", Board::from_str(ok));

    let fen = "r1bq1bnr/ppp1pkpp/3p4/5p2/1nPPPPPP/8/PP6/RNBQKBNR b KQkq - 0 1";
    println!("{:?}", Board::from_str(fen));
}
