use chessie::*;

/// All contents of this file should be ignored. I just use this `main` to test small things in `chessie`.
fn main() {
    // let fen = "3r4/6b1/5P2/3P4/3KN2q/8/8/k7 w - - 0 1";
    // let fen = "3r4/8/8/8/3K4/8/8/k7 w - - 0 1";
    // let fen = "8/8/4n3/8/3K4/8/8/k7 w - - 0 1";
    // let fen = "3r4/8/8/3N4/3K4/8/8/k7 w - - 0 1";
    // let fen = "4k3/8/8/8/8/8/8/R3K2R w KQ - 0 1"; // Both castling sides are legal
    let fen = "2n1k3/1P6/8/5pP1/5n2/2P1P3/P7/4K3 w - f6 0 1";
    // let fen = FEN_STARTPOS;
    // let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
    // let fen = "r3k2r/p1ppqpb1/b3pnp1/3PN3/1p2P3/2N1nQ1p/PPPB1PPP/R2B1K1R w kq - 4 3"; // Double check
    // let fen = "r3k2r/p1ppqpb1/b3pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R2B1K1R w kq - 4 3"; // Single check
    // let fen = FEN_KIWIPETE;
    // let fen = "8/8/8/8/8/1kn5/8/K7 w - - 0 1"; // Stalemate
    // let fen = "8/8/8/8/8/1k6/1q6/K7 w - - 0 1"; // Checkmate
    // let fen = "k7/8/8/8/8/8/5p2/3K1R2 w - - 0 1"; // King cannot move d1e1
    let pos = Position::from_fen(fen).unwrap();

    // let mask = pos.pawns(Color::White);
    // let mask = Square::B7.bitboard();
    // let mask = Square::E1.bitboard();
    let movegen = MoveGen::new(&pos);

    eprintln!("{movegen:?}");

    eprintln!("Moves:");
    let mut total = 0;
    for mv in movegen {
        eprintln!("{mv:?}");
        total += 1;
    }
    eprintln!("Total: {total}");
}
