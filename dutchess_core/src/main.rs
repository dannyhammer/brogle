// use chess::{BitBoard, Board, Color, Piece};

use dutchess_core::core::*;

fn main() {
    let fen = DEFAULT_FEN;
    // let fen = "1r4b1/8/4R3/pP6/8/1K1N3q/8/3k4 w - a6 0 1"; /* Pinmask */
    // let fen = "3r3b/8/5N2/3N4/1N1K2Nq/8/1N6/b7 w - - 0 1"; /* Pinmask; all knights */
    // let fen = "k2r4/5n1b/6P1/2pP4/8/P2KP2q/2P5/8 w - c6 0 1"; /* Pinmask; all pawn */
    //
    // let fen = "7k/r7/8/2pP4/8/8/5K2/8 w - c6 0 1"; /* Pinmask; en passant on diagonal is safe */
    // let fen = "7k/b7/8/2pP4/8/8/5K2/8 w - c6 0 1"; /* Pinmask; en passant on diagonal is NOT safe */
    // let fen = "7k/8/8/b1pP1K2/8/8/8/8 w - c6 0 1"; /* Pinmask; en passant on rank is safe */
    // let fen = "7k/8/8/r1pP1K2/8/8/8/8 w - c6 0 1"; /* Pinmask (pawn shielded by enemy); en passant on rank is NOT safe */
    // let fen = "7k/8/8/rPp2K2/8/8/8/8 w - c6 0 1"; /* Pinmask (pawn in danger); en passant on rank is NOT safe */
    // let fen = "7k/8/8/8/3P4/2K5/1PPP4/8 w - - 0 1"; /* Pawn double pushes blocked by friendlies */
    //
    // let fen = "3b3k/8/8/2pP4/8/8/3K4/8 w - c6 0 1"; /* Pinmask; en passant on file is safe */
    // let fen = "3r3k/8/8/2pP4/8/8/3K4/8 w - c6 0 1"; /* Pinmask; en passant on file is NOT safe */
    //
    // let fen = "2r5/2R2b2/4R3/8/1RK2Rq1/8/8/2R5 w - - 0 1"; /* Pinmask; all rooks */
    // let fen = "2r5/2B2b2/4B3/8/1BK2Bq1/8/8/2B5 w - - 0 1"; /* Pinmask; all bishops */
    // let fen = "2r5/2Q2b2/4Q3/8/1QK2Qq1/8/8/2Q5 w - - 0 1"; /* Pinmask; all queens */
    //
    // let fen = "1k6/8/8/8/8/8/8/R3K2R w KQha - 0 1"; /* King can kingside and queenside castle */
    // let fen = "16k/8/8/8/8/8/8/R3K2R w Kha - 0 1"; /* King can kingside castle */
    // let fen = "1k6/8/8/8/8/8/8/R3K2R w Qha - 0 1"; /* King can queenside castle */
    // let fen = "1k1r4/8/8/8/8/8/8/R3K2R w KQka - 0 1"; /* King can kingside, not queenside */
    // let fen = "1k3r2/8/8/8/8/8/8/R3K2R w KQka - 0 1"; /* King can queenside, not kingside */
    // let fen = "1k1r1r2/8/8/8/8/8/8/R3K2R w KQka - 0 1"; /* King cannot castle */
    //

    let mut game = Game::from_fen(fen).unwrap();
    // println!("{game}\n\n");
    // println!("{:?}", game.state().board().color(Color::White));

    // let moves = game.state().compute_legal_for(Color::White);
    // for (i, chessmove) in moves.into_iter().enumerate() {
    //     if !chessmove.is_empty() {
    //         let tile = Tile::from_index_unchecked(i);
    //         let piece = game.state().board().piece_at(tile).unwrap();
    //         println!("\n +------{piece}-{tile}------\n{chessmove:?}");
    //     }
    // }

    // std::process::exit(1);

    let moves = game.state().legal_moves();
    // println!("{moves:?}");
    println!("PERFT: {}", moves.len());

    // let moves_to_make = ["b1a3", "a7a5", "a2a4"];
    // let moves_to_make = ["g1h3", "a7a5", "e1g1"];
    let moves_to_make = ["a2a4", "b7b5", "a4a5"];

    for mv in moves_to_make {
        let mv = Move::from_uci(mv).unwrap();
        println!("Making move: {mv}");
        game.make_move(mv);
        println!("{game}");

        let moves = game.state().legal_moves();
        // println!("{moves}");
        println!("PERFT: {}", moves.len());
    }

    // let board = ChessBoard::new().with_default_setup();
    // println!("{board}");
    // println!("{}", state.to_fen());
    // let tile = Tile::C4;
    // let piece = Piece::BLACK_ROOK;
    // let moves = moves_for(&piece, tile, state.board());
    // println!("{moves}");
    // let board = Board::default();

    // println!("Eval: {}", eval(board));

    /*
    for from in Tile::iter() {
        for to in Tile::iter() {
            let ray = ray_between(from, to);
            if !ray.is_empty() {
                println!("{from} -> {to}\n{ray}\n---------------");
            }
        }
    }
     */
}
