use dutchess_core::*;

/*
fn perft(position: &mut Position, depth: usize) -> usize {
    if depth == 0 {
        return 1;
    }

    // println!("PERFT({depth}): {position}\n{position:?}");

    let mut nodes = 0;
    // let tab = " ".repeat(depth);

    let (moves, num_moves) = position.legal_moves();
    for chessmove in moves.iter() {
        let mut cloned = position.clone();
        // println!("{tab}Making  : {chessmove}");
        cloned.make_move(*chessmove);
        // println!("{tab}{cloned}");
        nodes += perft(&mut cloned, depth - 1);
        // println!("{tab}Unmaking: {chessmove}");
        // cloned.unmake_move(chessmove);
        // println!("{tab}{cloned}");
    }

    nodes
}
 */

fn main() {
    let fen = FEN_STARTPOS;

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
    // let fen = "1k3r2/8/8/8/8/8/8/R3K2R w KQka - 0 1"; /* King can queenside, not kingside */
    // let fen = "1k6/8/8/8/8/8/8/RN2K1NR w KQka - 0 1"; /* King blocked by Knight; cannot castle */
    // let fen = "1k6/8/8/8/8/8/8/Rr2K1NR w KQka - 0 1"; /* King checked by rook; cannot castle */
    // let fen = "1k1r1r2/8/8/8/8/8/8/R3K2R w KQka - 0 1"; /* King under attack; cannot castle */
    //
    // let fen = "k7/2K5/8/8/8/8/8/R7 w - - 0 1"; /* White should NOT be able to *actually capture* Black's King */
    // let fen = "rnbqkbnr/1ppppppp/p7/1N6/8/8/PPPPPPPP/R1BQKBNR b KQkq - 3 2";
    // let fen = "rnbq1bnr/pppkpppp/3N4/3p4/8/8/PPPPPPPP/R1BQKBNR b KQ - 5 3";
    // let fen = "rnb1kbnr/pp1ppppp/8/q1p5/8/3P4/PPPKPPPP/RNBQ1BNR w kq - 4 3"; /* White king in check */
    // let fen = "rnbqkb1r/pppppppp/8/8/4n3/3P4/PPPKPPPP/RNBQ1BNR w kq - 4 3"; /* Should be able to d3e4 */
    // let fen = "rnB1kbnr/ppp1pppp/3q4/3p4/8/6P1/PPPPPP1P/RNBQK1NR b KQkq - 5 3"; /* b queenside castling isn't legal! */
    // let fen = "rnbq1bnr/pppppkpp/5p2/8/2B1P3/5Q2/PPPP1PPP/RNB1K1NR b KQ - 3 3"; /* f6f5 isn't legal */
    // let fen = "rnbqkbnr/1p2pppp/8/p1Pp4/8/8/PPPKPPPP/RNBQ1BNR w kq d6 1 4"; /* Should be able to en passant c5d6 */
    // let fen = "rnb1k1nr/pppp1ppp/8/4p3/1b1P3q/2Q5/PPP1PPPP/RNB1KBNR w KQkq - 4 4"; /* Queen is pinned, cannot move from c3 */
    // let fen = "k7/8/8/7b/b7/5Q2/2Q5/3K4 w - - 0 1";
    // let fen = "k6b/r7/8/Q7/3Q4/8/8/K4Q1r w - - 0 1";
    // let fen = "7k/8/8/r1pP1K2/8/8/8/8 w - c6 0 1";
    // let fen = "8/1k6/8/2Pp3r/2K5/8/8/8 w - d6 4 4"; // Missing c5d6
    // let fen = "8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1"; // Produces illegal d5e6 at depth 5
    // let fen = "8/8/1n6/3K4/8/8/q7/5k2 w - - 2 3"; // d5e6 illegal
    // let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
    // let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"; // Kiwipete; fails at depth 5

    // let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/P1N2Q2/1PPBBP1P/1R2K2b w Kkq - 1 3"; // e1g1 is illegal!
    // let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/P1N2Q2/1PPBBP1P/1R2K2B w Kkq - 1 3"; // e1g1 is illegal
    // let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/P1N2Q2/1PPBBPpP/1R2K2R b Kkq - 0 2";

    // let fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1";
    // let fen = "r3k3/8/8/8/8/8/3K4/R7 b - - 3 3";
    // let fen = "8/PPPk4/8/8/8/8/4Kppp/8 w - - 0 1 ";
    // let fen = "Q2k4/2B5/8/8/8/8/4Kppp/8 b - - 4 3";
    // let fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
    // let fen = "8/2p5/3p4/KP6/5pk1/7P/4P3/6R1 b - - 4 3";

    let mut game = Game::from_fen(fen).unwrap();
    println!("{game}\n\n");

    // let moves = game
    //     .position()
    //     .compute_legal_for(game.position().current_player());
    // for (i, chessmove) in moves.into_iter().enumerate() {
    //     if !chessmove.is_empty() {
    //         let tile = Tile::from_index_unchecked(i);
    //         if tile != Tile::G2 {
    //             continue;
    //         }
    //         let piece = game.position().bitboards().piece_at(tile).unwrap();
    //         println!("\n +------{piece}-{tile}------\n{chessmove:?}");
    //     }
    // }

    // let moves = game.position().legal_moves();
    // println!("{moves:?}");
    // println!("PERFT: {}", moves.len());

    // let moves_to_make = ["b1a3", "a7a5", "a2a4"];
    // let moves_to_make = ["g1h3", "a7a5", "e1g1"];
    // let moves_to_make = ["a2a4", "b7b5", "a4a5"];
    // let moves_to_make = ["c2c3", "a7a5", "d1a4", "d7d5", "a4e8"]; // Captures black king
    // let moves_to_make = ["c2c3", "a7a5", "d1a4", "d7d5"];
    // let moves_to_make = ["c2c3", "a7a5", "d1a4"];
    // let moves_to_make = ["b1a3", "a7a6", "a3b5", "a6b5", "b5b6"];
    // let moves_to_make = ["b1a3", "a7a6", "a3b5", "a6b5"];
    // let moves_to_make = ["g2g4", "f7f6", "g4g5", "f6g5", "g5g6"];
    // let moves_to_make = ["b1a3", "a7a5", "a1b1", "c7c6", "a3b5", "c6b5"];
    // let moves_to_make = ["b1a3", "a7a6", "a3b5", "a6b5"];
    // let moves_to_make = ["d2d3", "c7c5", "e1d2", "d8a5"];
    // let moves_to_make = ["d2d3", "c7c5", "e1d2", "d8a5", "d2e1"];
    // let moves_to_make = ["d2d3", "g8f6", "e1d2", "f6e4"]; // Missing d3e4
    // let moves_to_make = ["g2g3", "d7d5", "f1h3", "d8d6", "h3c8", "e8c8"]; // e8c8 isn't legal
    // let moves_to_make = ["g2g3", "d7d5", "f1h3", "d8d6", "h3c8"];
    // let moves_to_make = ["e2e4", "f7f6", "d1f3", "e8f7", "f1c4", "f6f5"]; // f6f5 isn't legal
    // let moves_to_make = ["e2e4", "f7f6", "d1f3", "e8f7", "f1c4"];
    // let moves_to_make = ["d2d3", "a7a5", "c1e3", "a5a4", "d1d2", "a4a3", "e1c1"];
    // let moves_to_make = ["d2d3", "a7a5", "c1e3", "a5a4", "d1d2", "a4a3"]; // e1c1 isn't legal
    // let moves_to_make = ["d2d4", "a7a5", "e1d2", "c7c5", "d4c5", "d7d5", "c5d6"]; // c5d6 isn't legal
    // let moves_to_make = ["c2c3", "b7b5", "d1a4", "b5b4"];
    // let moves_to_make = ["c2c3", "b7b5", "d1a4"]; // Missing b5b4
    // let moves_to_make = ["d2d4", "a7a5", "e1d2", "c7c5", "d4c5", "d7d5"]; // Missing c5d6
    // let moves_to_make = ["d2d4", "e7e5", "d1d2", "f8b4", "d2c3", "d8h4"]; // c3g3 is not legal!
    // let moves_to_make = ["d8c7", "a5b4", "c7b7", "b4c4", "d7d5"]; // Illegal EP c5d6
    // let moves_to_make = ["b3a2", "e6d5", "c4b6"]; // d5e6 is illegal!
    // let moves_to_make = ["g2h1b"]; // kiwipete

    // let moves_to_make = ["a1a2", "h8h1", "e1d2", "h1a1", "a2a1"]; // Missing e8c8
    // let moves_to_make = ["a7a8q", "d7c7", "b7b8b", "c7d8", "b8c7"]; // Missing d8c7
    // let moves_to_make = ["b4b1", "h4g4", "b1g1", "h5h3", "g2h3"]; // Missing g4f5

    /*
    for mv in moves_to_make {
        let mv = Move::from_uci(&game.position(), mv).unwrap();
        println!("\nMaking move: {mv:?}");
        game.make_move(mv);
        println!("State after {mv}:\n{game}");

        let mut moves: Vec<_> = game
            .position()
            .legal_moves()
            .into_iter()
            .map(|m| m.to_string())
            .collect();
        moves.sort();
        print!("{} Legal Moves:\n\t", moves.len());
        for mv in &moves {
            print!("{mv}, ");
        }
        println!("\n");
    }
     */

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
