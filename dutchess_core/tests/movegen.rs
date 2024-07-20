use std::collections::HashSet;

use dutchess_core::core::{Move, Position, DEFAULT_FEN};

// Sets up a game from the provided FEN
fn setup_game(fen: &str) -> Position {
    Position::new().from_fen(fen).unwrap()
}

/// Checks if `moves` and `legal_moves` contain all the same elements, ignoring order
fn lists_match<'a>(
    pos: &Position,
    moves: impl IntoIterator<Item = &'a Move>,
    legal_moves: impl IntoIterator<Item = &'a str>,
) {
    let board = pos.board();
    let fen = pos.to_fen();

    let mut moves = moves
        .into_iter()
        .map(|m| format!("{m}"))
        .collect::<Vec<String>>();
    let mut legal_moves = legal_moves
        .into_iter()
        .map(|m| m.to_string())
        .collect::<Vec<String>>();

    moves.sort();
    legal_moves.sort();

    let mut diff = Vec::with_capacity(moves.len());

    let missing_or_extra = if moves.len() > legal_moves.len() {
        for m in &moves {
            if !legal_moves.contains(&m) {
                diff.push(m);
            }
        }
        format!("Extra: {diff:?}")
    } else {
        for m in &legal_moves {
            if !moves.contains(&m) {
                diff.push(m);
            }
        }
        format!("Missing: {diff:?}")
    };

    assert_eq!(
        moves.len(),
        legal_moves.len(),
        "Moves: {moves:?}\nLegal: {legal_moves:?}\n{missing_or_extra}\nPosition: {fen}\n{board}"
    );

    for mv in moves {
        let mv = mv.to_string();
        assert!(
            legal_moves.contains(&mv),
            "\n\tIllegal move {mv}\nPosition: {fen}\n{board}\n\tLegal moves:\n\t{legal_moves:?}",
        );
    }
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_rnbqkbnr_pppppppp_8_8_8_8_PPPPPPPP_RNBQKBNR_w_KQkq_X_0_1() {
    let pos = setup_game("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "a2a3", "a2a4", "b2b3", "b2b4", "c2c3", "c2c4", "d2d3", "d2d4", "e2e3", "e2e4", "f2f3", "f2f4", "g2g3", "g2g4", "h2h3", "h2h4", "b1a3", "b1c3", "g1f3", "g1h3" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_r3k2r_p1ppqpb1_bn2pnp1_3PN3_1p2P3_2N2Q1p_PPPBBPPP_R3K2R_w_KQkq_X_0_1() {
    let pos = setup_game("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "a2a3", "a2a4", "b2b3", "g2g3", "g2h3", "g2g4", "d5d6", "d5e6", "c3b1", "c3d1", "c3a4", "c3b5", "e5d3", "e5c4", "e5g4", "e5c6", "e5g6", "e5d7", "e5f7", "d2c1", "d2e3", "d2f4", "d2g5", "d2h6", "e2d1", "e2f1", "e2d3", "e2c4", "e2b5", "e2a6", "a1b1", "a1c1", "a1d1", "h1f1", "h1g1", "f3d3", "f3e3", "f3g3", "f3h3", "f3f4", "f3g4", "f3f5", "f3h5", "f3f6", "e1c1", "e1d1", "e1f1", "e1g1" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_1r4b1_8_4R3_pP6_8_1K1N3q_8_3k4_w_X_a6_0_1() {
    let pos = setup_game("1r4b1/8/4R3/pP6/8/1K1N3q/8/3k4 w - a6 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "b5b6", "b3a2", "b3b2", "b3a3", "b3c3", "b3a4", "b3c4" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_k2r3b_8_5N2_3N4_1N1K2Nq_8_1N6_b7_w_X_X_0_1() {
    let pos = setup_game("k2r3b/8/5N2/3N4/1N1K2Nq/8/1N6/b7 w - - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "b4a2", "b4c2", "b4d3", "b4a6", "b4c6", "d4c3", "d4d3", "d4e3", "d4c4", "d4e4", "d4c5", "d4e5" ];

    lists_match(&pos, &moves, legal_moves);
}

#[cfg(test)]
mod pawn_move_tests {
    use super::*;

    #[test]
    #[allow(non_snake_case)]
    fn test_moves_from_k2r4_5n1b_6P1_2pP4_8_P2KP2q_2P5_8_w_X_c6_0_1() {
        let pos = setup_game("k2r4/5n1b/6P1/2pP4/8/P2KP2q/2P5/8 w - c6 0 1");
        let moves = pos.legal_moves();
        let legal_moves = [
            "c2c3", "c2c4", "a3a4", "d5d6", "g6h7", "d3d2", "d3e2", "d3c3", "d3c4", "d3e4",
        ];

        lists_match(&pos, &moves, legal_moves);
    }

    #[test]
    #[allow(non_snake_case)]
    fn test_moves_from_7k_r7_8_2pP4_8_8_5K2_8_w_X_c6_0_1() {
        let pos = setup_game("7k/r7/8/2pP4/8/8/5K2/8 w - c6 0 1");
        let moves = pos.legal_moves();
        let legal_moves = [
            "d5d6", "d5c6", "f2e1", "f2f1", "f2g1", "f2e2", "f2g2", "f2e3", "f2f3", "f2g3",
        ];

        lists_match(&pos, &moves, legal_moves);
    }

    #[test]
    #[allow(non_snake_case)]
    fn test_moves_from_7k_b7_8_2pP4_8_8_5K2_8_w_X_c6_0_1() {
        let pos = setup_game("7k/b7/8/2pP4/8/8/5K2/8 w - c6 0 1");
        let moves = pos.legal_moves();
        let legal_moves = [
            "d5d6", "f2e1", "f2f1", "f2g1", "f2e2", "f2g2", "f2e3", "f2f3", "f2g3",
        ];

        lists_match(&pos, &moves, legal_moves);
    }

    #[test]
    #[allow(non_snake_case)]
    fn test_moves_from_7k_8_8_b1pP1K2_8_8_8_8_w_X_c6_0_1() {
        let pos = setup_game("7k/8/8/b1pP1K2/8/8/8/8 w - c6 0 1");
        let moves = pos.legal_moves();
        let legal_moves = [
            "d5d6", "d5c6", "f5e4", "f5f4", "f5g4", "f5e5", "f5g5", "f5e6", "f5f6", "f5g6",
        ];

        lists_match(&pos, &moves, legal_moves);
    }

    #[test]
    #[allow(non_snake_case)]
    fn test_moves_from_7k_8_8_r1pP1K2_8_8_8_8_w_X_c6_0_1() {
        let pos = setup_game("7k/8/8/r1pP1K2/8/8/8/8 w - c6 0 1");
        let moves = pos.legal_moves();
        let legal_moves = [
            "d5d6", "f5e4", "f5f4", "f5g4", "f5e5", "f5g5", "f5e6", "f5f6", "f5g6",
        ];

        lists_match(&pos, &moves, legal_moves);
    }

    #[test]
    #[allow(non_snake_case)]
    fn test_moves_from_3b3k_8_8_2pP4_8_8_3K4_8_w_X_c6_0_1() {
        let pos = setup_game("3b3k/8/8/2pP4/8/8/3K4/8 w - c6 0 1");
        let moves = pos.legal_moves();
        let legal_moves = [
            "d5d6", "d5c6", "d2c1", "d2d1", "d2e1", "d2c2", "d2e2", "d2c3", "d2d3", "d2e3",
        ];

        lists_match(&pos, &moves, legal_moves);
    }

    #[test]
    #[allow(non_snake_case)]
    fn test_moves_from_3r3k_8_8_2pP4_8_8_3K4_8_w_X_c6_0_1() {
        let pos = setup_game("3r3k/8/8/2pP4/8/8/3K4/8 w - c6 0 1");
        let moves = pos.legal_moves();
        let legal_moves = [
            "d5d6", "d2c1", "d2d1", "d2e1", "d2c2", "d2e2", "d2c3", "d2d3", "d2e3",
        ];

        lists_match(&pos, &moves, legal_moves);
    }
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_k1r5_2R2b2_4R3_8_1RK2Rq1_8_8_2R5_w_X_X_0_1() {
    let pos = setup_game("k1r5/2R2b2/4R3/8/1RK2Rq1/8/8/2R5 w - - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "c1a1", "c1b1", "c1d1", "c1e1", "c1f1", "c1g1", "c1h1", "c1c2", "c1c3", "b4b1", "b4b2", "b4b3", "b4a4", "b4b5", "b4b6", "b4b7", "b4b8", "f4d4", "f4e4", "f4g4", "c7c5", "c7c6", "c7c8", "c4b3", "c4c3", "c4d3", "c4d4", "c4b5", "c4c5", "c4d5" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_k1r5_2B2b2_4B3_8_1BK2Bq1_8_8_2B5_w_X_X_0_1() {
    let pos = setup_game("k1r5/2B2b2/4B3/8/1BK2Bq1/8/8/2B5 w - - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "c1b2", "c1d2", "c1a3", "c1e3", "b4e1", "b4d2", "b4a3", "b4c3", "b4a5", "b4c5", "b4d6", "b4e7", "b4f8", "e6d5", "e6f7", "c4b3", "c4c3", "c4d3", "c4d4", "c4b5", "c4c5", "c4d5" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_k1r5_2Q2b2_4Q3_8_1QK2Qq1_8_8_2Q5_w_X_X_0_1() {
    let pos = setup_game("k1r5/2Q2b2/4Q3/8/1QK2Qq1/8/8/2Q5 w - - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "c1a1", "c1b1", "c1d1", "c1e1", "c1f1", "c1g1", "c1h1", "c1b2", "c1c2", "c1d2", "c1a3", "c1c3", "c1e3", "b4b1", "b4e1", "b4b2", "b4d2", "b4a3", "b4b3", "b4c3", "b4a4", "b4a5", "b4b5", "b4c5", "b4b6", "b4d6", "b4b7", "b4e7", "b4b8", "b4f8", "f4d4", "f4e4", "f4g4", "e6d5", "e6f7", "c7c5", "c7c6", "c7c8", "c4b3", "c4c3", "c4d3", "c4d4", "c4b5", "c4c5", "c4d5" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_1k6_8_8_8_8_8_8_R3K2R_w_KQ_X_0_1() {
    let pos = setup_game("1k6/8/8/8/8/8/8/R3K2R w KQ - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "a1b1", "a1c1", "a1d1", "a1a2", "a1a3", "a1a4", "a1a5", "a1a6", "a1a7", "a1a8", "h1f1", "h1g1", "h1h2", "h1h3", "h1h4", "h1h5", "h1h6", "h1h7", "h1h8", "e1c1", "e1d1", "e1f1", "e1g1", "e1d2", "e1e2", "e1f2" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_1k6_8_8_8_8_8_8_R3K2R_w_Kh_X_0_1() {
    let pos = setup_game("1k6/8/8/8/8/8/8/R3K2R w Kh - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "a1b1", "a1c1", "a1d1", "a1a2", "a1a3", "a1a4", "a1a5", "a1a6", "a1a7", "a1a8", "h1f1", "h1g1", "h1h2", "h1h3", "h1h4", "h1h5", "h1h6", "h1h7", "h1h8", "e1d1", "e1f1", "e1g1", "e1d2", "e1e2", "e1f2" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_1k6_8_8_8_8_8_8_R3K2R_w_Qh_X_0_1() {
    let pos = setup_game("1k6/8/8/8/8/8/8/R3K2R w Qh - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "a1b1", "a1c1", "a1d1", "a1a2", "a1a3", "a1a4", "a1a5", "a1a6", "a1a7", "a1a8", "h1f1", "h1g1", "h1h2", "h1h3", "h1h4", "h1h5", "h1h6", "h1h7", "h1h8", "e1c1", "e1d1", "e1f1", "e1d2", "e1e2", "e1f2" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_1k1r4_8_8_8_8_8_8_R3K2R_w_KQ_X_0_1() {
    let pos = setup_game("1k1r4/8/8/8/8/8/8/R3K2R w KQ - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "a1b1", "a1c1", "a1d1", "a1a2", "a1a3", "a1a4", "a1a5", "a1a6", "a1a7", "a1a8", "h1f1", "h1g1", "h1h2", "h1h3", "h1h4", "h1h5", "h1h6", "h1h7", "h1h8", "e1f1", "e1g1", "e1e2", "e1f2" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_1k3r2_8_8_8_8_8_8_R3K2R_w_KQ_X_0_1() {
    let pos = setup_game("1k3r2/8/8/8/8/8/8/R3K2R w KQ - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "a1b1", "a1c1", "a1d1", "a1a2", "a1a3", "a1a4", "a1a5", "a1a6", "a1a7", "a1a8", "h1f1", "h1g1", "h1h2", "h1h3", "h1h4", "h1h5", "h1h6", "h1h7", "h1h8", "e1c1", "e1d1", "e1d2", "e1e2" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_1k1r1r2_8_8_8_8_8_8_R3K2R_w_KQ_X_0_1() {
    let pos = setup_game("1k1r1r2/8/8/8/8/8/8/R3K2R w KQ - 0 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "a1b1", "a1c1", "a1d1", "a1a2", "a1a3", "a1a4", "a1a5", "a1a6", "a1a7", "a1a8", "h1f1", "h1g1", "h1h2", "h1h3", "h1h4", "h1h5", "h1h6", "h1h7", "h1h8", "e1e2" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_rnbqkbnr_1ppppppp_8_p7_8_N7_PPPPPPPP_R1BQKBNR_w_KQkq_X_2_1() {
    let pos = setup_game("rnbqkbnr/1ppppppp/8/p7/8/N7/PPPPPPPP/R1BQKBNR w KQkq - 2 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "b2b3", "b2b4", "c2c3", "c2c4", "d2d3", "d2d4", "e2e3", "e2e4", "f2f3", "f2f4", "g2g3", "g2g4", "h2h3", "h2h4", "g1f3", "g1h3", "a3b1", "a3c4", "a3b5", "a1b1" ];

    lists_match(&pos, &moves, legal_moves);
}

#[test]
#[allow(non_snake_case)]
#[rustfmt::skip]
fn test_moves_from_rnbqkbnr_1ppppppp_8_p7_8_7N_PPPPPPPP_RNBQKB1R_w_KQkq_X_2_1() {
    let pos = setup_game("rnbqkbnr/1ppppppp/8/p7/8/7N/PPPPPPPP/RNBQKB1R w KQkq - 2 1");
    let moves = pos.legal_moves();
    let legal_moves = [ "a2a3", "a2a4", "b2b3", "b2b4", "c2c3", "c2c4", "d2d3", "d2d4", "e2e3", "e2e4", "f2f3", "f2f4", "g2g3", "g2g4", "b1a3", "b1c3", "h3g1", "h3f4", "h3g5", "h1g1" ];

    lists_match(&pos, &moves, legal_moves);
}
