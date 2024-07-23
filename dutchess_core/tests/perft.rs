use std::ops::{Add, AddAssign};

use dutchess_core::Position;

#[derive(Default, Debug, Clone, Copy)]
struct PerftResult {
    nodes: usize,
    captures: usize,
    // eps: usize,
    castles: usize,
    // promotions: usize,
    checks: usize,
    // discovery_checks: usize,
    // double_checks: usize,
    checkmates: usize,
}

impl Add for PerftResult {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            nodes: self.nodes + rhs.nodes,
            captures: self.captures + rhs.captures,
            checks: self.checks + rhs.checks,
            checkmates: self.checkmates + rhs.checkmates,
            castles: self.castles + rhs.castles,
            // nodes: self.nodes + rhs.nodes,
            // nodes: self.nodes + rhs.nodes,
            // nodes: self.nodes + rhs.nodes,
            // nodes: self.nodes + rhs.nodes,
        }
    }
}

impl AddAssign for PerftResult {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

// https://www.chessprogramming.org/Perft_Results#Initial_Position
/*
Depth   Nodes	                        Captures	    E.p.	    Castles	        Promotions	    Checks          Discovery Checks    Double Checks   Checkmates
0       1	                            0	            0	        0	            0	            0	            0	                0	            0
1	    20	                            0	            0	        0	            0	            0           	0	                0	            0
2	    400	                            0	            0	        0	            0	            0	            0	                0	            0
3	    8_902	                        34	            0	        0	            0	            12	            0	                0	            0
4	    197_281	                        1576	        0	        0	            0	            469	            0	                0	            8
5	    4_865_609	                    82_719	        258	        0	            0	            27_351	        6	                0	            347
6	    119_060_324	                    2_812_008	    5248	    0	            0	            809_099	        329	                46	            10_828
7	    3_195_901_860	                108_329_926	    319_617	    883_453	        0	            33_103_848	    18_026	            1628	        435_767
8	    84_998_978_956	                3_523_740_106	7_187_977	23_605_205	    0	            968_981_593	    847_039	            147_215	        9_852_036
9	    2_439_530_234_167	            125_208_536_153	319_496_827	1_784_356_000	17_334_376	    36_095_901_903	37_101_713	        5_547_231	    400_191_963
10	    69_352_859_712_417
11	    2_097_651_003_696_806
12	    62_854_969_236_701_747
13	    1_981_066_775_000_396_239
14	    61_885_021_521_585_529_237
15	    2_015_099_950_053_364_471_960
*/

/*
// Kiwipete: https://www.chessprogramming.org/Perft_Results#Position_2
Depth	Nodes	Captures	E.p.	Castles	Promotions	Checks	Discovery Checks	Double Checks	Checkmates
1	48	8	0	2	0	0	0	0	0
2	2039	351	1	91	0	3	0	0	0
3	97862	17102	45	3162	0	993	0	0	1
4	4085603	757163	1929	128013	15172	25523	42	6	43
5	193690690	35043416	73365	4993637	8392	3309887	19883	2637	30171
6	8031647685	1558445089	3577504	184513607	56627920	92238050	568417	54948	360003
 */

// Position 3: 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -
/*
Depth	Nodes	Captures	E.p.	Castles	Promotions	Checks	Discovery Checks	Double Checks	Checkmates
1	14	1	0	0	0	2	0	0	0
2	191	14	0	0	0	10	0	0	0
3	2812	209	2	0	0	267	3	0	0
4	43238	3348	123	0	0	1680	106	0	17
5	[9] 674624	52051	1165	0	0	52950	1292	3	0
6	11030083	940350	33325	0	7552	452473	26067	0	2733
7	178633661	14519036	294874	0	140024	12797406	370630	3612	87
8	3009794393	267586558	8009239	0	6578076	135626805	7181487	1630	45041
*/

/*
const STARTING_PERFT_RESULTS: [PerftResult; 5] = [
    PerftResult {
        nodes: 1,
        captures: 0,
        castles: 0,
        checks: 0,
        checkmates: 0,
    },
    PerftResult {
        nodes: 20,
        captures: 0,
        castles: 0,
        checks: 0,
        checkmates: 0,
    },
    PerftResult {
        nodes: 400,
        captures: 0,
        castles: 0,
        checks: 0,
        checkmates: 0,
    },
    PerftResult {
        nodes: 8_902,
        captures: 34,
        castles: 0,
        checks: 12,
        checkmates: 0,
    },
    PerftResult {
        nodes: 197_281,
        captures: 1_576,
        castles: 0,
        checks: 469,
        checkmates: 8,
    },
];
 */

/*
const KIWIPETE_PERFT_RESULTS: [PerftResult; 2] = [
    PerftResult {
        nodes: 1,
        captures: 0,
        castles: 0,
        checks: 0,
        checkmates: 0,
    },
    PerftResult {
        nodes: 48,
        captures: 8,
        castles: 2,
        checks: 0,
        checkmates: 0,
    },
];
 */

/*
const POS3_PERFT_RESULTS: [PerftResult; 2] = [
    PerftResult {
        nodes: 1,
        captures: 0,
        castles: 0,
        checks: 0,
        checkmates: 0,
    },
    PerftResult {
        nodes: 14,
        captures: 1,
        castles: 0,
        checks: 2,
        checkmates: 0,
    },
];
*/

fn perft(position: Position, depth: usize) -> PerftResult {
    if depth == 0 {
        return PerftResult {
            nodes: 1,
            captures: 32 - position.bitboards().occupied().population() as usize, // TODO: Fetch original number of pieces
            checks: position.is_check() as usize,
            castles: position.times_castled(),
            checkmates: position.is_checkmate() as usize,
        };
    }

    let mut res = PerftResult::default();

    let moves = position.legal_moves();
    // println!("Valid moves: {moves:#?}");
    for chessmove in moves {
        let mut cloned = position.clone();
        cloned.make_move(chessmove);
        res += perft(cloned, depth - 1);
        // position.unmake_move(chessmove);
    }

    res
}

fn perft_nodes_only(position: Position, depth: usize) -> usize {
    if depth == 0 {
        return 1;
    }

    let mut nodes = 0;

    let moves = position.legal_moves();
    // println!("Valid moves: {moves:#?}");
    for chessmove in moves {
        let mut cloned = position.clone();
        cloned.make_move(chessmove);
        nodes += perft_nodes_only(cloned, depth - 1);
        // position.unmake_move(chessmove);
    }

    nodes
}

fn test_perft_fen(depth: usize, fen: &str, expected: &[PerftResult]) {
    let position = Position::new().from_fen(fen).unwrap();
    let res = perft(position, depth);
    assert_eq!(res.nodes, expected[depth].nodes);
    assert_eq!(res.captures, expected[depth].captures);
    assert_eq!(res.checks, expected[depth].checks);
    // assert_eq!(res.castles, expected[depth].castles);
    assert_eq!(res.checkmates, expected[depth].checkmates);
}

fn test_perft_fen_nodes(depth: usize, fen: &str, expected: usize) {
    let position = Position::new().from_fen(fen).unwrap();
    let res = perft_nodes_only(position, depth);
    assert_eq!(res, expected);
}

#[test]
fn test_standard_epd() {
    let contents = std::fs::read_to_string("tests/standard.epd").unwrap();

    for (i, entry) in contents.lines().enumerate() {
        let mut parts = entry.split(';');

        let fen = parts.next().unwrap().trim();

        for perft_data in parts {
            let depth = usize::from_str_radix(perft_data.get(1..2).unwrap().trim(), 10).unwrap();
            let expected = usize::from_str_radix(perft_data.get(3..).unwrap().trim(), 10).unwrap();
            // println!("perft({depth}, \"{fen}\") := {expected}");

            let position = Position::new().from_fen(fen).unwrap();

            let nodes = perft_nodes_only(position, depth);

            assert_eq!(
                nodes, expected,
                "\n{i} Perft({depth}, \"{fen}\") failed\nExpected: {expected}\nGot: {nodes}"
            );
        }
    }
}

#[cfg(test)]
mod kiwipete_perft {

    use super::*;

    #[test]
    fn kiwipete_perft_1() {
        test_perft_fen_nodes(
            1,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 ",
            48,
        );
    }

    #[test]
    fn kiwipete_perft_2() {
        test_perft_fen_nodes(
            2,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            2039,
        );
    }

    #[test]
    fn kiwipete_perft_3() {
        test_perft_fen_nodes(
            3,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            97862,
        );
    }

    #[test]
    fn kiwipete_perft_4() {
        test_perft_fen_nodes(
            4,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            4085603,
        );
    }
    #[test]
    fn kiwipete_perft_5() {
        test_perft_fen_nodes(
            5,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 ",
            193690690,
        );
    }

    #[test]
    fn kiwipete_perft_6() {
        test_perft_fen_nodes(
            6,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 ",
            8031647685,
        );
    }
}

#[cfg(test)]
mod promotion_perft {
    use crate::test_perft_fen_nodes;

    #[test]
    fn test_promotion_perft_1() {
        test_perft_fen_nodes(1, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 24);
    }
    #[test]
    fn test_promotion_perft_2() {
        test_perft_fen_nodes(2, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 496);
    }
    #[test]
    fn test_promotion_perft_3() {
        test_perft_fen_nodes(3, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 9483);
    }
    #[test]
    fn test_promotion_perft_4() {
        test_perft_fen_nodes(4, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 182838);
    }
    #[test]
    fn test_promotion_perft_5() {
        test_perft_fen_nodes(5, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 3605103);
    }

    #[test]
    fn test_promotion_perft_6() {
        test_perft_fen_nodes(6, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 71179139);
    }
}

/*
#[test]
fn perft_0() {
    test_starting_pos_perft(0);
}

#[test]
fn perft_1() {
    test_starting_pos_perft(1);
}

#[test]
fn perft_2() {
    test_starting_pos_perft(2);
}

#[test]
fn perft_3() {
    test_starting_pos_perft(3);
}

#[test]
fn perft_4() {
    test_starting_pos_perft(4);
}

#[test]
fn perft_5() {
    test_starting_pos_perft(5);
}

 */

/// https://www.chessprogramming.net/perfect-perft/
#[cfg(test)]
mod simple_perfts {
    use super::*;

    #[test]
    fn test_simple_perft_1() {
        test_perft_fen_nodes(
            6,
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            119060324,
        );
    }

    #[test]
    fn test_simple_perft_2() {
        test_perft_fen_nodes(
            5,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            193690690,
        );
    }

    // #[test]
    // fn test_simple_perft_3() {
    //     test_perft_fen_nodes(7, "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -", 178633661);
    // }

    // #[test]
    // fn test_simple_perft_4() {
    //     test_perft_fen_nodes(
    //         6,
    //         "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
    //         706045033,
    //     );
    // }

    #[test]
    fn test_simple_perft_5() {
        test_perft_fen_nodes(5, "1k6/1b6/8/8/7R/8/8/4K2R b K - 0 1", 1063513);
    }
}

/// https://www.chessprogramming.net/perfect-perft/
#[cfg(test)]
mod special_perfts {
    use super::*;

    #[test]
    fn test_special_perft_illegal_ep_move_1() {
        test_perft_fen_nodes(6, "3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1", 1134888);
    }

    #[test]
    fn test_special_perft_illegal_ep_move_2() {
        test_perft_fen_nodes(6, "8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1", 1015133);
    }

    #[test]
    fn test_special_perft_ep_capture_checks_opponent() {
        test_perft_fen_nodes(6, "8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", 1440467);
    }

    #[test]
    fn test_special_perft_short_castling_gives_check() {
        test_perft_fen_nodes(6, "5k2/8/8/8/8/8/8/4K2R w K - 0 1", 661072);
    }

    #[test]
    fn test_special_perft_long_castling_gives_check() {
        test_perft_fen_nodes(6, "3k4/8/8/8/8/8/8/R3K3 w Q - 0 1", 803711);
    }

    #[test]
    fn test_special_perft_castling_rights() {
        test_perft_fen_nodes(4, "r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1", 1274206);
    }

    #[test]
    fn test_special_perft_castling_prevented() {
        test_perft_fen_nodes(4, "r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1", 1720476);
    }

    #[test]
    fn test_special_perft_promote_out_of_check() {
        test_perft_fen_nodes(6, "2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", 3821001);
    }

    #[test]
    fn test_special_perft_discovered_check() {
        test_perft_fen_nodes(5, "8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1", 1004658);
    }

    #[test]
    fn test_special_perft_promote_to_give_check() {
        test_perft_fen_nodes(6, "4k3/1P6/8/8/8/8/K7/8 w - - 0 1", 217342);
    }

    #[test]
    fn test_special_perft_under_promote_to_give_check() {
        test_perft_fen_nodes(6, "8/P1k5/K7/8/8/8/8/8 w - - 0 1", 92683);
    }

    #[test]
    fn test_special_perft_self_stalemate() {
        test_perft_fen_nodes(6, "K1k5/8/P7/8/8/8/8/8 w - - 0 1", 2217);
    }

    #[test]
    fn test_special_perft_stalemate_and_checkmate_1() {
        test_perft_fen_nodes(7, "8/k1P5/8/1K6/8/8/8/8 w - - 0 1", 567584);
    }

    #[test]
    fn test_special_perft_stalemate_and_checkmate_2() {
        test_perft_fen_nodes(4, "8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1", 23527);
    }
}
