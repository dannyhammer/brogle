use std::ops::{Add, AddAssign};

use dutchess_core::core::{Position, DEFAULT_FEN};

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

/*
u64 Perft(int depth)
{
  MOVE move_list[256];
  int n_moves_ i;
  u64 nodes = 0;

  if (depth == 0)
    return 1ULL;

  n_moves = GenerateLegalMoves(move_list);
  for (i = 0; i < n_moves; i++) {
    MakeMove(move_list[i]);
    nodes += Perft(depth - 1);
    UndoMove(move_list[i]);
  }
  return nodes;
}
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

fn test_starting_pos_perft(depth: usize) {
    test_perft_fen(depth, DEFAULT_FEN, &STARTING_PERFT_RESULTS);
}

fn test_kiwipete_perft(depth: usize) {
    test_perft_fen(
        depth,
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
        &KIWIPETE_PERFT_RESULTS,
    );
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

#[test]
fn kiwipete_perft_0() {
    test_kiwipete_perft(0);
}

#[test]
fn kiwipete_perft_1() {
    test_kiwipete_perft(1);
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
 */

// #[test]
// fn perft_5() {
//     test_perft(5);
// }
