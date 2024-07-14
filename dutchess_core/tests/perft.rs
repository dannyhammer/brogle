use dutchess_core::core::Game;

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
const NODES: [usize; 14] = [
    1,
    20,
    400,
    8_902,
    197_281,
    4_865_609,
    119_060_324,
    3_195_901_860,
    84_998_978_956,
    2_439_530_234_167,
    69_352_859_712_417,
    2_097_651_003_696_806,
    62_854_969_236_701_747,
    1_981_066_775_000_396_239,
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
fn perft(game: &mut Game, depth: usize) -> usize {
    if depth == 0 {
        return 1;
    }

    let mut nodes = 0;

    for chessmove in game.get_legal_moves() {
        game.make_move(chessmove);
        nodes += perft(game, depth - 1);
        game.unmake_move();
    }

    nodes
}

fn test_perft(depth: usize) {
    let mut game = Game::default_setup();
    let nodes = perft(&mut game, depth);
    assert_eq!(nodes, NODES[depth]);
}

#[test]
fn perft_0() {
    test_perft(0);
}

#[test]
fn perft_1() {
    test_perft(1);
}

#[test]
fn perft_2() {
    test_perft(2);
}

/*
#[test]
fn perft_3() {
    test_perft(3);
}
 */
