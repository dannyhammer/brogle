use core::fmt;
use std::{
    ops::{Add, AddAssign},
    time::Instant,
};

use super::Position;

#[derive(Default, Debug, Clone, Copy)]
pub struct PerftResult {
    /// Depth searched
    depth: usize,

    /// Number of game states reachable.
    nodes: u128,

    /// Number of captures possible.
    captures: u128,

    /// Number of times en passant can be performed.
    eps: u128,

    /// Number of times castling can occur.
    castles: u128,

    /// Number of times a pawn can be promoted. A single move counts as one promotion, not the four possible promotions.
    promotions: u128,

    /// Number of checks that can occur.
    checks: u128,

    /// Number of discovery checks possible.
    discovery_checks: u128,

    /// Number of double checks that can occur.
    double_checks: u128,

    /// Number of checkmates that can occur.
    checkmates: u128,
}

impl Add for PerftResult {
    type Output = Self;
    /// Convenience implementation to add perft results.
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            depth: self.depth,
            nodes: self.nodes + rhs.nodes,
            captures: self.captures + rhs.captures,
            eps: self.eps + rhs.eps,
            castles: self.castles + rhs.castles,
            promotions: self.promotions + rhs.promotions,
            checks: self.checks + rhs.checks,
            discovery_checks: self.discovery_checks + rhs.discovery_checks,
            double_checks: self.double_checks + rhs.double_checks,
            checkmates: self.checkmates + rhs.checkmates,
        }
    }
}

impl AddAssign for PerftResult {
    /// Convenience implementation to add perft results.
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

impl fmt::Display for PerftResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let entry = |val, width| format!("{val:>width$}");

        let format_line =
            |depth, nodes, captures, eps, castles, promos, checks, discoveries, doubles, mates| {
                format!(
                    "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |",
                    entry(depth, self.depth.to_string().len()),
                    entry(nodes, self.nodes.to_string().len().max("Nodes".len())),
                    entry(captures, self.captures.to_string().len().max("Capt.".len())),
                    entry(eps, self.eps.to_string().len().max("E.p.".len())),
                    entry(castles, self.castles.to_string().len().max("Cast.".len())),
                    entry(
                        promos,
                        self.promotions.to_string().len().max("Promo.".len())
                    ),
                    entry(checks, self.checks.to_string().len().max("Checks".len())),
                    entry(
                        discoveries,
                        self.discovery_checks.to_string().len().max("Disc.".len())
                    ),
                    entry(
                        doubles,
                        self.double_checks.to_string().len().max("Dbl.".len())
                    ),
                    entry(mates, self.checkmates.to_string().len().max("Mates".len()))
                )
            };

        let mut result = format_line(
            String::from(""),
            String::from("Nodes"),
            String::from("Capt."),
            String::from("E.p."),
            String::from("Cast."),
            String::from("Promo."),
            String::from("Checks"),
            String::from("Disc."),
            String::from("Dbl."),
            String::from("Mates"),
        );
        let divider = "-".repeat(result.len());
        result += "\n";
        result += &divider;
        result += "\n";

        for i in 0..self.depth {
            let line = format_line(
                i.to_string(),
                self.nodes.to_string(),
                self.captures.to_string(),
                self.eps.to_string(),
                self.castles.to_string(),
                self.promotions.to_string(),
                self.checkmates.to_string(),
                self.discovery_checks.to_string(),
                self.double_checks.to_string(),
                self.checkmates.to_string(),
            );
            result += &line;
            result += "\n";
        }

        write!(f, "{result}")
    }
}

pub fn print_split_perft(position: &Position, depth: usize) {
    // println!("SplitPerft({depth}) for {}", position.to_fen());
    let mut total_nodes = 1;
    let moves = position.legal_moves();
    for chessmove in moves {
        let new_pos = position.with_move_made(*chessmove);
        let nodes = perft(&new_pos, depth - 1);
        println!("{chessmove:>8} {nodes:>width$}", width = depth * 2 + 1);
        total_nodes += nodes;
    }

    println!("\n{total_nodes}");
}

pub fn print_split_perft_pretty(position: &Position, depth: usize) {
    println!("SplitPerft({depth}) for {}", position.to_fen());
    let mut total_nodes = 1;
    let moves = position.legal_moves();
    for chessmove in moves {
        let new_pos = position.with_move_made(*chessmove);
        let nodes = perft(&new_pos, depth - 1);
        println!("{chessmove:>8} {nodes:>width$}", width = depth * 2 + 1);
        total_nodes += nodes;
    }

    println!("\nMoves: {}", moves.len());
    println!("Nodes: {total_nodes}");
}

pub fn print_perft(position: &Position, depth: usize) {
    println!("Computing PERFT({depth}) of the following position:\n{position:?}\n");

    // Time the perft
    let now = Instant::now();
    let total_nodes = perft(position, depth);
    let elapsed = now.elapsed();

    // Math
    let nps = total_nodes as f64 / elapsed.as_secs_f64();
    let m_nps = nps / 1_000_000.0;

    println!("Elapsed Time:          {elapsed:?}");
    println!("Total Nodes:           {total_nodes}");
    println!("Nodes / Sec:           {nps:.0}");
    println!("M Nodes / Sec:         {m_nps:.1}");
}

pub fn perft_full(position: &Position, depth: usize) -> PerftResult {
    let mut res = PerftResult::default();

    if depth == 0 {
        res.nodes = 1;
        res.captures = 32 - position.bitboards().occupied().population() as u128; // TODO: Fetch original number of pieces
        res.castles = position.times_castled() as u128;
        res.checks = position.is_check() as u128;
        res.checkmates = position.is_checkmate() as u128;
        return res;
    }

    let moves = position.legal_moves();

    if depth == 1 {
        res.nodes = moves.len() as u128;
        return res;
    }

    for chessmove in moves {
        let new_pos = position.with_move_made(*chessmove);
        res += perft_full(&new_pos, depth - 1);
    }

    res
}

pub fn perft(position: &Position, depth: usize) -> u128 {
    if depth == 0 {
        return 1;
    }

    let mut nodes = 0;

    let moves = position.legal_moves();

    if depth == 1 {
        return moves.len() as u128;
    }

    for chessmove in moves {
        let new_pos = position.with_move_made(*chessmove);
        nodes += perft(&new_pos, depth - 1);
    }

    nodes
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
