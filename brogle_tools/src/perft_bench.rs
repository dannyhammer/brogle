use std::time::Instant;

use colored::*;

use chessie::{perft, Game};

/// Total number of nodes in the `standard.epd` test suite.
const TOTAL_NODES_IN_SUITE: u64 = 4805793839;

fn main() {
    let contents = std::fs::read_to_string("chessie/tests/standard.epd").unwrap();

    let mut total_nodes_tested = 0;

    let now = Instant::now();
    for (i, entry) in contents.lines().enumerate() {
        let mut parts = entry.split(';');

        let fen = parts.next().unwrap().trim();

        print!("{}", "\n[INIT]".yellow());
        println!(" Beginning perft on {fen:?}");
        for perft_data in parts {
            let depth = perft_data
                .get(1..2)
                .unwrap()
                .trim()
                .parse::<usize>()
                .unwrap();
            let expected = perft_data.get(3..).unwrap().trim().parse::<u64>().unwrap();

            let position = Game::from_fen(fen).unwrap();

            let start = Instant::now();
            let nodes = perft(&position, depth);
            let elapsed = start.elapsed();
            total_nodes_tested += nodes;

            assert_eq!(
                nodes, expected,
                "\nTest #{i}: Perft({depth}, \"{fen}\") failed\nExpected: {expected}\nGot     : {nodes}",
            );
            print!("{}", "[PASS]".green());
            let nps = nodes as f32 / elapsed.as_secs_f32();
            let m_nps = nps / 1_000_000.0;
            println!(" Depth {depth}: {nodes} nodes / {elapsed:?} = {m_nps} mNPS",);
        }

        let elapsed = now.elapsed();
        // let percentage = ((1.0 + i as f32) / len as f32) * 100.0;
        let percentage = ((total_nodes_tested as f32) / TOTAL_NODES_IN_SUITE as f32) * 100.0;
        let nps = total_nodes_tested as f32 / elapsed.as_secs_f32();
        let m_nps = nps / 1_000_000.0;
        print!("{}", "[INFO]".cyan());
        println!(" {percentage:>3.1}% completed, avg NPS: {nps:.0}, avg mNPS {m_nps:.1}",);
    }
    let elapsed = now.elapsed();

    // Math
    let nps = total_nodes_tested as f32 / elapsed.as_secs_f32();
    let m_nps = nps / 1_000_000.0;

    println!();
    println!("Elapsed Time:          {elapsed:.1?}");
    println!("Total Nodes:           {total_nodes_tested}");
    println!("Nodes / Sec:           {nps:.0}");
    println!("M Nodes / Sec:         {m_nps:.1}");
}
