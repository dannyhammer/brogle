use std::time::Instant;

use dutchess_core::{perft, Position};

fn main() {
    let contents = std::fs::read_to_string("dutchess_core/tests/standard.epd").unwrap();

    let lines = contents.lines().collect::<Vec<&str>>();
    let len = lines.len();

    let mut total_nodes = 0;

    let now = Instant::now();
    for (i, entry) in contents.lines().enumerate() {
        let mut parts = entry.split(';');

        let fen = parts.next().unwrap().trim();

        for perft_data in parts {
            let depth = usize::from_str_radix(perft_data.get(1..2).unwrap().trim(), 10).unwrap();
            let expected = u64::from_str_radix(perft_data.get(3..).unwrap().trim(), 10).unwrap();
            // println!("perft({depth}, \"{fen}\") := {expected}");

            let mut position = Position::from_fen(fen).unwrap();

            let nodes = perft(&mut position, depth);
            total_nodes += nodes;

            assert_eq!(
                nodes, expected,
                "\n{i} Perft({depth}, \"{fen}\") failed\nExpected: {expected}\nGot     : {nodes}",
            );
        }

        let elapsed = now.elapsed();
        let nps = total_nodes as f32 / elapsed.as_secs_f32();
        let m_nps = nps / 1_000_000.0;
        let percentage = ((1.0 + i as f32) / len as f32) * 100.0;
        println!("{percentage:>5.1}% complete - NPS: {nps:.0}, mNPS {m_nps:.1}",);
    }
    let elapsed = now.elapsed();

    // Math
    let nps = total_nodes as f32 / elapsed.as_secs_f32();
    let m_nps = nps / 1_000_000.0;

    println!();
    println!("Elapsed Time:          {elapsed:.1?}");
    println!("Total Nodes:           {total_nodes}");
    println!("Nodes / Sec:           {nps:.0}");
    println!("M Nodes / Sec:         {m_nps:.1}");
}
