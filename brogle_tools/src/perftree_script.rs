use std::process;

use brogle::Engine;

/// This script exists exclusively to be used with the [perftree](https://github.com/agausmann/perftree) program for debugging.
fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 3 {
        println!("Usage: {} <depth> <fen> [moves]", args[0]);
        process::exit(1);
    }

    let depth: usize = args[1].parse().expect("Failed to parse depth value");
    let fen = &args[2];
    let moves = if args.len() > 3 { &args[3] } else { "" };

    let mut engine = Engine::from_fen(fen).expect("Invalid FEN provided");
    engine
        .make_move(moves.split_ascii_whitespace())
        .expect("msg");

    _ = engine.perft(depth, false, true);
}
