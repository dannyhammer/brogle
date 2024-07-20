use std::process;

use dutchess_core::core::{Move, Position};

fn perft(position: Position, depth: usize) -> usize {
    if depth == 0 {
        return 1;
    }

    let mut nodes = 0;

    let moves = position.legal_moves();
    for chessmove in moves {
        let mut position = position.clone();
        position.make_move(chessmove);
        nodes += perft(position.clone(), depth - 1);
        position.unmake_move(chessmove);
    }

    nodes
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 3 {
        println!("Usage: {} <depth> <fen> [moves]", args[0]);
        process::exit(1);
    }

    let depth: usize = args[1].parse().expect("Failed to parse depth value");
    let fen = &args[2];
    let moves = if args.len() > 3 { &args[3] } else { "" };

    let mut position = Position::new().from_fen(fen).expect("Bad fen");
    for move_str in moves.split_ascii_whitespace() {
        let parsed = Move::from_san(&position, move_str).unwrap();
        position.make_move(parsed);
    }

    let mut nodes = 0;
    let moves = position.legal_moves();

    for chessmove in moves {
        let mut position = position.clone();
        position.make_move(chessmove);
        let new_nodes = perft(position.clone(), depth - 1);
        position.unmake_move(chessmove);
        nodes += new_nodes;

        println!("{chessmove} {new_nodes}");
    }

    println!("\n{nodes}");
}
