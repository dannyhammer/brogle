/*
use brogle_core::{Move, MoveGenerator};

fn perft(movegen: MoveGenerator, depth: usize) -> usize {
    if depth == 0 {
        return 1;
    }

    // let tab = " ".repeat(depth);
    // eprintln!("\n{tab}PERFT({depth}): {position}\n{position:?}");

    let mut nodes = 0;

    let moves = movegen.legal_moves();
    // eprintln!("LEGAL MOVES: {moves:?}");
    for chessmove in moves.iter() {
        let mut cloned = position.clone();
        // eprintln!("{tab}Making  : {chessmove}");
        cloned.make_move(*chessmove);
        // eprintln!("{tab}{cloned}");
        nodes += perft(cloned, depth - 1);
        // eprintln!("{tab}Unmaking: {chessmove}");
        // cloned.unmake_move(chessmove);
        // eprintln!("{tab}{cloned}");
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
        let parsed = Move::from_uci(&position, move_str).unwrap();

        eprintln!("Applying {parsed} to {position}");
        position.make_move(parsed);
    }

    let mut nodes = 0;
    let moves = position.legal_moves();

    for chessmove in moves.iter() {
        let mut cloned = position.clone();
        cloned.make_move(*chessmove);
        let new_nodes = perft(cloned, depth - 1);
        // cloned.unmake_move(chessmove);
        nodes += new_nodes;

        println!("{chessmove} {new_nodes}");
    }

    println!("\n{nodes}");
}

*/
use std::process;

use brogle_core::{perft, Game, Move};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 3 {
        println!("Usage: {} <depth> <fen> [moves]", args[0]);
        process::exit(1);
    }

    let depth: usize = args[1].parse().expect("Failed to parse depth value");
    let fen = &args[2];
    let moves = if args.len() > 3 { &args[3] } else { "" };

    let mut game = Game::from_fen(fen).expect("Bad fen");
    for move_str in moves.split_ascii_whitespace() {
        let parsed = Move::from_uci(&game, move_str).unwrap();

        eprintln!("Applying {parsed} to {}", game.position());
        game.make_move(parsed);
    }

    let mut nodes = 0;
    let moves = game.legal_moves();

    for chessmove in moves.iter() {
        let mut cloned = game.clone();
        cloned.make_move(*chessmove);
        let new_nodes = perft(&cloned, depth - 1);
        // cloned.unmake_move(chessmove);
        nodes += new_nodes;

        println!("{chessmove} {new_nodes}");
    }

    println!("\n{nodes}");
}