use std::{fs, str::FromStr};

use chess::{Board, MoveGen};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        println!("Usage: {} <fen> | <path to fens.txt>", args[0]);
        std::process::exit(1);
    }

    if let Ok(fens) = fs::read_to_string(&args[1]) {
        for fen in fens.lines() {
            print_to_code(fen);
        }
    } else {
        print_to_code(&args[1])
    }
}

fn print_to_code(fen: &str) {
    let board = match Board::from_str(fen) {
        Ok(board) => board,
        Err(e) => panic!("{e}\nInvalid board: {fen}"),
    };

    let legal_moves = MoveGen::new_legal(&board)
        .map(|m| format!("\"{m}\""))
        .collect::<Vec<_>>()
        .join(", ");

    let pos_str = fen.replace(['/', ' '], "_").replace('-', "X");

    let template = format!(
        "
#[test]
#[allow(non_snake_case)]
fn test_moves_from_{pos_str}() {{
    let pos = setup_game(\"{fen}\");
    let moves = pos.legal_moves();
    let legal_moves = [ {legal_moves} ];

    lists_match(&pos, &moves, legal_moves);
}}"
    );

    println!("{template}");
}
