/// This script exists exclusively to be used with the [perftree](https://github.com/agausmann/perftree) program for debugging.
fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    // Print usage if insufficient arguments provided
    if args.len() < 3 {
        println!("Usage: {} <depth> <fen> [moves]", args[0]);
        std::process::exit(1);
    }

    // Parse args appropriately
    let Ok(depth) = args[1].parse() else {
        panic!("Failed to parse {:?} as depth value", args[1]);
    };
    let mut game = chessie::Game::from_fen(&args[2])?;
    // Apply moves, if any were provided
    if args.len() > 3 {
        for mv_str in args[3].split_ascii_whitespace() {
            // Parse move string and apply it
            let mv = chessie::Move::from_uci(&game, mv_str)?;
            game.make_move(mv);
        }
    }

    // Perform a splitperft
    chessie::print_perft::<false, true>(&game, depth);

    Ok(())
}
