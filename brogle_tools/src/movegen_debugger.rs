use std::{process::Command, str::FromStr};

use anyhow::{anyhow, bail, Context, Result};
use chess;

const PATH_TO_EPD: &str = "chessie/tests/standard.epd";

/// Encapsulates frequently-used data like the user-supplied script
struct PerftChecker<'a> {
    user_script: &'a str,
}

impl<'a> PerftChecker<'a> {
    /// Create the checker with the user-supplied script
    fn new(user_script: &'a str) -> Self {
        Self { user_script }
    }

    /// Executes the user-supplied script on the provided position
    fn exec_user_perft(
        &self,
        depth: usize,
        fen: &str,
        moves: &[String],
    ) -> Result<(Vec<(String, usize)>, usize)> {
        // Create the command
        let mut cmd = Command::new(self.user_script);

        // Execute the command, saving its output
        let output = cmd
            .arg(depth.to_string())
            .arg(fen)
            .arg(moves.join(" "))
            .output()
            .context(format!(
                "Failed to execute user splitperft script:\n{cmd:?}"
            ))?;

        // Extract the stdout and stderr from the script
        let stdout = String::from_utf8(output.stdout)
            .context("Failed to convert stdout of child process to String")?;

        if !output.status.success() {
            let stderr = String::from_utf8(output.stderr)
                .context("Failed to convert stderr of child process to String")?;

            if let Some(mv) = moves.last() {
                // Generate the FEN of the position after applying all of the problematic moves
                let new_fen = generate_fen_from(fen, moves);

                // Format and return the error
                let moves_str = moves.join(", ");
                bail!("Error: Move generator crashed after applying {mv:?}\nApplied moves: {moves_str}\nResulting FEN: {new_fen:?}\n\nFull error:\n{stderr}");
            } else {
                bail!("Error: Move generator crashed on: {fen:?}\n\nFull error:\n{stderr}");
            }
        }

        // Parse the splitperft results
        let results = stdout
            .lines()
            .filter_map(|line| {
                // If there is no space to delimit the move and the node count, then we've reached the end
                let mid = line.find(' ')?;

                // Split the move and the node count
                let (mv, nodes) = line.split_at(mid);
                let mv = mv.trim().to_string();
                let nodes = nodes.trim().parse().expect("Failed to parse node count");

                Some((mv, nodes))
            })
            .collect();

        let nodes = stdout.lines().last().ok_or(anyhow!(
            "User script must have a final line containing total number of nodes"
        ))?;
        let nodes = nodes
            .parse()
            .context("Failed to parse final line of user script output")?;

        Ok((results, nodes))
    }

    /// Generates a (correct) splitperft.
    ///
    /// For each legal move, it generates the possible nodes reachable from playing that move.
    fn generate_splitperft(
        &self,
        depth: usize,
        fen: &str,
        moves: &[String],
    ) -> (Vec<(String, usize)>, usize) {
        // println!("Generating splitperft for {fen} at depth {depth}");
        let mut results = Vec::with_capacity(128);
        let mut board: chess::Board = fen.parse().unwrap();

        // If there were any moves supplied, apply them
        for move_text in moves {
            match chess::ChessMove::from_str(move_text) {
                Ok(mv) => board = board.make_move_new(mv),
                Err(_) => panic!("Invalid move {move_text} for position {board}"),
            };
        }

        let movegen = chess::MoveGen::new_legal(&board);

        let mut nodes = 0;
        for mv in movegen {
            let new_board = board.make_move_new(mv);

            let new_nodes = perft(new_board, depth - 1);
            nodes += new_nodes;

            // println!("{mv} {new_nodes}");
            results.push((mv.to_string(), new_nodes));
        }

        (results, nodes)
    }

    /// Checks that all of the PERFT results on the provided `epd` string are valid.
    fn check_epd(&self, epd: &str) -> Result<()> {
        let mut parts = epd.split(';');

        // Extract the FEN
        let fen = parts.next().unwrap().trim();

        println!("Beginning perft check on {fen:?}");
        for perft_data in parts {
            // Extract and parse the depth
            let depth = perft_data
                .get(1..2)
                .context(format!("Missing depth value in {perft_data:?}"))?
                .trim();
            let depth = depth
                .parse::<usize>()
                .context(format!("Invalid depth value {depth:?}"))?;

            // Extract and parse the expected nodes
            let expected = perft_data
                .get(3..)
                .context(format!("Missing expected nodes value in {perft_data:?}"))?
                .trim();
            let expected = expected
                .parse::<usize>()
                .context(format!("Invalid expected nodes value {expected:?}"))?;

            println!("\tChecking perft({depth}) := {expected}");
            // Check if the user-supplied move generator is correct for this depth and FEN
            self.check_splitperft::<false>(depth, fen, &[])?;
        }

        Ok(())
    }

    fn check_splitperft<const ILLEGAL: bool>(
        &self,
        depth: usize,
        fen: &str,
        moves: &[String],
    ) -> Result<()> {
        // Get the perft results from the user-supplied script
        let (user_results, user_nodes) = self.exec_user_perft(depth, fen, moves)?;

        // Generate the correct result
        let (correct_results, correct_nodes) = self.generate_splitperft(depth, fen, moves);

        // If we've reached depth 1 and we're in a chain of moves that leads to an illegal position, we need to report the problematic move.
        if ILLEGAL && depth == 1 {
            // Fetch all of the legal moves for this position
            let mut legal_moves = correct_results
                .into_iter()
                .map(|(mv, _)| mv)
                .collect::<Vec<_>>();
            legal_moves.sort();

            // Fetch all of the moves we've created for this position
            let mut generated_moves = user_results
                .into_iter()
                .map(|(mv, _)| mv)
                .collect::<Vec<_>>();
            generated_moves.sort();

            // Get the reason as to why these lists are different.
            let list_diff_err = check_move_lists(legal_moves, generated_moves).unwrap_err();

            // Generate the FEN of the position after applying all of the problematic moves
            let new_fen = generate_fen_from(fen, moves);

            // Format and return the error message
            let moves_str = moves.join(", ");
            bail!("Error: {list_diff_err}\nApplied moves: {moves_str}\nResulting FEN: {new_fen:?}");
        }

        // If the user-supplied script is incorrect, we need to follow this chain of moves until we find the source
        if user_nodes != correct_nodes {
            // Loop over every move in the correct result
            for (mv, nodes) in correct_results {
                // If the user-supplied results don't have this move, it's an error
                let Some(user_result) = user_results.iter().find(|(user_mv, _)| *user_mv == mv)
                else {
                    // Generate the FEN of the position after applying all of the problematic moves
                    let new_fen = generate_fen_from(fen, moves);

                    // Format and return the error
                    let moves_str = moves.join(", ");
                    bail!("Error: Failed to generate legal move {mv:?}\nApplied moves: {moves_str}\nResulting FEN: {new_fen:?}");
                };
                let user_nodes_for_mv = user_result.1;

                // If the user-supplied script did not generate the appropriate number of nodes, then this move (eventually) leads to problems.
                if user_nodes_for_mv != nodes {
                    // eprintln!("{mv:?} at depth {depth} on {fen:?} yields an incorrect {user_nodes_for_mv} nodes. Correct: {nodes}");

                    // Append this move onto the line we're inspecting
                    let mut moves_to_inspect = Vec::from(moves);
                    moves_to_inspect.push(mv);

                    // Recursively check this line, noting that now we've found an illegal position
                    self.check_splitperft::<true>(depth - 1, fen, &moves_to_inspect)?;
                }
            }
        }

        Ok(())
    }
}

/// Checks the contents of two lists.
///
/// The length of the lists is checked first.
/// If the lengths do not match, either you have generated an illegal move, or failed to generate a legal one.
/// If both lists are of equal length, [`check_move_lists_of_equal_length`] is called.
fn check_move_lists(mut expected: Vec<String>, mut generated: Vec<String>) -> Result<()> {
    // If there are more moves in the expected list, the supplied move generator failed to generate some legal moves
    if expected.len() > generated.len() {
        expected.retain(|mv| !generated.contains(&mv));
        let word = if expected.len() > 1 { "moves" } else { "move" };
        bail!("Failed to generate legal {word}: {}", expected.join(", "));
    }
    // If there are more moves in the supplied list, the supplied move generator generated illegal moves
    else if generated.len() > expected.len() {
        generated.retain(|mv| !expected.contains(&mv));
        let word = if generated.len() > 1 { "moves" } else { "move" };
        bail!("Illegal {word} generated: {}", generated.join(", "));
    }

    // If the lengths of both lists match, we need to check that each move generated is correct.
    check_move_lists_of_equal_length(expected, generated)
}

/// Generates a FEN string after applying all of `moves` to the provided `fen`.
fn generate_fen_from(fen: &str, moves: &[String]) -> String {
    let mut board = chess::Board::from_str(fen).unwrap();

    for move_text in moves {
        let mv = chess::ChessMove::from_str(move_text).unwrap();
        board = board.make_move_new(mv);
    }

    board.to_string()
}

/// Checks the contents of two lists of equal length.
///
/// If `generated` contains a move that `expected` does not, this returns an error.
fn check_move_lists_of_equal_length(expected: Vec<String>, generated: Vec<String>) -> Result<()> {
    // Create a list of all moves that are in `generated` and NOT in `expected`.
    let illegal = generated
        .iter()
        .filter(|mv| !expected.contains(mv))
        .collect::<Vec<_>>();

    // If there are any such moves, they are illegal
    if illegal.len() > 0 {
        let word = if illegal.len() > 1 { "moves" } else { "move" };
        bail!(
            "Generated illegal {word}: {}\nAnd neglected to generate: {}",
            generated.join(", "),
            expected.join(", ")
        )
    }

    // If there are no such moves, we're good to go!
    Ok(())
}

/// Simple [PERFT](https://www.chessprogramming.org/Perft) (performance test) function.
///
/// Internally uses bulk counting.
fn perft(board: chess::Board, depth: usize) -> usize {
    let movegen = chess::MoveGen::new_legal(&board);

    // (Bulk counting) No need to enumerate all possible moves if we're at depth 1; just return the number of moves.
    if depth == 1 {
        return movegen.len();
    } else if depth == 0 {
        return 1;
    }

    // Recursively accumulate total number of nodes remaining.
    movegen.fold(0, |n, mv| n + perft(board.make_move_new(mv), depth - 1))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        println!("Usage: {} <your/perft/script.sh>", args[0]);
        std::process::exit(1);
    }

    let checker = PerftChecker::new(&args[1]);

    let contents = std::fs::read_to_string(PATH_TO_EPD).unwrap();
    for epd in contents.lines().skip(1) {
        if let Err(e) = checker.check_epd(epd) {
            eprintln!("\n{e}");
            break;
        }
    }
}
