use std::{cmp::Ordering, ops::Neg, time::Instant};

use dutchess_core::{Move, Position};
use rand::{seq::SliceRandom, thread_rng};

use crate::eval;
use crate::uci::SearchOptions;

#[derive(Debug, Clone, Copy)]
pub struct SearchResult {
    pub bestmove: Option<Move>,
    pub score: i32,
    pub nodes_searched: usize,
}

impl Default for SearchResult {
    fn default() -> Self {
        Self {
            bestmove: None,
            score: -20_000,
            nodes_searched: 0,
        }
    }
}

/*
impl SearchResult {
    fn new(bestmove: Move, score: i32) -> Self {
        Self {
            bestmove,
            score,
            nodes: 0,
        }
    }
}
 */

impl PartialEq for SearchResult {
    fn eq(&self, other: &Self) -> bool {
        self.score.eq(&other.score)
    }
}
impl Eq for SearchResult {}

impl PartialOrd for SearchResult {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.score.partial_cmp(&other.score)
    }
}
// impl Ord for SearchResult {
//     fn cmp(&self, other: &Self) -> std::cmp::Ordering {
//         self.score.cmp(&other.score)
//     }
// }

impl Neg for SearchResult {
    type Output = Self;
    fn neg(mut self) -> Self::Output {
        self.score = -self.score;
        self
    }
}

pub struct Search<'a> {
    position: Position,
    ply_to_search: u32,
    options: SearchOptions<'a>,
    result: SearchResult,
    // stopper: Arc<AtomicBool>,
    // moves: [Move; MAX_NUM_MOVES],
}

impl<'a> Search<'a> {
    pub fn new(position: Position, ply_to_search: u32) -> Self {
        Self {
            position,
            ply_to_search,
            options: SearchOptions::default(),
            result: SearchResult::default(),
            // moves: position.legal_moves(),
            // moves: [Move::illegal(); MAX_NUM_MOVES],
        }
    }

    pub fn with_options(mut self, options: SearchOptions<'a>) -> Self {
        self.options = options;
        self
    }

    pub fn result(&self) -> SearchResult {
        self.result
    }

    pub fn start_sync(mut self) -> SearchResult {
        let now = Instant::now();
        // self.result = negamax(self.position, self.ply_to_search, -32_000, 32_000);
        self.result = self.negamax(self.position, self.ply_to_search, -32_000, 32_000);
        let elapsed = now.elapsed();

        let nps = self.result.nodes_searched as f64 / elapsed.as_secs_f64();
        eprintln!(
            "Searched {} nodes in {elapsed:?} ({nps:.2} n/s)",
            self.result.nodes_searched
        );
        self.result
    }

    fn order_moves(&self, moves: &mut [Move]) {
        // Order captures
        moves.sort_unstable_by(|a, b| {
            if a.kind() == b.kind() {
                Ordering::Equal
            } else if a.is_capture() && !b.is_capture() {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });
    }

    fn negamax(&self, position: Position, ply: u32, mut alpha: i32, beta: i32) -> SearchResult {
        // println!("Searching moves for {:?}", position.current_player());
        // Start with a default (very bad) result.
        let mut best = SearchResult::default();

        // Reached the end of the ply; return board's evaluation.
        if ply == 0 {
            // return quiescence(position, alpha, beta);

            // Root nodes in negamax must be evaluated from the current player's perspective
            // best.score = eval_for(&position, position.current_player());
            best.score = eval(&position);
            best.nodes_searched = 1;
            return best;
        }

        let mut cloned = position.clone();
        let moves = cloned.legal_moves_mut();
        self.order_moves(moves);
        // println!("MOVES: {moves:?}");

        for mv in moves.iter() {
            // Make the current move on the position, getting a new position in return
            let new_pos = position.with_move_made(*mv);

            // Recursively search our opponent's responses
            let current = -self.negamax(new_pos, ply - 1, -beta, -alpha);
            best.nodes_searched += current.nodes_searched;
            // eprintln!("Evaluating {mv} to have score {}", new_res.score);

            // Fail soft beta-cutoff;
            if current.score >= beta {
                best.score = current.score;
                best.bestmove = Some(*mv);
                return best;
            }

            // If we've found a better move than our current best, update our result
            if current.score > best.score {
                best.score = current.score;
                best.bestmove = Some(*mv);
            }
            // Update alpha.
            alpha = alpha.max(current.score);

            // Opponent would never choose this branch, so we can prune
            if alpha >= beta {
                // eprintln!("{alpha} >= {beta}: Pruning branch {mv}");
                break;
            }
        }

        // Handle cases for checkmate and
        if best.bestmove.is_none() {
            if moves.is_empty() {
                // eprintln!("No legal moves available at: {position}\nRes: {best:?}");
            } else {
                let random = moves.choose(&mut thread_rng()).map(|m| *m);
                // eprintln!("No best move found. Choosing randomly: {random:?}");
                best.bestmove = random;
            }
        }

        best
    }
}

pub fn search(position: Position, ply: u32) -> SearchResult {
    negamax(position, ply, -32_000, 32_000)
}

fn order_moves(moves: &[Move]) -> Vec<Move> {
    let mut moves = moves.to_owned();

    // Order captures
    moves.sort_unstable_by(|a, b| {
        if a.kind() == b.kind() {
            Ordering::Equal
        } else if a.is_capture() && !b.is_capture() {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    });

    moves
}

fn negamax(position: Position, ply: u32, mut alpha: i32, beta: i32) -> SearchResult {
    // println!("Searching moves for {:?}", position.current_player());
    // Start with a default (very bad) result.
    let mut best = SearchResult::default();

    // Reached the end of the ply; return board's evaluation.
    if ply == 0 {
        // return quiescence(position, alpha, beta);

        // Root nodes in negamax must be evaluated from the current player's perspective
        best.score = eval(&position);
        best.nodes_searched = 1;
        return best;
    }

    let moves = position.legal_moves();
    let moves = order_moves(moves);
    // let moves = position.legal_moves();
    // println!("MOVES: {moves:?}");

    for mv in &moves {
        // Make the current move on the position, getting a new position in return
        let new_pos = position.with_move_made(*mv);

        // Recursively search our opponent's responses
        let current = -negamax(new_pos, ply - 1, -beta, -alpha);
        best.nodes_searched += current.nodes_searched;
        // eprintln!("Evaluating {mv} to have score {}", new_res.score);

        // Fail soft beta-cutoff;
        if current.score >= beta {
            best.score = current.score;
            best.bestmove = Some(*mv);
            return best;
        }

        // If we've found a better move than our current best, update our result
        if current.score > best.score {
            best.score = current.score;
            best.bestmove = Some(*mv);
        }
        // Update alpha.
        alpha = alpha.max(current.score);

        // Opponent would never choose this branch, so we can prune
        if alpha >= beta {
            // eprintln!("{alpha} >= {beta}: Pruning branch {mv}");
            break;
        }
    }

    // Handle cases for checkmate and
    if best.bestmove.is_none() {
        if moves.is_empty() {
            // eprintln!("No legal moves available at: {position}\nRes: {best:?}");
        } else {
            let random = moves.choose(&mut thread_rng()).map(|m| *m);
            // eprintln!("No best move found. Choosing randomly: {random:?}");
            best.bestmove = random;
        }
    }

    best
}

/*
fn quiescence(position: &Position, mut alpha: i32, beta: i32) -> SearchResult {
    let mut best = SearchResult::default();
    best.score = eval_for(position, position.current_player());

    if best.score >= beta {
        best.score = beta;
        return best;
    }

    alpha = alpha.max(best.score);

    let captures = position
        .legal_moves()
        .into_iter()
        .filter(|mv| mv.is_capture());

    for mv in captures {
        let new_pos = position.with_move_made(*mv);
        let current = -quiescence(&new_pos, -beta, -alpha);
        best.nodes_searched += current.nodes_searched;

        if current.score >= beta {
            best.score = current.score;
            best.bestmove = Some(*mv);
            return best;
        }

        alpha = alpha.max(current.score);
    }

    best.score = alpha;
    return best;
}
 */

/*
fn negamax(state: &Position, ply: u32) -> (i32, usize) {
    // eprintln!("Negamax with ply of {ply}");
    // Reached the end of the ply; return board's evaluation.
    if ply == 0 {
        // eprintln!("Reached ply of 0. Returning {score}");
        return (eval(state), 1);
    }

    // Our goal is to maximize this number
    let mut max = i32::MAX;

    // let moves = MoveGen::new_legal(&state);
    let moves = state.legal_moves();

    // If there are no moves available, it's a special case.
    if moves.len() == 0 {
        let score = eval(state);
        eprintln!("No legal moves remaining. Returning {score}");
        return (score, 1);
    }

    let mut nodes = 0;

    // For every move, recursively call this function on the new state created by
    // applying that move
    for mv in moves {
        let mut new_state = state.clone();
        new_state.make_move(*mv);

        let (score, new_nodes) = negamax(&new_state, ply - 1);
        let score = -score;
        nodes += new_nodes;

        // If the score is better, record it.
        max = score.max(max);
    }

    (max, nodes)
}
 */

/*
pub trait Search {
    fn search(&mut self, state: GameState, ply: u32, max_ply: u32) -> SearchResult;
}

struct RandomMove;
impl Search for RandomMove {
    fn search(&mut self, state: GameState, _ply: u32, _max_ply: u32) -> SearchResult {
        let mut res = SearchResult::default();
        res.bestmove = state.legal_moves().choose(&mut thread_rng()).unwrap();
        res
    }
}
 */
