use std::time::{Duration, Instant};

use chess::{Board, ChessMove, Color, MoveGen};
use rand::{seq::IteratorRandom, thread_rng};
// use dutchess_core::{GameState, Move, PieceKind, Tile};

use crate::eval;

#[derive(Debug, Clone, Default)]
pub struct SearchResult {
    pub bestmove: ChessMove,
    pub value: i32,
}

impl SearchResult {
    fn new(bestmove: ChessMove, value: i32) -> Self {
        Self { bestmove, value }
    }
}

impl PartialEq for SearchResult {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}
impl Eq for SearchResult {}

impl PartialOrd for SearchResult {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}
impl Ord for SearchResult {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}

pub fn search(state: Board, depth: u32) -> SearchResult {
    // Start with a default result, with a default (probably illegal) move.
    let mut res = SearchResult::new(ChessMove::default(), i32::MIN);

    if depth == 0 {
        return res;
    }

    let root_moves = MoveGen::new_legal(&state);

    // For every available move, make that move on a new board
    for root_move in root_moves {
        let new_state = state.make_move_new(root_move);

        // Call negamax to check the move's score.
        let score = -negamax(new_state, depth - 1);

        // If the score is better than our current best, record it.
        if score > res.value {
            res.value = score;
            res.bestmove = root_move;
        }
    }

    // if res.value == i32::MIN {
    //     res.bestmove = MoveGen::new_legal(&state)
    //         .choose(&mut thread_rng())
    //         .unwrap();
    // }

    res
}

fn negamax(state: Board, depth: u32) -> i32 {
    // Reached the end of the depth; return board's evaluation.
    if depth == 0 {
        return eval(&state);
    }

    // Our goal is to maximize this number
    let mut max = i32::MAX;

    let moves = MoveGen::new_legal(&state);

    // If there are no moves available, it's a special case.
    if moves.len() == 0 {
        // eprintln!("No moves available");
        return eval(&state);
    }

    // For every move, recursively call this function on the new state created by
    // applying that move
    for mv in moves {
        let new_state = state.make_move_new(mv);
        let score = -negamax(new_state, depth - 1);

        // If the score is better, record it.
        max = score.max(max);
    }

    max
}

/*
fn negamax(
    state: Board,
    depth: u32,
    player: Color,
    mut alpha: i32,
    beta: i32,
    mut chessmove: ChessMove,
) -> SearchResult {
    if depth == 0 {
        return SearchResult::new(chessmove, eval(&state));
    }

    let moves = MoveGen::new_legal(&state);

    // If there are mo moves available, the player is in trouble
    if moves.len() == 0 {
        // If they're in check, that's bad
        if state.checkers().popcnt() == 0 {
            return SearchResult::new(chessmove, i32::MIN);
        } else {
            // Otherwise, we don't know
            return SearchResult::new(chessmove, 0);
        }
    }

    for mv in moves {
        let new_state = state.make_move_new(mv);
        let res = negamax(new_state, depth - 1, !player, -beta, -alpha, mv);

        if res.value >= beta {
            return res;
        }

        if res.value >= alpha {
            chessmove = mv;
            alpha = res.value;
        }
    }

    SearchResult::new(chessmove, alpha)
}
 */

/*
pub trait Search {
    fn search(&mut self, state: GameState, depth: u32, max_depth: u32) -> SearchResult;
}

struct RandomMove;
impl Search for RandomMove {
    fn search(&mut self, state: GameState, _depth: u32, _max_depth: u32) -> SearchResult {
        let mut res = SearchResult::default();
        res.bestmove = state.legal_moves().choose(&mut thread_rng()).unwrap();
        res
    }
}
 */
