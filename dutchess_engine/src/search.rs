use dutchess_core::{Move, Position};

use rand::prelude::*;

use crate::eval;

#[derive(Debug, Clone, Default)]
pub struct SearchResult {
    pub bestmove: Move,
    pub value: i32,
}

impl SearchResult {
    fn new(bestmove: Move, value: i32) -> Self {
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

pub fn search(state: &Position, depth: u32) -> SearchResult {
    // Start with a default result, with a default (probably illegal) move.
    let mut res = SearchResult::new(Move::default(), i32::MIN);

    if depth == 0 {
        return res;
    }

    let root_moves = state.legal_moves();

    // For every available move, make that move on a new board
    for root_move in root_moves {
        let mut new_state = state.clone();
        new_state.make_move(*root_move);

        // Call negamax to check the move's score.
        let score = -negamax(&new_state, depth - 1);

        // If the score is better than our current best, record it.
        if score > res.value {
            res.value = score;
            res.bestmove = *root_move;
        }
    }

    if res.value == i32::MIN {
        res.bestmove = *root_moves.choose(&mut thread_rng()).unwrap();
    }

    res
}

fn negamax(state: &Position, depth: u32) -> i32 {
    // Reached the end of the depth; return board's evaluation.
    if depth == 0 {
        return eval(state.bitboards());
    }

    // Our goal is to maximize this number
    let mut max = i32::MAX;

    // let moves = MoveGen::new_legal(&state);
    let moves = state.legal_moves();

    // If there are no moves available, it's a special case.
    if moves.len() == 0 {
        // eprintln!("No moves available");
        return eval(state.bitboards());
    }

    // For every move, recursively call this function on the new state created by
    // applying that move
    for mv in moves {
        let mut new_state = state.clone();
        new_state.make_move(*mv);

        let score = -negamax(&new_state, depth - 1);

        // If the score is better, record it.
        max = score.max(max);
    }

    max
}

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
