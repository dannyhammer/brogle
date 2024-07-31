use std::{ops::Neg, time::Instant};

use dutchess_core::{Move, Position};
use rand::{seq::SliceRandom, thread_rng};

use crate::uci::SearchOptions;
use crate::{value_of, Evaluator};

const INITIAL_SCORE: i32 = -32_000;

#[derive(Debug, Clone, Copy)]
pub struct SearchResult {
    pub bestmove: Option<Move>,
    pub score: i32,
    pub nodes_searched: usize,
    pub ponder: Option<Move>,
}

impl Default for SearchResult {
    fn default() -> Self {
        Self {
            bestmove: None,
            ponder: None,
            score: INITIAL_SCORE,
            nodes_searched: 1, // By default, we're searching the root node
        }
    }
}

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

impl Neg for SearchResult {
    type Output = Self;
    fn neg(mut self) -> Self::Output {
        self.score = -self.score;
        self
    }
}

pub struct Search<'a> {
    position: Position,
    depth_to_search: u32,
    options: SearchOptions<'a>,
    result: SearchResult,
}

impl<'a> Search<'a> {
    pub fn new(position: Position, depth_to_search: u32) -> Self {
        Self {
            position,
            depth_to_search,
            options: SearchOptions::default(),
            result: SearchResult::default(),
        }
    }

    pub fn with_ponder(mut self, ponder: Move) -> Self {
        self.result.ponder = Some(ponder);
        self
    }

    pub fn with_options(mut self, options: SearchOptions<'a>) -> Self {
        self.options = options;
        self
    }

    pub fn result(&self) -> SearchResult {
        self.result
    }

    pub fn start(mut self) -> SearchResult {
        let now = Instant::now();
        self.result = self.search(self.position, self.depth_to_search);
        let elapsed = now.elapsed();

        let nps = self.result.nodes_searched as f64 / elapsed.as_secs_f64();
        eprintln!(
            "Searched {} nodes in {elapsed:?} ({nps:.2} n/s)",
            self.result.nodes_searched
        );
        self.result
    }

    fn order_moves(&self, position: &Position, moves: &mut [Move]) {
        let board = position.bitboards();
        let opponent = position.current_player();
        let attacks = position.attacks_by(opponent);

        moves.sort_by_cached_key(|mv| {
            if let Some(ponder) = self.result().ponder {
                if *mv == ponder {
                    return i32::MIN;
                }
            }
            let mut score = 0;
            let kind = board.kind_at(mv.from()).unwrap();

            // Capturing a high-value piece with a low-value piece is a good idea
            if let Some(captured) = board.kind_at(mv.to()) {
                score += 10 * value_of(captured) - value_of(kind);
            }

            // Promoting is also a good idea
            if let Some(promotion) = mv.promotion() {
                score += value_of(promotion);
            }

            // Going somewhere attacked by an opponent is not a good idea
            if attacks.get(mv.to()) {
                score -= value_of(kind);
            }

            -score // We're sorting, so a lower number is better
        })
    }

    fn search(&mut self, position: Position, depth: u32) -> SearchResult {
        // Start with a default (very bad) result.
        let mut alpha = INITIAL_SCORE;
        let beta = -INITIAL_SCORE;

        // Reached the end of the depth; return board's evaluation.
        if depth == 0 {
            // Root nodes in negamax must be evaluated from the current player's perspective
            self.result.score = Evaluator::new(&position).eval();
            self.result.nodes_searched += 1;
            return self.result;
        }

        let mut cloned = position.clone();
        let moves = cloned.legal_moves_mut();
        self.order_moves(&position, moves);
        // println!("MOVES: {moves:?}");

        for mv in moves.iter() {
            // Make the current move on the position, getting a new position in return
            let new_pos = position.with_move_made(*mv);

            // Recursively search our opponent's responses
            let current = -self.negamax(new_pos, depth - 1, -beta, -alpha);
            self.result.nodes_searched += 1;

            // Fail soft beta-cutoff;
            if current >= beta {
                self.result.score = beta;
                self.result.bestmove = Some(*mv);
                return self.result;
            }

            // If we've found a better move than our current best, update our result
            if current > self.result.score {
                self.result.score = current;
                self.result.bestmove = Some(*mv);
            }

            // Keep increasing alpha
            alpha = alpha.max(current);

            // Opponent would never choose this branch, so we can prune
            if alpha >= beta {
                break;
            }
        }

        // Handle cases for checkmate and stalemate
        if self.result.bestmove.is_none() {
            if moves.is_empty() {
                // eprintln!("No legal moves available at: {position}\nRes: {best:?}");
                if position.is_check() {
                    self.result.score = i32::MIN + depth as i32; // Prefer earlier checkmates
                } else {
                    self.result.score = 0;
                }
            } else {
                let random = moves.choose(&mut thread_rng()).map(|m| *m);
                // eprintln!("No best move found. Choosing randomly: {random:?}");
                self.result.bestmove = random;
            }
        }

        self.result
    }

    fn negamax(&mut self, position: Position, depth: u32, mut alpha: i32, beta: i32) -> i32 {
        // Start with a default (very bad) result.
        let mut best = INITIAL_SCORE;

        // Reached the end of the depth; start a qsearch for captures only
        if depth == 0 {
            return self.quiescence(position, alpha, beta);
        }

        let mut cloned = position.clone();
        let moves = cloned.legal_moves_mut();
        if moves.is_empty() {
            if position.is_check() {
                return i32::MIN + depth as i32; // Prefer earlier checks
            } else {
                return 0;
            }
        }

        self.order_moves(&position, moves);

        for mv in moves.iter() {
            // Make the current move on the position, getting a new position in return
            let new_pos = position.with_move_made(*mv);

            // Recursively search our opponent's responses
            let current = -self.negamax(new_pos, depth - 1, -beta, -alpha);
            self.result.nodes_searched += 1;

            // Fail soft beta-cutoff;
            if current >= beta {
                return beta;
            }

            // If we've found a better move than our current best, update our result
            best = best.max(current);

            // Update alpha.
            alpha = alpha.max(current);

            // Opponent would never choose this branch, so we can prune
            if alpha >= beta {
                // eprintln!("{alpha} >= {beta}: Pruning branch {mv}");
                break;
            }
        }

        best
    }

    fn quiescence(&mut self, position: Position, mut alpha: i32, beta: i32) -> i32 {
        // Root nodes in negamax must be evaluated from the current player's perspective
        let stand_pat = Evaluator::new(&position).eval();
        if stand_pat >= beta {
            return beta;
        } else if stand_pat > alpha {
            alpha = stand_pat;
        }

        let mut cloned = position.clone();
        let moves = cloned.legal_moves_mut();
        // Handle cases for checkmate and stalemate
        if moves.is_empty() {
            if position.is_check() {
                return i32::MIN;
            } else {
                return 0;
            }
        }

        self.order_moves(&position, moves);
        // println!("MOVES: {moves:?}");

        // Only search captures
        for mv in moves.iter().filter(|mv| mv.is_capture()) {
            // Make the current move on the position, getting a new position in return
            let new_pos = position.with_move_made(*mv);
            self.result.nodes_searched += 1;

            // Recursively search our opponent's responses
            let current = -self.quiescence(new_pos, -beta, -alpha);
            // eprintln!("Evaluating {mv} to have score {}", new_res.score);

            // Fail soft beta-cutoff;
            if current >= beta {
                return beta;
            }

            // Update alpha.
            alpha = alpha.max(current);

            // Opponent would never choose this branch, so we can prune
            if alpha >= beta {
                break;
            }
        }

        alpha
    }
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
