use std::{ops::Neg, time::Instant};

use arrayvec::ArrayVec;
use dutchess_core::{Game, Move, MAX_NUM_MOVES};
use rand::{prelude::SliceRandom, thread_rng};

use crate::uci::SearchOptions;
use crate::{value_of, Evaluator};

const INITIAL_SCORE: i32 = -32_000;

#[derive(Debug, Clone)]
pub struct SearchResult {
    pub bestmove: Option<Move>,
    pub score: i32,
    pub nodes_searched: usize,
    pub ponder: Option<Move>,
    pub pv: Vec<Move>,
}

impl Default for SearchResult {
    fn default() -> Self {
        Self {
            bestmove: None,
            ponder: None,
            score: INITIAL_SCORE,
            nodes_searched: 1,          // By default, we're searching the root node
            pv: Vec::with_capacity(16), // Allocate enough space for a search of depth 16
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
    game: Game,
    depth_to_search: u32,
    options: SearchOptions<'a>,
    result: SearchResult,
}

impl<'a> Search<'a> {
    pub fn new(game: Game, depth_to_search: u32) -> Self {
        Self {
            game,
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

    pub fn result(&self) -> &SearchResult {
        &self.result
    }

    pub fn start(mut self) -> SearchResult {
        // eprintln!(
        //     "Starting search of depth {} on {}",
        //     self.depth_to_search,
        //     self.game.fen()
        // );
        let now = Instant::now();
        self.search(self.game.clone(), self.depth_to_search);
        let elapsed = now.elapsed();

        let nps = self.result.nodes_searched as f64 / elapsed.as_secs_f64();
        _ = nps;
        // eprintln!(
        //     "Searched {} nodes in {elapsed:?} ({nps:.2} n/s): Score {:?}",
        //     self.result.nodes_searched, self.result
        // );
        self.result
    }

    fn score_move(&self, game: &Game, mv: &Move) -> i32 {
        if let Some(ponder) = self.result().ponder {
            if *mv == ponder {
                return i32::MIN;
            }
        }

        let mut score = 0;
        let kind = game.kind_at(mv.from()).unwrap();

        // Capturing a high-value piece with a low-value piece is a good idea
        // TODO: Refactor this into its own function and verify that its values are good: https://discord.com/channels/719576389245993010/719576389690589244/1268914745298391071
        if let Some(captured) = game.kind_at(mv.to()) {
            score += 10 * value_of(captured) - value_of(kind);
        }

        // Promoting is also a good idea
        if let Some(promotion) = mv.promotion() {
            score += value_of(promotion);
        }

        // Going somewhere attacked by an opponent is not a good idea
        let attacks = game.attacks_by_color(game.current_player().opponent());
        if attacks.get(mv.to()) {
            score -= value_of(kind);
        }

        -score // We're sorting, so a lower number is better
    }

    fn search(&mut self, game: Game, depth: u32) {
        // Start with a default (very bad) result.
        let mut alpha = INITIAL_SCORE;
        let beta = -INITIAL_SCORE;

        // Reached the end of the depth; return board's evaluation.
        if depth == 0 {
            // Root nodes in negamax must be evaluated from the current player's perspective
            self.result.score = Evaluator::new(game.position()).eval();
            self.result.nodes_searched += 1;
            return;
        }

        self.game = game.clone();
        let mut moves = ArrayVec::<Move, MAX_NUM_MOVES>::new();
        game.compute_legal_moves(&mut moves);
        moves.sort_by_cached_key(|mv| self.score_move(&game, mv));

        // println!("MOVES: {moves:?}");

        for i in 0..moves.len() {
            let mv = moves[i];
            // Make the current move on the position, getting a new position in return
            let new_pos = game.with_move_made(mv);

            // Recursively search our opponent's responses
            let current = -self.negamax(new_pos, depth - 1, -beta, -alpha);
            self.result.nodes_searched += 1;

            // Fail soft beta-cutoff;
            if current >= beta {
                self.result.score = beta;
                self.result.bestmove = Some(mv);
                self.result.pv.push(mv);
                return;
            }

            // If we've found a better move than our current best, update our result
            if current > self.result.score {
                self.result.score = current;
                self.result.bestmove = Some(mv);
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
                if game.is_in_check() {
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
    }

    fn negamax(&mut self, game: Game, depth: u32, mut alpha: i32, beta: i32) -> i32 {
        // Start with a default (very bad) result.
        let mut best = INITIAL_SCORE;

        // println!("DEPTH");
        // Reached the end of the depth; start a qsearch for captures only
        if depth == 0 {
            return self.quiescence(game, alpha, beta);
        }

        let mut moves = ArrayVec::<Move, MAX_NUM_MOVES>::new();
        game.compute_legal_moves(&mut moves);
        if moves.len() == 0 {
            if game.is_in_check() {
                return i32::MIN + depth as i32; // Prefer earlier checks
            } else {
                return 0;
            }
        }

        moves.sort_by_cached_key(|mv| self.score_move(&game, mv));

        // println!("MOVES: {moves:?}");

        for i in 0..moves.len() {
            let mv = moves[i];
            // Make the current move on the position, getting a new position in return
            let new_pos = game.with_move_made(mv);

            // Recursively search our opponent's responses
            let current = -self.negamax(new_pos, depth - 1, -beta, -alpha);
            self.result.nodes_searched += 1;

            // Fail soft beta-cutoff;
            if current >= beta {
                // self.result.pv.push(*mv);
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

    fn quiescence(&mut self, game: Game, mut alpha: i32, beta: i32) -> i32 {
        // eprintln!("QSearch on {}", game.fen());
        // Root nodes in negamax must be evaluated from the current player's perspective
        let stand_pat = Evaluator::new(game.position()).eval();
        if stand_pat >= beta {
            return beta;
        } else if stand_pat > alpha {
            alpha = stand_pat;
        }

        let mut moves = ArrayVec::<Move, MAX_NUM_MOVES>::new();
        game.compute_legal_moves(&mut moves);

        // Handle cases for checkmate and stalemate
        if moves.len() == 0 {
            if game.is_in_check() {
                return i32::MIN;
            } else {
                return 0;
            }
        }

        moves.sort_by_cached_key(|mv| self.score_move(&game, mv));

        // println!("MOVES: {moves:?}");

        // Only search captures
        for i in 0..moves.len() {
            // for mv in moves.iter().filter(|mv| mv.is_capture()) {
            let mv = moves[i];
            if !mv.is_capture() {
                continue;
            }
            // Make the current move on the position, getting a new position in return
            let new_pos = game.with_move_made(mv);
            self.result.nodes_searched += 1;

            // Recursively search our opponent's responses
            let current = -self.quiescence(new_pos, -beta, -alpha);
            // eprintln!("Evaluating {mv} to have score {current}");

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
fn search(mut game: Game, depth: u32) -> SearchResult {
    // Start with a default (very bad) result.
    let mut alpha = INITIAL_SCORE;
    let beta = -INITIAL_SCORE;
    let mut result = SearchResult::default();

    // Reached the end of the depth; return board's evaluation.
    if depth == 0 {
        // Root nodes in negamax must be evaluated from the current player's perspective
        result.score = Evaluator::new(game.position()).eval();
        result.nodes_searched += 1;
        return result;
    }

    // self.game = game.clone();
    // game.movegen_mut().order_moves(|mv| self.score_move(mv));
    let moves = game.legal_moves();
    // println!("MOVES: {moves:?}");

    for mv in moves.iter() {
        // Make the current move on the position, getting a new position in return
        let new_pos = game.with_move_made(*mv);

        // Recursively search our opponent's responses
        let current = -negamax(new_pos, depth - 1, -beta, -alpha);
        result.nodes_searched += 1;

        // Fail soft beta-cutoff;
        if current >= beta {
            result.score = beta;
            result.bestmove = Some(*mv);
            result.pv.push(*mv);
            return result;
        }

        // If we've found a better move than our current best, update our result
        if current > result.score {
            result.score = current;
            result.bestmove = Some(*mv);
        }

        // Keep increasing alpha
        alpha = alpha.max(current);

        // Opponent would never choose this branch, so we can prune
        if alpha >= beta {
            break;
        }
    }

    // Handle cases for checkmate and stalemate
    if result.bestmove.is_none() {
        if moves.is_empty() {
            // eprintln!("No legal moves available at: {position}\nRes: {best:?}");
            if game.is_in_check() {
                result.score = i32::MIN + depth as i32; // Prefer earlier checkmates
            } else {
                result.score = 0;
            }
        } else {
            let random = moves.choose(&mut thread_rng()).map(|m| *m);
            // eprintln!("No best move found. Choosing randomly: {random:?}");
            result.bestmove = random;
        }
    }
    return result;
}

fn negamax(mut game: Game, depth: u32, mut alpha: i32, beta: i32) -> i32 {
    // Start with a default (very bad) result.
    let mut best = INITIAL_SCORE;

    // println!("DEPTH");
    // Reached the end of the depth; start a qsearch for captures only
    if depth == 0 {
        return quiescence(game, alpha, beta);
    }

    if game.num_legal_moves() == 0 {
        if game.is_in_check() {
            return i32::MIN + depth as i32; // Prefer earlier checks
        } else {
            return 0;
        }
    }

    // self.game = game.clone();
    // game.movegen_mut().order_moves(|mv| self.score_move(mv));
    let moves = game.legal_moves();

    for mv in moves.iter() {
        // Make the current move on the position, getting a new position in return
        let new_pos = game.with_move_made(*mv);

        // Recursively search our opponent's responses
        let current = -negamax(new_pos, depth - 1, -beta, -alpha);
        // self.result.nodes_searched += 1;

        // Fail soft beta-cutoff;
        if current >= beta {
            // self.result.pv.push(*mv);
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

fn quiescence(mut game: Game, mut alpha: i32, beta: i32) -> i32 {
    // eprintln!("QSearch on {}", game.fen());
    // Root nodes in negamax must be evaluated from the current player's perspective
    let stand_pat = Evaluator::new(game.position()).eval();
    if stand_pat >= beta {
        return beta;
    } else if stand_pat > alpha {
        alpha = stand_pat;
    }

    // Handle cases for checkmate and stalemate
    if game.num_legal_moves() == 0 {
        if game.is_in_check() {
            return i32::MIN;
        } else {
            return 0;
        }
    }

    let cloned = game.clone();
    game.movegen_mut().order_moves(|mv| score_move(&cloned, mv));
    let moves = game.legal_moves();

    // println!("MOVES: {moves:?}");

    // Only search captures
    for mv in moves.iter().filter(|mv| mv.is_capture()) {
        // Make the current move on the position, getting a new position in return
        let new_pos = game.with_move_made(*mv);
        // self.result.nodes_searched += 1;

        // Recursively search our opponent's responses
        let current = -quiescence(new_pos, -beta, -alpha);
        // eprintln!("Evaluating {mv} to have score {current}");

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

fn score_move(game: &Game, mv: &Move) -> i32 {
    // if let Some(ponder) = self.result().ponder {
    //     if *mv == ponder {
    //         return i32::MIN;
    //     }
    // }

    let mut score = 0;
    let kind = game.kind_at(mv.from()).unwrap();

    // Capturing a high-value piece with a low-value piece is a good idea
    // TODO: Refactor this into its own function and verify that its values are good: https://discord.com/channels/719576389245993010/719576389690589244/1268914745298391071
    if let Some(captured) = game.kind_at(mv.to()) {
        score += 10 * value_of(captured) - value_of(kind);
    }

    // Promoting is also a good idea
    if let Some(promotion) = mv.promotion() {
        score += value_of(promotion);
    }

    // Going somewhere attacked by an opponent is not a good idea
    let attacks = game.attacks_by_color(game.current_player().opponent());
    if attacks.get(mv.to()) {
        score -= value_of(kind);
    }

    -score // We're sorting, so a lower number is better
}
 */
