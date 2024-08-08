use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};
use std::time::Duration;
use std::{ops::Neg, time::Instant};

use anyhow::{bail, Result};
use dutchess_core::{Game, Move, PieceKind};

use crate::uci::{UciInfo, UciSearchOptions};
use crate::{value_of, Evaluator};

const INF: i32 = 32_000;

/// The result of a search, containing the best move, score, and other metadata.
#[derive(Debug, Clone)]
pub struct SearchResult {
    /// The best move found during this search. If `None`, then no valid move was found (i.e. when mated).
    pub bestmove: Option<Move>,
    /// The score of the best move. A higher score is better for the current player.
    pub score: i32,
    /// If supplied, this move is checked *first*, before all others.
    pub ponder: Option<Move>,
}

impl Default for SearchResult {
    /// A default search result has no best move and a Very Bad score.
    fn default() -> Self {
        Self {
            bestmove: None,
            ponder: None,
            score: -INF, // Initially, our score is Very Bad
        }
    }
}

impl PartialEq for SearchResult {
    /// Search results are compared by their `score` fields.
    fn eq(&self, other: &Self) -> bool {
        self.score.eq(&other.score)
    }
}
impl Eq for SearchResult {}

impl PartialOrd for SearchResult {
    /// Search results are compared by their `score` fields.
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.score.partial_cmp(&other.score)
    }
}

impl Neg for SearchResult {
    type Output = Self;
    /// Negating a search result just negates its score.
    fn neg(mut self) -> Self::Output {
        self.score = -self.score;
        self
    }
}

/// A struct to encapsulate the logic of searching through moves for a given a chess position.
pub struct Search<'a> {
    game: &'a Game,
    timeout: Duration,
    stopper: Arc<AtomicBool>,
    options: UciSearchOptions,

    // Search data
    pub(crate) result: Arc<RwLock<SearchResult>>,
    pub(crate) starttime: Instant,
    pub(crate) nodes_searched: usize,
    pub(crate) info: Arc<RwLock<UciInfo>>,
    /*
    /// Principle Variation of the search
    /// pv[i][j] is the PV at the i'th ply (0 is root), which has j descendants
    pub(crate) pv: Vec<Vec<Move>>,
     */
}

impl<'a> Search<'a> {
    /// Create a new search that will search the provided position at a depth of 1.
    pub fn new(
        game: &'a Game,
        timeout: Duration,
        stopper: Arc<AtomicBool>,
        result: Arc<RwLock<SearchResult>>,
        info: Arc<RwLock<UciInfo>>,
    ) -> Self {
        Self {
            game,
            timeout,
            stopper,
            result,
            info,
            options: UciSearchOptions::default(),
            starttime: Instant::now(),
            nodes_searched: 0,
            // pv: Vec::default(),
        }
    }

    pub fn with_options(mut self, options: UciSearchOptions) -> Self {
        self.options = options;
        self
    }

    /// Starts a search from the supplied depth.
    // pub fn start(&mut self, depth: usize) -> Result<SearchResult> {
    pub fn start(&mut self, depth: usize) -> Result<()> {
        // eprintln!("\nStarting search of depth {depth} on {}", self.game.fen());

        self.starttime = Instant::now();
        // Populate the PV list
        // self.pv = Vec::with_capacity(depth);
        // self.pv = vec![Vec::with_capacity(depth); depth];
        let result = self.search(depth)?;

        // If the search timed out, this result is garbage, so don't return it.
        *self.result.write().unwrap() = result.clone();
        let elapsed = self.starttime.elapsed();

        // let pv = self.pv[0].clone();

        let mut lock = self.info.write().unwrap();
        let mut info = lock.clone();
        info = info
            .depth(depth)
            .score(result.score)
            .nodes(self.nodes_searched)
            .nps((self.nodes_searched as f64 / elapsed.as_secs_f64()) as usize)
            .time(elapsed.as_millis())
            // .pv(pv)
            ;

        *lock = info;

        // let nps = self.nodes_searched as f64 / elapsed.as_secs_f64();
        // eprintln!(
        //     "\nSearched {} nodes in {:?} ({nps:.2} n/s): {result:?}",
        //     self.nodes_searched, elapsed
        // );

        // Ok(result)
        Ok(())
    }

    // pub fn stop(&mut self) {
    //     self.stopper.store(false, Ordering::Relaxed);
    // }

    fn search(&mut self, depth: usize) -> Result<SearchResult> {
        // Start with a default (very bad) result.
        let mut result = SearchResult::default();
        let mut alpha = -INF;
        let beta = INF;
        let ply = 0;

        // Clear the PV for current node
        // self.pv.push(Vec::with_capacity(depth));
        // self.pv[ply] = Vec::with_capacity(depth);

        // Reached the end of the depth; return board's evaluation.
        if depth == 0 {
            // Root nodes in negamax must be evaluated from the current player's perspective
            result.score = Evaluator::new(&self.game).eval();
            self.nodes_searched += 1;
            return Ok(result);
            // return self.quiescence(game, ply + 1, alpha, beta);
        }

        let mut moves = self.game.legal_moves();
        moves.sort_by_cached_key(|mv| score_move(&self.game, mv));

        // println!("MOVES: {moves:?}");

        for i in 0..moves.len() {
            let mv = moves[i];
            // Make the current move on the position, getting a new position in return
            let new_pos = self.game.with_move_made(mv);

            if new_pos.is_repetition() || new_pos.can_draw_by_fifty() {
                // eprintln!("Repetition in Search after {mv} on {}", new_pos.fen());
                continue;
            }

            // Recursively search our opponent's responses
            let current = -self.negamax(&new_pos, depth - 1, ply + 1, -beta, -alpha)?;
            self.nodes_searched += 1;

            // Check if we've run out of time or if we've been told to stop searching
            if self.starttime.elapsed() >= self.timeout || !self.stopper.load(Ordering::Relaxed) {
                // If we must cancel this search, we need to return the result from the previous iteration
                bail!(
                    "Search was stopped while evaluating {mv}. Current bestmove: {:?}",
                    result.bestmove
                );
            }

            // Fail soft beta-cutoff;
            if current >= beta {
                // eprintln!("Search: current >= beta for {mv} at ply {ply}");
                result.score = beta;
                result.bestmove = Some(mv);
                return Ok(result);
            }

            // If we've found a better move than our current best, update our result
            if current > result.score {
                // eprintln!("Search: current > best for {mv} at ply {ply}");
                result.score = current;
                result.bestmove = Some(mv);
            }

            // Keep increasing alpha
            // alpha = alpha.max(current);
            if current > alpha {
                alpha = current;
                // self.pv[ply] = Vec::with_capacity(depth + 1);
                // self.pv[ply].push(mv);
                // let children_pvs = self.pv[ply + 1].clone();
                // self.pv[ply].extend(children_pvs);
            }

            // Opponent would never choose this branch, so we can prune
            if alpha >= beta {
                break;
            }
        }

        // Handle cases for checkmate and stalemate
        if result.bestmove.is_none() {
            if moves.is_empty() {
                // eprintln!("No legal moves available at: {position}\nRes: {best:?}");
                if self.game.is_in_check() {
                    result.score = -INF + depth as i32; // Prefer earlier checkmates
                } else {
                    result.score = 0;
                }
            } else {
                result.bestmove = moves.first().copied();
            }
        }

        // eprintln!(
        //     "search: returning {} at ply {ply}",
        //     result.bestmove.unwrap(),
        // );
        // self.pv[ply] = result.bestmove.unwrap();
        Ok(result)
    }

    fn negamax(
        &mut self,
        game: &Game,
        depth: usize,
        ply: usize,
        mut alpha: i32,
        beta: i32,
    ) -> Result<i32> {
        // Start with a default (very bad) result.
        let mut best = -INF;

        // Clear the PV for current node
        // self.pv.push(Vec::with_capacity(depth));
        // self.pv[ply] = Vec::with_capacity(depth);

        // Reached the end of the depth; start a qsearch for captures only
        if depth == 0 {
            return self.quiescence(game, ply + 1, alpha, beta);
            // return Ok(Evaluator::new(game).eval());
        }

        let mut moves = game.legal_moves();
        if moves.len() == 0 {
            if game.is_in_check() {
                return Ok(-INF + ply as i32); // Prefer earlier checks
            } else {
                return Ok(0); // A draw is better than losing
            }
        }

        moves.sort_by_cached_key(|mv| score_move(&game, mv));

        // println!("MOVES: {moves:?}");

        for i in 0..moves.len() {
            let mv = moves[i];

            // Make the current move on the position, getting a new position in return
            let new_pos = game.with_move_made(mv);
            if new_pos.is_repetition() || new_pos.can_draw_by_fifty() {
                // eprintln!("Repetition in Negamax after {mv} on {}", new_pos.fen());
                continue;
            }

            // Recursively search our opponent's responses
            let current = -self.negamax(&new_pos, depth - 1, ply + 1, -beta, -alpha)?;
            self.nodes_searched += 1;

            // Check if we've run out of time or if we've been told to stop searching
            if self.starttime.elapsed() >= self.timeout || !self.stopper.load(Ordering::Relaxed) {
                // If we must cancel this search, we need to return the result from the previous iteration
                bail!("Negamax was stopped while evaluating {mv}. Current best score: {best}");
            }

            // Fail soft beta-cutoff;
            if current >= beta {
                return Ok(beta);
            }

            // If we've found a better move than our current best, update our result
            if current > best {
                best = current;
            }

            // Update alpha.
            if current > alpha {
                alpha = current;
                // self.pv[ply] = Vec::with_capacity(depth + 1);
                // self.pv[ply].push(mv);
                // let children_pvs = self.pv[ply + 1].clone();
                // self.pv[ply].extend(children_pvs);
            }

            // Opponent would never choose this branch, so we can prune
            if alpha >= beta {
                // eprintln!("{alpha} >= {beta}: Pruning branch {mv}");
                break;
            }
        }

        // eprintln!("Negamax: returning {best} at ply {ply}");
        Ok(best)
    }

    fn quiescence(&mut self, game: &Game, ply: usize, mut alpha: i32, beta: i32) -> Result<i32> {
        // Clear the PV for current node
        // self.pv.push(Vec::with_capacity(8));

        // eprintln!("QSearch on {}", game.fen());
        // Root nodes in negamax must be evaluated from the current player's perspective
        let stand_pat = Evaluator::new(&game).eval();
        if stand_pat >= beta {
            return Ok(beta);
        } else if stand_pat > alpha {
            alpha = stand_pat;
        }

        let mut moves = game.legal_moves();

        // Handle cases for checkmate and stalemate
        if moves.len() == 0 {
            if game.is_in_check() {
                return Ok(-INF + ply as i32);
            } else {
                return Ok(0);
            }
        }

        moves.sort_by_cached_key(|mv| score_move(&game, mv));

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
            if new_pos.is_repetition() {
                eprintln!("Repetition in QSearch after {mv} on {}", new_pos.fen());
                continue;
            }
            // self.result.nodes_searched += 1;

            // Recursively search our opponent's responses
            let current = -self.quiescence(&new_pos, ply + 1, -beta, -alpha)?;
            self.nodes_searched += 1;

            // Check if we've run out of time or if we've been told to stop searching
            if self.starttime.elapsed() >= self.timeout || !self.stopper.load(Ordering::Relaxed) {
                // If we must cancel this search, we need to return the result from the previous iteration
                bail!("QSearch was stopped while evaluating {mv}");
            }

            // Fail soft beta-cutoff;
            if current >= beta {
                return Ok(beta);
            }

            // Update alpha.
            if current > alpha {
                alpha = current;
                // self.pv[ply] = Vec::with_capacity(8); // seems like a good number
                // self.pv[ply].push(mv);
                // let children_pvs = self.pv[ply + 1].clone();
                // self.pv[ply].extend(children_pvs);
            }

            // Opponent would never choose this branch, so we can prune
            if alpha >= beta {
                break;
            }
        }

        Ok(alpha)
    }
}

// TODO: Refactor this into its own function and verify that its values are good: https://discord.com/channels/719576389245993010/719576389690589244/1268914745298391071
fn mvv_lva(kind: PieceKind, captured: PieceKind) -> i32 {
    10 * value_of(captured) - value_of(kind)
}

fn score_move(game: &Game, mv: &Move) -> i32 {
    // if let Some(ponder) = self.result.ponder.take() {
    //     if *mv == ponder {
    //         return i32::MIN;
    //     }
    // }

    let mut score = 0;
    let kind = game.kind_at(mv.from()).unwrap();

    // Capturing a high-value piece with a low-value piece is a good idea
    if let Some(captured) = game.kind_at(mv.to()) {
        score += mvv_lva(kind, captured);
    }

    // Promoting is also a good idea
    // if let Some(promotion) = mv.promotion() {
    //     score += value_of(promotion);
    // }

    // Going somewhere attacked by an opponent is not a good idea
    // let attacks = game.attacks_by_color(game.current_player().opponent());
    // if attacks.get(mv.to()) {
    //     score -= value_of(kind);
    // }

    -score // We're sorting, so a lower number is better
}
