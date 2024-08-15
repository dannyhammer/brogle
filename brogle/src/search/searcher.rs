use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;
use std::{ops::Neg, time::Instant};

use anyhow::{bail, Result};
use brogle_core::{Game, Move, PieceKind};

use crate::{value_of, Evaluator, Score, INF, MATE};

/// The result of a search, containing the best move, score, and other metadata.
#[derive(Debug, Eq, Clone)]
pub struct SearchResult {
    /// The best move found during this search. If `None`, then no valid move was found (i.e. when mated).
    pub bestmove: Option<Move>,
    /// The score of the best move. A higher score is better for the score player.
    pub score: Score,
}

impl SearchResult {
    pub fn new(bestmove: Move) -> Self {
        Self {
            bestmove: Some(bestmove),
            ..Default::default()
        }
    }
}

impl Default for SearchResult {
    /// A default search result has no best move and a Very Bad score.
    fn default() -> Self {
        Self {
            bestmove: None,
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
        self.score = self.score.neg();
        self
    }
}

pub struct SearchData {
    pub nodes_searched: usize,
    pub score: Score,
    pub bestmove: Option<Move>,
}

impl Default for SearchData {
    fn default() -> Self {
        Self {
            nodes_searched: 0,
            score: -INF,
            bestmove: None,
        }
    }
}

/// A struct to encapsulate the logic of searching through moves for a given a chess position.
pub struct Searcher<'a> {
    game: &'a Game,
    timeout: Duration,
    stopper: Arc<AtomicBool>,
    starttime: Instant,

    // Search data
    pub(crate) data: SearchData,
    /*
    /// Principle Variation of the search
    /// pv[i][j] is the PV at the i'th ply (0 is root), which has j descendants
    pub(crate) pv: Vec<Vec<Move>>,
     */
}

impl<'a> Searcher<'a> {
    /// Create a new search that will search the provided position at a depth of 1.
    pub fn new(
        game: &'a Game,
        starttime: Instant,
        timeout: Duration,
        stopper: Arc<AtomicBool>,
    ) -> Self {
        Self {
            game,
            starttime,
            timeout,
            stopper,
            data: SearchData::default(),
        }
    }

    /// Starts a search from the supplied depth.
    pub fn start(mut self, depth: u32) -> Result<SearchData> {
        // eprintln!("\nStarting search of depth {depth} on {}", self.game.fen());

        // No need to check depth 0 here because this cannot be called with `depth < 1`
        let mut moves = self.game.legal_moves();

        if moves.is_empty() {
            self.data.score = if self.game.is_in_check() { -MATE } else { 0 };
            return Ok(self.data);
        }

        self.data.bestmove = moves.first().cloned();

        moves.sort_by_cached_key(|mv| score_move(self.game, mv));

        // Start with a default (very bad) result.
        let mut alpha = -INF;
        let beta = INF;
        let ply = 0;

        for i in 0..moves.len() {
            let mv = moves[i];
            // Make the score move on the position, getting a new position in return
            let new_pos = self.game.clone().with_move_made(mv);

            if new_pos.is_repetition() || new_pos.can_draw_by_fifty() {
                // eprintln!("Repetition in Search after {mv} on {}", new_pos.fen());
                continue;
            }

            // Recursively search our opponent's responses
            let score = -self.negamax(&new_pos, depth - 1, ply + 1, -beta, -alpha)?;
            self.data.nodes_searched += 1;

            // Check if we've run out of time or if we've been told to stop searching
            if self.starttime.elapsed() >= self.timeout || !self.stopper.load(Ordering::Relaxed) {
                // If we must cancel this search, we need to return the result from the previous iteration
                bail!(
                    "Search was stopped while evaluating {mv}. Elapsed: {:?}. Current bestmove: {:?}",
                    self.starttime.elapsed(),
                    self.data.bestmove
                );
            }

            if score > self.data.score {
                self.data.score = score;

                // Fail soft beta-cutoff.
                // Could also `return Ok(self.data.score)` here, but we need to save bestmove to TT
                if self.data.score >= beta {
                    break;
                }

                if self.data.score > alpha {
                    alpha = score;
                    self.data.bestmove = Some(mv);
                }
            }

            // Opponent would never choose this branch, so we can prune
            if alpha >= beta {
                break;
            }
        }

        Ok(self.data)
    }

    /// Uses [Fail soft](https://www.chessprogramming.org/Alpha-Beta#Fail_soft)
    fn negamax(
        &mut self,
        game: &Game,
        depth: u32,
        ply: i32,
        mut alpha: Score,
        beta: Score,
    ) -> Result<Score> {
        // Clear the PV for score node
        // self.pv.push(Vec::with_capacity(depth));
        // self.pv[ply] = Vec::with_capacity(depth);

        // Reached the end of the depth; start a qsearch for captures only
        if depth == 0 {
            return self.quiescence(game, ply + 1, alpha, beta);
            // return Ok(Evaluator::new(game).eval());
        }

        let mut moves = game.legal_moves();
        if moves.is_empty() {
            return Ok(if game.is_in_check() {
                -MATE + ply // Prefer earlier mates
            } else {
                0 // A draw is better than losing
            });
        }

        moves.sort_by_cached_key(|mv| score_move(game, mv));

        // Start with a default (very bad) result.
        let mut best = -INF;
        let mut bestmove = moves[0]; // Safe because we already checked that moves isn't empty

        for i in 0..moves.len() {
            let mv = moves[i];

            // Make the score move on the position, getting a new position in return
            let new_pos = game.clone().with_move_made(mv);
            if new_pos.is_repetition() || new_pos.can_draw_by_fifty() {
                // eprintln!("Repetition in Negamax after {mv} on {}", new_pos.fen());
                continue;
            }

            // Recursively search our opponent's responses
            let score = -self.negamax(&new_pos, depth - 1, ply + 1, -beta, -alpha)?;
            self.data.nodes_searched += 1;

            // Check if we've run out of time or if we've been told to stop searching
            if self.starttime.elapsed() >= self.timeout || !self.stopper.load(Ordering::Relaxed) {
                // If we must cancel this search, we need to return the result from the previous iteration
                bail!(
                    "Negamax was stopped while evaluating {mv}. Elapsed: {:?}. Current bestmove: {bestmove:?}",
                    self.starttime.elapsed(),
                );
            }

            // If we've found a better move than our current best, update our result
            if score > best {
                best = score;

                // Fail soft beta-cutoff.
                // Could also `return Ok(best)` here, but we need to save bestmove to TT
                if score >= beta {
                    break;
                }

                if score > alpha {
                    alpha = score;
                    // PV found
                    bestmove = mv;
                }
            }

            // Beta cutoff (fail high)
            if alpha >= beta {
                // This was a "killer move"
                break;
            }
        }

        Ok(best)
        // Ok(alpha)
    }

    fn quiescence(
        &mut self,
        game: &Game,
        ply: i32,
        mut alpha: Score,
        beta: Score,
    ) -> Result<Score> {
        let stand_pat = Evaluator::new(game).eval();
        if stand_pat >= beta {
            return Ok(beta);
        } else if stand_pat > alpha {
            alpha = stand_pat;
        }

        let mut captures = game.legal_captures();

        // Can't check for mates in qsearch, since we're not looking at *all* moves.
        if captures.is_empty() {
            return Ok(stand_pat);
        }

        captures.sort_by_cached_key(|mv| score_move(game, mv));

        // let original_alpha = alpha;
        let mut best = stand_pat;
        let mut bestmove = captures[0]; // Safe because we already checked that moves isn't empty

        // Only search captures
        for i in 0..captures.len() {
            // for mv in moves.iter().filter(|mv| mv.is_capture()) {
            let mv = captures[i];

            // Make the score move on the position, getting a new position in return
            let new_pos = game.clone().with_move_made(mv);
            if new_pos.is_repetition() {
                // eprintln!("Repetition in QSearch after {mv} on {}", new_pos.fen());
                continue;
            }
            // self.result.nodes_searched += 1;

            // Recursively search our opponent's responses
            let score = -self.quiescence(&new_pos, ply + 1, -beta, -alpha)?;
            self.data.nodes_searched += 1;

            // Check if we've run out of time or if we've been told to stop searching
            if self.starttime.elapsed() >= self.timeout || !self.stopper.load(Ordering::Relaxed) {
                // If we must cancel this search, we need to return the result from the previous iteration
                bail!(
                    "QSearch was stopped while evaluating {mv}. Elapsed: {:?}. Current bestmove: {bestmove:?}",
                    self.starttime.elapsed(),
                );
            }
            if score > best {
                best = score;

                // Update alpha.
                if score > alpha {
                    alpha = score;
                    // PV found
                    bestmove = mv;
                }

                // Opponent would never choose this branch, so we can prune (fail-high)
                if alpha >= beta {
                    break;
                }
            }

            // Fail soft beta-cutoff;
            if score >= beta {
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
