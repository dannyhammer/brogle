use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use anyhow::{bail, Result};
use brogle_core::{Game, Move, PieceKind, ZobristKey};

use super::{NodeType, TTable, TTableEntry};
use crate::{value_of, Evaluator, INF, MATE};

pub struct SearchData {
    pub nodes_searched: usize,
    pub score: i32,
    pub bestmove: Option<Move>,
    /*
    /// Principle Variation of the search.
    ///
    /// pv[i][j] is the PV at the i'th ply (0 is root), which has j descendants
    ///
    /// For example, if you searched at depth 4, then 'i' would be at most 3.
    /// pv[0] would have depth+q entries, where 'q' is the number of plies searched in qsearch.
    pub(crate) pv: Vec<Vec<Move>>,
     */
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
    ttable: &'a mut TTable,
    starttime: Instant,
    timeout: Duration,
    stopper: Arc<AtomicBool>,

    // Search data
    data: SearchData,
}

impl<'a> Searcher<'a> {
    /// Create a new search that will search the provided position.
    pub fn new(
        game: &'a Game,
        ttable: &'a mut TTable,
        starttime: Instant,
        timeout: Duration,
        stopper: Arc<AtomicBool>,
    ) -> Self {
        Self {
            game,
            ttable,
            starttime,
            timeout,
            stopper,
            data: SearchData::default(),
        }
    }

    /// Start a search of depth `depth` at the root node.
    pub fn start(mut self, depth: u32) -> Result<SearchData> {
        let key = self.game.key();
        self.data.score = self.negamax(self.game, depth, 0, -INF, INF)?;
        self.data.bestmove = self.ttable.get(&key).map(|entry| entry.bestmove);

        Ok(self.data)
    }

    /// Uses [Fail soft](https://www.chessprogramming.org/Alpha-Beta#Fail_soft)
    fn negamax(
        &mut self,
        game: &Game,
        depth: u32,
        ply: i32,
        mut alpha: i32,
        beta: i32,
    ) -> Result<i32> {
        // Reached the end of the depth; start a qsearch for captures only
        if depth == 0 {
            return self.quiescence(game, ply + 1, alpha, beta);
            // return Ok(Evaluator::new(game).eval());
        }

        let mut moves = game.legal_moves();
        if moves.is_empty() {
            // Prefer earlier mates, and drawing is better than being mated.
            let score = if game.is_in_check() { -MATE + ply } else { 0 };
            return Ok(score);
        }

        let tt_bestmove = self.get_tt_bestmove(game.key());
        moves.sort_by_cached_key(|mv| score_move(game, mv, tt_bestmove));

        // Start with a default (very bad) result.
        let mut best = -INF;
        let mut bestmove = moves[0]; // Safe because we already checked that `moves` isn't empty
        let original_alpha = alpha;

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
                bail!("Negamax was cancelled while evaluating {mv}");
            }

            // If we've found a better move than our current best, update our result
            if score > best {
                best = score;

                if score > alpha {
                    alpha = score;
                    // PV found
                    bestmove = mv;
                }

                // Fail soft beta-cutoff.
                // Could also `return Ok(best)` here, but we need to save bestmove to TT
                if score >= beta {
                    break;
                }
            }
        }

        let flag = NodeType::new(best, original_alpha, beta);
        self.save_to_ttable(game.key(), bestmove, best, depth, flag);

        Ok(best) // fail-soft
    }

    fn quiescence(&mut self, game: &Game, ply: i32, mut alpha: i32, beta: i32) -> Result<i32> {
        let stand_pat = Evaluator::new(game).eval_current_player();
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

        let tt_bestmove = self.get_tt_bestmove(game.key());
        captures.sort_by_cached_key(|mv| score_move(game, mv, tt_bestmove));

        // let original_alpha = alpha;
        let mut best = stand_pat;
        let mut bestmove = captures[0]; // Safe because we already checked that `captures` isn't empty
        let original_alpha = alpha;

        // Only search captures
        for i in 0..captures.len() {
            let mv = captures[i];

            // Make the score move on the position, getting a new position in return
            let new_pos = game.clone().with_move_made(mv);

            // Recursively search our opponent's responses
            let score = -self.quiescence(&new_pos, ply + 1, -beta, -alpha)?;
            self.data.nodes_searched += 1;

            // Check if we've run out of time or if we've been told to stop searching
            if self.starttime.elapsed() >= self.timeout || !self.stopper.load(Ordering::Relaxed) {
                bail!("QSearch was cancelled while evaluating {mv}");
            }

            // If we've found a better move than our current best, update our result
            if score > best {
                best = score;

                if score > alpha {
                    alpha = score;
                    // PV found
                    bestmove = mv;
                }

                // Fail soft beta-cutoff.
                // Could also `return Ok(best)` here, but we need to save bestmove to TT
                if score >= beta {
                    break;
                }
            }
        }

        let flag = NodeType::new(best, original_alpha, beta);
        self.save_to_ttable(game.key(), bestmove, best, 0, flag);

        Ok(best) // fail-soft
    }

    fn save_to_ttable(
        &mut self,
        key: ZobristKey,
        bestmove: Move,
        score: i32,
        depth: u32,
        flag: NodeType,
    ) {
        self.ttable.store(TTableEntry {
            key,
            bestmove,
            score,
            depth,
            flag,
        });
    }

    fn get_tt_bestmove(&self, key: ZobristKey) -> Option<Move> {
        self.ttable.get(&key).map(|entry| entry.bestmove)
    }
}

// TODO: verify the values are good: https://discord.com/channels/719576389245993010/719576389690589244/1268914745298391071
fn mvv_lva(kind: PieceKind, captured: PieceKind) -> i32 {
    10 * value_of(captured) - value_of(kind)
}

fn score_move(game: &Game, mv: &Move, tt_bestmove: Option<Move>) -> i32 {
    if tt_bestmove.is_some_and(|tt_mv| tt_mv == *mv) {
        return i32::MIN;
    }

    // Safe unwrap because we can't move unless there's a piece at `from`
    let kind = game.kind_at(mv.from()).unwrap();
    let mut score = 0;

    // Capturing a high-value piece with a low-value piece is a good idea
    if let Some(captured) = game.kind_at(mv.to()) {
        score += mvv_lva(kind, captured);
    }

    // Promoting is also a good idea
    if let Some(promotion) = mv.promotion() {
        score += value_of(promotion);
    }

    // Going somewhere attacked by an opponent is not a good idea, but may be necessary,
    // so negate it, but not by much
    let attacks = game.attacks_by_color(game.current_player().opponent());
    if attacks.get(mv.to()) {
        score -= value_of(kind) / 10;
    }

    -score // We're sorting, so a lower number is better
}