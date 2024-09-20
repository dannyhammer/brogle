use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use anyhow::{bail, Result};
use chessie::{Game, Move, PieceKind, ZobristKey};

use super::{NodeType, Score, TTable, TTableEntry};
use crate::{value_of, Evaluator};

pub struct SearchData {
    /// Total number of nodes evaluated during this search.
    ///
    /// Note this is *not* the total number of nodes possible, as it does not consider nodes that were pruned through alpha/beta.
    pub nodes_searched: usize,

    /// Best move found during this search.
    pub bestmove: Option<Move>,

    /// Score for making the associated `bestmove`.
    pub score: Score,
    /*
    /// Principle Variation of the search.
    ///
    /// pv[i][j] is the PV at the i'th ply (0 is root), which has j descendants
    ///
    /// For example, if you searched at depth 4, then 'i' would be at most 3.
    /// pv[0] would have depth+q entries, where 'q' is the number of plies searched in qsearch.
    pub pv: ArrayVec<ArrayVec<Move, MAX_DEPTH>, MAX_DEPTH>,
     */
}

impl Default for SearchData {
    fn default() -> Self {
        Self {
            nodes_searched: 0,
            bestmove: None,
            score: -Score::INF,
            // pv: ArrayVec::new(),
        }
    }
}

/// A struct to encapsulate the logic of searching through moves for a given a chess position.
pub struct Searcher<'a> {
    /// Game to search on.
    game: &'a Game,

    /// Transposition table for lookups.
    ttable: &'a mut TTable,

    /// Start time of the search.
    starttime: Instant,

    /// How long we can search for.
    timeout: Duration,

    /// We can continue searching as long as this is `true`.
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
        // Initialize PV arrays to be empty
        // Note this doesn't allocate enough to add PVs in QSearch,
        // but QSearch doesn't search all moves, so adding PVs there doesn't make sense.
        // for _ in 0..=depth {
        //     self.data.pv.push(ArrayVec::new());
        // }

        let key = self.game.key();
        self.data.score = self.negamax::<true>(self.game, depth, 0, -Score::INF, Score::INF)?;

        self.data.bestmove = self.ttable.get(&key).map(|entry| entry.bestmove);

        Ok(self.data)
    }

    /// Uses [Fail soft](https://www.chessprogramming.org/Alpha-Beta#Fail_soft)
    fn negamax<const IS_PV_NODE: bool>(
        &mut self,
        game: &Game,
        depth: u32,
        ply: i32,
        mut alpha: Score,
        beta: Score,
    ) -> Result<Score> {
        // Clear PV for this node
        // self.data.pv[ply as usize] = ArrayVec::new();

        // Store original alpha to be compared with at the end of the function
        let original_alpha = alpha;

        /*
        // TODO: Don't do TT cutoffs in PV node!
        if let Some(tt_entry) = self.ttable.get(&game.key()) {
            if let Some(score) = tt_entry.try_score(alpha, beta, depth, ply) {
                // Adjust score to be relative to this ply
                // let score = score.relative(ply);
                return Ok(score);
            }
        }
         */

        // Reached the end of the depth; start a qsearch for captures only
        if depth == 0 {
            return self.quiescence(game, ply + 1, alpha, beta);
        }

        // If we've no legal moves available, it's either checkmate or stalemate.
        let mut moves = game.get_legal_moves();
        if moves.is_empty() {
            // Prefer earlier mates, and drawing is better than being mated.
            let score = if game.is_in_check() {
                -Score::MATE + ply
            } else {
                Score::DRAW
            };
            return Ok(score);
        }

        let tt_bestmove = self.get_tt_bestmove(game.key());
        moves.sort_by_cached_key(|mv| score_move(game, mv, tt_bestmove));

        // Start with a default (very bad) result.
        let mut best = -Score::INF;
        let mut bestmove = moves[0]; // Safe because we already checked that `moves` isn't empty

        for (i, mv) in moves.into_iter().enumerate() {
            // Make the score move on the position, getting a new position in return
            let new_pos = game.with_move_made(mv);

            // If it's a draw, don't recurse, but set the score to 0 and continue as normal
            let score = if new_pos.is_repetition() || new_pos.can_draw_by_fifty() {
                Score::DRAW
            }
            // Otherwise, we run a Principal Variation Search: https://en.wikipedia.org/wiki/Principal_variation_search#Pseudocode
            else if i == 0 {
                // Normal a/b search on the first node of PVS
                -self.negamax::<IS_PV_NODE>(&new_pos, depth - 1, ply + 1, -beta, -alpha)?
            } else {
                // Null window search on all other nodes
                let score =
                    -self.negamax::<false>(&new_pos, depth - 1, ply + 1, -alpha - 1, -alpha)?;

                // If the null-window search failed high, do re-search with a normal a/b search
                if alpha < score && score < beta {
                    -self.negamax::<IS_PV_NODE>(&new_pos, depth - 1, ply + 1, -beta, -alpha)?
                }
                // Otherwise, return the result of the null-window search
                else {
                    score
                }
            };
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

                    // Clear PV for this node
                    // NOTE: The `clone` gets compiled away, and various methods all compile to the same thing: https://godbolt.org/z/45GM5TqGh
                    // self.data.pv[ply as usize].clear();
                    // self.data.pv[ply as usize].push(mv);
                    // let rest = self.data.pv[ply as usize + 1].clone();
                    // self.data.pv[ply as usize].extend(rest);
                }

                // Fail soft beta-cutoff.
                // Could also `return Ok(best)` here, but we need to save bestmove to TT
                if score >= beta {
                    break;
                }
            }
        }

        // Store this node in the TTable
        self.save_to_ttable(game.key(), bestmove, best, original_alpha, beta, depth, ply);

        Ok(best) // fail-soft
    }

    fn quiescence(
        &mut self,
        game: &Game,
        ply: i32,
        mut alpha: Score,
        beta: Score,
    ) -> Result<Score> {
        let stand_pat = Evaluator::new(game).eval_current_player();
        if stand_pat >= beta {
            return Ok(beta);
        } else if stand_pat > alpha {
            alpha = stand_pat;
        }

        // Check for mate in qsearch if and only if we're in check.
        let mut moves = if game.is_in_check() {
            let moves = game.get_legal_moves();

            // If we're in check and have no legal moves, we can return a mate score
            if moves.is_empty() {
                return Ok(-Score::MATE + ply);
            }

            moves
        } else {
            let captures = game.get_legal_captures();

            // Can't check for mates in normal qsearch, since we're not looking at *all* moves.
            if captures.is_empty() {
                return Ok(stand_pat);
            }

            captures
        };

        // Can't check for mates in normal qsearch, since we're not looking at *all* moves.
        if moves.is_empty() {
            return Ok(stand_pat);
        }

        let tt_bestmove = self.get_tt_bestmove(game.key());
        moves.sort_by_cached_key(|mv| score_move(game, mv, tt_bestmove));

        // let original_alpha = alpha;
        let mut best = stand_pat;
        let mut bestmove = moves[0]; // Safe because we already checked that `moves` isn't empty
        let original_alpha = alpha;

        // Only search moves
        for mv in moves {
            // Make the score move on the position, getting a new position in return
            let new_pos = game.with_move_made(mv);

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

        // Store this node in the TTable
        self.save_to_ttable(game.key(), bestmove, best, original_alpha, beta, 0, ply);

        Ok(best) // fail-soft
    }

    fn save_to_ttable(
        &mut self,
        key: ZobristKey,
        bestmove: Move,
        score: Score,
        alpha: Score,
        beta: Score,
        depth: u32,
        ply: i32,
    ) {
        // Determine what kind of node this is fist, before score adjustment
        let node_type = NodeType::new(score, alpha, beta);

        // We need to adjust the score by ply if it's mate.
        let score = if score >= Score::LOWEST_MATE {
            score + ply
        } else if score <= -Score::LOWEST_MATE {
            score - ply
        } else {
            score
        };

        // let score = score.absolute(ply);

        self.ttable.store(TTableEntry {
            key,
            bestmove,
            score,
            depth,
            node_type,
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
    // let attacks = game.attacks_by_color(game.current_player().opponent());
    // if attacks.get(mv.to()) {
    // score -= value_of(kind) / 10;
    // }

    -score // We're sorting, so a lower number is better
}
