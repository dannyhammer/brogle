use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::Sender;
use std::sync::{Arc, RwLock};
use std::time::Duration;
use std::{ops::Neg, time::Instant};

use anyhow::{bail, Result};
use brogle_core::{Game, Move, PieceKind};
use log::error;

use crate::protocols::{UciInfo, UciResponse, UciScore};
use crate::{value_of, EngineCommand, Evaluator};

/// Wrapper over `i32` for scoring positions
type Score = i32;

/// Largest possible score ever achievable
const INF: Score = i16::MAX as Score;

/// Score of mate in 1 move
const MATE: Score = INF - 1;

/// Maximum depth that can be searched
const MAX_DEPTH: Score = 255;

/// Maximum possible score for mate
const MAX_MATE: Score = MATE - MAX_DEPTH;

/// The result of a search, containing the best move, score, and other metadata.
#[derive(Debug, Clone)]
pub struct SearchResult {
    /// The best move found during this search. If `None`, then no valid move was found (i.e. when mated).
    pub bestmove: Option<Move>,
    /// The score of the best move. A higher score is better for the score player.
    pub score: Score,
    /// If supplied, this move is checked *first*, before all others.
    pub ponder: Option<Move>,
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
        self.score = self.score.neg();
        self
    }
}

pub struct SearchData {
    pub starttime: Instant,
    pub nodes_searched: usize,
}

impl Default for SearchData {
    fn default() -> Self {
        Self {
            starttime: Instant::now(),
            nodes_searched: 0,
        }
    }
}

/// A struct to encapsulate the logic of searching through moves for a given a chess position.
pub struct Searcher<'a> {
    game: &'a Game,
    timeout: Duration,
    stopper: Arc<AtomicBool>,
    sender: Sender<EngineCommand>,

    // Search data
    pub(crate) data: SearchData,
    pub(crate) result: Arc<RwLock<SearchResult>>,
    // pub(crate) info: Arc<RwLock<UciInfo>>,
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
        timeout: Duration,
        stopper: Arc<AtomicBool>,
        result: Arc<RwLock<SearchResult>>,
        sender: Sender<EngineCommand>,
    ) -> Self {
        Self {
            game,
            timeout,
            stopper,
            result,
            sender,
            data: SearchData::default(),
            // pv: Vec::default(),
        }
    }

    /// Starts a search from the supplied depth.
    // pub fn start(&mut self, depth: usize) -> Result<SearchResult> {
    pub fn start(mut self, depth: usize) -> Result<SearchData> {
        // eprintln!("\nStarting search of depth {depth} on {}", self.game.fen());

        self.data.starttime = Instant::now();
        let result = self.search(depth)?;

        // If the search timed out, this result is garbage, so don't return it.
        *self.result.write().unwrap() = result.clone();
        let elapsed = self.data.starttime.elapsed();

        // Determine whether the score is an evaluation or a "mate in y"
        // Assistance provided by @Ciekce on Discord
        // https://github.com/Ciekce/Stormphrax/blob/main/src/search.cpp#L1163
        // eprintln!("SCORE({depth})={}", result.score);
        let score = if result.score.abs() >= MAX_MATE {
            let moves_to_mate = if result.score > 0 {
                MATE - result.score + 1
            } else {
                -MATE - result.score
            } / 2;
            UciScore::mate(moves_to_mate)
        } else {
            UciScore::cp(result.score)
        };

        let info = UciInfo::default()
            .depth(depth)
            .score(score)
            .nodes(self.data.nodes_searched)
            .nps((self.data.nodes_searched as f64 / elapsed.as_secs_f64()) as usize)
            .time(elapsed.as_millis())
            // .pv(pv)
            ;

        if let Err(err) = self
            .sender
            .send(EngineCommand::UciResponse(Box::new(UciResponse::Info(
                Box::new(info),
            ))))
        {
            //
            error!("Failed to send 'info' to engine during search: {err:?}");
        }

        //  */
        // let nps = self.nodes_searched as f64 / elapsed.as_secs_f64();
        // eprintln!(
        //     "\nSearched {} nodes in {:?} ({nps:.2} n/s): {result:?}",
        //     self.nodes_searched, elapsed
        // );

        // Ok(result)
        Ok(self.data)
    }

    // pub fn stop(&mut self) {
    //     self.stopper.store(false, Ordering::Relaxed);
    // }

    fn search(&mut self, depth: usize) -> Result<SearchResult> {
        // No need to check depth 0 here because this cannot be called with `depth < 1`
        let mut moves = self.game.legal_moves();
        let mut result = SearchResult::default();

        if moves.is_empty() {
            result.score = if self.game.is_in_check() { -MATE } else { 0 };
            return Ok(result);
        }

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
            if self.data.starttime.elapsed() >= self.timeout
                || !self.stopper.load(Ordering::Relaxed)
            {
                // If we must cancel this search, we need to return the result from the previous iteration
                bail!(
                    "Search was stopped while evaluating {mv}. Current bestmove: {:?}",
                    result.bestmove
                );
            }

            if score > result.score {
                result.score = score;

                // Fail soft beta-cutoff.
                // Could also `return Ok(best)` here, but we need to save bestmove to TT
                if result.score >= beta {
                    break;
                }

                if result.score > alpha {
                    alpha = score;
                    result.bestmove = Some(mv);
                }
            }

            // Opponent would never choose this branch, so we can prune
            if alpha >= beta {
                break;
            }
        }

        Ok(result)
    }

    /// Uses [Fail soft](https://www.chessprogramming.org/Alpha-Beta#Fail_soft)
    fn negamax(
        &mut self,
        game: &Game,
        depth: usize,
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
        // let mut bestmove = *moves.first().unwrap();

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
            if self.data.starttime.elapsed() >= self.timeout
                || !self.stopper.load(Ordering::Relaxed)
            {
                // If we must cancel this search, we need to return the result from the previous iteration
                bail!("Negamax was stopped while evaluating {mv}. Current best score: {best}");
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
                    // bestmove = mv;
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
        // let mut bestmove = *captures.first().unwrap(); // Safe unwrap because we already checked that moves isn't empty

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
            if self.data.starttime.elapsed() >= self.timeout
                || !self.stopper.load(Ordering::Relaxed)
            {
                // If we must cancel this search, we need to return the result from the previous iteration
                bail!("QSearch was stopped while evaluating {mv}");
            }
            if score > best {
                best = score;

                // Update alpha.
                if score > alpha {
                    alpha = score;
                    // PV found
                    // bestmove = mv;
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
