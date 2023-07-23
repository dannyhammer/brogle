use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

use dutchess_core::{GameState, Move};
use rand::prelude::*;

use crate::uci::{SearchOptions, UciInfo, UciResponse, UciSearchMode};

// #[derive(Debug, Clone, Copy)]
// pub struct SearchParams {
//     pub max_depth: u32,
//     pub curr_depth: u32,
// }

// impl Default for SearchParams {
//     fn default() -> Self {
//         SearchParams {
//             max_depth: 10,
//             curr_depth: 0,
//         }
//     }
// }

#[derive(Debug, Clone)]
pub struct SearchResult {
    pub bestmove: Move,
    pub ponder: Option<Move>,
}

impl Default for SearchResult {
    fn default() -> Self {
        Self {
            bestmove: Move::illegal(),
            ponder: None,
        }
    }
}

pub fn search(state: GameState, depth: u32) -> SearchResult {
    let mut res = SearchResult::default();

    // Simulate an increasingly-costly search time
    // TODO: REMOVE ME
    thread::sleep(Duration::from_millis(depth as u64 * 10));

    // res.bestmove = state.legal_moves().next().unwrap();
    res.bestmove = state.legal_moves().choose(&mut thread_rng()).unwrap();

    res
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
