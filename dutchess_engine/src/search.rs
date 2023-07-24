use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

use chess::{Board, ChessMove, MoveGen};
// use dutchess_core::{GameState, Move, PieceKind, Tile};
use rand::prelude::*;

use crate::uci::{SearchOptions, UciInfo, UciResponse, UciSearchMode};

#[derive(Debug, Clone)]
pub struct SearchResult {
    // pub bestmove: Move,
    // pub ponder: Option<Move>,
    pub bestmove: ChessMove,
}

impl Default for SearchResult {
    fn default() -> Self {
        Self {
            bestmove: ChessMove::default(),
            // bestmove: Move::illegal(),
            // ponder: None,
        }
    }
}

pub fn search(state: Board, depth: u32) -> SearchResult {
    // pub fn search(state: GameState, depth: u32) -> SearchResult {
    let mut res = SearchResult::default();

    // Simulate an increasingly-costly search time
    // TODO: REMOVE ME
    thread::sleep(Duration::from_millis(depth as u64 * 10));

    // res.bestmove = state.legal_moves().next().unwrap();
    // res.bestmove = state.legal_moves().choose(&mut thread_rng()).unwrap();
    let moves = MoveGen::new_legal(&state);
    res.bestmove = moves.choose(&mut thread_rng()).unwrap();
    /*
    {
        let legal = moves.choose(&mut thread_rng()).unwrap();

        let src = Tile::from_index_unchecked(legal.get_source().to_index());
        let dst = Tile::from_index_unchecked(legal.get_dest().to_index());
        let promotion = legal.get_promotion().map(|piece| {
            use chess::Piece::*;
            match piece {
                Pawn => PieceKind::Pawn,
                Knight => PieceKind::Knight,
                Bishop => PieceKind::Bishop,
                Rook => PieceKind::Rook,
                Queen => PieceKind::Queen,
                King => PieceKind::King,
            }
        });

        Move::new(src, dst, promotion)
    };
     */

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
