use crate::{CastlingRights, ChessBoard, XoShiRo};

use super::Position;
use brogle_types::{Color, Tile, NUM_CASTLING_RIGHTS, NUM_PIECES, NUM_TILES};

/// Stores Zobrist hash keys, for hashing [`Position`]s.
///
/// Initialized upon program startup with library-supplied keys that remain constant between compilations.
pub const ZOBRIST_TABLE: ZobristTable = ZobristTable::new();

/// Encapsulates the logic of Zobrist hashing.
///
/// Primarily used to create the [`ZOBRIST_TABLE`] constant, though this struct remains public in case it has other uses...
pub struct ZobristTable {
    /// One unique key for every possible piece and every possible tile.
    piece_keys: [[u64; NUM_PIECES]; Tile::COUNT],
    /// One unique key for every tile where en passant is possible.
    ep_keys: [u64; Tile::COUNT],
    /// One key for every possible combination of castling rights.
    castling_keys: [u64; NUM_CASTLING_RIGHTS],
    /// One key for the side-to-move (only if side-to-move is Black).
    color_key: u64,
}

impl ZobristTable {
    /// Initialize this table, generating keys via the [`XoShiRo`] struct.
    pub const fn new() -> Self {
        let mut piece_keys = [[0; NUM_PIECES]; Tile::COUNT];
        let color_key;
        let mut ep_keys = [0; Tile::COUNT];
        let mut castling_keys = [0; NUM_CASTLING_RIGHTS];

        let mut prng = XoShiRo::new();

        // Initialize keys for pieces and EP
        let mut i = 0;
        while i < NUM_TILES {
            let mut j = 0;
            // Initialize keys for pieces
            while j < NUM_PIECES {
                let key;
                (key, prng) = prng.const_next();
                piece_keys[i][j] = key;
                j += 1;
            }

            // Initialize keys for en passant squares
            let key;
            (key, prng) = prng.const_next();
            ep_keys[i] = key;
            i += 1;
        }

        // Initialize keys for castling rights
        i = 0;
        while i < NUM_CASTLING_RIGHTS {
            let key;
            (key, prng) = prng.const_next();
            castling_keys[i] = key;
            i += 1;
        }

        // Initialize keys for side-to-move
        let (key, _) = prng.const_next();
        color_key = key;

        Self {
            piece_keys,
            ep_keys,
            castling_keys,
            color_key,
        }
    }

    pub fn hash_parts(
        &self,
        board: &ChessBoard,
        ep_tile: Option<Tile>,
        castling_rights: &CastlingRights,
        color: Color,
    ) -> u64 {
        let mut hash = 0;

        // Hash all pieces on the board
        for (tile, piece) in board.pieces() {
            hash ^= self.piece_keys[tile][piece];
        }

        // Hash the en passant square, if it exists
        if let Some(ep_tile) = ep_tile {
            hash ^= self.ep_keys[ep_tile];
        }

        // Hash the castling rights
        hash ^= self.castling_keys[castling_rights];

        // Hash the side-to-move
        if color.is_black() {
            hash ^= self.color_key
        }

        hash
    }

    pub fn hash(&self, position: &Position) -> u64 {
        self.hash_parts(
            position.board(),
            position.ep_tile(),
            position.castling_rights(),
            position.current_player(),
        )
    }
}
