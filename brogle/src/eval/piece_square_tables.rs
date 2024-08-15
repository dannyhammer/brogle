use std::fmt;

use brogle_core::{Color, File, Piece, PieceKind, Rank, Tile, NUM_TILES};

#[rustfmt::skip]
const PAWNS: Psq = Psq([
    0,  0,  0,  0,  0,  0,  0,  0,
    50, 50, 50, 50, 50, 50, 50, 50,
    10, 10, 20, 30, 30, 20, 10, 10,
    5,  5, 10, 25, 25, 10,  5,  5,
    0,  0,  0, 20, 20,  0,  0,  0,
    5, -5,-10,  0,  0,-10, -5,  5,
    5, 10, 10,-20,-20, 10, 10,  5,
    0,  0,  0,  0,  0,  0,  0,  0
]);

#[rustfmt::skip]
const KNIGHTS: Psq = Psq([
    -50,-40,-30,-30,-30,-30,-40,-50,
    -40,-20,  0,  0,  0,  0,-20,-40,
    -30,  0, 10, 15, 15, 10,  0,-30,
    -30,  5, 15, 20, 20, 15,  5,-30,
    -30,  0, 15, 20, 20, 15,  0,-30,
    -30,  5, 10, 15, 15, 10,  5,-30,
    -40,-20,  0,  5,  5,  0,-20,-40,
    -50,-40,-30,-30,-30,-30,-40,-50,
]);

#[rustfmt::skip]
const BISHOPS: Psq = Psq([
    -20,-10,-10,-10,-10,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5, 10, 10,  5,  0,-10,
    -10,  5,  5, 10, 10,  5,  5,-10,
    -10,  0, 10, 10, 10, 10,  0,-10,
    -10, 10, 10, 10, 10, 10, 10,-10,
    -10,  5,  0,  0,  0,  0,  5,-10,
    -20,-10,-10,-10,-10,-10,-10,-20,
]);

#[rustfmt::skip]
const ROOKS: Psq = Psq([
    0,  0,  0,  0,  0,  0,  0,  0,
    5, 10, 10, 10, 10, 10, 10,  5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    0,  0,  0,  5,  5,  0,  0,  0
]);

#[rustfmt::skip]
const QUEEN: Psq = Psq([
    -20,-10,-10, -5, -5,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5,  5,  5,  5,  0,-10,
    -5,  0,  5,  5,  5,  5,  0, -5,
    0,  0,  5,  5,  5,  5,  0, -5,
    -10,  5,  5,  5,  5,  5,  0,-10,
    -10,  0,  5,  0,  0,  0,  0,-10,
    -20,-10,-10, -5, -5,-10,-10,-20
]);

#[rustfmt::skip]
const KING_MG: Psq = Psq([
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -20,-30,-30,-40,-40,-30,-30,-20,
    -10,-20,-20,-20,-20,-20,-20,-10,
    20, 20,  0,  0,  0,  0, 20, 20,
    20, 30, 10,  0,  0, 10, 30, 20
]);

#[rustfmt::skip]
const KING_EG: Psq = Psq([
    -50,-40,-30,-20,-20,-30,-40,-50,
    -30,-20,-10,  0,  0,-10,-20,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-30,  0,  0,  0,  0,-30,-30,
    -50,-30,-30,-30,-30,-30,-30,-50
]);

/// A [Piece-Square Table](https://www.chessprogramming.org/Piece-Square_Tables) for weighting locations on the board.
///
/// When defining a PSQ, the table as-written in code will apply for White.
/// That is, the lowest 8 indices correspond to the first rank, and so on.
#[derive(PartialEq, Eq, Debug)]
struct Psq([i32; NUM_TILES]);

impl Psq {
    /// Get the value of this PSQ at the provided tile.
    const fn get(&self, tile: Tile) -> i32 {
        self.0[tile.index()]
    }

    /// Get the value of this PSQ at the provided tile, relative to `color`.
    const fn get_relative(&self, tile: Tile, color: Color) -> i32 {
        match color {
            Color::White => self.get(tile.flipped()),
            Color::Black => self.get(tile),
        }
    }

    /// Interpolate a value between this PSQ and another.
    ///
    /// `weight` should be `[0, 100]`.
    const fn get_weighted(&self, tile: Tile, other: &Self, weight: i32) -> i32 {
        lerp_i32(self.get(tile), other.get(tile), weight)
    }

    /// Interpolate a value between this PSQ and another, relative to `color`.
    ///
    /// `weight` should be `[0, 100]`.
    const fn get_relative_weighted(
        &self,
        tile: Tile,
        color: Color,
        other: &Self,
        weight: i32,
    ) -> i32 {
        match color {
            Color::White => self.get_weighted(tile.flipped(), other, weight),
            Color::Black => self.get_weighted(tile, other, weight),
        }
    }
}

impl fmt::Display for Psq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut pst = String::with_capacity(278); // Pre-allocate *just* enough space

        for rank in Rank::iter().rev() {
            pst += rank.as_ref();
            pst += "| ";
            for file in File::iter() {
                let entry = format!("{:02} ", self.0[file * rank]);
                pst += &entry;
            }
            pst += "\n";
        }

        pst += " +";
        for _ in File::iter() {
            pst += "---";
        }
        pst += "\n   ";
        for file in File::iter() {
            pst += file.as_ref();
            pst += "  "
        }

        write!(f, "{pst}")
    }
}

pub const fn psq_eval(piece: Piece, tile: Tile, endgame_weight: i32) -> i32 {
    match piece.kind() {
        PieceKind::Pawn => PAWNS.get_relative(tile, piece.color()),
        PieceKind::Knight => KNIGHTS.get_relative(tile, piece.color()),
        PieceKind::Bishop => BISHOPS.get_relative(tile, piece.color()),
        PieceKind::Rook => ROOKS.get_relative(tile, piece.color()),
        PieceKind::Queen => QUEEN.get_relative(tile, piece.color()),
        PieceKind::King => {
            KING_MG.get_relative_weighted(tile, piece.color(), &KING_EG, endgame_weight)
        }
    }
}

const fn lerp_i32(x: i32, y: i32, t: i32) -> i32 {
    x + (y - x) * t / 100
}
