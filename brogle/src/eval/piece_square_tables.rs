use std::fmt;

use brogle_core::{Color, File, Piece, PieceKind, Rank, Tile};

#[rustfmt::skip]
const PAWNS: Psq = Psq::new_flipped_by_rank([
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
const KNIGHTS: Psq = Psq::new_flipped_by_rank([
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
const BISHOPS: Psq = Psq::new_flipped_by_rank([
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
const ROOKS: Psq = Psq::new_flipped_by_rank([
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
const QUEEN: Psq = Psq::new_flipped_by_rank([
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
const KING_MG: Psq = Psq::new_flipped_by_rank([
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
const KING_EG: Psq = Psq::new_flipped_by_rank([
    -50,-40,-30,-20,-20,-30,-40,-50,
    -30,-20,-10,  0,  0,-10,-20,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-30,  0,  0,  0,  0,-30,-30,
    -50,-30,-30,-30,-30,-30,-30,-50
]);

/*
#[rustfmt::skip]
pub const TEST_PSQ: Psq = Psq::new_flipped_by_rank([
    56, 57, 58, 59, 60, 61, 62, 63,
    48, 49, 50, 51, 52, 53, 54, 55,
    40, 41, 42, 43, 44, 45, 46, 47,
    32, 33, 34, 35, 36, 37, 38, 39,
    24, 25, 26, 27, 28, 29, 30, 31,
    16, 17, 18, 19, 20, 21, 22, 23,
    8,   9, 10, 11, 12, 13, 14, 15,
    0,   1,  2,  3,  4,  5,  6,  7
]);
 */

/// A [Piece-Square Table](https://www.chessprogramming.org/Piece-Square_Tables) for weighting locations on the board.
///
/// When defining a PSQ, the table as-written in code will apply for White.
/// That is, the first entry is the value at A8, the second, B8, and so on...
#[derive(PartialEq, Eq, Debug)]
pub struct Psq([i32; Tile::COUNT]);

impl Psq {
    /// Flips `psq` by rank, so that the representation as-written in code will correspond to White's perspective.
    pub const fn new_flipped_by_rank(psq: [i32; Tile::COUNT]) -> Self {
        let mut flipped = psq;

        let mut i = 0;
        while i < psq.len() {
            flipped[i] = psq[i ^ 56]; // Flip the rank, not the file
            i += 1;
        }

        Self(flipped)
    }

    /// Get the value of this PSQ at the provided tile.
    const fn get(&self, tile: Tile) -> i32 {
        self.0[tile.index()]
    }

    /// Get the value of this PSQ at the provided tile, relative to `color`.
    const fn get_relative(&self, tile: Tile, color: Color) -> i32 {
        self.get(tile.file_relative_to(color))
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
        self.get_weighted(tile.file_relative_to(color), other, weight)
    }
}

impl fmt::Display for Psq {
    /// Printing a [`Psq`] will display it in the same way it is written in the code.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Format actual PSQ values
        for rank in Rank::iter().rev() {
            write!(f, "{rank}")?;
            write!(f, "| ")?;
            for file in File::iter() {
                write!(f, "{:3} ", self.get(Tile::new(file, rank)))?;
            }
            writeln!(f)?;
        }
        // Format line at bottom of board
        write!(f, " +")?;
        for _ in File::iter() {
            write!(f, "----")?;
        }
        write!(f, "\n    ")?;
        // Format file characters
        for file in File::iter() {
            write!(f, "{file}")?;
            write!(f, "   ")?;
        }

        Ok(())
    }
}

/// Fetch the Piece-Square Table value for `piece` at `tile`.
///
/// Presently, `endgame_weight` is only factored in when computing King values.
pub fn psq_eval(piece: Piece, tile: Tile, endgame_weight: i32) -> i32 {
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

/// Performs linear interpolation between `x` and `y` by `t`, as integer values.
const fn lerp_i32(x: i32, y: i32, t: i32) -> i32 {
    x + (y - x) * t / 100
}
