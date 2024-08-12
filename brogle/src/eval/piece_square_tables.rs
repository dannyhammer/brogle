use core::fmt;

use brogle_core::{BitBoard, ChessBoard, Color, File, PieceKind, Rank, Tile};

/*
pub const PAWN_PUSH: PieceSquareTable = PieceSquareTable([
    60, 60, 60, 60, 60, 60, 60, 60, //
    50, 50, 50, 50, 50, 50, 50, 50, //
    40, 40, 40, 40, 40, 40, 40, 40, //
    30, 30, 30, 30, 30, 30, 30, 30, //
    20, 20, 20, 20, 20, 20, 20, 20, //
    10, 10, 10, 10, 10, 10, 10, 10, //
    01, 01, 01, 01, 01, 01, 01, 01, //
    00, 00, 00, 00, 00, 00, 00, 00, //
]);

pub const CONTROL_CENTER: PieceSquareTable = PieceSquareTable([
    10, 11, 12, 13, 13, 12, 11, 10, //
    21, 22, 23, 24, 24, 23, 22, 21, //
    32, 33, 34, 35, 35, 34, 33, 32, //
    43, 44, 45, 46, 46, 45, 44, 43, //
    43, 44, 45, 46, 46, 45, 44, 43, //
    32, 33, 34, 35, 35, 34, 33, 32, //
    21, 22, 23, 24, 24, 23, 22, 21, //
    10, 11, 12, 13, 13, 12, 11, 10, //
]);

pub const KING_SAFETY: PieceSquareTable = PieceSquareTable([
    01, 01, 01, 01, 01, 01, 01, 01, //
    01, 01, 01, 01, 01, 01, 01, 01, //
    01, 01, 01, 01, 01, 01, 01, 01, //
    01, 01, 01, 01, 01, 01, 01, 01, //
    01, 01, 01, 01, 01, 01, 01, 01, //
    75, 70, 65, 60, 60, 65, 70, 75, //
    85, 80, 75, 70, 70, 75, 80, 85, //
    90, 85, 80, 75, 75, 80, 85, 90, //
]);
 */

/// A [Piece-Square Table] for weighting locations on the board.
///
/// When defining a PSQ, the table as-written in code will apply for White.
/// That is, the lowest 8 indices correspond to the first rank, and so on.
#[derive(PartialEq, Eq, Debug)]
pub struct PieceSquareTable([i32; Tile::COUNT]);

impl PieceSquareTable {
    pub fn relative_to(self, color: Color) -> Self {
        match color {
            Color::White => self.flipped(),
            Color::Black => self,
        }
    }

    pub fn flipped(mut self) -> Self {
        self.0.reverse();
        Self(self.0)
    }

    pub fn apply_to(self, board: BitBoard) -> i32 {
        // println!("Applying:\n{self} to:\n{:?}", board);
        let mut score = 0;

        for square in board {
            score += self.0[square] * board.get(square) as i32;
        }

        score
    }

    pub fn apply_for(self, board: &ChessBoard, color: Color, kind: Option<PieceKind>) -> i32 {
        let psq = self.relative_to(color);
        if let Some(kind) = kind {
            psq.apply_to(board.piece_parts(color, kind))
        } else {
            psq.relative_to(color).apply_to(board.color(color))
        }
    }
}

impl fmt::Display for PieceSquareTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut pst = String::with_capacity(278);

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
