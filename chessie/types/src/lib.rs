/// All things related to Bitboards.
pub mod bitboard;
/// Code to generate magic bitboards.
pub mod magicgen;
/// Enums for piece kinds, colors, and a struct for a chess piece.
pub mod piece;
/// Squares on a chessboard (including files and ranks).
pub mod square;
/// Misc utility functions and constants.
pub mod utils;

pub use bitboard::*;
// pub use magicgen::*;
pub use piece::*;
pub use square::*;
pub use utils::*;

/// Re-exports all the things you'll need.
pub mod prelude {
    pub use crate::bitboard::*;
    // pub use crate::magicgen::*;
    pub use crate::piece::*;
    pub use crate::square::*;
    pub use crate::utils::*;
}
