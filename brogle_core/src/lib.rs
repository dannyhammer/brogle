/// All things related to Bitboards!
pub mod bitboard;

/// Simple enum for white and black colors.
pub mod color;
// pub mod magicgen;
pub mod movegen;
pub mod moves;
pub mod perft;
pub mod piece;
pub mod position;
pub mod tile;
pub mod utils;

pub use bitboard::*;
pub use color::*;
// pub use magicgen::*;
pub use movegen::*;
pub use moves::*;
pub use perft::*;
pub use piece::*;
pub use position::*;
pub use tile::*;
pub use utils::*;

/// Re-exports all the things you'll need.
pub mod prelude {
    pub use crate::bitboard::*;
    pub use crate::color::*;
    pub use crate::movegen::*;
    pub use crate::moves::*;
    pub use crate::perft::*;
    pub use crate::piece::*;
    pub use crate::position::*;
    pub use crate::tile::*;
    pub use crate::utils::*;
}
