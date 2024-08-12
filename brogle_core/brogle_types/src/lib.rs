/// All things related to Bitboards!
pub mod bitboard;
/// Simple enum for white and black colors.
pub mod color;
pub mod magicgen;
pub mod piece;
pub mod tile;
pub mod utils;

pub use bitboard::*;
pub use color::*;
pub use magicgen::*;
pub use piece::*;
pub use tile::*;
pub use utils::*;

/// Re-exports all the things you'll need.
pub mod prelude {
    pub use crate::bitboard::*;
    pub use crate::color::*;
    pub use crate::magicgen::*;
    pub use crate::piece::*;
    pub use crate::tile::*;
    pub use crate::utils::*;
}
