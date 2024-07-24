pub mod board;
pub mod color;
pub mod error;
pub mod magicgen;
pub mod movegen;
pub mod moves;
pub mod piece;
pub mod position;
pub mod tile;
pub mod utils;

pub use board::*;
pub use color::*;
pub use error::*;
pub use magicgen::*;
pub use movegen::*;
pub use moves::*;
pub use piece::*;
pub use position::*;
pub use tile::*;
pub use utils::*;

pub mod prelude {
    pub use crate::board::*;
    pub use crate::movegen::*;
    pub use crate::moves::*;
    pub use crate::piece::*;
    pub use crate::position::*;
    pub use crate::tile::*;
}
