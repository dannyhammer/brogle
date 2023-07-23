pub mod board;
pub mod game;
pub mod maskgen;
pub mod movegen;
pub mod moves;
pub mod piece;
pub mod tile;
pub mod utils;

pub mod prelude {
    pub use crate::board::*;
    pub use crate::game::*;
    pub use crate::movegen::*;
    pub use crate::moves::*;
    pub use crate::piece::*;
    pub use crate::tile::*;
}

pub use prelude::*;
