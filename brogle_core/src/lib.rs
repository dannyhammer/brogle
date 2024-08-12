pub use brogle_types::*;

pub mod movegen;
pub mod moves;
pub mod perft;
pub mod position;
pub mod prng;
pub mod zobrist;

// pub use magicgen::*;
pub use movegen::*;
pub use moves::*;
pub use perft::*;
pub use position::*;
pub use prng::*;
pub use zobrist::*;

/// Re-exports all the things you'll need.
pub mod prelude {
    pub use crate::movegen::*;
    pub use crate::moves::*;
    pub use crate::perft::*;
    pub use crate::position::*;
    pub use crate::prng::*;
    pub use crate::zobrist::*;
}
