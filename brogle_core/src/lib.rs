pub use brogle_types::*;

pub mod movegen;
pub mod moves;
pub mod perft;
pub mod position;

// pub use magicgen::*;
pub use movegen::*;
pub use moves::*;
pub use perft::*;
pub use position::*;

/// Re-exports all the things you'll need.
pub mod prelude {
    pub use crate::movegen::*;
    pub use crate::moves::*;
    pub use crate::perft::*;
    pub use crate::position::*;
}
