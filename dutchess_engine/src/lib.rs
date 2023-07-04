pub mod game;
pub mod piece;
pub mod position;

pub mod prelude {
    pub use crate::game::*;
    pub use crate::piece::*;
    pub use crate::position::*;
}

pub use prelude::*;
