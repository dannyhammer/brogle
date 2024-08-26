/// Largest possible score ever achievable
pub const INF: i32 = i16::MAX as i32;

/// Score of mate in 1 move
pub const MATE: i32 = INF - 1;

/// Maximum depth that can be searched
pub const MAX_DEPTH: usize = 255;

/// Maximum possible score for mate
///
/// This is only obtainable if mate is possible in [`MAX_DEPTH`] moves.
pub const MAX_MATE: i32 = MATE - MAX_DEPTH as i32;
