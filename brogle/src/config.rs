/// Wrapper over `i32` for scoring positions
pub type Score = i32;

/// Largest possible score ever achievable
pub const INF: Score = i16::MAX as Score;

/// Score of mate in 1 move
pub const MATE: Score = INF - 1;

/// Maximum depth that can be searched
pub const MAX_DEPTH: u32 = 255;

/// Maximum possible score for mate
pub const MAX_MATE: Score = MATE - MAX_DEPTH as Score;
