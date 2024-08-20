/// Largest possible score ever achievable
pub const INF: i32 = i16::MAX as i32;

/// Score of mate in 1 move
pub const MATE: i32 = INF - 1;

/// Maximum depth that can be searched
pub const MAX_DEPTH: u32 = 255;

/// Maximum possible score for mate
pub const MAX_MATE: i32 = MATE - MAX_DEPTH as i32;
