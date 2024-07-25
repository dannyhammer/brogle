use std::{error::Error, fmt};

use super::{Color, Move, Tile};

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum ChessError {
    OutOfBounds { val: usize, min: usize, max: usize },
    InvalidFileChar { val: char },
    InvalidRankChar { val: char },
    InvalidColorChar { val: char },
    InvalidColorStr,
    InvalidTileNotation,
    InvalidPieceNotation,
    InvalidPieceChar { val: char },
    InvalidBitBoardString,
    InvalidCastlingRights,
    InvalidFenString,
    InvalidMove { color: Color, chessmove: Move },
    NoPieceAtTile { tile: Tile },
}

impl fmt::Display for ChessError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OutOfBounds { val, min, max } => {
                write!(f, "value {val} must be within {min}..={max}")
            }
            Self::InvalidFileChar { val } => write!(f, "file chars must be [a, h]. found {val}"),
            Self::InvalidRankChar { val } => write!(f, "rank chars must be [1, 8]. found {val}"),
            Self::InvalidColorChar { val } => write!(f, "color chars must be `w` or `b`. found {val}"),
            Self::InvalidColorStr => write!(f, "color strings must be `w` or `b`"),
            Self::InvalidTileNotation => write!(
                f,
                "tile is not valid notation. notation must be <file><rank>"
            ),
            Self::InvalidPieceNotation => write!(
                f,
                "tile is not valid notation. notation must be <file><rank>"
            ),
            Self::InvalidPieceChar { val } => write!(
                f,
                "pieces must be [p | n | b | r | q | k] or uppercase equivalent. found {val}"
            ),
            Self::InvalidBitBoardString => write!(f, "BitBoards must be constructed by either hexadecimal strings of length 16 or binary strings of length 64"),
            Self::InvalidCastlingRights => write!(f, "Invalid castling rights in FEN string"),
            Self::InvalidFenString => write!(f, "Invalid FEN string"),
            Self::InvalidMove { color, chessmove } => write!(f, "Player `{color}` attempted to perform `{chessmove}`"),
            Self::NoPieceAtTile { tile }=> write!(f, "No piece at the specified tile {tile}")
        }
    }
}

impl Error for ChessError {
    //
}
