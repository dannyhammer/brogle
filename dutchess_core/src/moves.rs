use std::fmt;

use crate::{PieceKind, Tile};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Move {
    from: Tile,
    to: Tile,
    promote: Option<PieceKind>,
}

impl Move {
    pub fn new(from: Tile, to: Tile, promote: Option<PieceKind>) -> Self {
        Self { from, to, promote }
    }

    pub fn from_indices(from: usize, to: usize) -> Result<Self, String> {
        let from = Tile::from_index(from)?;
        let to = Tile::from_index(to)?;
        Ok(Self::new(from, to, None))
    }

    pub fn from(&self) -> Tile {
        self.from
    }

    pub fn to(&self) -> Tile {
        self.to
    }

    pub fn promote(&self) -> Option<PieceKind> {
        self.promote
    }

    pub fn from_san(san: &str) -> Result<Self, String> {
        todo!()
    }

    /// Pure coordinate notation
    pub fn from_pcn(pcn: &str) -> Result<Self, String> {
        todo!()
    }

    pub fn from_uci(uci: &str) -> Result<Self, String> {
        let from = uci.get(0..2).ok_or(String::from("Invalid string length"))?;
        let to = uci.get(2..4).ok_or(String::from("Invalid string length"))?;

        let from = Tile::from_uci(from)?;
        let to = Tile::from_uci(to)?;

        let promote = if let Some(promote) = uci.get(5..5) {
            Some(PieceKind::from_str(promote)?)
        } else {
            None
        };

        Ok(Self::new(from, to, promote))
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(promote) = self.promote() {
            write!(f, "{}{}{}", self.from(), self.to(), promote)
        } else {
            write!(f, "{}{}", self.from(), self.to())
        }
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn test_from_uci() {
        assert_eq!(
            Move::from_uci("e2e4").unwrap(),
            Move::new(Tile::E2, Tile::E4, None)
        );
    }
}
