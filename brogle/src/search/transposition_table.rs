use brogle_core::{Move, ZobristKey};

use crate::search::Score;

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash, Default)]
pub enum NodeType {
    /// The score is exact.
    #[default]
    Pv,

    /// The score is less than alpha (upper bound).
    All,

    /// The score is greater than or equal to beta (lower bound).
    Cut,
}

impl NodeType {
    /// Creates a new [`NodeType`] based on the parameters as follows:
    ///
    /// ```text
    /// if score <= alpha:
    ///     UPPERBOUND
    /// else if score >= beta:
    ///     LOWERBOUND
    /// else:
    ///     EXACT
    /// ```
    pub fn new(score: Score, alpha: Score, beta: Score) -> Self {
        if score <= alpha {
            Self::All
        } else if score >= beta {
            Self::Cut
        } else {
            Self::Pv
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub struct TTableEntry {
    pub key: ZobristKey,
    pub depth: u32,
    pub bestmove: Move,
    pub score: Score,
    pub flag: NodeType,
}

impl TTableEntry {
    /// Determine whether the score in this entry can be used and, if so, return it.
    // Credit: https://github.com/sroelants/simbelmyne/blob/main/simbelmyne/src/transpositions.rs#L178
    pub fn try_score(&self, alpha: Score, beta: Score, depth: u32, ply: i32) -> Option<Score> {
        // If this entry came from a shallower search, we can't use it's score.
        if self.depth < depth {
            return None;
        }

        let absolute_score = self.score.absolute(ply);
        match self.flag {
            NodeType::Pv => Some(absolute_score),
            NodeType::All if absolute_score <= alpha => Some(absolute_score),
            NodeType::Cut if absolute_score >= beta => Some(absolute_score),
            _ => None,
        }
    }
}

/// Default size of the Transposition Table, in bytes
pub(crate) const DEFAULT_TTABLE_SIZE: usize = 16_777_216; // 16 mb

/// Transposition Table
#[derive(Debug)]
pub struct TTable(Vec<Option<TTableEntry>>);

impl TTable {
    /// Create a new [`TTable`] that is `size` bytes.
    ///
    /// Its size will be `size_of::<TTableEntry>() * capacity`
    pub fn new(size: usize) -> Self {
        Self::from_capacity(size / size_of::<TTableEntry>())
    }

    /// Create a new [`TTable`] that can hold `capacity` entries.
    pub fn from_capacity(capacity: usize) -> Self {
        Self(vec![None; capacity])
    }

    /// Clears the entries of this [`TTable`].
    pub fn clear(&mut self) {
        self.0.iter_mut().for_each(|entry| *entry = None);
    }

    /// Returns the size of this [`TTable`], in bytes.
    pub fn size(&self) -> usize {
        self.0.len() * size_of::<TTableEntry>()
    }

    /// Returns the number of `Some` entries in this [`TTable`].
    pub fn num_entries(&self) -> usize {
        self.0.iter().filter(|entry| entry.is_some()).count()
    }

    /// Map `key` to an index into this [`TTable`].
    pub fn index(&self, key: &ZobristKey) -> usize {
        // TODO: Enforce size as a power of two so you can use & instead of %
        key.inner() as usize % self.0.len()
    }

    /// Get the entry, without regards for whether it matches the provided key
    fn entry(&self, key: &ZobristKey) -> Option<&TTableEntry> {
        // We can safely index as we've initialized this ttable to be non-empty
        self.0[self.index(key)].as_ref()
    }

    /// Mutably get the entry, without regards for whether it matches the provided key
    fn entry_mut(&mut self, key: &ZobristKey) -> Option<&mut TTableEntry> {
        let index = self.index(key);
        self.0[index].as_mut()
    }

    /// Get the entry if and only if it matches the provided key
    pub fn get(&self, key: &ZobristKey) -> Option<&TTableEntry> {
        let entry = self.entry(key);
        if entry.is_some_and(|entry| &entry.key == key) {
            return entry;
        }
        None
    }

    /// Mutably get the entry if and only if it matches the provided key
    pub fn get_mut(&mut self, key: &ZobristKey) -> Option<&mut TTableEntry> {
        let entry = self.entry_mut(key);
        if entry.as_ref().is_some_and(|entry| &entry.key == key) {
            return entry;
        }
        None
    }

    /*
    pub fn update_flag(&mut self, key: &ZobristKey, flag: NodeType) {
        if let Some(entry) = self.get_mut(key) {
            entry.flag = flag;
        }
    }

    pub fn update_score(&mut self, key: &ZobristKey, new_score: i32) {
        if let Some(entry) = self.get_mut(key) {
            entry.score = new_score;
        }
    }
     */

    /// Store `entry` in the table at `entry.key`, overriding whatever was there.
    pub fn store(&mut self, entry: TTableEntry) {
        let index = self.index(&entry.key);
        self.0[index] = Some(entry);
    }

    /*
    /// Store `entry` in the table at `entry.key`, if the existing entry at `entry.key` is either `None` or has a lower `depth` than `entry`.
    pub fn store_if_greater_depth(&mut self, entry: TTableEntry) {
        if self
            .get(&entry.key)
            .is_some_and(|old_entry| old_entry.depth < entry.depth)
        {
            self.store(entry);
        }
    }

    /// Inserts `entry` in the table at `entry.key`, overriding whatever was there and returning a mutable reference to it.
    pub fn insert(&mut self, entry: TTableEntry) -> &mut TTableEntry {
        let index = self.index(&entry.key);
        self.0[index].insert(entry)
    }
     */
}

impl Default for TTable {
    fn default() -> Self {
        Self::new(DEFAULT_TTABLE_SIZE)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use brogle_core::*;

    #[test]
    fn test_ttable() {
        // Create two positions whose Zobrist keys are equal mod 2
        // TODO: Ensure that if Zobrist algorithm changes, the above statement remains true
        let startpos = Position::from_fen(FEN_STARTPOS).unwrap();
        let kiwipete = Position::from_fen(FEN_KIWIPETE).unwrap();

        // Create Zobrist keys for both positions
        let startpos_key = ZobristKey::new(&startpos);
        let kiwipete_key = ZobristKey::new(&kiwipete);

        // Create entries for both positions
        let startpos_entry = TTableEntry {
            key: startpos_key,
            bestmove: Move::illegal(),
            score: Score::DRAW,
            depth: 0,
            flag: NodeType::Pv,
        };

        let kiwipete_entry = TTableEntry {
            key: kiwipete_key,
            bestmove: Move::illegal(),
            score: Score::MATE,
            depth: 0,
            flag: NodeType::Pv,
        };

        // Create a TTable that can hold two elements.
        // This is important as both elements will need to map to the same index
        let mut tt = TTable::from_capacity(2);
        assert_eq!(
            tt.num_entries(),
            0,
            "TTable should initialize to being empty"
        );

        tt.store(startpos_entry.clone());
        assert_eq!(
            tt.num_entries(),
            1,
            "After storing one entry, TTable should only have 1 entry"
        );
        assert_eq!(
            tt.entry(&startpos_key),
            Some(&startpos_entry),
            "Getting an entry by key returns the appropriate entry"
        );

        tt.store(kiwipete_entry.clone());
        assert_eq!(tt.num_entries(), 1, "After storing another entry that overwrites a previous one, TTable should only have 1 entry");

        assert_ne!(
            tt.entry(&startpos_key),
            Some(&startpos_entry),
            "Cannot get an entry that has been overridden"
        );

        assert!(
            tt.get(&startpos_key).is_none(),
            "Cannot get an entry that has been overridden"
        );

        assert_eq!(
            tt.get(&kiwipete_key),
            Some(&kiwipete_entry),
            "Getting an entry by key returns the appropriate entry"
        );
    }
}
