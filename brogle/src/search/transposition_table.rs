use brogle_core::{Move, ZobristKey};

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
    pub fn new(score: i32, alpha: i32, beta: i32) -> Self {
        if score <= alpha {
            Self::All
        } else if score >= beta {
            Self::Cut
        } else {
            Self::Pv
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash, Default)]
pub struct TTableEntry {
    pub(crate) key: ZobristKey,
    pub(crate) depth: u32,
    pub(crate) bestmove: Move,
    pub(crate) score: i32,
    pub(crate) flag: NodeType,
}

/// Default size of the Transposition Table, in bytes
pub(crate) const DEFAULT_TTABLE_SIZE: usize = 1_048_576; // 1 mb

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

    /// Map `key` to an index into this [`TTable`].
    pub fn index(&self, key: &ZobristKey) -> usize {
        // TODO: Enforce size as a power of two so you can use & instead of %
        key.inner() as usize % self.0.len()
    }

    /// Get the entry, without regards for whether it matches the provided key
    fn get_entry(&self, key: &ZobristKey) -> Option<&TTableEntry> {
        // We can safely index as we've initialized this ttable to be non-empty
        self.0[self.index(key)].as_ref()
    }

    /// Mutably get the entry, without regards for whether it matches the provided key
    fn get_entry_mut(&mut self, key: &ZobristKey) -> Option<&mut TTableEntry> {
        let index = self.index(key);
        self.0[index].as_mut()
    }

    /// Get the entry if and only if it matches the provided key
    pub fn get(&self, key: &ZobristKey) -> Option<&TTableEntry> {
        let entry = self.get_entry(key);
        if entry.is_some_and(|entry| &entry.key == key) {
            return entry;
        }
        None
    }

    /// Mutably get the entry if and only if it matches the provided key
    pub fn get_mut(&mut self, key: &ZobristKey) -> Option<&mut TTableEntry> {
        let entry = self.get_entry_mut(key);
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
