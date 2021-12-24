#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct TransactionId(u64);

impl TransactionId {
    pub fn from_u64(id: u64) -> TransactionId {
        TransactionId(id)
    }

    pub fn to_u64(&self) -> u64 {
        self.0
    }
}
