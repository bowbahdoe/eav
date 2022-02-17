use ux::{u20, u42};

#[derive(Debug, Copy, Clone)]
pub struct EntityId(u64);

impl EntityId {
    pub fn partition(&self) -> u20 {
        u20::new(((((2u64.pow(20) - 1) << 42) & self.0) >> 42) as u32)
    }

    pub fn is_temp(&self) -> bool {
        self.0 & (1 << 63) != 0
    }

    pub fn is_retracted(&self) -> bool {
        self.0 & (1 << 62) != 0
    }

    pub fn is_added(&self) -> bool {
        !self.is_retracted()
    }

    pub fn n(&self) -> u42 {
        u42::new((2u64.pow(42) - 1) & self.0)
    }

    pub fn to_u64(&self) -> u64 {
        self.0
    }

    pub fn to_i64(&self) -> i64 {
        self.0 as i64
    }

    pub fn from_u64(id: u64) -> EntityId {
        EntityId(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zero() {
        let zero_id = EntityId(0);
        assert_eq!(zero_id.n(), u42::new(0));
        assert_eq!(zero_id.partition(), u20::new(0));
        assert_eq!(zero_id.is_added(), true);
        assert_eq!(zero_id.is_retracted(), false);
        assert_eq!(zero_id.is_temp(), false);
    }
}
