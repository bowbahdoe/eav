use crate::edn::Keyword;
use crate::entity_id::EntityId;
use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use std::time;
use uuid::Uuid;

#[derive(Debug, Clone)]
pub enum Tuple {
    Tuple2(DatomValue, DatomValue),
    Tuple3(DatomValue, DatomValue, DatomValue),
    Tuple4(DatomValue, DatomValue, DatomValue, DatomValue),
    Tuple5(DatomValue, DatomValue, DatomValue, DatomValue, DatomValue),
    Tuple6(
        DatomValue,
        DatomValue,
        DatomValue,
        DatomValue,
        DatomValue,
        DatomValue,
    ),
    Tuple7(
        DatomValue,
        DatomValue,
        DatomValue,
        DatomValue,
        DatomValue,
        DatomValue,
    ),
    Tuple8(
        DatomValue,
        DatomValue,
        DatomValue,
        DatomValue,
        DatomValue,
        DatomValue,
        DatomValue,
    ),
}

/// Note: boxing here is to lower the size on stack of a datom value
/// in the more common cases. As is this comes out to 32 bytes
#[derive(Debug, Clone)]
pub enum DatomValue {
    BigDec(Box<BigDecimal>),
    BigInt(Box<BigInt>),
    Boolean(bool),
    Double(f64),
    Float(f32),
    Instant(time::Instant),
    Keyword(Box<Keyword>),
    Long(i64),
    Ref(EntityId),
    String(String),
    Symbol(String),
    Tuple(Box<Tuple>),
    Uuid(Uuid),
    Uri(String),
    Bytes(Vec<u8>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_datom_value_size() {
        assert_eq!(std::mem::size_of::<DatomValue>(), 32);
    }
}
