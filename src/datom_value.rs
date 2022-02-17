use edn_format::{Keyword, Symbol, Value};
use crate::entity_id::EntityId;
use bigdecimal::BigDecimal;
use chrono::{DateTime, FixedOffset};
use num_bigint::BigInt;
use uuid::Uuid;
use edn_format as edn;
use ordered_float::OrderedFloat;

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
    Instant(DateTime<FixedOffset>),
    Keyword(Box<Keyword>),
    Long(i64),
    Ref(EntityId),
    String(String),
    Symbol(Box<Symbol>),
    Tuple(Box<Tuple>),
    Uuid(Uuid),
    Uri(String),
    Bytes(Vec<u8>),
}

impl Into<edn::Value> for DatomValue {
    fn into(self) -> Value {
        match self {
            DatomValue::BigDec(bd) => edn::Value::BigDec(*bd),
            DatomValue::BigInt(bi) => edn::Value::BigInt(*bi),
            DatomValue::Boolean(b) => edn::Value::Boolean(b),
            DatomValue::Double(d) => edn::Value::Float(OrderedFloat(d)),
            DatomValue::Float(f) => edn::Value::Float(OrderedFloat(f as f64)),
            DatomValue::Instant(inst) => edn::Value::Inst(inst),
            DatomValue::Keyword(kw) => edn::Value::Keyword(*kw),
            DatomValue::Long(l) => edn::Value::Integer(l),
            DatomValue::Ref(r) => edn::Value::TaggedElement(
                edn::Symbol::from_namespace_and_name("eav", "ref"),
                Box::new(edn::Value::Integer(r.to_i64()))
            ),
            DatomValue::String(s) => edn::Value::String(s),
            DatomValue::Symbol(sym) => edn::Value::Symbol(*sym),
            DatomValue::Tuple(tuple) => match *tuple {
                Tuple::Tuple2(a, b) => edn::Value::Vector(vec![
                    a.into(), b.into()
                ]),
                Tuple::Tuple3(a, b, c) => edn::Value::Vector(vec![
                    a.into(), b.into(), c.into()
                ]),
                Tuple::Tuple4(a, b, c, d) => edn::Value::Vector(vec![
                    a.into(), b.into(), c.into(), d.into()
                ]),
                Tuple::Tuple5(a, b, c, d, e) => edn::Value::Vector(vec![
                    a.into(), b.into(), c.into(), d.into(), e.into()
                ]),
                Tuple::Tuple6(a, b, c, d, e, f) =>edn::Value::Vector(vec![
                    a.into(), b.into(), c.into(), d.into(), e.into(), f.into()
                ]),
                Tuple::Tuple7(a, b, c, d, e, f, g) => edn::Value::Vector(vec![
                    a.into(), b.into(), c.into(), d.into(), e.into(), f.into(), g.into()
                ]),
                Tuple::Tuple8(a, b, c, d, e, f, g, h) => edn::Value::Vector(vec![
                    a.into(), b.into(), c.into(), d.into(), e.into(), f.into(), g.into(), h.into()
                ])
            },
            DatomValue::Uuid(uuid) => edn::Value::Uuid(uuid),
            DatomValue::Uri(uri) => edn::Value::TaggedElement(
                edn::Symbol::from_namespace_and_name("eav", "uri"),
                Box::new(
                    edn::Value::String(uri)
                )
            ),
            DatomValue::Bytes(bytes) => edn::Value::List(bytes
                .into_iter()
                .map(|byte| u8::to_string(&byte))
                .map(|s| edn::Value::String(s))
                .collect()
            ),
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_datom_value_size() {
        assert_eq!(std::mem::size_of::<DatomValue>(), 32);
    }
}
