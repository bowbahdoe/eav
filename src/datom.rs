use std::collections::BTreeMap;
use std::iter::FromIterator;
use crate::datom_value::{DatomValue, Tuple};
use edn_format as edn;
use num_bigint::BigInt;
use ordered_float::OrderedFloat;
use crate::entity_id::EntityId;
use crate::transaction_id::TransactionId;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum DatomKind {
    Addition,
    Retraction,
}

#[derive(Debug)]
pub struct Datom {
    pub e: EntityId,
    pub a: edn::Keyword,
    pub v: DatomValue,
    pub tx: TransactionId,
    pub kind: DatomKind,
}

impl Into<edn::Value> for Datom {
    fn into(self) -> edn::Value {
        edn::Value::TaggedElement(
            edn::Symbol::from_namespace_and_name("eav", "datom"),
            Box::new(edn::Value::Vector(vec![
                edn::Value::from(self.e.to_i64()),
                edn::Value::from(self.a.clone()),
                self.v.into(),
                edn::Value::from(BigInt::from(self.tx.to_u64()) ),
                match self.kind {
                    DatomKind::Addition => edn::Value::from(true),
                    DatomKind::Retraction => edn::Value::from(false)
                }
            ]))
        )
    }
}
