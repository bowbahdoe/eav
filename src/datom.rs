use std::collections::BTreeMap;
use std::iter::FromIterator;
use crate::datom_value::{DatomValue, Tuple};
use edn_format as edn;
use edn_format::Keyword;
use num_bigint::BigInt;
use ordered_float::OrderedFloat;
use crate::entity_id::EntityId;
use crate::transaction_id::TransactionId;

/// Datoms can either be assertions of a fact or an explicit retraction.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum DatomKind {
    Assertion,
    Retraction,
}

#[derive(Debug)]
pub struct Datom {
    pub e: EntityId,
    pub a: Attribute,
    pub v: DatomValue,
    pub tx: TransactionId,
    pub kind: DatomKind,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Attribute {
    namespace: Option<String>,
    name: String
}

impl From<edn::Keyword> for Attribute {
    fn from(kw: Keyword) -> Self {
        Attribute {
            namespace: kw.namespace().map(|ns| ns.to_owned()),
            name: kw.name().to_owned()
        }
    }
}

impl Into<edn::Keyword> for Attribute {
    fn into(self) -> Keyword {
        match self.namespace {
            Some(ns) => Keyword::from_namespace_and_name(&ns, &self.name),
            None => Keyword::from_name(&self.name)
        }
    }
}

impl Into<edn::Value> for Datom {
    fn into(self) -> edn::Value {
        let attr: edn::Keyword = self.a.into();
        edn::Value::TaggedElement(
            edn::Symbol::from_namespace_and_name("eav", "datom"),
            Box::new(edn::Value::Vector(vec![
                edn::Value::from(self.e.to_i64()),
                edn::Value::from(attr),
                self.v.into(),
                edn::Value::from(BigInt::from(self.tx.to_u64()) ),
                match self.kind {
                    DatomKind::Assertion => edn::Value::from(true),
                    DatomKind::Retraction => edn::Value::from(false)
                }
            ]))
        )
    }
}
