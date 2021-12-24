use crate::datom_value::DatomValue;
use crate::edn::Keyword;
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
    pub a: Keyword,
    pub v: DatomValue,
    pub tx: TransactionId,
    pub kind: DatomKind,
}
