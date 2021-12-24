use crate::datom::{Datom, DatomKind};
use crate::datom_value::DatomValue;
use crate::edn::Keyword;
use crate::entity_id::EntityId;

mod byte_string;
mod datom;
mod datom_value;
mod edn;
mod entity_id;
mod functions;
mod query;
mod storage;
mod transaction_id;

use std::time;

fn main() {
    let datom = Datom {
        e: EntityId::from_u64(123),
        a: Keyword::from_namespace_and_name("user", "age"),
        v: DatomValue::Long(45),
        tx: transaction_id::TransactionId::from_u64(123),
        kind: DatomKind::Addition,
    };

    println!("{}", Keyword::from_namespace_and_name("user", "age"));
    println!("{:?}", datom);
}
