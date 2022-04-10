use crate::datom::{Attribute, Datom, DatomKind};
use crate::datom_value::DatomValue;
use edn_format::Keyword;
use crate::entity_id::EntityId;

mod byte_string;
mod datom;
mod datom_value;
mod entity_id;
mod query;
mod storage;
mod transaction_id;
mod edn_decode;
mod binary;

use std::time;
use edn_format as edn;
use serde::ser::Serialize;

#[derive(Serialize, Deserialize, Debug)]
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let datom = Datom {
        e: EntityId::from_u64(123),
        a: Attribute::from(Keyword::from_namespace_and_name("user", "age")),
        v: DatomValue::Long(45),
        tx: transaction_id::TransactionId::from_u64(123),
        kind: DatomKind::Assertion,
    };

    println!("{}", Keyword::from_namespace_and_name("user", "age"));
    println!("{}", <Datom as Into<edn::Value>>::into(datom));

    println!(
        "{}",
        edn::emit_str(
            &edn::parse_str_with_options("[{:person/name \"A\nnna\" \
            \n;;here is this\n\
            :person/email \"anna@example.com\"}]",
            edn::ParserOptions::default() ).unwrap()
        )
    );

    println!(
        "{:?}",
        edn::parse_str(
            "{:mvn/repos {\"jcenter\" {:url       \"https://jcenter.bintray.com\"
            :snapshots false
                :releases  {:checksum :fail :update :always}}
\"maven\"   {:url      \"https://repo1.maven.org/maven2/\"
:releases {:checksum :fail :update :always}}
\"clojars\" {:url      \"https://repo.clojars.org\"
:releases {:checksum :fail :update :always}}}

:deps      {org.clojure/clojure                             {:mvn/version \"1.10.1\"}
org.clojure/clojurescript                       {:mvn/version \"1.10.844\"}}}"
        )
        .unwrap()
    )
}
