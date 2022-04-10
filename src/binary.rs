use std::io::Write;
use bigdecimal::num_traits::AsPrimitive;
use edn_format::Value;

const NIL: u8 = 0x00000001;
const TRUE: u8 = 0x00000010;
const FALSE: u8 = 0x00000011;

const START_LIST: u8 = 0;
const START_VECTOR: u8 = 0;
const START_MAP: u8 = 0;
const END_COLL: u8 = 0;

fn edn_to_binary(edn: &edn_format::Value) -> Box<[u8]> {
    let mut buffer: Vec<u8> = vec![];

    let mut queue: Vec<&edn_format::Value> = vec![edn];
    loop {
        match queue.pop() {
            Some(value) => {
                match value {
                    Value::Nil => buffer.push(NIL),
                    Value::Boolean(true) => buffer.push(TRUE),
                    Value::Boolean(false) => buffer.push(FALSE),
                    Value::Character(c) => {
                        let x: u32 = (*c).into();
                    }
                    Value::String(s) => {
                        s.clone().into_boxed_str().into_boxed_bytes();
                    }
                    Value::Symbol(s) => {

                    }
                    Value::Keyword(k) => {

                    }
                    Value::Integer(i) => {
                        i.to_le_bytes();
                    }
                    Value::Float(f) => {
                        f.to_le_bytes();
                    }
                    Value::BigInt(bi) => {
                        bi.to_bytes_le();
                    }
                    Value::BigDec(bd) => {
                        let (bi, exp) = bd.clone().into_bigint_and_exponent();
                        let a = bi.to_bytes_le();
                        let b = exp.to_le_bytes();
                    }
                    Value::List(l) => {

                    }
                    Value::Vector(v) => {

                    }
                    Value::Map(m) => {

                    }
                    Value::Set(s) => {

                    }
                    Value::Inst(i) => {
                        let z = i.to_rfc3339();
                    }
                    Value::Uuid(u) => {
                        let a = u.as_bytes();
                    }
                    Value::TaggedElement(tag, element) => {

                    }
                }
            }
            None => {
                break;
            }
        }
    }
    buffer.into()
}