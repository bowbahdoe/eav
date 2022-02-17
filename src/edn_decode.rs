use std::rc::Rc;
use edn_format as edn;
use edn_format::{Keyword, Symbol, Value};

pub struct DecodingError;

pub trait Decoder<T> {
    fn decode(&self, value: &edn::Value) -> Result<T, DecodingError>;
}

impl <F, T> Decoder<T> for F
   where F: Fn(&edn::Value) -> Result<T, DecodingError> {
    fn decode(&self, value: &edn::Value) -> Result<T, DecodingError> {
        self(value)
    }
}

pub fn symbol(value: &edn::Value) -> Result<&Symbol, DecodingError> {
    match value {
        edn::Value::Symbol(symbol) => Ok(symbol),
        _ => Err(DecodingError)
    }
}

pub fn simple_symbol(value: &edn::Value) -> Result<&Symbol, DecodingError> {
    let symbol = symbol(value)?;
    if symbol.namespace().is_some() {
        Err(DecodingError)
    }
    else {
        Ok(symbol)
    }
}

pub fn keyword(value: &edn::Value) -> Result<&Keyword, DecodingError> {
    match value {
        edn::Value::Keyword(keyword) => Ok(keyword),
        _ => Err(DecodingError)
    }
}

pub fn simple_keyword(value: &edn::Value) -> Result<&Keyword, DecodingError> {
    let keyword = keyword(value)?;
    if keyword.namespace().is_some() {
        Err(DecodingError)
    }
    else {
        Ok(keyword)
    }
}

pub fn vector<'a, T>(value: &'a edn::Value, value_decoder: Rc<dyn Decoder<T>>) -> Result<Vec<T>, DecodingError> {
    match value {
        edn::Value::Vector(vector) => {
            let mut values = Vec::with_capacity(vector.len());
            for value in vector {
                values.push(value_decoder.decode(value)?);
            }
            Ok(values)
        }
        _ => Err(DecodingError)
    }
}

pub fn list<'a, T>(value: &'a edn::Value, value_decoder:  Rc<dyn Decoder<T>>) -> Result<Vec<T>, DecodingError> {
    match value {
        edn::Value::List(vector) => {
            let mut values = Vec::with_capacity(vector.len());
            for value in vector {
                values.push(value_decoder.decode(value)?);
            }
            Ok(values)
        }
        _ => Err(DecodingError)
    }
}

pub fn one_of<T>(
    value: &edn::Value,
    decoder_a: Rc<dyn Decoder<T>>,
    decoder_b: Rc<dyn Decoder<T>>
) -> Result<T, DecodingError> {
    match decoder_a.decode(value) {
        Ok(result) => Ok(result),
        Err(_) => match decoder_b.decode(value) {
            Ok(result) => Ok(result),
            Err(_) => Err(DecodingError)
        }
    }
}

/*
pub fn sequence(value: &edn::Value, value_decoder:  Rc<dyn Decoder<T>>) -> Result<Vec<T>, DecodingError> {
    one_of(
        value,
        Rc::new(move |value| vector(value, value_decoder.clone())),
        Rc::new(move |value| list(value, value_decoder)),
    )
}
*/