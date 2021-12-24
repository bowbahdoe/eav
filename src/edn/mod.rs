use chrono::format;
use chrono::FixedOffset;
use internship;
use internship::IStr;
use itertools::Itertools;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{Display, Formatter};
use thiserror::Error;
use uuid::Uuid;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Keyword {
    namespace: Option<IStr>,
    name: IStr,
}

impl Keyword {
    pub fn namespace(&self) -> Option<String> {
        self.namespace.as_ref().map(|s| s.as_str().to_owned())
    }

    pub fn name(&self) -> String {
        self.name.as_str().to_owned()
    }

    pub fn from_name(name: &str) -> Keyword {
        Keyword {
            namespace: Option::None,
            name: IStr::new(&name),
        }
    }

    pub fn from_namespace_and_name(namespace: &str, name: &str) -> Keyword {
        Keyword {
            namespace: Option::Some(IStr::new(namespace)),
            name: IStr::new(name),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.namespace {
            Some(ns) => write!(f, ":{}/{}", ns, self.name),
            None => write!(f, ":{}", self.name),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Symbol {
    namespace: Option<IStr>,
    name: IStr,
}

impl Symbol {
    pub fn namespace(&self) -> Option<String> {
        self.namespace.as_ref().map(|s| s.as_str().to_owned())
    }

    pub fn name(&self) -> String {
        self.name.as_str().to_owned()
    }

    pub fn from_name(name: &str) -> Symbol {
        Symbol {
            namespace: Option::None,
            name: IStr::new(&name),
        }
    }

    pub fn from_namespace_and_name(namespace: &str, name: &str) -> Symbol {
        Symbol {
            namespace: Option::Some(IStr::new(namespace)),
            name: IStr::new(name),
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.namespace {
            Some(ns) => write!(f, "{}/{}", ns, self.name),
            None => write!(f, "{}", self.name),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    String(String),
    Character(char),
    Symbol(Symbol),
    Keyword(Keyword),
    Integer(i64),
    Float(f64),
    List(Vec<Value>),
    Vector(Vec<Value>),
    Map(Vec<(Value, Value)>),
    Set(Vec<Value>),
    Boolean(bool),
    Inst(chrono::DateTime<FixedOffset>),
    Uuid(Uuid),
    TaggedElement(Symbol, Box<Value>),
}

#[derive(Debug, Error, PartialEq)]
enum ParserError {
    #[error("The input was entirely blank")]
    EmptyInput,

    #[error("Unexpected end of input")]
    UnexpectedEndOfInput,

    #[error("Invalid escape sequence in string")]
    InvalidStringEscape,

    #[error("Invalid UTF-8")]
    InvalidUtf8 { chars: Vec<char> },

    #[error("Duplicate value in a set")]
    DuplicateValueInSet { value: Value },

    #[error("Duplicate key in a map")]
    DuplicateKeyInMap { value: Value },

    #[error("Only symbols can be used as tags")]
    InvalidElementForTag { value: Value },

    #[error("Invalid character specification")]
    InvalidCharacterSpecification,

    #[error("Unexpected character")]
    UnexpectedCharacter(char),

    #[error("Invalid keyword")]
    InvalidKeyword,

    #[error("Invalid Symbol")]
    InvalidSymbol,

    #[error("Map must have an even number of elements")]
    OddNumberOfMapElements,

    #[error("Error parsing #inst")]
    InvalidInst(Option<chrono::format::ParseError>),

    #[error("Error parsing #uuid")]
    InvalidUuid(Option<uuid::Error>),

    #[error("Cannot have slash at the beginning of symbol")]
    CannotHaveSlashAtBeginningOfSymbol,

    #[error("Cannot have slash at the end of symbol")]
    CannotHaveSlashAtEndOfSymbol,

    #[error("Cannot have more than one slash in a symbol")]
    CannotHaveMoreThanOneSlashInSymbol,

    #[error("Unexpected Extra Input")]
    ExtraInput {
        parsed_value: Value,
        extra_input: Vec<char>,
    },
}

struct ParserSuccess<'a> {
    remaining_input: &'a [char],
    value: Value,
}

#[derive(Debug)]
enum ParserState {
    Begin,
    ParsingList {
        values_so_far: Vec<Value>,
    },
    ParsingVector {
        values_so_far: Vec<Value>,
    },
    ParsingMap {
        values_so_far: Vec<Value>,
    },
    ParsingSet {
        values_so_far: Vec<Value>,
    },
    ParsingSymbol {
        characters_before_a_slash: Vec<char>,
        characters_after_a_slash: Vec<char>,
        saw_slash: bool,
    }, // Decide after parsing symbol if it is true, false, or nil
    ParsingNumeric {
        characters_so_far: Vec<char>,
    },
    ParsingString {
        built_up: String,
    },
    ParsingCharacter,
    SelectingDispatch,
}

/// Commas are considered whitespace for EDN
fn is_whitespace(c: char) -> bool {
    c.is_whitespace() || c == ','
}

fn is_allowed_anywhere_symbol_character(c: char) -> bool {
    c == '.'
        || c == '*'
        || c == '+'
        || c == '!'
        || c == '-'
        || c == '_'
        || c == '?'
        || c == '$'
        || c == '%'
        || c == '&'
        || c == '='
        || c == '<'
        || c == '>'
        || c.is_alphabetic()
}

fn is_allowed_symbol_after_first_character(c: char) -> bool {
    is_allowed_anywhere_symbol_character(c) || c.is_numeric()
}

fn equal(v1: &Value, v2: &Value) -> bool {
    println!("{:?} {:?}", v1, v2);
    match (v1, v2) {
        // nil, booleans, strings, characters, and symbols
        // are equal to values of the same type with the same edn representation
        (Value::Nil, Value::Nil) => true,
        (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
        (Value::String(s1), Value::String(s2)) => s1 == s2,
        (Value::Character(c1), Value::Character(c2)) => c1 == c2,
        (Value::Symbol(s1), Value::Symbol(s2)) => s1 == s2,
        (Value::Keyword(k1), Value::Keyword(k2)) => k1 == k2,

        // integers and floating point numbers should be considered equal to values only of the
        // same magnitude, type, and precision. Comingling numeric types and precision in
        // map/set key/elements, or constituents therein, is not advised.
        (Value::Float(f1), Value::Float(f2)) => f1 == f2,
        (Value::Integer(i1), Value::Integer(i2)) => i1 == i2,

        // sequences (lists and vectors) are equal to other sequences whose count
        // of elements is the same, and for which each corresponding pair of
        // elements (by ordinal) is equal.
        (Value::List(vals1) | Value::Vector(vals1), Value::List(vals2) | Value::Vector(vals2)) => {
            if vals1.len() != vals2.len() {
                false
            } else {
                vals1
                    .iter()
                    .zip(vals2.iter())
                    .fold(true, |all_same, (v1, v2)| all_same && equal(v1, v2))
            }
        }

        // sets are equal if they have the same count of elements and,
        // for every element in one set, an equal element is in the other.
        (Value::Set(vals1), Value::Set(vals2)) => {
            if vals1.len() != vals2.len() {
                false
            } else {
                for v1 in vals1 {
                    let mut found = false;
                    for v2 in vals2 {
                        if equal(v1, v2) {
                            found = true;
                        }
                    }
                    if !found {
                        return false;
                    }
                }

                return true;
            }
        }

        // maps are equal if they have the same number of entries,
        // and for every key/value entry in one map an equal key is present
        // and mapped to an equal value in the other.
        (Value::Map(entries1), Value::Map(entries2)) => {
            if entries1.len() != entries2.len() {
                false
            } else {
                for (v1, k1) in entries1 {
                    let mut found = false;
                    for (v2, k2) in entries2 {
                        if equal(v1, v2) && equal(k1, k2) {
                            found = true;
                        }
                    }
                    if !found {
                        return false;
                    }
                }

                return true;
            }
        }

        // tagged elements must define their own equality semantics.
        // #uuid elements are equal if their canonic representations are equal.
        // #inst elements are equal if their representation strings designate
        // the same timestamp per RFC-3339.
        (Value::Uuid(uuid1), Value::Uuid(uuid2)) => uuid1 == uuid2,
        (Value::Inst(dt1), Value::Inst(dt2)) => dt1 == dt2,
        (Value::TaggedElement(tag1, value1), Value::TaggedElement(tag2, value2)) => {
            equal(&Value::Symbol(tag1.clone()), &Value::Symbol(tag2.clone()))
                && equal(value1, value2)
        }

        _ => false,
    }
}

/// Likely *very* suboptimal parsing. Focus for now should be getting actually correct
/// results, since there is no good edn library for rust.
fn parse_helper(s: &[char], mut parser_state: ParserState) -> Result<ParserSuccess, ParserError> {
    println!("{:?}", parser_state);
    println!("{:?}", s);
    println!();
    match parser_state {
        ParserState::Begin => {
            if s.len() == 0 {
                Err(ParserError::EmptyInput)
            } else if is_whitespace(s[0]) {
                parse_helper(&s[1..], ParserState::Begin)
            } else if s[0] == '(' {
                parse_helper(
                    &s[1..],
                    ParserState::ParsingList {
                        values_so_far: vec![],
                    },
                )
            } else if s[0] == '[' {
                parse_helper(
                    &s[1..],
                    ParserState::ParsingVector {
                        values_so_far: vec![],
                    },
                )
            } else if s[0] == '{' {
                parse_helper(
                    &s[1..],
                    ParserState::ParsingMap {
                        values_so_far: vec![],
                    },
                )
            } else if s[0] == '"' {
                parse_helper(
                    &s[1..],
                    ParserState::ParsingString {
                        built_up: "".to_string(),
                    },
                )
            } else if s[0] == ':' {
                // For parsing a keyword, we can just fall back on the logic for parsing a symbol
                // **somewhat** of a hack and it means we need to move the logic for converting
                // symbols for true, false, and nil higher up and the error messages might
                // end up strange but i am okay with that.
                let ParserSuccess {
                    remaining_input,
                    value,
                } = parse_helper(&s[1..], ParserState::Begin)?;
                if let Value::Symbol(symbol) = value {
                    Ok(ParserSuccess {
                        remaining_input,
                        value: Value::Keyword(Keyword {
                            namespace: symbol.namespace,
                            name: symbol.name,
                        }),
                    })
                } else {
                    Err(ParserError::InvalidKeyword)
                }
            } else if s[0] == '\\' {
                parse_helper(
                    &s[1..],
                    ParserState::ParsingCharacter,
                )
            } else if s[0] == '#' {
                parse_helper(&s[1..], ParserState::SelectingDispatch)
            } else if is_allowed_anywhere_symbol_character(s[0]) || s[0] == '/' {
                parse_helper(
                    &s,
                    ParserState::ParsingSymbol {
                        characters_before_a_slash: vec![],
                        characters_after_a_slash: vec![],
                        saw_slash: false,
                    },
                )
            } else if s[0].is_numeric() {
                parse_helper(
                    &s[1..],
                    ParserState::ParsingNumeric {
                        characters_so_far: vec![s[0]],
                    },
                )
            } else {
                Err(ParserError::UnexpectedCharacter(s[0]))
            }
        }

        ParserState::ParsingList { mut values_so_far } => {
            if s.len() == 0 {
                Err(ParserError::UnexpectedEndOfInput)
            } else if is_whitespace(s[0]) {
                parse_helper(&s[1..], ParserState::ParsingList { values_so_far })
            } else if s[0] == ')' {
                Ok(ParserSuccess {
                    remaining_input: &s[1..],
                    value: Value::List(values_so_far),
                })
            } else {
                let ParserSuccess {
                    remaining_input,
                    value,
                } = parse_helper(s, ParserState::Begin)?;
                values_so_far.push(value);
                parse_helper(remaining_input, ParserState::ParsingList { values_so_far })
            }
        }

        // Almost total duplicate of ParsingList
        ParserState::ParsingVector { mut values_so_far } => {
            if s.len() == 0 {
                Err(ParserError::UnexpectedEndOfInput)
            } else if is_whitespace(s[0]) {
                parse_helper(&s[1..], ParserState::ParsingVector { values_so_far })
            } else if s[0] == ']' {
                Ok(ParserSuccess {
                    remaining_input: &s[1..],
                    value: Value::Vector(values_so_far),
                })
            } else {
                let ParserSuccess {
                    remaining_input,
                    value,
                } = parse_helper(s, ParserState::Begin)?;
                values_so_far.push(value);
                parse_helper(
                    remaining_input,
                    ParserState::ParsingVector { values_so_far },
                )
            }
        }

        ParserState::ParsingMap { mut values_so_far } => {
            if s.len() == 0 {
                Err(ParserError::UnexpectedEndOfInput)
            } else if is_whitespace(s[0]) {
                parse_helper(&s[1..], ParserState::ParsingMap { values_so_far })
            } else if s[0] == '}' {
                if values_so_far.len() % 2 != 0 {
                    Err(ParserError::OddNumberOfMapElements)
                } else {
                    // I'm confident there has to be a better way to do this
                    let entries: Vec<(Value, Value)> = values_so_far
                        .into_iter()
                        .batching(|it| match it.next() {
                            None => None,
                            Some(x) => match it.next() {
                                None => None,
                                Some(y) => Some((x, y)),
                            },
                        })
                        .collect();

                    // Floats can't be put in a hashset, so we need to get creative
                    for (k1, _) in entries.iter() {
                        let mut count = 0;
                        for (k2, _) in entries.iter() {
                            if equal(k1, k2) {
                                count += 1;
                            }
                        }
                        if count > 1 {
                            return Err(ParserError::DuplicateKeyInMap { value: k1.clone() });
                        }
                    }
                    let value = Value::Map(entries);

                    Ok(ParserSuccess {
                        remaining_input: &s[1..],
                        value,
                    })
                }
            } else {
                let ParserSuccess {
                    remaining_input,
                    value,
                } = parse_helper(s, ParserState::Begin)?;
                values_so_far.push(value);
                parse_helper(remaining_input, ParserState::ParsingMap { values_so_far })
            }
        }

        ParserState::ParsingSet { mut values_so_far } => {
            if s.len() == 0 {
                Err(ParserError::UnexpectedEndOfInput)
            } else if is_whitespace(s[0]) {
                parse_helper(&s[1..], ParserState::ParsingSet { values_so_far })
            } else if s[0] == '}' {
                for v1 in values_so_far.iter() {
                    let mut count = 0;
                    for v2 in values_so_far.iter() {
                        if equal(v1, v2) {
                            count += 1;
                        }
                    }
                    if count > 1 {
                        return Err(ParserError::DuplicateValueInSet { value: v1.clone() });
                    }
                }
                Ok(ParserSuccess {
                    remaining_input: &s[1..],
                    value: Value::Set(values_so_far),
                })
            } else {
                let ParserSuccess {
                    remaining_input,
                    value,
                } = parse_helper(s, ParserState::Begin)?;
                values_so_far.push(value);
                parse_helper(remaining_input, ParserState::ParsingSet { values_so_far })
            }
        }

        ParserState::ParsingSymbol {
            mut characters_before_a_slash,
            mut characters_after_a_slash,
            saw_slash,
        } => {
            if s.is_empty() {
                if characters_before_a_slash.is_empty() {
                    Err(ParserError::UnexpectedEndOfInput)
                } else if characters_after_a_slash.is_empty() {
                    if saw_slash {
                        Err(ParserError::UnexpectedEndOfInput)
                    } else {
                        let name: String = characters_before_a_slash.into_iter().collect();
                        Ok(ParserSuccess {
                            remaining_input: s,
                            value: Value::Symbol(Symbol::from_name(&name)),
                        })
                    }
                } else {
                    let namespace: String = characters_before_a_slash.into_iter().collect();
                    let name: String = characters_after_a_slash.into_iter().collect();
                    Ok(ParserSuccess {
                        remaining_input: s,
                        value: Value::Symbol(Symbol::from_namespace_and_name(&namespace, &name)),
                    })
                }
            } else {
                if characters_before_a_slash.is_empty() && !saw_slash {
                    if is_allowed_anywhere_symbol_character(s[0]) {
                        characters_before_a_slash.push(s[0]);
                        parse_helper(
                            &s[1..],
                            ParserState::ParsingSymbol {
                                characters_before_a_slash,
                                characters_after_a_slash,
                                saw_slash,
                            },
                        )
                    } else if s[0] == '/' {
                        if s.len() > 1 && is_allowed_symbol_after_first_character(s[1]) {
                            Err(ParserError::CannotHaveSlashAtBeginningOfSymbol)
                        } else {
                            Ok(ParserSuccess {
                                remaining_input: &s[1..],
                                value: Value::Symbol(Symbol::from_name("/")),
                            })
                        }
                    } else {
                        Err(ParserError::UnexpectedCharacter(s[0]))
                    }
                } else if !saw_slash {
                    if is_allowed_symbol_after_first_character(s[0]) {
                        characters_before_a_slash.push(s[0]);
                        parse_helper(
                            &s[1..],
                            ParserState::ParsingSymbol {
                                characters_before_a_slash,
                                characters_after_a_slash,
                                saw_slash,
                            },
                        )
                    } else if s[0] == '/' {
                        if s.len() == 1
                            || (s.len() > 1 && !is_allowed_symbol_after_first_character(s[1]))
                        {
                            Err(ParserError::CannotHaveSlashAtEndOfSymbol)
                        } else {
                            parse_helper(
                                &s[1..],
                                ParserState::ParsingSymbol {
                                    characters_before_a_slash,
                                    characters_after_a_slash,
                                    saw_slash: true,
                                },
                            )
                        }
                    } else {
                        let name: String = characters_before_a_slash.into_iter().collect();
                        Ok(ParserSuccess {
                            remaining_input: s,
                            value: Value::Symbol(Symbol::from_name(&name)),
                        })
                    }
                } else {
                    if (characters_after_a_slash.is_empty()
                        && is_allowed_anywhere_symbol_character(s[0]))
                        || is_allowed_symbol_after_first_character(s[0])
                    {
                        characters_after_a_slash.push(s[0]);
                        parse_helper(
                            &s[1..],
                            ParserState::ParsingSymbol {
                                characters_before_a_slash,
                                characters_after_a_slash,
                                saw_slash,
                            },
                        )
                    } else if s[0] == '/' {
                        Err(ParserError::CannotHaveMoreThanOneSlashInSymbol)
                    } else {
                        let namespace: String = characters_before_a_slash.into_iter().collect();
                        let name: String = characters_after_a_slash.into_iter().collect();
                        Ok(ParserSuccess {
                            remaining_input: s,
                            value: Value::Symbol(Symbol::from_namespace_and_name(
                                &namespace, &name,
                            )),
                        })
                    }
                }
            }
        }

        ParserState::ParsingString { mut built_up } => {
            if s.len() == 0 {
                Err(ParserError::UnexpectedEndOfInput)
            } else if s[0] == '"' {
                Ok(ParserSuccess {
                    remaining_input: &s[1..],
                    value: Value::String(built_up),
                })
            } else if s[0] == '\\' {
                if s.len() == 1 {
                    Err(ParserError::InvalidStringEscape)
                } else {
                    match s[1] {
                        't' => built_up.push('\t'),
                        'r' => built_up.push('\r'),
                        'n' => built_up.push('\n'),

                        '\\' => built_up.push('\\'),
                        '"' => built_up.push('"'),
                        'b' => built_up.push('b'),
                        'f' => built_up.push('f'),
                        'u' => {
                            return if s.len() >= 5 {
                                let str: String = s[2..6].into_iter().map(|c| *c).collect();
                                let unicode = u32::from_str_radix(&str, 16)
                                    .map_err(|_| ParserError::InvalidStringEscape)?;
                                match char::from_u32(unicode) {
                                    None => return Err(ParserError::InvalidStringEscape),
                                    Some(c) => built_up.push(c),
                                }
                                parse_helper(&s[6..], ParserState::ParsingString { built_up })
                            } else {
                                Err(ParserError::InvalidStringEscape)
                            }
                        }
                        _ => return Err(ParserError::InvalidStringEscape),
                    }
                    parse_helper(&s[2..], ParserState::ParsingString { built_up })
                }
            } else {
                built_up.push(s[0]);
                parse_helper(&s[1..], ParserState::ParsingString { built_up })
            }
        }
        /*

        ParserState::ParsingNumeric { .. } => {}*/
        ParserState::SelectingDispatch => {
            if s.len() == 0 {
                Err(ParserError::UnexpectedEndOfInput)
            } else if s[0] == '_' {
                // Drop the next form. Still error if that form is malformed
                let ParserSuccess {
                    remaining_input, ..
                } = parse_helper(&s[1..], ParserState::Begin)?;
                parse_helper(remaining_input, ParserState::Begin)
            } else if s[0] == '{' {
                parse_helper(
                    &s[1..],
                    ParserState::ParsingSet {
                        values_so_far: vec![],
                    },
                )
            } else {
                // We expect to read a symbol next and we will associate that symbol as the tag of
                // the following element
                let ParserSuccess {
                    remaining_input,
                    value,
                } = parse_helper(&s, ParserState::Begin)?;
                match value {
                    Value::Symbol(symbol) => {
                        let next_success = parse_helper(remaining_input, ParserState::Begin)?;

                        // Handle builtin #inst
                        if symbol.namespace == None && symbol.name == "inst" {
                            if let Value::String(timestamp) = next_success.value {
                                let datetime = chrono::DateTime::parse_from_rfc3339(&timestamp)
                                    .map_err(|parse_error| {
                                        ParserError::InvalidInst(Some(parse_error))
                                    })?;
                                Ok(ParserSuccess {
                                    remaining_input: next_success.remaining_input,
                                    value: Value::Inst(datetime),
                                })
                            } else {
                                Err(ParserError::InvalidInst(None))
                            }
                        }
                        // Handle builtin #uuid
                        else if symbol.namespace == None && symbol.name == "uuid" {
                            if let Value::String(uuid_str) = next_success.value {
                                let uuid = Uuid::parse_str(&uuid_str).map_err(|parse_error| {
                                    ParserError::InvalidUuid(Some(parse_error))
                                })?;
                                Ok(ParserSuccess {
                                    remaining_input: next_success.remaining_input,
                                    value: Value::Uuid(uuid),
                                })
                            } else {
                                Err(ParserError::InvalidUuid(None))
                            }
                        }
                        // Everything else becomes a generic TaggedElement
                        else {
                            Ok(ParserSuccess {
                                remaining_input: next_success.remaining_input,
                                value: Value::TaggedElement(symbol, Box::new(next_success.value)),
                            })
                        }
                    }
                    _ => Err(ParserError::InvalidElementForTag { value }),
                }
            }
        }

        ParserState::ParsingCharacter => {
            let ParserSuccess {
                remaining_input,
                value
            } = parse_helper(s, ParserState::Begin)?;

            if let Value::Symbol(symbol) = value {
                if symbol.namespace == None {
                    if symbol.name.len() == 1 {
                        Ok(ParserSuccess {
                            remaining_input,
                            value: Value::Character(symbol.name.chars().next().expect(
                                "Asserted that this string has at least one character."
                            ))
                        })
                    }
                    else {
                        match &symbol.name.as_str() {
                            &"newline" => Ok(ParserSuccess {
                                remaining_input,
                                value: Value::Character('\n')
                            }),
                            &"return" => Ok(ParserSuccess {
                                remaining_input,
                                value: Value::Character('\r')
                            }),
                            &"space" => Ok(ParserSuccess {
                                remaining_input,
                                value: Value::Character(' ')
                            }),
                            &"tab" => Ok(ParserSuccess {
                                remaining_input,
                                value: Value::Character('\t')
                            }),
                            _ => Err(ParserError::InvalidCharacterSpecification)
                        }
                    }
                }
                else {
                    Err(ParserError::InvalidCharacterSpecification)
                }
            } else {
                Err(ParserError::InvalidCharacterSpecification)
            }
        }

        _ => Err(ParserError::InvalidCharacterSpecification)
    }
}

/// Crawls the tree mutably to avoid pointless allocations
fn replace_nil_false_true(value: &mut Value) {
    match value {
        Value::Symbol(symbol) => {
            if symbol.namespace == None {
                if symbol.name == "true" {
                    *value = Value::Boolean(true)
                } else if symbol.name == "false" {
                    *value = Value::Boolean(false)
                } else if symbol.name == "nil" {
                    *value = Value::Nil
                }
            }
        }
        Value::List(elements) => {
            for element in elements.iter_mut() {
                replace_nil_false_true(element);
            }
        }
        Value::Vector(elements) => {
            for element in elements.iter_mut() {
                replace_nil_false_true(element);
            }
        }
        Value::Map(entries) => {
            for (k, v) in entries.iter_mut() {
                replace_nil_false_true(k);
                replace_nil_false_true(v);
            }
        }
        Value::Set(elements) => {
            for element in elements.iter_mut() {
                replace_nil_false_true(element);
            }
        }
        Value::TaggedElement(_, val) => replace_nil_false_true(val),
        _ => {}
    }
}

// Parse EDN from the given input string
fn parse(s: &str) -> Result<Value, ParserError> {
    let chars: Vec<char> = s.chars().collect();
    // TODO: pre-strip comments
    let ParserSuccess {
        remaining_input,
        mut value,
    } = parse_helper(&chars, ParserState::Begin)?;
    for c in remaining_input {
        if !is_whitespace(*c) {
            return Err(ParserError::ExtraInput {
                parsed_value: value,
                extra_input: remaining_input.to_vec(),
            });
        }
    }
    // previous step interprets nil, false, and true as symbols
    replace_nil_false_true(&mut value);
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::DateTime;

    #[test]
    fn test_display_symbol() {
        assert_eq!(format!("{}", Symbol::from_name("abc")), "abc");
        assert_eq!(
            format!("{}", Symbol::from_namespace_and_name("abc", "def")),
            "abc/def"
        );
    }

    #[test]
    fn test_display_keyword() {
        assert_eq!(format!("{}", Keyword::from_name("abc")), ":abc");
        assert_eq!(
            format!("{}", Keyword::from_namespace_and_name("abc", "def")),
            ":abc/def"
        );
    }

    #[test]
    fn test_parsing_empty_list() {
        assert_eq!(Value::List(vec![]), parse("()").unwrap())
    }

    #[test]
    fn test_parsing_empty_vector() {
        assert_eq!(Value::Vector(vec![]), parse("[]").unwrap())
    }

    #[test]
    fn test_parsing_nested_empty_collections() {
        assert_eq!(
            Value::Vector(vec![
                Value::List(vec![]),
                Value::Vector(vec![]),
                Value::List(vec![]),
                Value::Vector(vec![])
            ]),
            parse("[()[]()[]]").unwrap()
        )
    }

    #[test]
    fn test_parsing_nested_empty_collections_with_whitespace() {
        assert_eq!(
            Value::Vector(vec![
                Value::List(vec![]),
                Value::Vector(vec![]),
                Value::List(vec![]),
                Value::Vector(vec![])
            ]),
            parse("   ,, , [ ,, , ,()[,,,]( ) []]").unwrap()
        )
    }

    #[test]
    fn test_parsing_empty_map() {
        assert_eq!(Value::Map(vec![]), parse("{}").unwrap())
    }

    #[test]
    fn test_parsing_uneven_map() {
        assert_eq!(Err(ParserError::OddNumberOfMapElements), parse("{()}"));
        assert_eq!(
            Err(ParserError::OddNumberOfMapElements),
            parse("{() [] []}")
        )
    }

    #[test]
    fn test_parsing_even_map() {
        assert_eq!(
            Value::Map(vec![(Value::List(vec![]), Value::List(vec![]))]),
            parse("{() ()}").unwrap()
        );
        assert_eq!(
            Value::Map(vec![
                (Value::List(vec![]), Value::Vector(vec![])),
                (Value::Keyword(Keyword::from_name("a")), Value::List(vec![]))
            ]),
            parse("{()[] :a ()}").unwrap()
        )
    }

    #[test]
    fn test_parsing_duplicate_map_keys() {
        assert_eq!(
            Value::Map(vec![(Value::List(vec![]), Value::List(vec![]))]),
            parse("{() ()}").unwrap()
        );
        assert_eq!(
            Err(ParserError::DuplicateKeyInMap {
                value: Value::List(vec![])
            }),
            parse("{()[] () ()}")
        )
    }

    #[test]
    fn test_equals_for_list_and_vector() {
        assert!(equal(&Value::List(vec![]), &Value::Vector(vec![])));
        assert!(equal(
            &Value::List(vec![Value::Boolean(true)]),
            &Value::Vector(vec![Value::Boolean(true)])
        ));
        assert!(!equal(
            &Value::List(vec![Value::Boolean(true)]),
            &Value::Vector(vec![Value::Boolean(false)])
        ));
        assert!(!equal(
            &Value::List(vec![Value::Boolean(true)]),
            &Value::Vector(vec![Value::Boolean(true), Value::Boolean(true)])
        ));
    }

    #[test]
    fn test_parsing_string() {
        assert_eq!(
            Value::String("ꪪ".to_string()),
            parse("\"\\uAAAA\"").unwrap()
        )
    }

    #[test]
    fn test_parsing_string_nested() {
        assert_eq!(
            Value::Vector(vec![Value::String("ꪪ".to_string())]),
            parse("[\"\\uAAAA\"]").unwrap()
        )
    }

    #[test]
    fn test_parsing_multiline_string() {
        assert_eq!(
            Value::String("abc\n    \ndef    \n".to_string()),
            parse("\"abc\n    \ndef    \n\"").unwrap()
        )
    }

    #[test]
    fn test_parsing_string_map() {
        assert_eq!(
            Value::Map(vec![(
                Value::String("abc".to_string()),
                Value::String("def".to_string())
            )]),
            parse("{\"abc\" \"def\"}").unwrap()
        );

        assert_eq!(
            Value::Map(vec![(
                Value::String("abc".to_string()),
                Value::String("def".to_string())
            )]),
            parse("{\"abc\"\"def\"}").unwrap()
        )
    }

    #[test]
    fn test_parsing_inst() {
        assert_eq!(
            Value::Inst(DateTime::parse_from_rfc3339("1985-04-12T23:20:50.52Z").unwrap()),
            parse("#inst\"1985-04-12T23:20:50.52Z\"").unwrap()
        )
    }

    #[test]
    fn test_parsing_uuid() {
        assert_eq!(
            Value::Uuid(Uuid::parse_str("f81d4fae-7dec-11d0-a765-00a0c91e6bf6").unwrap()),
            parse("#uuid\"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"").unwrap()
        )
    }

    #[test]
    fn test_parsing_symbol() {
        assert_eq!(
            Value::Vector(vec![
                Value::Symbol(Symbol::from_name("a")),
                Value::Symbol(Symbol::from_name("abc")),
                Value::Symbol(Symbol::from_namespace_and_name("abc", "def")),
                Value::Symbol(Symbol::from_name("->")),
                Value::Symbol(Symbol::from_name("/")),
                Value::Symbol(Symbol::from_namespace_and_name("my.org", "stuff")),
            ]),
            parse("[ a  abc abc/def -> / my.org/stuff ]").unwrap()
        );
    }

    #[test]
    fn test_parsing_symbol_errs() {
        assert_eq!(
            Err(ParserError::CannotHaveSlashAtBeginningOfSymbol),
            parse("/abc")
        );
        assert_eq!(
            Err(ParserError::CannotHaveSlashAtEndOfSymbol),
            parse("abc/")
        );
        assert_eq!(
            Err(ParserError::CannotHaveSlashAtEndOfSymbol),
            parse("abc/ ")
        );
        assert_eq!(
            Err(ParserError::CannotHaveSlashAtEndOfSymbol),
            parse("abc/ []")
        );
        assert_eq!(
            Err(ParserError::CannotHaveMoreThanOneSlashInSymbol),
            parse("a/b/c")
        );
    }

    #[test]
    fn test_parsing_keyword() {
        assert_eq!(
            Value::Vector(vec![
                Value::Keyword(Keyword::from_name("a")),
                Value::Keyword(Keyword::from_name("abc")),
                Value::Keyword(Keyword::from_namespace_and_name("abc", "def")),
                Value::Keyword(Keyword::from_name("->")),
                Value::Keyword(Keyword::from_name("/")),
                Value::Keyword(Keyword::from_namespace_and_name("my.org", "stuff")),
            ]),
            parse("[ :a  :abc :abc/def :-> :/ :my.org/stuff ]").unwrap()
        );
    }

    #[test]
    fn test_parsing_keyword_errs() {
        assert_eq!(
            Err(ParserError::CannotHaveSlashAtBeginningOfSymbol),
            parse(":/abc")
        );
        assert_eq!(
            Err(ParserError::CannotHaveSlashAtEndOfSymbol),
            parse(":abc/")
        );
        assert_eq!(
            Err(ParserError::CannotHaveSlashAtEndOfSymbol),
            parse(":abc/ ")
        );
        assert_eq!(
            Err(ParserError::CannotHaveSlashAtEndOfSymbol),
            parse(":abc/ []")
        );
        assert_eq!(
            Err(ParserError::CannotHaveMoreThanOneSlashInSymbol),
            parse(":a/b/c")
        );
        assert_eq!(Err(ParserError::InvalidKeyword), parse("::namespaced"));
    }

    #[test]
    fn test_parse_set() {
        assert_eq!(
            Value::Set(vec![
                Value::Keyword(Keyword::from_name("abc")),
                Value::Keyword(Keyword::from_name("def")),
                Value::Keyword(Keyword::from_namespace_and_name("ghi", "jkl"))
            ]),
            parse("#{:abc :def :ghi/jkl }").unwrap()
        );

        assert_eq!(
            Err(ParserError::DuplicateValueInSet {
                value: Value::Symbol(Symbol::from_name("a"))
            }),
            parse("#{a b c d e f a g h}")
        )
    }

    #[test]
    fn test_set_equals() {
        assert!(equal(
            &Value::Set(vec![
                Value::Keyword(Keyword::from_name("def")),
                Value::Keyword(Keyword::from_namespace_and_name("ghi", "jkl")),
                Value::Keyword(Keyword::from_name("abc"))
            ]),
            &Value::Set(vec![
                Value::Keyword(Keyword::from_name("abc")),
                Value::Keyword(Keyword::from_name("def")),
                Value::Keyword(Keyword::from_namespace_and_name("ghi", "jkl"))
            ])
        ))
    }
    #[test]
    fn test_example_map() {
        assert_eq!(
            Value::Map(vec![
                (
                    Value::Keyword(Keyword::from_namespace_and_name("person", "name")),
                    Value::String("Joe".to_string())
                ),
                (
                    Value::Keyword(Keyword::from_namespace_and_name("person", "parent")),
                    Value::String("Bob".to_string())
                ),
                (
                    Value::Keyword(Keyword::from_name("ssn")),
                    Value::String("123".to_string())
                ),
                (
                    Value::Symbol(Symbol::from_name("friends")),
                    Value::Vector(vec![
                        Value::String("sally".to_string()),
                        Value::String("john".to_string()),
                        Value::String("linda".to_string())
                    ])
                ),
                (
                    Value::String("other".to_string()),
                    Value::Map(vec![(
                        Value::Keyword(Keyword::from_name("stuff")),
                        Value::Keyword(Keyword::from_name("here"))
                    )])
                )
            ]),
            parse(
                "\
            {:person/name \"Joe\"\
             :person/parent \"Bob\"\
             :ssn \"123\"\
             friends [\"sally\" \"john\" \"linda\"]\
             \"other\" {:stuff :here}}"
            )
            .unwrap()
        )
    }

    #[test]
    fn test_basic_keyword_and_symbol() {
        assert!(equal(&parse("name").unwrap(), &parse("name").unwrap()));
        assert!(equal(
            &parse("person/name").unwrap(),
            &parse("person/name").unwrap()
        ));
        assert!(equal(&parse(":name").unwrap(), &parse(":name").unwrap()));
        assert!(equal(
            &parse(":person/name").unwrap(),
            &parse(":person/name").unwrap()
        ));

        // Had an issue with whitespace
        assert!(equal(&parse("name ").unwrap(), &parse("name ").unwrap()));
        assert!(equal(
            &parse("person/name ").unwrap(),
            &parse("person/name ").unwrap()
        ));
        assert!(equal(&parse(":name ").unwrap(), &parse(":name ").unwrap()));
        assert!(equal(
            &parse(":person/name ").unwrap(),
            &parse(":person/name ").unwrap()
        ));
    }

    #[test]
    fn test_complex_equals() {
        assert!(equal(
            &parse(
                "\
            {:person/parent \"Bob\"\
             :person/name \"Joe\"\
             :ssn \"123\"\
             friends [\"sally\" \"john\" \"linda\"]\
             \"other\" {:stuff :here}}"
            )
            .unwrap(),
            &parse(
                "\
            {:person/name \"Joe\"\
             :person/parent \"Bob\"\
             :ssn \"123\"\
             friends [\"sally\" \"john\" \"linda\"]\
             \"other\" {:stuff :here}}"
            )
            .unwrap()
        ))
    }

    #[test]
    fn test_nil_false_true() {
        assert_eq!(
            Value::List(vec![
                Value::Nil,
                Value::Boolean(false),
                Value::Boolean(true)
            ]),
            parse("(nil false true)").unwrap()
        )
    }

    #[test]
    fn test_parse_char() {
        assert_eq!(
            Value::Map(vec![
                (Value::Character(' '), Value::Character('z')),
                (Value::Character('a'), Value::Character('\n')),
                (Value::Character('b'), Value::Character('\r')),
                (Value::Character('r'), Value::Character('c')),
                (Value::Character('\t'), Value::Character('d')),

            ]),
            parse("{\\space \\z\\a \\newline \\b \\return \\r \\c \\tab \\d}").unwrap()
        )
    }
}
