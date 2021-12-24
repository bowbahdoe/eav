pub mod keyword;
mod symbol;

use crate::edn::ParserError::UnexpectedEndOfInput;
use internship;
use internship::IStr;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use thiserror::Error;

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

#[derive(Debug, PartialEq)]
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
    TaggedElement { tag: Symbol, value: Box<Value> },
}

#[derive(Debug, Error)]
enum ParserError {
    #[error("The input was entirely blank")]
    EmptyInput,

    #[error("Unexpected end of input")]
    UnexpectedEndOfInput,

    #[error("Invalid UTF-8")]
    InvalidUtf8 { chars: Vec<char> },

    #[error("Duplicate values in a set")]
    DuplicateValuesInSet { value: Value },

    #[error("Only symbols can be used as tags")]
    InvalidElementForTag { value: Value },

    #[error("Unexpected character")]
    UnexpectedCharacter(char),

    #[error("Invalid keyword")]
    InvalidKeyword,

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
    ParsingList { values_so_far: Vec<Value> },
    ParsingVector { values_so_far: Vec<Value> },
    ParsingMap { entries_so_far: Vec<(Value, Value)> },
    ParsingSet { values_so_far: Vec<Value> },
    ParsingSymbol { characters_so_far: Vec<char> }, // Decide after parsing symbol if it is true, false, or nil
    ParsingNumeric { characters_so_far: Vec<char> },
    ParsingString { characters_so_far: Vec<char> },
    SelectingDispatch { characters_so_far: Vec<char> },
}

/// Commas are considered whitespace for EDN
fn is_whitespace(c: char) -> bool {
    c.is_whitespace() || c == ','
}

/// Likely *very* suboptimal parsing. Focus for now should be getting actually correct
/// results, since there is no good edn library for rust.
fn parse_helper(s: &[char], mut parser_state: ParserState) -> Result<ParserSuccess, ParserError> {
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
                        entries_so_far: vec![],
                    },
                )
            } else if s[0] == '"' {
                parse_helper(
                    &s[1..],
                    ParserState::ParsingString {
                        characters_so_far: vec![],
                    },
                )
            } else if s[0] == ':' {
                /// For parsing a keyword, we can just fall back on the logic for parsing a symbol
                /// **somewhat** of a hack and it means we need to move the logic for converting
                /// symbols for true, false, and nil higher up and the error messages might
                /// end up strange but i am okay with that.
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
            } else if s[0] == '#' {
                parse_helper(
                    &s[1..],
                    ParserState::SelectingDispatch {
                        characters_so_far: vec![],
                    },
                )
            } else if s[0].is_alphabetic() {
                parse_helper(
                    &s[1..],
                    ParserState::ParsingSymbol {
                        characters_so_far: vec![s[0]],
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
                Err(UnexpectedEndOfInput)
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

        /// Almost total duplicate of ParsingList
        ParserState::ParsingVector { mut values_so_far } => {
            if s.len() == 0 {
                Err(UnexpectedEndOfInput)
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

        /*
        ParserState::ParsingMap { .. } => {}
        ParserState::ParsingSet { .. } => {}
        ParserState::ParsingSymbol { .. } => {}
        ParserState::ParsingNumeric { .. } => {}
        ParserState::ParsingString { .. } => {} */
        ParserState::SelectingDispatch { characters_so_far } => {
            if s.len() == 0 {
                Err(UnexpectedEndOfInput)
            } else if s[0] == '_' && characters_so_far.len() == 0 {
                /// Drop the next form. Still error if that form is malformed
                let ParserSuccess {
                    remaining_input, ..
                } = parse_helper(&s[1..], ParserState::Begin)?;
                parse_helper(remaining_input, ParserState::Begin)
            } else if s[0] == '{' && characters_so_far.len() == 0 {
                parse_helper(
                    &s[1..],
                    ParserState::ParsingSet {
                        values_so_far: vec![],
                    },
                )
            } else {
                /// We expect to read a symbol next and we will associate that symbol as the tag of
                /// the following element
                let ParserSuccess {
                    remaining_input,
                    value,
                } = parse_helper(&s[1..], ParserState::Begin)?;
                match value {
                    Value::Symbol(symbol) => {
                        let next_success = parse_helper(remaining_input, ParserState::Begin)?;
                        Ok(ParserSuccess {
                            remaining_input: next_success.remaining_input,
                            value: Value::TaggedElement {
                                tag: symbol,
                                value: Box::new(next_success.value),
                            },
                        })
                    }
                    _ => Err(ParserError::InvalidElementForTag { value }),
                }
            }
        }

        _ => Err(ParserError::UnexpectedEndOfInput),
    }
}

/// Parse EDN from the given input string
fn parse(s: &str) -> Result<Value, ParserError> {
    let chars: Vec<char> = s.chars().collect();
    let ParserSuccess {
        remaining_input,
        value,
    } = parse_helper(&chars, ParserState::Begin)?;
    for c in remaining_input {
        if !is_whitespace(*c) {
            return Err(ParserError::ExtraInput {
                parsed_value: value,
                extra_input: remaining_input.to_vec(),
            });
        }
    }
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
