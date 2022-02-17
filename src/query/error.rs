use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use thiserror::Error;

/*
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/error.clj#L18
#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Invalid find spec")]
    FindSpec,
    #[error("Invalid inputs")]
    Inputs,
    #[error("Invalid with clause")]
    With,
    #[error("Invalid clause")]
    Clause,
    #[error("Invalid or-clause")]
    OrClause,
    #[error("Invalid or-join-clause")]
    OrJoin,
    #[error("Invalid where-clause")]
    Where,
    #[error("Invalid query form")]
    Query,
}

/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/error.clj#L29
#[derive(Error, Debug)]
pub enum Builtin {
    #[error("An error occurred during the execution of a built-in function")]
    Function,
    #[error("An error occurred during the execution of a built-in aggregate function")]
    Aggregate,
} */

#[derive(Debug, Clone, Error)]
pub(crate) struct ParseError(Cow<'static, str>);

impl ParseError {
    pub(crate) fn from(s: &'static str) -> ParseError {
        ParseError(Cow::Borrowed(s))
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Datalog Error: {}", self.0)
    }
}