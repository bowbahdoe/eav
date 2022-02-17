use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::iter::Map;
use edn_format as edn;
use edn_format::Value;
use crate::DatomValue::Keyword;
use crate::query::spec::RecursionLimit::DotDotDot;
use thiserror::Error;
use crate::query::error::ParseError;

pub(crate) trait Named {
    fn name(&self) -> &str;
    fn namespace(&self) -> Option<&str>;
}

impl Named for edn::Keyword {
    fn name(&self) -> &str {
        self.name()
    }

    fn namespace(&self) -> Option<&str> {
        self.namespace()
    }
}

impl Named for edn::Symbol {
    fn name(&self) -> &str {
        self.name()
    }

    fn namespace(&self) -> Option<&str> {
        self.namespace()
    }
}

pub(crate) trait NamedOps {
    fn starts_with(&self, s: &str) -> bool;
}

impl <T: Named> NamedOps for T {
    fn starts_with(&self, s: &str) -> bool {
        self.namespace().map(|ns| ns.starts_with(s)).unwrap_or(false)
            || self.name().starts_with(s)
    }
}

// Pull grammar

/// pattern-data-literal = [attr-spec+]
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L54
struct PatternDataLiteral {
    attr_specs: (Box<AttrSpec>, Vec<AttrSpec>)
}

enum AttrSpec {
    AttrName(AttrName),
    Wildcard(Wildcard),
    MapSpec(MapSpec),
    AttrExpr(AttrExpr),
}

/// attr-name = an edn keyword that names an attr
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj
enum AttrName {
    DbId,
    RevAttr(edn::Keyword),
    FwdAttr(edn::Keyword)
}

impl TryFrom<&edn::Value> for AttrName {
    type Error = ParseError;

    fn try_from(value: &edn::Value) -> Result<Self, Self::Error> {
        match value {
            Value::Keyword(ref kw) => {
                if kw == &edn::Keyword::from_namespace_and_name("db","id") {
                    Ok(AttrName::DbId)
                }
                else if kw.starts_with("_") {
                    Ok(AttrName::FwdAttr(kw.clone()))
                }
                else {
                    Ok(AttrName::RevAttr(kw.clone()))
                }
            }
            _ => Err(ParseError::from("Expected a keyword for an attr-name"))
        }
    }
}

/// wildcard = "*" or '*'
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L69
struct Wildcard;

/// map-spec = { ((attr-name | limit-expr) (pattern-data-literal | recursion-limit))+ }
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L71
enum MapSpecKey {
    AttrName(AttrName),
    LimitExpr(LimitExpr)
}

enum MapSpecValue {
    PatternDataLiteral(PatternDataLiteral),
    RecursionLimit(RecursionLimit)
}

struct MapSpec {
    values: ((MapSpecKey, MapSpecValue), Vec<(MapSpecKey, MapSpecValue)>)
}

impl TryFrom<&edn::Value> for MapSpec {
    type Error = ();

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        todo!()
    }
}

/// attr-expr = limit-expr | default-expr
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L80
enum AttrExpr {
    LimitExpr(LimitExpr),
    DefaultExpr(DefaultExpr)
}

/// limit-expr = [("limit" | 'limit') attr-name (positive-number | nil)]
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L83
struct LimitExpr {
    attr_name: AttrName,
    limit: Option<u32>
}

impl TryFrom<&edn::Value> for LimitExpr {
    type Error = ParseError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        let bad_limit = ParseError::from("Expected a positive number or nil");
        let bad_values_vector = ParseError::from("Expected a vector: [(\"limit\" | 'limit') attr-name (positive-number | nil)]");
        match value {
            Value::Vector(values) => {
                match &values[..] {
                    [limit_literal, attr_name, number_or_nil] => {
                        if limit_literal == &edn::Value::from("limit")
                            || limit_literal == &edn::Value::from(edn::Symbol::from_name("limit")) {
                            Ok(())
                        }
                        else {
                            Err(ParseError::from("Expected either \"limit\" or the symbol limit"))
                        }?;
                        let attr_name = AttrName::try_from(attr_name)?;
                        let limit = match number_or_nil {
                            &edn::Value::Nil => Ok(None),
                            &edn::Value::Integer(i) => {
                                if i > 0 {
                                    Ok(Some(i as u32))
                                }
                                else {
                                    Err(bad_limit)
                                }
                            }
                            _ => Err(bad_limit)
                        }?;

                        Ok(LimitExpr {
                            attr_name,
                            limit
                        })
                    },
                    _ => Err(bad_values_vector)
                }
            },
            _ => Err(bad_values_vector)
        }
    }
}

/// default-expr = [("default" | 'default') attr-name any-value]
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L89
struct DefaultExpr {
    attr_name: AttrName,
    any_value: edn::Value
}

/// recursion-limit = positive-number | '...'
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L94
enum RecursionLimit {
    Known(u32),
    DotDotDot
}

impl TryFrom<&edn::Value> for RecursionLimit {
    type Error = ParseError;

    fn try_from(value: &edn::Value) -> Result<Self, Self::Error> {
        match value {
            edn::Value::Symbol(sym) => {
                if sym == &edn::Symbol::from_name("...") {
                    Ok(RecursionLimit::DotDotDot)
                }
                else {
                    Err(ParseError::from("Only allowed symbol is ..."))
                }
            }
            edn::Value::Integer(i) => {
                if i.is_positive() {
                    Ok(RecursionLimit::Known(*i as u32))
                }
                else {
                    Err(ParseError::from("Expected a positive number"))
                }
            }
            _ => Err(ParseError::from("Expected a positive number or the symbol ..."))
        }
    }
}

// Query grammar

/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L105
/// rules-var = the symbol "%"
pub struct RulesVar;

impl TryFrom<&edn::Value> for RulesVar {
    type Error = edn::Value;

    fn try_from(value: &edn::Value) -> Result<Self, Self::Error> {
        match value {
            edn::Value::Symbol(ref symbol) => if symbol == &edn::Symbol::from_name("%") {
                Ok(RulesVar)
            } else {
                Err(value.clone())
            }
            _ => Err(value.clone())
        }
    }
}

/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L108
/// src-var = symbol starting with "$"
struct SrcVar(edn::Symbol);

impl TryFrom<&edn::Value> for SrcVar {
    type Error = edn::Value;

    fn try_from(value: &edn::Value) -> Result<Self, Self::Error> {
        match value {
            edn::Value::Symbol(ref symbol) => if symbol.starts_with("$") {
                Ok(SrcVar(symbol.clone()))
            } else {
                Err(value.clone())
            }
            _ => Err(value.clone())
        }
    }
}

/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L111
/// variable = symbol starting with "?"
struct Variable(edn::Symbol);

impl TryFrom<edn::Value> for Variable {
    type Error = edn::Value;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            edn::Value::Symbol(ref symbol) => if symbol.starts_with( "$") {
                Ok(Variable(symbol.clone()))
            } else {
                Err(value)
            }
            _ => Err(value)
        }
    }
}

/// constant = any non-variable data literal
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L123
struct Constant(edn::Value);

impl TryFrom<edn::Value> for Constant {
    type Error = edn::Value;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        // TODO: Validate structure
        Ok(Constant(value))
    }
}

/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L123
pub struct Unbound;

impl TryFrom<edn::Value> for Unbound {
    type Error = edn::Value;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            edn::Value::Symbol(ref symbol) => if symbol.namespace() == None && symbol.name() == "_" {
                Ok(Unbound)
            } else {
                Err(value)
            }
            _ => Err(value)
        }
    }
}

/// clause = (not-clause | not-join-clause | or-clause | or-join-clause | expression-clause)
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L216
pub enum Clause {
    NotClause(NotClause),
    NotJoinClause(NotJoinClause),
    OrClause(OrClause),
    OrJoinClause(OrJoinClause),
    ExpressionClause(ExpressionClause),
}

/// not-clause = [ src-var? 'not' clause+ ]
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L223
pub struct NotClause {
    src_var: Option<SrcVar>,
    clauses: (Box<Clause>, Vec<Clause>)
}

/// not-join-clause = [ src-var? 'not-join' [variable+] clause+ ]
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L228
pub struct NotJoinClause {
    src_var: Option<SrcVar>,
    variables: (Variable, Vec<Variable>),
    clauses: (Box<Clause>, Vec<Clause>)
}

/// and-clause = [ 'and' clause+ ]
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L234
pub struct AndClause {
    clauses: (Box<Clause>, Vec<Clause>)
}

/// union of clause and and-clause
enum ClauseOrAndClause {
    Clause(Box<Clause>),
    AndClause(AndClause)
}

/// or-clause = [ src-var? 'or' (clause | and-clause)+]
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L238
pub struct OrClause {
    src_var: Option<SrcVar>,
    clauses: (ClauseOrAndClause, Vec<ClauseOrAndClause>)
}

/// rule-vars = [variable+ | ([variable+] variable*)]
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L244
pub struct RuleVars {
}

///  or-join-clause = [ src-var? 'or-join' rule-vars (clause | and-clause)+ ]
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L250
pub struct OrJoinClause {
    src_var: Option<SrcVar>,
    rule_cars: RuleVars,
    clauses: (ClauseOrAndClause, Vec<ClauseOrAndClause>)
}

/// pred-expr = [ [pred fn-arg+] ]
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L258
pub struct PredExpr {}

/// fn-expr = [ [fn fn-arg+] binding]
/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/dialect/spec.clj#L262
pub struct FnExpr {}

/// expression-clause = (data-pattern | pred-expr | fn-expr | rule-expr)
/// NOTE: we have to evaluate rule-expr before data-pattern to avoid ambiguity
///       with plain-symbol constants in data patterns.
pub struct ExpressionClause {}