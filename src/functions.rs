use edn_format as edn;

/// "small clojure interpreter" dialect of clojure
pub struct SciFunction {
    params: Vec<edn::Symbol>,
    code: edn::Value,
    imports: Option<edn::Value>,
    requires: Option<edn::Value>,
    fn_name: edn::Symbol,
}

/// https://github.com/Workiva/eva/blob/master/core/src/eva/functions.clj#L92
/// A database function. currently the plan is to *just* support sci through
/// something like babashka, but i might end up needing to pivot to a
/// different language that's easier to call from rust.
pub enum DatabaseFunction {
    SciFunction(SciFunction),
}
