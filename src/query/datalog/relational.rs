// Nota Bene:
// EDB and Evaluable both capture the spirit of a protocol I would just call Relational.
// Extensional relations and Evaluable relations are, for most practical purposes,
// functionally identical. Early discussion resulted in the decision to treat them
// distinctly for the sake of potential future unknown modifications/optimizations.
//
// In terms of intent, EDB sources/predicates are assumed to be finite and thus
// range-restricted. Evaluable sources/predicates could be anything -- once you
// allow arbitrary Evaluable predicate into the program, safety guarantees go out
// the window.

/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/datalog/protocols.clj#L17
trait EDB {
    /// Given an extensional predicate and a collection of groups of terms,
    /// returns a sequence containing all distinct matches for any of the term groups.
    fn extensions(&self, coll_of_terms: ());
}

/// https://github.com/Workiva/eva/blob/master/core/src/eva/query/datalog/protocols.clj#L20
trait Evaluable {
    /// Given an evaluable predicate and a collection of groups of terms,
    /// returns a sequence containing all distinct matches for any of the term groups.
    fn evaluations(&mut self, coll_of_terms: ());
}
