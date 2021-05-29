use std::collections::{HashMap, HashSet};

struct Lifetime<'a>(&'a str);
struct Type<'a> { name: &'a str, lifetime_args: Vec<Lifetime<'a>>, type_args: Vec<Type<'a>> }
struct Kind { lifetimes: u8, types: u8 }
type ValScope<'a> = HashMap<&'a str, (Lifetime<'a>, Type<'a>)>;
type TypeScope<'a> = HashMap<&'a str, Kind>;
struct Scope<'a> { vals: ValScope<'a>, types: TypeScope<'a>}
type Stack<T> = Vec<T>;

const EMPTY_KIND: Kind = Kind { lifetimes: 0u8, types: 0u8 };

pub struct Context {
    /// A stack of variable scopes
    // pub scopes: Stack<Scope<'a>>,
    /// if false, generate syntactically valid code that may be semantically incorrect
    pub regard_semantics: bool,
    // pub ty: Type,
    /// To prevent airbitrarly nested expressions
    pub depth: usize,
    /// for loops (and possibly other things) don't allow annotating the type of the expression
    /// (e.g. for foo in bar : T ...)
    pub allow_type_annotations: bool,
    // to avoid nonsensical code of the type let foo = return bar;
    pub has_value: bool,
    /// E.g. valid targets forvariable assignments like foo, bar[2], baz.quux
    // https://doc.rust-lang.org/stable/reference/expressions.html#place-expressions-and-value-expressions
    pub is_place_expression: bool,
    /// All fields in a struct should either be named or not named
    pub named_fields: bool,
    /// Only some things are allowed on a top level, e.g. static
    pub is_top_level: bool,
    /// Let bindings and function arguments can't have patterns that can fail
    pub is_refutable: bool,
    /// Ranges are not allowed in reference patterns
    pub allow_range: bool,
    /// Blocks with labels don't make sense e.g. as closure bodies. 
    /// Only ever applies to the outer block
    pub allow_block_labels: bool,
    /// If for example the cond of an if contains a block,
    /// the block needs to be parenthesized
    pub parenthesize_block: bool
}

// const PRELUDE_SCOPE: Scope = Scope {
//      vals: [
     
//      ].to_iter().collect(), 
//      types: [
//          "Send", 
//          "Sized",
//          "Sync",
//      ].to_iter().collect()
// };

impl Context {
    pub fn make_context(regard_semantics: bool) -> Self {
        Context {
            // scopes: vec![PRELUDE_SCOPE],
            regard_semantics,
            depth: 0,
            allow_type_annotations: true,
            has_value: false,
            is_place_expression: false,
            named_fields: true,
            is_top_level: false,
            is_refutable: true,
            allow_range: true,
            allow_block_labels: true,
            parenthesize_block: false
        }
    }
}

/// Used to declare certain enum options as only available if certain context flags are set
#[derive(PartialEq, Debug)]
pub enum Flag {
    // ContainsAnnotations,
    PlaceExpression,
    // ValueExpression,
    TopLevel,
    // IsRange,
    NonRecursive,
    Irrefutable,
    Range
}

#[macro_export]
macro_rules! with_attrs {
    ($obj: ident { }, $e:expr) => ($e);
    ($obj: ident { $attr:ident = $val:expr $(,$attrs:ident = $vals:expr)* }, $e:expr) => {
        {
            let old_val = $obj.$attr;
            $obj.$attr = $val;
            let result = with_attrs!($obj { $($attrs = $vals),* }, $e);
            $obj.$attr = old_val;
            result
        }
    }
}

// #[macro_export]
// macro_rules! with_type {
//     ($ctx: ident, $ty: expr, $e: expr) => (with_attrs!($ctx { ty = $ty }, $e))
// }

#[macro_export]
macro_rules! no_annotations {
    ($ctx: ident, $e: expr) => (with_attrs!($ctx { allow_type_annotations = false }, $e))
}

#[macro_export]
macro_rules! rhs {
    ($ctx: ident, $e: expr) => (with_attrs!($ctx { has_value = true }, $e))
}

#[macro_export]
macro_rules! name_fields {
($ctx: ident, $bool: expr, $e: expr) => (with_attrs!($ctx { named_fields = $bool }, $e))
}

#[macro_export]
macro_rules! not_top_level {
($ctx: ident, $e: expr) => (with_attrs!($ctx { is_top_level = false }, $e))
}

#[macro_export]
macro_rules! irrefutable {
($ctx: ident, $e: expr) => (with_attrs!($ctx { is_refutable = false }, $e))
}

#[macro_export]
macro_rules! no_range {
($ctx: ident, $e: expr) => (with_attrs!($ctx { allow_range = false }, $e))
}

#[macro_export]
macro_rules! no_block_labels {
($ctx: ident, $e: expr) => (with_attrs!($ctx { allow_block_labels = false }, $e))
}

#[macro_export]
macro_rules! place_expression {
($ctx: ident, $e: expr) => (with_attrs!($ctx { is_place_expression = true }, $e))
}

#[macro_export]
macro_rules! parens_block {
($ctx: ident, $e: expr) => (with_attrs!($ctx { parenthesize_block = true }, $e))
}
