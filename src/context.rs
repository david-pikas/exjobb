use std::collections::HashMap;

use crate::semantics;

use super::semantics::{Stack, Scope, prelude_scope, primitive_scope};


pub struct Context {
    /// A stack of variable scopes
    pub scopes: Stack<Scope>,
    /// if false, generate syntactically valid code that may be semantically incorrect
    pub regard_semantics: bool,
    /// To prevent airbitrarly nested expressions
    pub depth: usize,
    /// for loops (and possibly other things) don't allow annotating the type of the expression
    /// (e.g. for foo in bar : T ...)
    pub allow_type_annotations: bool,
    // to avoid nonsensical code of the type let foo = return bar;
    pub has_value: bool,
    /// (Semantics only) E.g. valid targets for variable assignments like 
    /// foo, bar[2], baz.quux
    // https://doc.rust-lang.org/stable/reference/expressions.html#place-expressions-and-value-expressions
    pub is_place_expression: bool,
    /// All fields in a struct should either be named or not named
    pub named_fields: bool,
    /// Only some things are allowed on a top level, e.g. static
    pub is_top_level: bool,
    /// (Semantics only) Let bindings and function arguments can't have
    /// patterns that can fail
    pub is_refutable: bool,
    /// Lifetimes can't contain raw identifiers
    pub is_lifetime: bool,
    /// Ranges are not allowed in reference patterns
    pub allow_range: bool,
    /// Closure blocks can't have labels
    pub allow_block_labels: bool,
    /// If for example the cond of an if contains a block,
    /// the block needs to be parenthesized
    pub parenthesize_block: bool,
    /// (Semantics only) The expected type of the current expression
    pub expected_type: semantics::Type,
    /// (Semantics only) does the file have a main function?
    pub has_main: bool,
    /// (Semantics only) are non ascii identifiers allowed?
    pub non_ascii: bool,
    /// Is this a valid const expression?
    pub is_const: bool,
    /// Some paths like crate paths don't make sense to have generics
    pub no_generics: bool,
}
impl Context {
    pub fn make_context(regard_semantics: bool) -> Context {
        Context {
            scopes: vec![
                primitive_scope(),
                prelude_scope(),
                Scope {
                    owned: true,
                    vals: HashMap::new(),
                    types: HashMap::new(),
                    structs: HashMap::new(),
                    macros: HashMap::new()
                }
            ],
            regard_semantics,
            depth: 0,
            allow_type_annotations: true,
            has_value: false,
            is_place_expression: false,
            named_fields: true,
            is_top_level: false,
            is_refutable: true,
            is_lifetime: false,
            allow_range: true,
            allow_block_labels: true,
            parenthesize_block: false,
            expected_type: crate::make_type!(()),
            has_main: true,
            non_ascii: false,
            is_const: false,
            no_generics: false,
        }
    }
}

#[macro_export]
macro_rules! with_attrs {
    ($obj: ident { }, $e:expr) => ($e);
    ($obj: ident { $attr:ident = $val:expr $(,$attrs:ident = $vals:expr)* }, $e:expr) => {
        {
            let old_val = std::mem::replace(&mut $obj.$attr, $val);
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
macro_rules! lifetime {
($ctx: ident, $e: expr) => (with_attrs!($ctx { is_lifetime = true }, $e))
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

#[macro_export]
macro_rules! with_type {
($ctx: ident, $ty: expr, $e: expr) => (with_attrs!($ctx { expected_type = $ty }, $e))
}

#[macro_export]
macro_rules! is_const {
($ctx: ident, $e: expr) => (with_attrs!($ctx { is_const = true }, $e))
}

#[macro_export]
macro_rules! crate_path {
($ctx: ident, $e: expr) => (with_attrs!($ctx { no_generics = true }, $e))
}

