use std::collections::HashMap;
use std::cell::{RefCell, Cell};
use std::rc::Rc;

use crate::semantics;
use crate::semantics::Lifetime;

use super::semantics::{Stack, Scope, Operator, prelude_scope, operators, primitive_scope};


pub struct Options {
    pub use_semantics: bool,
    pub use_panics: bool,
    pub print_vars: bool,
}
pub struct Context {
    /// Options passed from the command line
    pub options: Options,
    /// A stack of variable scopes
    pub scopes: Stack<Scope>,
    /// Used for functions since they don't inherent their parents scope
    pub basic_scopes: Stack<Scope>,
    /// Since operators are globaly defined, they live here seperate from the scopes
    pub operators: HashMap<&'static str, Operator>,
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
    /// (Semantics only) the lifetime of the current expression
    pub lifetime: usize,
    /// (Semantics only) does the file have a main function?
    pub has_main: bool,
    /// (Semantics only) are non ascii identifiers allowed?
    pub non_ascii: bool,
    /// Is this a valid const expression?
    pub is_const: bool,
    /// Some paths like crate paths don't make sense to have generics
    pub no_generics: bool,
    /// (Semantics only): is the curent struct an enum variant? if so, don't specify that the fields are pub
    pub is_enum_variant: bool,
    /// (Semantics only): does the current block need a new scope, (usually yes) or does it 
    /// already have a new scope (such as in the case with functions)
    pub needs_new_scope: bool,
    /// (Semantics only): can the type of the current expression be infered from context or does it have to be unamigious?
    /// In a pattern, allow_ambiguity means that the pattern needs a type anotation
    pub allow_ambiguity: bool,
    /// Type patterns are only allowed to exist at the top level
    pub is_top_pattern: bool,
    /// (CURRENTLY UNUSED): Does this block belong to a function?
    pub is_fn_block: bool,
    /// (Semantics only): is this the final statement of a block?
    pub is_final_stmnt: bool,
    /// (Semantics only): if a function returns e.g. a reference, the only way to construct
    /// an expression with a reference of appropriate lifetime is if it comes as an arg.
    /// The only circumstance when semantics::Expr::AdditionalArg is allowed to be used.
    pub can_demand_additional_args: bool,
    /// (Semantics only): when creating a value that can demand additional args, this must
    /// be done before creating the rest of the body so that recursive calls to the function
    /// in the body have the right number of arguments
    pub precomputed_final: Option<semantics::Expr>,
    /// (Semantics only): the size of the current expression being made.  It is
    /// set by the children of a recursive call back to their parent so that if
    /// one sibling is very large, other siblings have to be smaller etc.
    pub size: Cell<usize>,
}
impl Context {
    pub fn make_context(options: Options) -> Context {
        let basic_scopes: Stack<Scope> = Rc::new([
            primitive_scope(),
            prelude_scope(options.use_panics),
        ].iter().cloned().collect());
        Context {
            scopes: Rc::new(crate::semantics::RcList::Cons(
                RefCell::new(Scope {
                    owned: true,
                    vars: HashMap::new(),
                    types: HashMap::new(),
                    lifetimes: HashMap::new(),
                    traits: HashMap::new(),
                    structs: HashMap::new(),
                    macros: HashMap::new(),
                    methods: vec![],
                    by_ty_name: HashMap::new(),
                }),
                Rc::clone(&basic_scopes)
            )),
            basic_scopes,
            operators: operators(),
            regard_semantics: options.use_semantics,
            options,
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
            needs_new_scope: true,
            expected_type: crate::make_type!(()),
            has_main: true,
            lifetime: 0,
            non_ascii: false,
            is_const: false,
            no_generics: false,
            is_enum_variant: false,
            allow_ambiguity: false,
            is_top_pattern: false,
            is_fn_block: false,
            is_final_stmnt: false,
            can_demand_additional_args: false,
            precomputed_final: None,
            size: Cell::new(0),
        }
    }
}

pub fn fresh_lt(ctx: &mut Context) -> Lifetime {
    let result = Lifetime::Anon(ctx.lifetime);
    ctx.lifetime += 1;
    return result;
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

#[macro_export]
macro_rules! enum_variant {
($ctx: ident, $e: expr) => (with_attrs!($ctx { is_enum_variant = true }, $e))
}

#[macro_export]
macro_rules! ty_ambigious {
($ctx: ident, $e: expr) => (with_attrs!($ctx { allow_ambiguity = true }, $e))
}

#[macro_export]
macro_rules! ty_unambigous {
($ctx: ident, $e: expr) => (with_attrs!($ctx { allow_ambiguity = false }, $e))
}

#[macro_export]
macro_rules! sub_pattern {
($ctx: ident, $e: expr) => (with_attrs!($ctx{ is_top_pattern = false }, $e))
}

#[macro_export]
macro_rules! already_scoped {
($ctx: ident, $e: expr) => (with_attrs!($ctx{ needs_new_scope = false }, $e))
}

#[macro_export]
macro_rules! fn_block {
($ctx: ident, $e: expr) => (with_attrs!($ctx{ is_fn_block = true }, $e))
}

#[macro_export]
macro_rules! not_fn_block {
($ctx: ident, $e: expr) => (with_attrs!($ctx{ is_fn_block = false }, $e))
}

#[macro_export]
macro_rules! can_demand_args {
($ctx: ident, $e: expr) => (with_attrs!($ctx{ can_demand_additional_args = true }, $e))
}
