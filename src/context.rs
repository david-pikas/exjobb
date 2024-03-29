use std::collections::{HashSet, HashMap};
use std::cell::{RefCell, Cell};
use std::rc::Rc;
use std::env;

use crate::semantics::{self, Stack, Scope, Operator, Lifetime};
use crate::scopes::*;
use crate::branching::Branches;
use crate::string_wrapper::StringWrapper;


pub struct Options {
    /// If true generate semantically valid code only
    pub use_semantics: bool,
    /// Allow panics in semantically valid code
    pub use_panics: bool,
    /// Print the value of all vars at the end of a scope in semantically valid code
    pub print_vars: bool,
    pub runnable: bool
}
impl Options {
    pub fn export_env(&self) {
        env::set_var("RUSTSMITH_USE_SEMANTICS", u8::from(self.use_semantics).to_string());
        env::set_var("RUSTSMITH_USE_PANICS", u8::from(self.use_panics).to_string());
        env::set_var("RUSTSMITH_PRINT_VARS", u8::from(self.print_vars).to_string());
        env::set_var("RUSTSMITH_RUNNABLE", u8::from(self.runnable).to_string());
    }
    pub fn import_env() -> Self {
        Options {
            use_semantics: env::var("RUSTSMITH_USE_SEMANTICS") == Ok("1".to_string()),
            use_panics:    env::var("RUSTSMITH_USE_PANICS")    == Ok("1".to_string()),
            print_vars:    env::var("RUSTSMITH_PRINT_VARS")    == Ok("1".to_string()),
            runnable:      env::var("RUSTSMITH_RUNNABLE")      == Ok("1".to_string()),
        }
    }
}
#[derive(PartialEq, Clone, Copy)]
pub enum RefType { Borrow, MoveOrMut }
pub struct Context {
    /// Options passed from the command line
    pub options: Options,
    /// A stack of variable scopes
    pub scopes: Stack<Scope>,
    /// Convenient to have a reference for when setting not_in_use_scopes
    pub basic_scopes: Stack<Scope>,
    /// Nested functions don't inherit variables from their parent scope, but they
    /// do inherent e.g. structs and enums. If Some(a,b) all scopes between a and
    /// b can't have variables picked from them, but their structs, enums, use statments
    /// and possibly more are still in use
    pub not_in_use_scopes: Option<(Stack<Scope>, Stack<Scope>)>,
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
    pub lifetime: Cell<usize>,
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
    /// already have a new scope (such as with functions)
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
    /// (Semantics only): The extra data required to keep track of branches. If not None,
    /// anytime a lifetime or the references a var has are changed we have to make sure 
    /// that the original value is saved so that it is possible to restore for other branches
    pub branches: Option<RefCell<Branches>>,
    /// (Semantics only): In order to assure that, for example, a struct don't have two
    /// fields with the same name, the struct can temporarily put field names here.
    pub reserved_names: HashSet<StringWrapper>,
    /// (Semantics only): A lifetime can't end if it's being used in an expression,
    /// `foo(&bar, &mut bar)` is an error because there is no way to make the first
    /// borrow short enough to prevent it from overlapping with the mutable one.
    pub reserved_lts: RefCell<Vec<HashMap<Lifetime, RefType>>>,
    /// Temporary values die at the end of their line, with some exceptions.
    /// in `let a = &mut tmp()` tmp doesn't die, but it does in
    /// `let a = Some(&mut tmp())` for example.
    /// see rustc --explain E0716
    pub temporaries: RefCell<Vec<Lifetime>>,
    /// Are we in a context where we need to store temporaries for deleteion?
    pub kill_temps: Cell<bool>
}

impl Context {
    pub fn make_context(options: Options) -> Context {
        let top_scope: Stack<Scope> = Rc::new([
            Default::default(),
            primitive_scope(),
            prelude_scope(options.use_panics),
        ].iter().cloned().collect());
        Context {
            scopes: top_scope.clone(),
            basic_scopes: top_scope,
            not_in_use_scopes: None,
            operators: operators(),
            regard_semantics: options.use_semantics,
            options,
            depth: 0,
            allow_type_annotations: true,
            has_value: false,
            is_place_expression: false,
            named_fields: true,
            is_top_level: true,
            is_refutable: true,
            is_lifetime: false,
            allow_range: true,
            allow_block_labels: true,
            parenthesize_block: false,
            needs_new_scope: true,
            expected_type: crate::make_type!(()),
            has_main: true,
            lifetime: Cell::new(0),
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
            branches: None,
            reserved_names: HashSet::new(),
            reserved_lts: RefCell::new(vec![]),
            temporaries: RefCell::new(vec![]),
            kill_temps: Cell::new(false)
        }
    }
}

pub fn fresh_lt(ctx: &Context) -> Lifetime {
    let result = Lifetime::Anon(ctx.lifetime.get());
    ctx.lifetime.update(|n| n + 1);
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

#[macro_export]
macro_rules! reserve_names {
($ctx: ident, $e: expr) => (reserve_names!($ctx, std::collections::HashSet::new(), $e));
($ctx: ident, $new: expr, $e: expr) => (with_attrs!($ctx{ reserved_names = $new }, $e))
}
