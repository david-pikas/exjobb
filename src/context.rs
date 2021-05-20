// use syn::Type;

pub struct Context {
    // pub ty: Type,
    pub allow_type_annotations: bool,
    pub has_value: bool,
    pub named_fields: bool
}

impl Default for Context {
    fn default() -> Self {
        Context {
            // for loops (and possibly other things) don't allow annotating the type of the expression 
            // (e.g. for foo in bar : T ...)
            allow_type_annotations: true,
            // to avoid nonsensical code of the type let foo = return bar;
            has_value: false,
            // All fields in a struct should either be named or not named
            named_fields: true
        }
    }
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

macro_rules! name_fields {
    ($ctx: ident, $bool: expr, $e: expr) => (with_attrs!($ctx { named_fields = $bool }, $e))
}
