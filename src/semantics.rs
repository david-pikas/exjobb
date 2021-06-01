use std::collections::HashMap;
use arbitrary::Unstructured;
use crate::context_arbitrary::choose_consume;

use super::context::Context;

#[derive(PartialEq, Clone)]
#[allow(dead_code)]
pub enum Mutability { Immutable, Unnasigned, Mutable }
#[derive(PartialEq, Clone)]
pub struct Lifetime(String);
#[derive(PartialEq, Clone)]
pub struct Type {
    pub name: String,
    pub mutability: Mutability,
    pub lt_generics: Vec<Lifetime>,
    pub type_generics: Vec<String>,
    pub lt_args: Vec<Lifetime>,
    pub type_args: Vec<Type>
}
#[derive(PartialEq, Clone)]
pub struct Kind { pub lifetimes: u8, pub types: u8 }
pub type ValScope = HashMap<String, Type>;
pub type TypeScope = HashMap<String, Kind>;
#[derive(PartialEq, Clone)]
pub struct Scope { pub owned: bool, pub vals: ValScope, pub types: TypeScope}
pub type Stack<T> = Vec<T>;

impl Type {
    pub fn matches(&self, other: &Type) -> bool {
        // TODO: check that the types are actually the same
        self.name == other.name
    }
}

#[allow(dead_code)]
pub fn push_scope(ctx: &mut Context) {
    ctx.scopes.push(Scope { owned: true, vals: HashMap::new(), types: HashMap::new() });
}

#[allow(dead_code)]
pub fn pop_scope(ctx: &mut Context) {
    ctx.scopes.pop();
}

#[allow(dead_code)]
pub fn add_var<'a>(ctx: &mut Context, name: String, ty: Type) {
    ctx.scopes.last_mut().map(|l| l.vals.insert(name, ty));
}

#[allow(dead_code)]
pub fn add_type<'a>(ctx: &mut Context, name: String, ty: Kind) {
    ctx.scopes.last_mut().map(|l| l.types.insert(name, ty));
}

#[allow(dead_code)]
pub fn lookup_var<'a,'b>(ctx: &'b mut Context, name: &String) -> Option<&'b Type> {
    for scope in ctx.scopes.iter().rev() {
        if let Some(ty) = scope.vals.get(name) {
            return Some(ty);
        }
    }
    return None;
}

#[allow(dead_code)]
pub fn lookup_type<'a,'b>(ctx: &'b mut Context, name: &str) -> Option<&'b Kind> {
    for scope in ctx.scopes.iter().rev() {
        if let Some(kind) = scope.types.get(name) {
            return Some(kind);
        }
    }
    return None;
}

/// Picks a scope at random, then runs the predicate on *all* variables in the scope,
/// then picks a variable from that scope at random. Most likely slower than it has to be.
#[allow(dead_code)]
pub fn pick_var_that<'a, 'b, F>(
    ctx: &'b mut Context,
    u: &'b mut Unstructured<'a>,
    pred: F
) -> arbitrary::Result<(&'b String, &'b Type)>
where F: Fn(&String, &Type) -> bool {
    let scope = u.choose(&ctx.scopes)?;
    let vars: Vec<(&String, &Type)> = scope.vals.iter().filter(|(n,t)| pred(n,t)).collect();
    let &(name, ty) = u.choose(&vars)?;
    return Ok((name, ty));
}

#[allow(dead_code)]
pub fn pick_var<'a, 'b>(
    ctx: &'b mut Context,
    u: &'b mut Unstructured<'a>,
) -> arbitrary::Result<(&'b String, &'b Type)> {
    return pick_var_that(ctx, u, |_,_| true);
}


#[allow(dead_code)]
pub fn pick_type<'a>(ctx: &mut Context, u: &mut Unstructured<'a>) -> arbitrary::Result<Type> {
    let non_empty_scopes: Vec<&Scope> =
        ctx.scopes.iter().filter(|s| !s.types.is_empty()).collect();
    let scope: &Scope = choose_consume(u, non_empty_scopes.into_iter())?;
    let kinds: Vec<(&String, &Kind)> = scope.types.iter().collect();
    let &(name, kind) = u.choose(&kinds)?;
    let n_of_args = kind.types;
    // TODO: add lifetimes
    // TODO: fix mutability
    Ok(Type { 
        name: name.to_owned(),
        type_args: {
            let mut type_args: Vec<Type> = vec![];
            for _ in 0..n_of_args {
                type_args.push(pick_type(ctx, u)?);
            }
            type_args
        },
        mutability: Mutability::Immutable,
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![]
    })
}


macro_rules! make_val {
    ($name:ident : $($ty:tt)*) =>
        ((stringify!($name).to_string(), make_type!($($ty)*)))
}

#[macro_export]
macro_rules! make_type {
    (%Fn($($args:tt)*) $($ret:tt)*) =>
        (make_type!(%Fn{}($($args)*) $($ret)*));
    (%Fn{$($gen:tt)*}($($args:tt)*)) => 
        (make_type!(%Fn{$($gen)*}($($args)*) -> ()));
    (%Fn{$($gen:tt)*}($($args:tt)*) -> $($ret:tt)*) => {
        {
            let (lt_generics, type_generics) = parse_generics!([],[];$($gen)*,);
            let (lt_args, type_args) = parse_args!([],[];$($args)*,);
            crate::semantics::Type {
                name: "#Fn".to_string(),
                mutability: crate::semantics::Mutability::Immutable,
                lt_generics, type_generics,
                lt_args,
                type_args: vec![
                    crate::semantics::Type {
                        name: "#Args".to_string(),
                        mutability: crate::semantics::Mutability::Immutable,
                        lt_generics: vec![],
                        type_generics: vec![],
                        lt_args: vec![],
                        type_args,
                    },
                    make_type!($($ret)*)
                ]
            }
        }
    };
    ($name:ident$({})?$([])?) => (crate::semantics::Type {
        name: stringify!($name).to_string(),
        mutability: crate::semantics::Mutability::Immutable,
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args:vec![]
    });
    (()) => (crate::semantics::Type {
        name: "#Unit".to_string(),
        mutability: crate::semantics::Mutability::Immutable,
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args:vec![]
    });
    (($len:expr; $fst:expr $(,$rest:expr)*)) => (crate::semantics::Type {
        name: format!("#Tuple{}", $len),
        mutability: crate::semantics::Mutability::Immutable,
        lt_generics: vec![],
        type_generics: vec![make_type!($fst) $(, make_type!($rest))*],
        lt_args: vec![],
        type_args:vec![]
    });
    ($name:ident[$($args:tt)*]) => (make_type!($name{}[$($args)*]));
    ($name:ident{$($gen:tt)*}[$($args:tt)*]) => {
        {
            let (lt_generics, type_generics) = parse_generics!([],[];$($gen)*,);
            let (lt_args, type_args) = parse_args!([],[];$($args)*,);
            crate::semantics::Type {
                name: stringify!($name).to_string(),
                mutability: Mutability::Immutable,
                lt_generics, type_generics,
                lt_args, type_args
            }
        }
    };
}

macro_rules! parse_args {
    ([$(,$lt:expr)*],[$(,$ty:expr)*];$(,)?) => ( (vec![$($lt),*],vec![$($ty),*]) );
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$lt:lifetime, $($arg:tt)*) => {
        parse_args!([$(,$lts)*,Lifetime(stringify!($lt).to_string())],[$(,$tys)*];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$name:ident$([$($args:tt)*])?, $($arg:tt)*) => {
        parse_args!([$(,$lts)*],[$(,$tys)*, make_type!($name$([$($args)*])?)];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];#($($quoted:tt)*), $($arg:tt)*) => {
        parse_args!([$(,$lts)*],[$(,$tys)*, make_type!($($quoted)*)];$($arg)*)
    };
}

macro_rules! parse_generics {
    ([$(,$lt:expr)*],[$(,$ty:expr)*];$(,)?) => ( (vec![$($lt),*],vec![$($ty),*]) );
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$lt:lifetime, $($arg:tt)*) => {
        parse_generics!([$(,$lts)*,Lifetime(stringify!($lt).to_string())],[$(,$tys)*];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$name:ident, $($arg:tt)*) => {
        parse_generics!([$(,$lts)*],[$(,$tys)*, stringify!($name).to_string()];$($arg)*)
    };
}

macro_rules! make_kind {
    ($name:ident$({})?) => {
        (stringify!($name).to_string(), crate::semantics::Kind { lifetimes: 0, types: 0 })
    };
    ($name:ident{$ty:expr}) => {
        (stringify!($name).to_string(), crate::semantics::Kind { lifetimes: 0, types: $ty })
    };
    (()) => ("#Unit".to_string(), crate::semantics::Kind { lifetimes: 0, types: 0 });
    (($len:expr;)) => {
        (format!("#Tuple{}", $len), crate::semantics::Kind { lifetimes: 0, types: $len })
    };
    ($name:ident{$lt:expr; $ty:expr}) => {
        (stringify!($name).to_string(), Kind { lifetimes: $lt, types: $ty })
    };
}

pub fn prelude_scope() -> Scope {
    Scope {
        owned: false,
        vals: [
            make_val!(drop: %Fn{T}(T)),
            make_val!(None: Maybe{T}[T]),
            make_val!(Some: %Fn{T}(T) -> Maybe[T]),
            make_val!(Ok : %Fn{T,U}(T) -> Result[T,U]),
            make_val!(Err: %Fn{T,U}(U) -> Result[T,U]),
        ].iter().cloned().collect(), 
        types: [
            make_kind!(Send),
            make_kind!(Sized),
            make_kind!(Sync),
            make_kind!(Unpin),
            make_kind!(Drop),
            make_kind!(Fn{1}),
            make_kind!(FnOnce{1}),
            make_kind!(AsMut{1}),
            make_kind!(From{1}),
            make_kind!(Into{1}),
            make_kind!(DoubleEndedIterator),
            make_kind!(ExactSizeIterator),
            make_kind!(Extend{1}),
            make_kind!(IntoIterator),
            make_kind!(Iterator),
            make_kind!(Option{1}),
            make_kind!(Result{2}),
            make_kind!(Clone),
            make_kind!(Default),
            make_kind!(Eq),
            make_kind!(Ord),
            make_kind!(ParitalEq),
            make_kind!(ParitalOrd),
            make_kind!(ToOwned),
            make_kind!(Box{1}),
            make_kind!(String),
            make_kind!(ToString),
            make_kind!(Vec{1}),
        ].iter().cloned().collect()
    }
}

pub fn primitive_scope() -> Scope {
    Scope {
        owned: false,
        vals: HashMap::new(),
        types: [
            make_kind!(bool),
            make_kind!(u8),
            make_kind!(u16),
            make_kind!(u32),
            make_kind!(u64),
            make_kind!(u128),
            make_kind!(i8),
            make_kind!(i16),
            make_kind!(i32),
            make_kind!(i64),
            make_kind!(i128),
            make_kind!(char),
            make_kind!(str),
            make_kind!(String),
            make_kind!(Unit),
            make_kind!((2;)),
            make_kind!((3;)),
            make_kind!((4;)),
            make_kind!((5;)),
            make_kind!((6;)),
            make_kind!((7;)),
            make_kind!((8;)),
            make_kind!((9;)),
            make_kind!((10;)),
            make_kind!((11;)),
            make_kind!((12;))
        ].iter().cloned().collect()
    }
}

