use std::collections::HashMap;
use arbitrary::{Arbitrary, Unstructured};
use crate::context_arbitrary::{GenerationError, choose_consume, c_arbitrary_iter_with_non_mut};
use crate::context_arbitrary as context_arbitrary;

use super::context::Context;
use lazy_static::lazy_static;

#[derive(PartialEq, Clone, Debug)]
#[allow(dead_code)]
pub enum Mutability { Immutable, Unnasigned, Mutable }
#[derive(PartialEq, Clone, Debug)]
pub struct Lifetime(pub String);
#[derive(PartialEq, Clone, Debug)]
pub struct Type {
    pub name: String,
    pub mutability: Mutability,
    pub lt_generics: Vec<Lifetime>,
    pub type_generics: Vec<String>,
    pub lt_args: Vec<Lifetime>,
    pub type_args: Vec<Type>
}
#[derive(PartialEq, Clone, Debug)]
pub struct Kind { pub lifetimes: u8, pub types: u8 }
#[derive(PartialEq, Clone, Debug)]
pub struct Field { pub visible: bool, pub ty: Type }
#[derive(PartialEq, Clone, Debug)]
pub enum Fields { Unnamed(Vec<Field>), Named(HashMap<String, Field>) }
#[derive(PartialEq, Clone, Debug)]
pub struct Struct {
    pub ty: Type,
    pub fields: Fields
}
#[derive(PartialEq, Clone, Debug)]
#[allow(dead_code)]
pub enum BracketType { Round, Square, Curly }
#[derive(PartialEq, Clone, Debug)]
#[allow(dead_code)]
pub enum Token { Expr(Expr), Ident(String), Type(Type) }
#[derive(PartialEq, Clone, Debug)]
#[allow(dead_code)]
pub enum Seperator { Comma, Semi }
#[derive(PartialEq, Clone, Debug)]
pub struct MacroBody {
    pub brackets: BracketType,
    pub tokens: Vec<Token>,
    pub seperator: Seperator
}
type MacroConstructor = fn(Vec<Type>, &Context, &mut Unstructured) -> context_arbitrary::Result<MacroBody>;
pub struct Macro {
    pub constructor: MacroConstructor,
    pub ty: Type
}
impl PartialEq for Macro {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}
impl Clone for Macro {
    fn clone(&self) -> Self{
        Macro {
            constructor: self.constructor.clone(),
            ty: self.ty.clone()
        }
    }
}
impl std::fmt::Debug for Macro {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Macro")
         .field("constructor", &Box::new("[macro_constructor]"))
         .field("ty", &self.ty)
         .finish()
    }
}
pub type ValScope = HashMap<String, Type>;
pub type TypeScope = HashMap<String, Kind>;
pub type StructScope = HashMap<String, Struct>;
pub type MacroScope = HashMap<String, Macro>;
#[derive(PartialEq, Clone, Debug)]
pub struct Scope {
    pub owned: bool,
    pub vals: ValScope,
    pub types: TypeScope,
    pub structs: StructScope,
    pub macros: MacroScope
}
pub type Stack<T> = Vec<T>;
#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Lit(String),
    Struct(String, HashMap<String, Expr>),
    Fn(String, Vec<Expr>),
    Var(String),
    Macro(String, MacroBody)
}


impl Type {
    pub fn matches(&self, other: &Type) -> bool {
        // println!("Checking if {:?} and {:?} match...", self, other);
        if self.name != other.name {
            return false;
        }
        let zipped = self.type_args.iter().zip(other.type_args.iter());
        for (self_arg, other_arg) in zipped {
            let self_arg_is_generic = self.type_generics.iter().any(|gen| {
                // println!("\"{}\" is generic!", gen);
                &self_arg.name == gen
            });
            let other_arg_is_generic = other.type_generics.iter().any(|gen| {
                // println!("\"{}\" is generic!", gen);
                &other_arg.name == gen
            });
            if !self_arg_is_generic && !other_arg_is_generic && !self_arg.matches(other_arg) {
                // println!("Type arguments where different: {:?}, {:?}", self_arg, other_arg);
                return false;
            }
        }
        return true;
    }
}

#[allow(dead_code)]
pub fn push_scope(ctx: &mut Context) {
    ctx.scopes.push(Scope {
        owned: true,
        macros: HashMap::new(),
        vals: HashMap::new(),
        types: HashMap::new(),
        structs: HashMap::new(),
    });
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
pub fn add_struct(ctx: &mut Context, name: String, struc: Struct) {
    ctx.scopes.last_mut().map(|l| l.structs.insert(name, struc));
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
) -> context_arbitrary::Result<(&'b String, &'b Type)>
where F: Fn(&String, &Type) -> bool {
    let scope = u.choose(&ctx.scopes)?;
    let vars: Vec<(&String, &Type)> = scope.vals.iter().filter(|(n,t)| pred(n,t)).collect();
    if vars.is_empty() {
        Err(GenerationError::AppropriateTypeFailure)
    } else {
        let &(name, ty) = u.choose(&vars)?;
        Ok((name, ty))
    }
}

#[allow(dead_code)]
pub fn pick_var<'a, 'b>(
    ctx: &'b mut Context,
    u: &'b mut Unstructured<'a>,
) -> context_arbitrary::Result<(&'b String, &'b Type)> {
    pick_var_that(ctx, u, |_,_| true)
}


#[allow(dead_code)]
pub fn pick_type<'a>(ctx: &Context, u: &mut Unstructured<'a>) -> arbitrary::Result<Type> {
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

#[allow(dead_code)]
pub fn construct_value<'a>(ctx: &Context, u: &mut Unstructured<'a>, ty: Type) -> context_arbitrary::Result<Expr> {

    // println!("construct_value called on type {:?}", ty);

    fn fields_to_struct(name: String, fields: HashMap<String, Type>, ctx: &Context, u: &mut Unstructured) -> context_arbitrary::Result<Expr> {
        // println!("fields_to_struct called with: {:?}", fields);
        let mut fields_expr: HashMap<String, Expr> = HashMap::new();
        for f_name in fields.keys() {
            fields_expr.insert(f_name.clone(), construct_value(ctx, u, fields[f_name].clone())?);
        }
        Ok(Expr::Struct(name, fields_expr))
    }

    fn handle_field_generics(generics: Vec<(String, Type)>, fields: &HashMap<String, Field>) -> HashMap<String, Type> {
        // println!("handle_field_generics called on {:?}, {:?}", generics, fields);
        let mut new_map = HashMap::new();
        'outer: for (name, field) in fields.iter() {
            for (ty_name, ty) in &generics {
                if ty_name == &field.ty.name {
                    new_map.insert(name.to_owned(), ty.clone() );
                    break 'outer;
                }
            }
            new_map.insert(name.clone(), field.ty.clone());
        }
        return new_map;           
    }

    fn handle_arg_generics(generics: Vec<(String, Type)>, args: &Vec<Type>) -> Vec<Type> {
        // println!("handle_arg_generics called on {:?}, {:?}", generics, args);
        let mut new_args = vec![];
        'vec_args: for arg in args {
            for (ty_name, ty) in &generics {
                if ty_name == &arg.name {
                    new_args.push(ty.clone());
                    break 'vec_args;
                }
            }
            new_args.push(arg.clone());
        }
        return new_args;
    }

    let mut possible_exprs: Vec<Box<dyn Fn(&Context, &mut Unstructured) -> context_arbitrary::Result<Expr>>> = vec![];

    for scope in ctx.scopes.iter() {
        for (name, struc) in scope.structs.iter() {
            if struc.ty.matches(&ty) {
                // TODO: do we really need to clone this twice?
                let type_generics = struc.ty.type_generics.clone();
                let type_args = ty.type_args.clone();
                possible_exprs.push(Box::new(move |ctx, u| {
                    let generics =
                        type_generics.clone().into_iter()
                          .zip(type_args.clone())
                          .collect();
                    match &struc.fields {
                        Fields::Named(fields) if !fields.is_empty() => {
                            fields_to_struct(name.clone(), handle_field_generics(generics, &fields), ctx, u)
                        }
                        Fields::Unnamed(fields) if !fields.is_empty() => {
                            let args = fields.into_iter().map(|f| f.ty.to_owned()).collect();
                            Ok(Expr::Fn(
                                name.clone(),
                                handle_arg_generics(generics, &args)
                                    .into_iter()
                                    .map(|t| construct_value(ctx, u, t))
                                    .collect::<context_arbitrary::Result<Vec<Expr>>>()?
                            ))
                        }
                        _ => Ok(Expr::Lit(name.clone()))
                    }
                }));
            }
        }
        for (name, var_ty) in scope.vals.iter() {
            if var_ty.matches(&ty) {
                possible_exprs.push(Box::new(move |_,_| Ok(Expr::Var(name.clone()))));
            } else if var_ty.name == "#Fn" && var_ty.type_args[1].matches(&ty) {
                // println!("var_ty: {:?}", var_ty);
                let type_generics = var_ty.type_generics.clone();
                possible_exprs.push(Box::new(move |ctx, u| {
                    let mut args = vec![];
                    let generics =
                        type_generics.clone().into_iter()
                          .map(|name| Ok((name, pick_type(ctx, u)?)))
                          .collect::<arbitrary::Result<Vec<(String, Type)>>>()?;
                    for arg in handle_arg_generics(generics, &var_ty.type_args[0].type_args) {
                        args.push(construct_value(ctx, u, arg)?);
                    }
                    Ok(Expr::Fn(name.clone(), args))
                }));
            }
        }
        for (name, macr) in scope.macros.iter() {
            if macr.ty.matches(&ty) {
                // does this really need to be copied twice?
                let type_args = ty.type_args.clone();
                possible_exprs.push(Box::new(move |ctx, u| {
                    let body = (macr.constructor)(type_args.clone(), ctx, u)?;
                    Ok(Expr::Macro(name.to_owned(), body))
                }));
            }
        }
    }

    if possible_exprs.is_empty() {
        println!("Couldn't find any values for type {:?}", ty);
    }
    let choice = choose_consume(u, possible_exprs.iter())?;
    return choice(ctx, u);
}

pub fn kind_to_type(name: String, mutability: Mutability, kind: &Kind) -> Type {
    Type {
        name,
        mutability,
        lt_generics: (*GEN_LTS)[0..(kind.lifetimes as usize)].to_vec(),
        type_generics: (*GEN_STRING)[0..(kind.types as usize)].to_vec(),
        lt_args: vec![],
        type_args: vec![]
    }
}

macro_rules! make_val {
    ($name:path : $($ty:tt)*) => ((stringify!($name).to_string(), make_type!($($ty)*)))
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
            let mut ret_type = make_type!($($ret)*);
            ret_type.lt_generics.extend(lt_generics.clone());
            ret_type.type_generics.extend(type_generics.clone());
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
                    ret_type
                ]
            }
        }
    };
    ($name:path$({})?$([])?) => (crate::semantics::Type {
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
    ($name:path[$($args:tt)*]) => (make_type!($name{}[$($args)*]));
    ($name:path{$($gen:tt)*}[$($args:tt)*]) => {
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
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$name:path$([$($args:tt)*])?, $($arg:tt)*) => {
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
    ($name:path$({})?) => {
        (stringify!($name).to_string(), crate::semantics::Kind { lifetimes: 0, types: 0 })
    };
    ($name:path{$ty:expr}) => {
        (stringify!($name).to_string(), crate::semantics::Kind { lifetimes: 0, types: $ty })
    };
    (()) => (("#Unit".to_string(), crate::semantics::Kind { lifetimes: 0, types: 0 }));
    (($len:expr;)) => {
        (format!("#Tuple{}", $len), crate::semantics::Kind { lifetimes: 0, types: $len })
    };
    ($name:path{$lt:expr; $ty:expr}) => {
        (stringify!($name).to_string(), Kind { lifetimes: $lt, types: $ty })
    };
}

lazy_static! {
    static ref GEN_NAMES: [Type; 12] = [
        make_type!(A), make_type!(B), make_type!(C), make_type!(D),
        make_type!(E), make_type!(F), make_type!(G), make_type!(H),
        make_type!(I), make_type!(J), make_type!(K), make_type!(L),
    ];
    static ref GEN_STRING: Vec<String> = "ABCDEFGHIJKL".chars().map(|x| x.to_string()).collect();
    static ref GEN_LTS: Vec<Lifetime> = "abcdefghijklmonop".chars().map(|c| Lifetime(format!("'{}", c))).collect();
}

macro_rules! make_struct {
    (()) => (("#Unit".to_string(), crate::semantics::Struct {
        ty: make_type!(()),
        fields: Fields::Unnamed(vec![])
    }));
    (($len:expr;)) => ((format!("#Tuple{}", $len), crate::semantics::Struct {
        ty: Type {
            name: format!("#Tuple{}", $len),
            mutability: Mutability::Immutable,
            lt_args: vec![],
            lt_generics: vec![],
            type_generics: GEN_STRING[0..$len].to_vec(),
            type_args: GEN_NAMES[0..$len].to_vec()
        },
        fields: Fields::Unnamed(GEN_NAMES[0..$len].iter().map(|ty| Field { visible: true, ty: ty.clone() }).collect())
    }));
    ($(:$ty:path:)? $name:path$([$($gen:tt)*])? ) =>
        (make_struct!($(:$ty:)? $name$([$($gen)*])? {}));
    ($name:ident$([$($gen:tt)*])? ( $($fields:tt)* )) =>
        (make_struct!(:$name:$name$([$($gen)*])? ( $($fields)* )));
    ($name:path$([$($gen:tt)*])? { $($fields:tt)* }) =>
        (make_struct!(:$name:$name$([$($gen)*])? { $($fields)* }));
    (:$ty:path: $name:ident $([$($gen:tt)*])? ( $($fields:tt)* )) =>
        ((stringify!($name).to_string(), crate::semantics::Struct {
            ty: make_type!($ty$({$($gen)*}[$($gen)*])?),
            fields: parse_unnamed_fields!(;$($fields)*,)
        }));
    (:$ty:path: $name:path $([$($gen:tt)*])? { $($fields:tt)* }) =>
        ((stringify!($name).to_string(), crate::semantics::Struct {
            ty: make_type!($ty$({$($gen)*}[$($gen)*])?),
            fields: parse_named_fields!(;$($fields)*)
        }))
}

#[allow(unused_macros)]
macro_rules! parse_unnamed_fields {
    ($(,$fields:expr)*;$(,)?) => (Fields::Unnamed(vec![$($fields),*]));
    ($(,$fields:expr)*; $ty:path$({$($gen:tt)*})? $([$($arg:tt)*])?, $($rest:tt)*) => {
        parse_unnamed_fields!($(,$fields)*; #($ty$({$($gen)*})?$([$($arg)*])?), $($rest)*)
    };
    ($(,$fields:expr)*;pub #($($ty:tt)*), $($rest:tt)*) => {
        parse_unnamed_fields!($(,$fields)*, crate::semantics::Field {
            visible: true,
            ty: make_type!($($ty)*; $($rest)*)
        }; $($rest)*);
    };
    ($(,$fields:expr)*; #($($ty:tt)*), $($rest:tt)*) => {
        parse_unnamed_fields!($(,$fields)*, crate::semantics::Field {
            visible: false,
            ty: make_type!($($ty)*)
        }; $($rest)*);
    };
}

macro_rules! parse_named_fields {
    ($(,$fields:expr)*;) => (Fields::Named([$($fields),*].iter().cloned().collect()));
    ($(,$fields:expr)*;$(pub)? $name:ident : $ty:ident$({$($gen:tt)*})?$([$($arg:tt)*])?, $($rest:tt)*) => 
        (parse_named_fields!($(,$fields:expr)*;$(pub)? $name : #($ty$({$($get)*})?$([$($arg:tt)*])?), $($rest)*));
    ($(,$fields:expr)*;pub $name:ident : #($($ty:tt)*), $($rest:tt)*) => {
        parse_named_fields!($(,$fields)*, (stringify!($name).to_string(), crate::semantics::Field {
            visible: true,
            ty: make_type!($($ty)*; $($rest)*)
        }; $($rest)*));
    };
    ($(,$fields:expr)*;$name:ident : #($($ty:tt)*), $($rest:tt)*) => {
        parse_named_fields!($(,$fields)*, (stringify!($name).to_string(), crate::semantics::Field {
            visible: false,
            ty: make_type!($($ty)*; $($rest)*)
        }; $($rest)*));
    };
}

#[allow(unused_macros)]
macro_rules! make_macro {
    ($ty:path$({$($gen:tt)*})?$([$($arg:tt)*])? : $name:ident($fn:expr)) => 
        (#($ty{$($gen)*}[$($arg)*]) : $name($fn));
    (#($($ty:tt)*) : $name:ident($fn:expr)) => {
        (stringify!($name).to_string(), Macro {
            constructor: $fn,
            ty: make_type!($($ty)*)
        })
    }
}


pub fn prelude_scope() -> Scope {
    Scope {
        owned: false,
        vals: [
            make_val!(drop: %Fn{T}(T)),
            make_val!(Box::new: %Fn{T}(T) -> Box[T]),
            make_val!(String::new: %Fn() -> String)
        ].iter().cloned().collect(), 
        macros: [
            make_macro!(#(Vec{T}[T]) : vec(|ty_args, ctx, u| {
                Ok(MacroBody {
                    brackets: if Arbitrary::arbitrary(u)? {
                        BracketType::Round
                    } else {
                        BracketType::Square
                    },
                    tokens: c_arbitrary_iter_with_non_mut(ctx, u, |ctx, u| {
                        Ok(Token::Expr(construct_value(ctx, u, ty_args[0].clone())?))
                    }).collect::<context_arbitrary::Result<Vec<Token>>>()?,
                    seperator: Seperator::Comma
                })
            })),
            make_macro!(#(std::io::Error) : panic(|_, ctx, u| {
                Ok(MacroBody {
                    brackets: BracketType::Round,
                    tokens: vec![Token::Expr(construct_value(ctx, u, make_type!(str))?)],
                    seperator: Seperator::Comma
                })
            }))
        ].iter().cloned().collect(),
        types: [
            // make_kind!(Send),
            // make_kind!(Sized),
            // make_kind!(Sync),
            // make_kind!(Unpin),
            // make_kind!(Drop),
            // make_kind!(Fn{2}),
            // make_kind!(FnOnce{2}),
            // make_kind!(AsMut{1}),
            // make_kind!(From{1}),
            // make_kind!(Into{1}),
            // make_kind!(DoubleEndedIterator),
            // make_kind!(ExactSizeIterator),
            // make_kind!(Extend{1}),
            // make_kind!(IntoIterator),
            // make_kind!(Iterator),
            make_kind!(Option{1}),
            make_kind!(Result{2}),
            // make_kind!(Clone),
            // make_kind!(Default),
            // make_kind!(Eq),
            // make_kind!(Ord),
            // make_kind!(ParitalEq),
            // make_kind!(ParitalOrd),
            // make_kind!(ToOwned),
            make_kind!(Box{1}),
            // make_kind!(String),
            // make_kind!(ToString),
            make_kind!(Vec{1}),
            make_kind!(std::io::Error)
        ].iter().cloned().collect(),
        structs: [
            make_struct!(:Option:None[T]),
            make_struct!(:Maybe:Some[T](T)),
            make_struct!(:Result:Ok[R,E](R)),
            make_struct!(:Result:Err[R,E](E)),
        ].iter().cloned().collect()
    }
}

pub fn primitive_scope() -> Scope {
    Scope {
        owned: false,
        vals: HashMap::new(),
        macros: HashMap::new(),
        structs: [
            make_struct!(bool),
            make_struct!(u8),
            make_struct!(u16),
            make_struct!(u32),
            make_struct!(u64),
            make_struct!(u128),
            make_struct!(i8),
            make_struct!(i16),
            make_struct!(i32),
            make_struct!(i64),
            make_struct!(i128),
            make_struct!(char),
            make_struct!(str),
            make_struct!(()),
            make_struct!((2;)),
            make_struct!((3;)),
            make_struct!((4;)),
            make_struct!((5;)),
            make_struct!((6;)),
            make_struct!((7;)),
            make_struct!((8;)),
            make_struct!((9;)),
            make_struct!((10;)),
            make_struct!((11;)),
            make_struct!((12;))

        ].iter().cloned().collect(),
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
            // can only really make &str types, str is for now innacurate
            make_kind!(str),
            make_kind!(String),
            make_kind!(()),
            make_kind!((2;)),
            make_kind!((3;)),
            make_kind!((4;)),
            // make_kind!((5;)),
            // make_kind!((6;)),
            // make_kind!((7;)),
            // make_kind!((8;)),
            // make_kind!((9;)),
            // make_kind!((10;)),
            // make_kind!((11;)),
            // make_kind!((12;))
        ].iter().cloned().collect(),
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn matches_generics() {
        assert!(make_type!(Option[i32]).matches(&make_type!(Option{T}[T])));
    }
    #[test]
    fn not_matches_different_parameters() {
        assert!(!make_type!(Option[i32]).matches(&make_type!(Option[str])));
    }
    #[test]
    fn fn_return_type_matches() {
        assert!(make_type!(%Fn{T}(T) -> Box[T]).type_args[1].matches(&make_type!(Box[u16])));
    }
}
