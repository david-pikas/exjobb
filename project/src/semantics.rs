use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;
use arbitrary::{Arbitrary, Unstructured};
use crate::context_arbitrary::{GenerationError, choose_consume, c_arbitrary_iter_with_non_mut};
use crate::context_arbitrary as context_arbitrary;

use super::context::Context;

#[derive(Clone, Debug)]
pub enum StringWrapper {
    Static(&'static str),
    Dynamic(Rc<String>)
}

impl Hash for StringWrapper {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl PartialEq<StringWrapper> for StringWrapper {
    fn eq(&self, other: &StringWrapper) -> bool {
        match (self, other) {
            (StringWrapper::Static(s1), StringWrapper::Static(s2)) => s1 == s2,
            (StringWrapper::Static(s1), StringWrapper::Dynamic(s2)) => s1 == &(*s2).as_str(),
            (StringWrapper::Dynamic(s1), StringWrapper::Static(s2)) => &(*s1).as_str() == s2,
            (StringWrapper::Dynamic(s1), StringWrapper::Dynamic(s2)) => s1 == s2
        }
    }
}
impl Eq for StringWrapper {}

impl StringWrapper {
    pub fn as_str<'a>(&'a self) -> &'a str {
        match self {
            StringWrapper::Static(s) => s,
            StringWrapper::Dynamic(s) => &*s
        }
    }
}

impl PartialEq<String> for StringWrapper {
    fn eq(&self, other: &String) -> bool {
        match self {
            StringWrapper::Static(s) => s == &other.as_str(),
            StringWrapper::Dynamic(s) => &**s == other
        }
    }
}


impl PartialEq<&str> for StringWrapper {
    fn eq(&self, other: &&str) -> bool {
        match self {
            StringWrapper::Static(s) => s == other,
            StringWrapper::Dynamic(s) => &(**s).as_str() == other
        }
    }
}

impl From<String> for StringWrapper {
    fn from(s: String) -> Self {
        StringWrapper::Dynamic(Rc::new(s))
    }
}

impl From<&'static str> for StringWrapper {
    fn from(s: &'static str) -> Self {
        StringWrapper::Static(s)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Mutability { Immutable, Unnasigned, Mutable }
#[derive(PartialEq, Clone, Debug)]
pub struct Lifetime(pub StringWrapper);
#[derive(PartialEq, Clone, Debug)]
pub struct Type {
    pub name: StringWrapper,
    pub mutability: Mutability,
    pub lt_generics: Vec<Lifetime>,
    pub type_generics: Vec<StringWrapper>,
    pub lt_args: Vec<Lifetime>,
    pub type_args: Vec<Type>,
    pub func: Option<Box<Func>>
}
#[derive(PartialEq, Clone, Debug)]
pub struct Func {
    pub args: Vec<Type>,
    pub ret_type: Type
}
#[derive(PartialEq, Clone, Debug)]
pub struct Kind { pub lifetimes: u8, pub types: u8 }
#[derive(PartialEq, Clone, Debug)]
pub struct Field { pub visible: bool, pub ty: Type }
#[derive(PartialEq, Clone, Debug)]
pub enum Fields { Unnamed(Vec<Field>), Named(HashMap<StringWrapper, Field>) }
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
pub enum Token { Expr(Expr), Ident(StringWrapper), Type(Type) }
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
#[derive(PartialEq, Clone, Debug)]
pub struct Method {
    pub name: StringWrapper,
    pub self_param: bool,
    pub lt_generics: Vec<Lifetime>,
    pub lt_args: Vec<Lifetime>,
    pub type_generics: Vec<StringWrapper>,
    pub type_args: Vec<Type>,
    pub func: Func
}
pub type ValScope = HashMap<StringWrapper, Type>;
pub type TypeScope = HashMap<StringWrapper, Kind>;
pub type StructScope = HashMap<StringWrapper, Struct>;
pub type MacroScope = HashMap<StringWrapper, Macro>;
pub type MethodScope = Vec<(Type, Vec<Method>)>;
#[derive(PartialEq, Clone, Debug)]
pub struct Scope {
    pub owned: bool,
    pub vals: ValScope,
    pub types: TypeScope,
    pub structs: StructScope,
    pub macros: MacroScope,
    pub methods: MethodScope
}
pub type Stack<T> = Vec<T>;
#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Lit(StringWrapper, Vec<Type>),
    Struct(StringWrapper, Vec<Type>, HashMap<StringWrapper, Expr>),
    Fn(StringWrapper, Vec<Type>, Vec<Expr>),
    AssocFn(StringWrapper, Vec<Type>, StringWrapper, Vec<Type>, Vec<Expr>),
    Method(Box<Expr>, StringWrapper, Vec<Type>, Vec<Expr>),
    Var(StringWrapper),
    Macro(StringWrapper, MacroBody)
}


impl Type {
    pub fn matches(&self, other: &Type) -> bool {
        self.matches_with_generics(other, &self.type_generics)
    }
    pub fn matches_with_generics(&self, other: &Type, generics: &Vec<StringWrapper>) -> bool {
        //! NOT reflexive: Option<N>.matches(Option<i32>) but not Option<i32>.matches(Option<N>)

        // if self.name == "Result" && other.name == "Result" {
        //     println!("Checking if {:?} and {:?} match...\n", self, other);
        // }

        // All types are equal to the "never" type

        // \forall x: !.matches(x)
        // \forall x: x != ! ==> not x.matches(!)
        if self.name == "!" {
            return true;
        }
        if self.name != other.name {
            return false;
        }
        let zipped = self.type_args.iter().zip(other.type_args.iter());
        for (self_arg, other_arg) in zipped {
            let self_arg_is_generic = generics.contains(&self_arg.name);
            if !self_arg_is_generic && !self_arg.matches(other_arg) {
                // println!("Type arguments where different: {:?}, {:?}", self_arg, other_arg);
                return false;
            }
        }
        return true;
    }
    pub fn contains(&self, name: &StringWrapper) -> bool {
        &self.name == name || self.type_args.iter().any(|arg| arg.contains(name))
    }
    pub fn diff<'a>(&self, other: &'a Type) -> Option<&'a Type> {
        if self.name != other.name {
            return Some(other);
        }
        for (self_arg, other_arg) in self.type_args.iter().zip(other.type_args.iter()) {
            if let Some(result) = self_arg.diff(other_arg) {
                return Some(result);
            }
        }
        return None;
    }
}

#[allow(dead_code)]
pub fn push_scope(ctx: &mut Context) {
    ctx.scopes.push(Scope {
        owned: true,
        macros: HashMap::new(),
        methods: vec![],
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
pub fn add_var<'a>(ctx: &mut Context, name: StringWrapper, ty: Type) {
    ctx.scopes.last_mut().map(|l| l.vals.insert(name, ty));
}

#[allow(dead_code)]
pub fn add_type<'a>(ctx: &mut Context, name: StringWrapper, ty: Kind) {
    ctx.scopes.last_mut().map(|l| l.types.insert(name, ty));
}

#[allow(dead_code)]
pub fn add_struct(ctx: &mut Context, name: StringWrapper, struc: Struct) {
    ctx.scopes.last_mut().map(|l| l.structs.insert(name, struc));
}

#[allow(dead_code)]
pub fn lookup_var<'a,'b>(ctx: &'b mut Context, name: &StringWrapper) -> Option<&'b Type> {
    for scope in ctx.scopes.iter().rev() {
        if let Some(ty) = scope.vals.get(name) {
            return Some(ty);
        }
    }
    return None;
}

#[allow(dead_code)]
pub fn lookup_type<'a,'b, S: Into<StringWrapper>>(ctx: &'b mut Context, name: S) -> Option<&'b Kind> {
    let wrapped_name = name.into();
    for scope in ctx.scopes.iter().rev() {
        if let Some(kind) = scope.types.get(&wrapped_name) {
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
) -> context_arbitrary::Result<(&'b StringWrapper, &'b Type)>
where F: Fn(&StringWrapper, &Type) -> bool {
    let scope = u.choose(&ctx.scopes)?;
    let vars: Vec<(&StringWrapper, &Type)> = scope.vals.iter().filter(|(n,t)| pred(n,t)).collect();
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
) -> context_arbitrary::Result<(&'b StringWrapper, &'b Type)> {
    pick_var_that(ctx, u, |_,_| true)
}


#[allow(dead_code)]
pub fn pick_type<'a>(ctx: &Context, u: &mut Unstructured<'a>) -> arbitrary::Result<Type> {
    let non_empty_scopes: Vec<&Scope> =
        ctx.scopes.iter().filter(|s| !s.types.is_empty()).collect();
    let scope: &Scope = choose_consume(u, non_empty_scopes.into_iter())?;
    let kinds: Vec<(&StringWrapper, &Kind)> = scope.types.iter().collect();
    let &(name, kind) = u.choose(&kinds)?;
    let n_of_args = kind.types;
    // TODO: add lifetimes
    // TODO: fix mutability
    // TODO: pick functions
    // TODO: handle not picking ! more elegantly 
    Ok(Type { 
        name: if name == &"!" {
            "#Unit".into()
        } else {
            name.clone()
        },
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
        lt_args: vec![],
        func: None
    })
}

#[allow(dead_code)]
pub fn construct_value<'a>(
    ctx: &Context,
    u: &mut Unstructured<'a>,
    ty: Type,
    allow_ambiguity: bool
) -> context_arbitrary::Result<Expr> {
    
    if ty.name == "T" {
        panic!("Construct value called with a generic");
    }

    // println!("construct_value called on type {:?}", ty);

    fn fields_to_struct(ctx: &Context, u: &mut Unstructured, name: StringWrapper, params: Vec<Type>, fields: HashMap<StringWrapper, Type>) -> context_arbitrary::Result<Expr> {
        // println!("fields_to_struct called with: {:?}", fields);
        let mut fields_expr: HashMap<StringWrapper, Expr> = HashMap::new();
        for f_name in fields.keys() {
            fields_expr.insert(f_name.clone(), construct_value(ctx, u, fields[f_name].clone(), true)?);
        }
        Ok(Expr::Struct(name, params, fields_expr))
    }

    fn handle_field_generics(generics: Vec<(StringWrapper, Type)>, fields: &HashMap<StringWrapper, Field>) -> HashMap<StringWrapper, Type> {
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

    fn handle_arg_generics(generics: Vec<(StringWrapper, Type)>, args: &Vec<Type>) -> Vec<Type> {
        // println!("handle_arg_generics called on {:?}, {:?}\n", generics, args);
        let mut new_args = vec![];
        'vec_args: for arg in args {
            for (ty_name, ty) in &generics {
                if ty_name == &arg.name {
                    new_args.push(ty.clone());
                    continue 'vec_args;
                }
            }
            new_args.push(arg.clone());
        }
        return new_args;
    }

    fn make_generics(u: &mut Unstructured, ty1: &Type, ty2: &Type) -> arbitrary::Result<Vec<Type>> {
        let mut result = vec![];
        let mut generics_required = false;

        for (i, name) in ty2.type_generics.iter().enumerate() {
            if let Some(arg) = ty2.type_args.get(i) {
                generics_required = true;
                result.push(arg.clone());
            } else {
                result.push(ty1.type_args.get(i).map_or_else(
                    || Type {
                        name: name.clone(), 
                        mutability: Mutability::Immutable,
                        type_generics: vec![],
                        lt_generics: vec![],
                        type_args: vec![],
                        lt_args: vec![],
                        func: None
                    }, 
                    |ty| ty.clone()
                ).clone());
            }
        }
    
        Ok(if !generics_required && Arbitrary::arbitrary(u)? {
            vec![]
        } else {
            result
        })

    }

    fn make_fn_generics(
        ctx: &Context, u: &mut Unstructured,
        allow_ambiguity: bool,
        ty1: &Type, 
        type_generics: &Vec<StringWrapper>, type_args: &Vec<Type>, func: &Func
    ) -> arbitrary::Result<(Option<Vec<usize>>, Vec<Type>)> {
    //! The output type is perhaps a bit counter-intuitive,
    //! (None, tys) means that the function *should* be given
    //! type parameters and that all parameters can be ambigous
    //! (Some(determined_args]), tys) mean that the function
    //! *should not* be given type parameters and if the index
    //! of an argument is in determined_args it can't be ambigous.
    //!
    //! (None, vec![Vec<u32>]) ==> drop::<Vec<u32>>(Vec::new())
    //! (Some(vec![1]), vec![Vec<u32>]) ==> drop(Vec::<u32>::new())
        let mut result = vec![];
        let mut args_determined_by_generics = vec![];
        let mut generics_required = false;

        for (i, gen) in type_generics.iter().enumerate() {
            if func.ret_type.contains(gen) {
                result.push(
                    type_args.get(i)
                        .or(ty1.type_args.get(i))
                        .or_else(|| func.ret_type.diff(ty1))
                        .map_or_else(
                            || Type {
                                name: gen.clone(),
                                mutability: Mutability::Immutable,
                                type_args: vec![],
                                type_generics: vec![],
                                lt_generics: vec![],
                                lt_args: vec![],
                                func: None
                            },
                            |ty| { generics_required |= !allow_ambiguity;
                                   ty.clone() }
                                    
                        )
                );
            } else if func.args.iter().any(|arg| arg.contains(gen)) {
                result.push(if let Some(arg) = type_args.get(i) {
                    arg.clone()
                } else {
                    pick_type(ctx, u)?
                });
                args_determined_by_generics.push(i);
            } else {
                generics_required = true;
                result.push(if let Some(arg) = type_args.get(i) {
                    arg.clone()
                } else {
                    pick_type(ctx, u)?
                });
            }
        }

        if !generics_required && Arbitrary::arbitrary(u)? {
            Ok((Some(args_determined_by_generics), result))
        } else {
            Ok((None, result))
        }

    }

    let mut possible_exprs: Vec<Box<dyn Fn(&Context, &mut Unstructured) -> context_arbitrary::Result<Expr>>> = vec![];

    for scope in ctx.scopes.iter() {
        for (name, struc) in scope.structs.iter() {
            if struc.ty.matches(&ty) {
                // TODO: do we really need to clone this twice?
                let type_generics = struc.ty.type_generics.clone();
                let type_args = ty.type_args.clone();
                let ty_clone = ty.clone();
                possible_exprs.push(Box::new(move |ctx, u| {
                    let generics =
                        type_generics.clone().into_iter()
                          .zip(type_args.clone())
                          .collect();
                    let generic_params = make_generics(u, &ty_clone.clone(), &struc.ty)?;
                    if name.as_str().starts_with("#Tuple") {
                        // println!("Tuple {:?}", struc);
                    }
                    match &struc.fields {
                        Fields::Named(fields) if !fields.is_empty() => {
                            fields_to_struct(ctx, u, name.clone(), generic_params, handle_field_generics(generics, &fields))
                        }
                        Fields::Unnamed(fields) if !fields.is_empty() => {
                            let args = fields.into_iter().map(|f| f.ty.to_owned()).collect();
                            Ok(Expr::Fn(
                                name.clone(),
                                generic_params,
                                handle_arg_generics(generics, &args)
                                    .into_iter()
                                    .map(|t| construct_value(ctx, u, t, true))
                                    .collect::<context_arbitrary::Result<Vec<Expr>>>()?
                            ))
                        }
                        _ => Ok(Expr::Lit(name.clone(), generic_params))
                    }
                }));
            }
        }
        for (name, var_ty) in scope.vals.iter() {
            if var_ty.matches(&ty) {
                possible_exprs.push(Box::new(move |_,_| Ok(Expr::Var(name.clone()))));
            } else if let Some(func) = &var_ty.func {
                let ty_clone = ty.clone();
                if func.ret_type.matches_with_generics(&ty, &var_ty.type_generics)  {
                    let type_generics = var_ty.type_generics.clone();
                    let type_args = var_ty.type_args.clone();
                    possible_exprs.push(Box::new(move |ctx, u| {
                        let mut args = vec![];
                        let (determined_args, generic_params) = make_fn_generics(
                            ctx, u, 
                            allow_ambiguity,
                            &ty_clone.clone(),
                            &type_generics,
                            &type_args.clone(),
                            func
                        )?;
                        let generics = type_generics.clone().into_iter().zip(generic_params.clone()).collect();
                        // println!("generics {:?} {:?}", type_generics, generic_params);
                        for (i, arg) in handle_arg_generics(generics, &func.args).into_iter().enumerate() {
                            let arg_ambigous = determined_args.as_ref().map_or(false,
                                |determined| !determined.contains(&i)
                            );
                            args.push(construct_value(ctx, u, arg, arg_ambigous)?);
                        }
                        let given_ty_args = if determined_args.is_none() {
                            generic_params
                        } else {
                            vec![]
                        };
                        Ok(Expr::Fn(name.clone(), given_ty_args, args))
                    }));
                }
            }
        }
        for (assoc_ty, methods) in scope.methods.iter() {
            for method in methods {
                let mut ret_gen = Vec::new();
                ret_gen.extend(assoc_ty.type_generics.clone()); 
                ret_gen.extend(method.type_generics.clone());
                if method.func.ret_type.matches_with_generics(&ty, &ret_gen) {
                    let ty_clone = ty.clone();
                    let type_generics = assoc_ty.type_generics.clone();
                    possible_exprs.push(Box::new(move |ctx, u| {
                        let (_, disc_ty_args) = make_fn_generics(
                            ctx, u, 
                            allow_ambiguity,
                            &ty_clone.clone(),
                            &type_generics.clone(),
                            &assoc_ty.type_args.clone(),
                            &method.func
                        )?;
                        let (determined_args, method_ty_args) = make_fn_generics(
                            ctx, u, 
                            allow_ambiguity,
                            &ty_clone.clone(),
                            &method.type_generics.clone(),
                            &method.type_args.clone(),
                            &method.func
                        )?;

                        let generics = 
                            assoc_ty.type_generics.clone().into_iter()
                                .chain(method.type_generics.clone().into_iter())
                                .zip(disc_ty_args.clone().into_iter()
                                    .chain(method_ty_args.clone().into_iter()))
                                .collect();
                        let mut args = vec![];
                        for (i, arg) in handle_arg_generics(generics, &method.func.args).into_iter().enumerate() {
                            let arg_ambigous = determined_args.as_ref().map_or(false,
                                |determined| !determined.contains(&i)
                            );
                            args.push(construct_value(ctx, u, arg, arg_ambigous)?);
                        }
                        let given_ty_args = if determined_args.is_none() {
                            method_ty_args
                        } else {
                            vec![]
                        };
                        if method.self_param {
                            Ok(Expr::Method(
                                Box::new(construct_value(ctx, u, Type {
                                    type_args: disc_ty_args,
                                    ..assoc_ty.clone()
                                }, false)?),
                                method.name.clone(), 
                                given_ty_args,
                                args
                            ))
                        } else {
                            Ok(Expr::AssocFn(
                                assoc_ty.name.clone(),
                                disc_ty_args,
                                method.name.clone(),
                                given_ty_args,
                                args   
                            ))
                        }
                    }))
                }
            }
        }
        if allow_ambiguity {
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
    }

    if possible_exprs.is_empty() {
        println!("Couldn't find any values for type {:?}", ty);
    }
    let choice = choose_consume(u, possible_exprs.iter())?;
    return choice(ctx, u);
}

pub fn kind_to_type<S: Into<StringWrapper>>(name: S, mutability: Mutability, kind: &Kind) -> Type {
    Type {
        name: name.into(),
        mutability,
        lt_generics: GEN_STRING[0..(kind.lifetimes as usize)].iter()
                            .map(|s| Lifetime((*s).into())).collect(),
        type_generics: GEN_STRING[0..(kind.types as usize)].iter()
                            .map(|s| (*s).into()).collect(),
        lt_args: vec![],
        type_args: vec![],
        func: None
    }
}

pub fn name_to_type<S: Into<StringWrapper>>(name: S) -> Type {
    Type {
        name: name.into(),
        mutability: Mutability::Immutable,
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args: vec![],
        func: None
    }
}

static TUPLE_NAMES: [&'static str; 12] = [
    "#Tuple1", "#Tuple2" , "#Tuple3" , "#Tuple4" ,
    "#Tuple5", "#Tuple6" , "#Tuple7" , "#Tuple8" ,
    "#Tuple9", "#Tuple10", "#Tuple11", "#Tuple12",
];

macro_rules! make_val {
    ($name:path : $($ty:tt)*) => ((stringify!($name).into(), make_type!($($ty)*)))
}

#[macro_export]
macro_rules! make_type {
    (!) => (crate::semantics::Type {
        name: "!".into(),
        mutability: crate::semantics::Mutability::Immutable,
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args: vec![],
        func: None
    });
    (%Fn($($args:tt)*) $($ret:tt)*) =>
        (make_type!(%Fn{}($($args)*) $($ret)*));
    (%Fn{$($gen:tt)*}($($args:tt)*)) => 
        (make_type!(%Fn{$($gen)*}($($args)*) -> ()));
    (%Fn{$($gen:tt)*}($($args:tt)*) -> $($ret:tt)*) => {
        {
            let (lt_generics, type_generics) = parse_generics!([],[];$($gen)*,);
            let mut ret_type = make_type!($($ret)*);
            let (_, mut args): (Vec<Lifetime>, Vec<Type>) = parse_args!([],[];$($args)*,);
            ret_type.lt_generics.extend(lt_generics.clone());
            ret_type.type_generics.extend(type_generics.clone());
            for arg in &mut args {
                arg.lt_generics.extend(lt_generics.clone());
                arg.type_generics.extend(type_generics.clone());
            }
            crate::semantics::Type {
                name: "#Fn".into(),
                mutability: crate::semantics::Mutability::Immutable,
                lt_generics, type_generics,
                lt_args: vec![],
                type_args: vec![],
                func: Some(Box::new(Func {
                    args,
                    ret_type
                }))
            }
        }
    };
    ($name:path$({})?$([])?) => (crate::semantics::Type {
        name: stringify!($name).into(),
        mutability: crate::semantics::Mutability::Immutable,
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args: vec![],
        func: None
    });
    (()) => (crate::semantics::Type {
        name: "#Unit".into(),
        mutability: crate::semantics::Mutability::Immutable,
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args: vec![],
        func: None
    });
    (($len:expr; $fst:expr $(,$rest:expr)*)) => (crate::semantics::Type {
        name: TUPLE_NAMES[len-1].into(),
        mutability: crate::semantics::Mutability::Immutable,
        lt_generics: vec![],
        type_generics: vec![make_type!($fst) $(, make_type!($rest))*],
        lt_args: vec![],
        type_args: vec![],
        func: None
    });
    ($name:path[$($args:tt)*]) => (make_type!($name{}[$($args)*]));
    ($name:path{$($args:tt)*}) => (make_type!($name{$($args)*}[]));
    ($name:path{$($gen:tt)*}[$($args:tt)*]) => {
        {
            let (lt_generics, type_generics) = parse_generics!([],[];$($gen)*,);
            let (lt_args, type_args) = parse_args!([],[];$($args)*,);
            crate::semantics::Type {
                name: stringify!($name).into(),
                mutability: Mutability::Immutable,
                lt_generics, type_generics,
                lt_args, type_args,
                func: None
            }
        }
    };
}

macro_rules! parse_args {
    ([$(,$lt:expr)*],[$(,$ty:expr)*];$(,)?) => ( (vec![$($lt),*],vec![$($ty),*]) );
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$lt:lifetime, $($arg:tt)*) => {
        parse_args!([$(,$lts)*,Lifetime(stringify!($lt).into())],[$(,$tys)*];$($arg)*)
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
        parse_generics!([$(,$lts)*,Lifetime(stringify!($lt).into())],[$(,$tys)*];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$name:ident, $($arg:tt)*) => {
        parse_generics!([$(,$lts)*],[$(,$tys)*, stringify!($name).into()];$($arg)*)
    };
}

macro_rules! make_kind {
    ($name:path$({})?) => {
        (stringify!($name).into(), crate::semantics::Kind { lifetimes: 0, types: 0 })
    };
    ($name:path{$ty:expr}) => {
        (stringify!($name).into(), crate::semantics::Kind { lifetimes: 0, types: $ty })
    };
    (()) => (("#Unit".into(), crate::semantics::Kind { lifetimes: 0, types: 0 }));
    (($len:expr;)) => {
        (TUPLE_NAMES[$len-1].into(), crate::semantics::Kind { lifetimes: 0, types: $len })
    };
    ($name:path{$lt:expr; $ty:expr}) => {
        (stringify!($name).into(), Kind { lifetimes: $lt, types: $ty })
    };
}

static GEN_STRING: [&'static str; 12] = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"];

macro_rules! make_methods {
    ($name:path$([$($type_args:tt)*])? {$($methods:tt)*}) => 
        (make_methods!(#($name$([$($type_args)*])?){$($methods)*}));
    (#($($ty:tt)*){$($methods:tt)*}) => (
        (make_type!($($ty)*), parse_methods!(;$($methods)*,))
    );
}

macro_rules! parse_methods {
    ($(,$expr:expr)*;$(,)?) => (vec![$($expr),*]);
    ($(,$expr:expr)*;
        $name:ident$({$($gen:tt)*})?($($args:tt)*) 
            -> $ret:path$([$($ret_arg:tt)*])?, $($rest:tt)*) => {
        parse_methods!($(,$expr)*; $name$({$($gen)*})?($($args)*)
            -> #($ret$([$($ret_arg)*])?), $($rest)*)
    };
    ($(,$expr:expr)*;
        $name:ident$({$($gen:tt)*})?($($args:tt)*), $($rest:tt)*) => {
        parse_methods!($(,$expr)*; $name$({$($gen)*})?($($args)*) -> #(()), $($rest)*)
    };
    ($(,$expr:expr)*;
        $name:ident$({$($gen:tt)*})?($($args:tt)*) -> #($($ret:tt)*), $($rest:tt)*) => {
        parse_methods!($(,$expr)*, {
            let (self_param, args): (bool, Vec<Type>) = parse_method_args!($($args)*,);
            let ret_type = make_type!($($ret)*);
            let (lt_generics, type_generics) = parse_generics!([],[];$($gen)*,);
            crate::semantics::Method {
                name: stringify!($name).into(),
                self_param,
                lt_generics,
                type_generics,
                lt_args: vec![],
                type_args: vec![],
                func: Func {
                    args,
                    ret_type
                }
            }
        }; $($rest)*)
    }
}

macro_rules! parse_method_args {
    (self, $($args:tt)*) => ((true, parse_types!(;$($args)*)));
    ($($args:tt)*) => ((false, parse_types!(;$($args)*)))
}

macro_rules! parse_types {
    ($(,$tys:expr)*;$(,)?) => (vec![$($tys),*]);
    ($(,$tys:expr)*;$name:path$({$($gen:tt)*})?$([$($type_args:tt)*])?, $($rest:tt)*) => 
        (parse_types!($(,$tys)*;#($name$({$($gen)*})?$([$($type_args)*])?), $($rest)*));
    ($(,$tys:expr)*;#($($ty:tt)*), $($rest:tt)*) => 
        (parse_types!($(,$tys)*,make_type!($($ty)*);$($rest)*));
}


macro_rules! make_struct {
    (()) => (("#Unit".into(), crate::semantics::Struct {
        ty: make_type!(()),
        fields: Fields::Unnamed(vec![])
    }));
    (($len:expr;)) => ({
        let len = $len;
        (TUPLE_NAMES[len-1].into(), crate::semantics::Struct {
            ty: Type {
                name: TUPLE_NAMES[len-1].into(),
                mutability: Mutability::Immutable,
                lt_args: vec![],
                lt_generics: vec![],
                type_generics: GEN_STRING[0..len].iter().map(|s| s.clone().into()).collect(),
                type_args: vec![], //GEN_NAMES[0..len].to_vec(),
                func: None
            },
            fields: Fields::Unnamed(GEN_STRING[0..len].iter().map(|name| Field {
                visible: true,
                ty: name_to_type(name.to_owned())
            }).collect())
        })
    });
    ($(:$ty:path:)? $name:path$([$($gen:tt)*])? ) =>
        (make_struct!($(:$ty:)? $name$([$($gen)*])? {}));
    ($name:ident$([$($gen:tt)*])? ( $($fields:tt)* )) =>
        (make_struct!(:$name:$name$([$($gen)*])? ( $($fields)* )));
    ($name:path$([$($gen:tt)*])? { $($fields:tt)* }) =>
        (make_struct!(:$name:$name$([$($gen)*])? { $($fields)* }));
    (:$ty:path: $name:ident $([$($gen:tt)*])? ( $($fields:tt)* )) =>
        ((StringWrapper::Static(stringify!($name)), crate::semantics::Struct {
            ty: make_type!($ty$({$($gen)*})?),
            fields: parse_unnamed_fields!(;$($fields)*,)
        }));
    (:$ty:path: $name:path $([$($gen:tt)*])? { $($fields:tt)* }) =>
        ((stringify!($name).into(), crate::semantics::Struct {
            ty: make_type!($ty$({$($gen)*})?),
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
        parse_named_fields!($(,$fields)*, (stringify!($name).into(), crate::semantics::Field {
            visible: true,
            ty: make_type!($($ty)*; $($rest)*)
        }; $($rest)*));
    };
    ($(,$fields:expr)*;$name:ident : #($($ty:tt)*), $($rest:tt)*) => {
        parse_named_fields!($(,$fields)*, (stringify!($name).into(), crate::semantics::Field {
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
        (stringify!($name).into(), Macro {
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
        ].iter().cloned().collect(), 
        methods: vec![
            make_methods!(#(Box{T}) {
                new(T) -> Box[T]
            }),
            make_methods!(#(Vec{T}) {
                new() -> Vec[T],
                with_capacity(usize) -> Vec[T],
                capacity(self) -> usize,
                reserve(self, usize),
                reserve_exact(self, usize),
                shrink_to_fit(self),
                // into_boxed_slie(self) -> Box{[T]},
                truncate(self, usize),
                // set_len(self, new_len),
                swap_remove(self, usize) -> T,
                insert(self, usize, T),
                remove(self, usize) -> T,
                // retain{F}(self, f),
                push(self, T),
                pop(self) -> Option[T],
                // append(self, &Vec{T})
                len(self) -> usize,
                is_empty(self) -> bool,
                split_off(self, usize) -> Vec[T]
            })
        ],
        macros: [
            make_macro!(#(Vec{T}) : vec(|ty_args, ctx, u| {
                Ok(MacroBody {
                    brackets: if Arbitrary::arbitrary(u)? {
                        BracketType::Round
                    } else {
                        BracketType::Square
                    },
                    tokens: c_arbitrary_iter_with_non_mut(ctx, u, |ctx, u| {
                        Ok(Token::Expr(construct_value(ctx, u, ty_args[0].clone(), true)?))
                    }).collect::<context_arbitrary::Result<Vec<Token>>>()?,
                    seperator: Seperator::Comma
                })
            })),
            make_macro!(#(!) : panic(|_, ctx, u| {
                Ok(MacroBody {
                    brackets: BracketType::Round,
                    tokens: vec![Token::Expr(construct_value(ctx, u, make_type!(str), true)?)],
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
            // make_kind!(std::io::Error)
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
        methods: vec![
            make_methods!(String {
                new() -> String
            })
        ],
        structs: [
            make_struct!(bool),
            make_struct!(usize),
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
            // make_kind!(str),
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
            ("!".into(), Kind { lifetimes: 0, types: 0 })
        ].iter().cloned().collect(),
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn matches_generics() {
        assert!(make_type!(Option{T}[T]).matches(&make_type!(Option[i32])));
    }
    #[test]
    fn not_matches_different_parameters() {
        assert!(!make_type!(Option[i32]).matches(&make_type!(Option[str])));
    }
    #[test]
    fn different_specificity_matches_correctly() {
        assert!(!make_type!(Option[i32]).matches(&make_type!(Option{T}[T])));
        assert!( make_type!(Option{T}[T]).matches(&make_type!(Option[i32])));
    }
    #[test]
    fn fn_return_type_matches() {
        assert!(make_type!(%Fn{T}(T) -> Box[T]).func.unwrap().ret_type.matches(&make_type!(Box[u16])));
    }
    #[test]
    fn assoc_type_generics() {
        assert!(make_methods!(#(Vec{T}) { a(), b() }).0.type_generics.len() == 1)
    }
    #[test]
    fn struct_type_generics() {
        assert!(make_struct!(:Result:Ok[R,E](R)).1.ty.type_generics.len() == 2)
    }
}
