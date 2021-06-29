use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::iter;
use std::rc::Rc;
use arbitrary::{Arbitrary, Unstructured};
use crate::context_arbitrary::{GenerationError, choose_consume, c_arbitrary_iter_with_non_mut};
use crate::context_arbitrary as context_arbitrary;

use super::context::Context;

#[derive(Clone, Debug)]
/// We have statically known names for things (like bool) as well as dynamically generated ones
/// This type allows using them interchangeably and cloning without actually copying the strings
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
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}",
            self.name.as_str(),
            if self.type_generics.is_empty() {
                "".to_string()
            } else {
                format!("{{{}}}",
                    self.type_generics.iter()
                        .map(|a| format!("{}", a.as_str()))
                        .collect::<String>()
                )
            },
            if self.type_generics.is_empty() {
                "".to_string()
            } else {
                format!("{{{}}}",
                    self.type_generics.iter()
                        .map(|a| format!("{}", a.as_str()))
                        .collect::<String>()
                )
            })
    }
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
pub enum Fields { None, Unnamed(Vec<Field>), Named(HashMap<StringWrapper, Field>) }
pub enum FieldsIter<'a> {
    Unnamed(std::slice::Iter<'a, Field>),
    Named(std::collections::hash_map::Values<'a, StringWrapper, Field>),
    None
}
impl<'a> Iterator for FieldsIter<'a> {
    type Item = &'a Field;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            FieldsIter::Unnamed(vec_iter) => vec_iter.next(),
            FieldsIter::Named(values_iter) => values_iter.next(),
            FieldsIter::None => None
        }
    }
}
impl Fields {
    fn iter(&self) -> FieldsIter {
        match self {
            Fields::Unnamed(vec) => FieldsIter::Unnamed(vec.iter()),
            Fields::Named(hm) => FieldsIter::Named(hm.values()),
            Fields::None => FieldsIter::None
        }
    }
}
impl IntoIterator for Fields {
    type Item = Field;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Fields::Unnamed(v) => v.into_iter(),
            Fields::Named(hm) =>
                hm.into_iter().map(|(_,v)| v)
                    .collect::<Vec<Self::Item>>().into_iter(),
            Fields::None => vec![].into_iter()
        }
    }
}
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
pub type ValScope = HashMap<StringWrapper, Rc<Type>>;
pub type TypeScope = HashMap<StringWrapper, Kind>;
pub type StructScope = HashMap<StringWrapper, Rc<Struct>>;
pub type MacroScope = HashMap<StringWrapper, Rc<Macro>>;
pub type MethodScope = Vec<(Rc<Type>, Vec<Rc<Method>>)>;
#[derive(Clone, Debug)]
pub struct Scope {
    pub owned: bool,
    pub vals: ValScope,
    pub types: TypeScope,
    pub structs: StructScope,
    pub macros: MacroScope,
    pub methods: MethodScope,
    pub by_ty_name: HashMap<StringWrapper, TypeIndexed>
}
impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        self.owned == other.owned &&
        self.vals == other.vals &&
        self.types == other.types &&
        self.structs == other.structs &&
        self.macros == other.macros
    }
}
impl Scope {
    fn make_ty_index(&mut self) -> &mut Self {
        for (name, struc) in &self.structs {
            self.by_ty_name.entry(struc.ty.name.clone())
                .or_insert(Default::default())
                .structs.insert(name.clone(), struc.clone());
        }
        for (name, val) in &self.vals {
            if let Some(func) = &val.func {
                self.by_ty_name.entry(func.ret_type.name.clone())
                    .or_insert(Default::default())
                    .vals.insert(name.clone(), val.clone());
            } else {
                self.by_ty_name.entry(val.name.clone())
                    .or_insert(Default::default())
                    .vals.insert(name.clone(), val.clone());
            }
        }
        for (name, macr) in &self.macros {
            self.by_ty_name.entry(macr.ty.name.clone())
                .or_insert(Default::default())
                .macros.insert(name.clone(), macr.clone());
        }
        for (assoc_ty, methods) in &self.methods {
            for method in methods {
                self.by_ty_name.entry(method.func.ret_type.name.clone())
                    .or_insert(Default::default())
                    .methods.push((assoc_ty.clone(), method.clone()));
            }
        }
        return self;
    }
}
#[derive(Clone, Debug, Default)]
pub struct TypeIndexed {
    pub vals: HashMap<StringWrapper, Rc<Type>>,
    pub structs: HashMap<StringWrapper, Rc<Struct>>,
    pub macros: HashMap<StringWrapper, Rc<Macro>>,
    pub methods: Vec<(Rc<Type>, Rc<Method>)>
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
    Macro(StringWrapper, MacroBody),
    ExactString(String)
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

        // The never type can be coerced into any type, but no type can be 
        // coerced into the never type

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
        &self.name == name
        || self.type_args.iter().any(|arg| arg.contains(name))
    }
    pub fn diff<'a>(&'a self, other: &'a Type) -> Box<dyn Iterator<Item = (&StringWrapper, &Type)> + 'a> {
        if self.name != other.name {
            return Box::new(iter::once((&self.name, other)));
        } else {
            Box::new(
                self.type_args.iter()
                    .zip(other.type_args.iter())
                    .flat_map(|(a,b)| a.diff(b))
                    .chain(self.type_generics.iter()
                            .zip(other.type_args.iter().skip(self.type_args.len())))
            )
        }
    }
    pub fn replace(&self, name: &StringWrapper, other: &Type) -> Type {
        if &self.name == name {
            return other.clone()
        }
        Type {
            type_args: self.type_args.iter().map(|arg| arg.replace(name, other)).collect(),
            ..self.clone()
        }
    }
    pub fn is_sized_by(&self, name: &StringWrapper) -> bool {
        !INDIRECT_TYPES.contains(&name.as_str()) &&
        (&self.name == name
         || self.type_args.iter().any(|arg| arg.is_sized_by(name)))
    }
}

const INDIRECT_TYPES: [&str; 4] = ["Rc", "Vec", "Cow", "Arc"];

#[allow(dead_code)]
pub fn push_scope(ctx: &mut Context) {
    ctx.scopes.push(Scope {
        owned: true,
        macros: HashMap::new(),
        methods: vec![],
        vals: HashMap::new(),
        types: HashMap::new(),
        structs: HashMap::new(),
        by_ty_name: HashMap::new()
    });
}

#[allow(dead_code)]
pub fn pop_scope(ctx: &mut Context) {
    ctx.scopes.pop();
}

macro_rules! with_scope {
    ($ctx: ident, $expr:expr) => ({
        push_scope($ctx);
        let result = $expr;
        pop_scope($ctx);
        result
    })
}

#[allow(dead_code)]
pub fn add_var<'a>(ctx: &mut Context, name: StringWrapper, ty: Type) {
    ctx.scopes.last_mut().map(|l| {
        let rc_ty = Rc::new(ty);
        l.by_ty_name.entry(rc_ty.name.clone())
            .or_insert(Default::default())
            .vals.insert(name.clone(), rc_ty.clone());
        l.vals.insert(name, rc_ty);
    });
}

#[allow(dead_code)]
pub fn add_type<'a>(ctx: &mut Context, name: StringWrapper, ty: Kind) {
    ctx.scopes.last_mut().map(|l| l.types.insert(name, ty));
}

#[allow(dead_code)]
pub fn add_struct(ctx: &mut Context, name: StringWrapper, struc: Struct) {
    ctx.scopes.last_mut().map(|l| {
        let rc_struc = Rc::new(struc);
        l.by_ty_name.entry(rc_struc.ty.name.clone())
            .or_insert(Default::default())
            .structs.insert(name.clone(), rc_struc.clone());
        l.structs.insert(name, rc_struc);
    });
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
    let vars: Vec<(&StringWrapper, &Type)> = scope.vals.iter().filter_map(|(n,t_rc)| {
        let t: &Type = t_rc.as_ref();
        if pred(n,t) {
            Some((n,t))
        } else {
            None
        }
    }).collect();
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

    // println!("construct_value called on type {:?}\n", ty);

    fn fields_to_struct(ctx: &Context, u: &mut Unstructured, name: StringWrapper, params: Vec<Type>, fields: HashMap<StringWrapper, Type>) -> context_arbitrary::Result<Expr> {
        // println!("fields_to_struct called with: {:?}", fields);
        let mut fields_expr: HashMap<StringWrapper, Expr> = HashMap::new();
        for f_name in fields.keys() {
            fields_expr.insert(f_name.clone(), construct_value(ctx, u, fields[f_name].clone(), true)?);
        }
        Ok(Expr::Struct(name, params, fields_expr))
    }

    fn handle_field_generics(generics: Vec<(&StringWrapper, Type)>, fields: &HashMap<StringWrapper, Field>) -> HashMap<StringWrapper, Type> {
        // println!("handle_field_generics called on {:?}, {:?}", generics, fields);
        let mut new_map = HashMap::new();
        'outer: for (name, field) in fields.iter() {
            for (ty_name, ty) in generics.iter() {
                if ty_name == &&field.ty.name {
                    new_map.insert(name.to_owned(), ty.clone() );
                    break 'outer;
                }
            }
            new_map.insert(name.clone(), field.ty.clone());
        }
        return new_map;           
    }

    fn handle_arg_generics(generics: Vec<(&StringWrapper, Type)>, args: &Vec<Type>) -> Vec<Type> {
        // println!("handle_arg_generics called on {:?}, {:?}\n", generics, args);
        let mut new_args = vec![];
        'vec_args: for arg in args {
            for (ty_name, ty) in generics.iter() {
                if arg.contains(&ty_name) {
                    new_args.push(ty.replace(&ty_name, &ty));
                    continue 'vec_args;
                }
            }
            new_args.push(arg.clone());
        }
        return new_args;
    }


    fn make_fn_generics(
        ctx: &Context, u: &mut Unstructured,
        allow_ambiguity: bool,
        ty1: &Type, ty2: &Type, 
        args: &Vec<Type>,
        type_generics: &Vec<StringWrapper>, type_args: &Vec<Type>,
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

        let mut output_diff =
            ty2.diff(ty1)
                .collect::<HashMap<&StringWrapper, &Type>>();

        // println!("make_generics called with: {:?} {:?} {:?} {:?} {:?}\n",
            // ty1, ty2, args, type_generics, type_args);
        // println!("diff is {:?}\n", output_diff);

        for (i, gen) in type_generics.iter().enumerate() {
            if ty2.contains(gen) {
                result.push(
                    type_args.get(i)
                        .or_else(|| output_diff.remove(&gen))
                        .map_or_else(
                            || name_to_type(gen.clone()),
                            |ty| {
                                generics_required |= !allow_ambiguity;
                                ty.clone()
                            }
                                    
                        )
                );
            } else if args.iter().any(|arg| arg.contains(gen)) {
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

    pub fn make_struct_generics(
        u: &mut Unstructured,
        allow_ambiguity: bool,
        ty: &Type, 
        fields: Vec<&Type>,
    ) -> arbitrary::Result<Option<Vec<usize>>> {
    //! Similar to make_fn_generics, but much less complicated because the output type
    //! always has the same generics as the struct unlike functions
    
        let mut determined_args = vec![];

        if allow_ambiguity {
            return Ok(Some(vec![]));
        }

        'outer: for gen in ty.type_generics.iter() {
            for (i, field) in fields.iter().enumerate() {
                if field.contains(&gen) {
                    determined_args.push(i);
                    continue 'outer;
                }
            }
            return Ok(None);
        }

        if Arbitrary::arbitrary(u)? {
            return Ok(None);
        } else {
            return Ok(Some(determined_args));
        }
        
    }

    enum PossibleExpression<'a> {
        Struct(&'a StringWrapper, &'a Struct),
        Var(&'a StringWrapper),
        Fn(&'a StringWrapper, &'a Type, &'a Func),
        Method(&'a Method, &'a Type),
        Macro(&'a StringWrapper, &'a Macro)
    }
    let mut possible_exprs: Vec<PossibleExpression> = vec![];

    for scope in ctx.scopes.iter() {

        let by_ty_name = &scope.by_ty_name;

        let matches_ty_iter = 
            by_ty_name.get(&ty.name).map(|cached| {
                (&cached.structs, &cached.vals, &cached.methods, &cached.macros)
            });
        let matches_never_iter = 
            by_ty_name.get(&"!".into()).map(|cached| {
                (&cached.structs, &cached.vals, &cached.methods, &cached.macros)
            });
        type Iterables<'a, A, B, C, D> = (
            Box<dyn Iterator<Item=A> + 'a>,
            Box<dyn Iterator<Item=B> + 'a>,
            Box<dyn Iterator<Item=C> + 'a>,
            Box<dyn Iterator<Item=D> + 'a>
        );
        let (structs, vals, methods, macros): Iterables<_,_,_,_> =
            match (matches_ty_iter, matches_never_iter) {
                (None, None) => (
                    Box::new(iter::empty()),
                    Box::new(iter::empty()),
                    Box::new(iter::empty()),
                    Box::new(iter::empty())
                ),
                (None, Some((a,b,c,d))) => (
                    Box::new(a.iter()),
                    Box::new(b.iter()),
                    Box::new(c.iter()),
                    Box::new(d.iter()),
                ),
                (Some((a,b,c,d)), None) => (
                    Box::new(a.iter()),
                    Box::new(b.iter()),
                    Box::new(c.iter()),
                    Box::new(d.iter()),
                ),
                (Some((a1,b1,c1,d1)), Some((a2,b2,c2,d2))) => (
                    Box::new(a1.iter().chain(a2.iter())),
                    Box::new(b1.iter().chain(b2.iter())),
                    Box::new(c1.iter().chain(c2.iter())),
                    Box::new(d1.iter().chain(d2.iter())),
                ),
            };


        for (name, struc) in structs {
            if struc.ty.matches(&ty) {
                possible_exprs.push(PossibleExpression::Struct(name, struc));
            }
        }
        for (name, var_ty) in vals {
            if var_ty.matches(&ty) {
                possible_exprs.push(PossibleExpression::Var(name));
            } else if let Some(func) = &var_ty.func {
                if func.ret_type.matches_with_generics(&ty, &var_ty.type_generics)  {
                    possible_exprs.push(PossibleExpression::Fn(name, var_ty, func));
                }
            }
        }
        for (assoc_ty, method) in methods {
            let mut ret_gen = Vec::new();
            ret_gen.extend(assoc_ty.type_generics.clone()); 
            ret_gen.extend(method.type_generics.clone());
            if method.func.ret_type.matches_with_generics(&ty, &ret_gen) {
                possible_exprs.push(PossibleExpression::Method(method, assoc_ty));
            }
        }
        if allow_ambiguity {
            for (name, macr) in macros {
                if macr.ty.matches(&ty) {
                    possible_exprs.push(PossibleExpression::Macro(name, macr));
                }
            }
        }
    }

    if possible_exprs.is_empty() {
        return Ok(report_unconstructable_variable(ty.clone()));
    }
    let choice = choose_consume(u, possible_exprs.into_iter())?;
    match choice {
        PossibleExpression::Struct(name, struc) => {
            let determined_fields = make_struct_generics(
                u, 
                allow_ambiguity,
                &struc.ty,
                struc.fields.iter().map(|f| &f.ty).collect(),
            )?;
            let generics =
                struc.ty.type_generics.iter()
                  .zip(ty.type_args.clone()).collect();
            match &struc.fields {
                Fields::Named(fields) => {
                    let mut args = vec![];
                    for (i, (name, arg)) in handle_field_generics(generics, &fields).into_iter().enumerate() {

                        let arg_ambigous = determined_fields.as_ref().map_or(true,
                            |determined| !determined.contains(&i)
                        );
                        args.push((name, construct_value(ctx, u, arg, arg_ambigous)?));
                    }
                    Ok(Expr::Struct(
                        name.clone(),
                        if determined_fields.is_none() {
                            ty.type_args.clone()
                        } else {
                            vec![]
                        },
                        args.into_iter().collect()
                    ))
                }
                Fields::Unnamed(fields) => {
                    let mut args = vec![];
                    let field_types = fields.into_iter().map(|f| f.ty.clone()).collect();
                    for (i, arg) in handle_arg_generics(generics, &field_types).into_iter().enumerate() {

                        // Tuples can't have their generics specified so they
                        // always have the same ambiguity on their arguments
                        // as the tuple as a whole
                        let arg_ambigous = if name.as_str().starts_with("#Tuple") {
                            allow_ambiguity
                        } else {
                            determined_fields.as_ref().map_or(true,
                                |determined| !determined.contains(&i)
                            )
                        };
                        args.push(construct_value(ctx, u, arg, arg_ambigous)?);
                    }
                    Ok(Expr::Fn(
                        name.clone(),
                        if determined_fields.is_none() {
                            ty.type_args.clone()
                        } else {
                            vec![]
                        },
                        args
                    ))
                }
                Fields::None => Ok(Expr::Lit(name.clone(), ty.type_args.clone()))
            }
        }
        PossibleExpression::Var(name) => Ok(Expr::Var(name.to_owned())),
        PossibleExpression::Fn(name, fn_ty, func) => {
            let mut args = vec![];
            let (determined_args, generic_params) = make_fn_generics(
                ctx, u, 
                allow_ambiguity,
                &ty, &func.ret_type,
                &func.args,
                &fn_ty.type_generics,
                &fn_ty.type_args
            )?;
            let generics = fn_ty.type_generics.iter().zip(generic_params.clone()).collect();
            // println!("generics {:?} {:?}", type_generics, generic_params);
            for (i, arg) in handle_arg_generics(generics, &func.args).into_iter().enumerate() {
                let arg_ambigous = determined_args.as_ref().map_or(true,
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
        }
        PossibleExpression::Method(method, assoc_ty) => {
            let (_, disc_ty_args) = make_fn_generics(
                ctx, u, 
                allow_ambiguity,
                &ty, &method.func.ret_type,
                &method.func.args,
                &assoc_ty.type_generics,
                &assoc_ty.type_args
            )?;
            let (determined_args, method_ty_args) = make_fn_generics(
                ctx, u, 
                allow_ambiguity,
                &ty, &method.func.ret_type,
                &method.func.args,
                &method.type_generics,
                &method.type_args,
            )?;

            let generics = 
                assoc_ty.type_generics.iter()
                    .chain(method.type_generics.iter())
                    .zip(disc_ty_args.clone().into_iter()
                        .chain(method_ty_args.clone().into_iter()))
                    .collect();
            let mut args = vec![];
            for (i, arg) in handle_arg_generics(generics, &method.func.args).into_iter().enumerate() {
                let arg_ambigous = determined_args.as_ref().map_or(true,
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
        }

        PossibleExpression::Macro(name, macr) => {
            let body = (macr.constructor)(ty.type_args, ctx, u)?;
            Ok(Expr::Macro(name.clone(), body))
        }
    }
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
        (Rc::new(make_type!($($ty)*)), parse_methods!(;$($methods)*,))
    );
}

macro_rules! parse_methods {
    ($(,$expr:expr)*;$(,)?) => (vec![$(Rc::new($expr)),*]);
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
        fields: Fields::None
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
    ($name:path$([$($gen:tt)*])? ) =>
        (make_struct!(:$name: $name$([$($gen)*])?));
    (:$ty:path: $name:path$([$($gen:tt)*])? ) =>
        ((stringify!($name).into(), crate::semantics::Struct {
            ty: make_type!($ty$({$($gen)*})?),
            fields: Fields::None
        }));
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

#[allow(unused_macros)]
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
        (StringWrapper::from(stringify!($name)), Macro {
            constructor: $fn,
            ty: make_type!($($ty)*)
        })
    }
}

fn report_unconstructable_variable(ty: Type) -> Expr {
    println!("Couldn't find value of type {}", ty);
    Expr::Macro("compile_error".into(), MacroBody {
        brackets: BracketType::Round,
        seperator: Seperator::Comma,
        tokens: vec![Token::Expr(Expr::ExactString(format!("Couldn't find value of type {}", ty)))]
    })
}


pub fn prelude_scope() -> Scope {
    let mut scope = Scope {
        by_ty_name: HashMap::new(),
        owned: false,
        vals: vec![
            make_val!(drop: %Fn{T}(T)),
        ].into_iter().map(|(k,v)| (StringWrapper::clone(&k), Rc::new(v))).collect(), 
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
        ].iter().cloned().map(|(k,v)| (k, Rc::new(v))).collect(), 
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
        ].iter().cloned().map(|(k,v)| (k, Rc::new(v))).collect(), 
    };
    scope.make_ty_index();
    scope
}

pub fn primitive_scope() -> Scope {
    let mut scope = Scope {
        by_ty_name: HashMap::new(),
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

        ].iter().cloned().map(|(k,v)| (k, Rc::new(v))).collect(), 
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
    };
    scope.make_ty_index();
    scope
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
    #[test]
    fn type_diff() {
        assert!(make_type!(A).diff(&make_type!(B)).next().unwrap().1.matches(&make_type!(B)));
        assert!(make_type!(Vec[A]).diff(&make_type!(Vec[B])).next().unwrap().1.matches(&make_type!(B)));
        assert!(make_type!(Vec{A}).diff(&make_type!(Vec{A}[B])).next().unwrap().1.matches(&make_type!(B)));
        let many_diffs_lhs = make_type!(Result[Vec[Result[A,B]],Box[C]]);
        let many_diffs_rhs = make_type!(Result[Vec[Result[D,E]],Box[F]]);
        let many_diffs = many_diffs_lhs.diff(&many_diffs_rhs).collect::<Vec<_>>();
        let expected = [("A".into(), make_type!(D)), ("B".into(), make_type!(E)), ("C", make_type!(F))];
        assert!(many_diffs.len() == 3);
        for (i, diff) in many_diffs.into_iter().enumerate() {
            assert!(diff.0 == &expected[i].0);
            assert!(diff.1.matches(&expected[i].1));
        }
    }
}
