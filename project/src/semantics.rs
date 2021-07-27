use std::collections::HashMap;
use std::cell::{Cell, RefCell, RefMut};
use std::fmt::Display;
use std::hash::Hash;
use std::iter;
use std::iter::FromIterator;
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
            (StringWrapper::Static(s1), StringWrapper::Dynamic(s2)) => s1 == &s2.as_ref().as_str(),
            (StringWrapper::Dynamic(s1), StringWrapper::Static(s2)) => &s1.as_ref().as_str() == s2,
            (StringWrapper::Dynamic(s1), StringWrapper::Dynamic(s2)) => s1 == s2
        }
    }
}
impl Eq for StringWrapper {}
impl ToString for StringWrapper {
    fn to_string(&self) -> String {
        match self {
            StringWrapper::Static(s) => s.to_string(),
            StringWrapper::Dynamic(s) => String::clone(s)
        }
    }
}

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
            StringWrapper::Dynamic(s) => s.as_ref() == other
        }
    }
}


impl PartialEq<&str> for StringWrapper {
    fn eq(&self, other: &&str) -> bool {
        match self {
            StringWrapper::Static(s) => s == other,
            StringWrapper::Dynamic(s) => &s.as_ref().as_str() == other
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
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Lifetime {
    Named(StringWrapper),
    Any,
    Anon(usize),
}
impl Lifetime {
    pub fn name(&self) -> Option<&StringWrapper> {
        match self {
            Lifetime::Named(s) => Some(&s),
            _ => None
        }
    }
    pub fn matches_with_generics(&self, other: &Self, generics: &Vec<StringWrapper>) -> bool {
        if other == &Lifetime::Any ||
            self.name().map_or(false, |name| generics.contains(name)) {
            return true
        } else {
            return self == other
        }
    }
    pub fn is_valid(&self, ctx: &Context) -> bool {
        for scope in ctx.scopes.iter() {
            if let Some((is_valid, _)) = scope.lifetimes.get(self) {
                return is_valid.get();
            }
        }
        return false
    }
}
pub type Generics = Vec<Generic>;
#[derive(PartialEq, Clone, Debug)]
pub struct Generic {
    pub name: StringWrapper,
    pub is_arg_for_other: bool,
    pub constraints: Vec<Constraint>
}
#[derive(PartialEq, Clone, Debug)]
pub struct Constraint {
    pub trait_name: StringWrapper,
    pub trait_args: Vec<Type>
}
#[derive(PartialEq, Clone, Debug)]
pub struct Type {
    pub name: StringWrapper,
    pub lt_generics: Vec<StringWrapper>,
    pub type_generics: Generics,
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
                        .map(|a| format!("{}", a.name.as_str()))
                        .chain(
                            self.lt_generics.iter().map(StringWrapper::to_string)
                        )
                        .reduce(|acc, gen| format!("{}, {}", acc, gen)).unwrap()
                )
            },
            if self.type_args.is_empty() {
                "".to_string()
            } else {
                format!("[{}]",
                    self.type_args.iter()
                        .map(|a| format!("{}, ", a))
                        .chain(
                            self.lt_args.iter().map(|lt| match lt {
                                Lifetime::Named(n) => n.as_str().to_string(),
                                Lifetime::Anon(a) => format!("#anon({})", a),
                                Lifetime::Any => "#any".to_string()
                            })
                        )
                        .reduce(|acc, gen| format!("{}, {}", acc, gen)).unwrap()
                )
            }
        )
    }
}
#[derive(PartialEq, Clone, Debug)]
pub struct Func {
    pub args: Vec<Type>,
    pub ret_type: Type
}
#[derive(PartialEq, Clone, Debug)]
pub struct Kind { pub is_visible: bool, pub lifetimes: u8, pub types: u8 }
#[derive(PartialEq, Clone, Debug)]
pub struct Field { pub visible: bool, pub ty: Rc<Type> }
#[derive(PartialEq, Clone, Debug)]
pub enum Fields { None, Unnamed(Vec<Field>), Named(HashMap<StringWrapper, Field>) }
pub enum FieldsIter<'a> {
    Unnamed(std::slice::Iter<'a, Field>, usize),
    Named(std::collections::hash_map::Iter<'a, StringWrapper, Field>),
    None
}
impl<'a> Iterator for FieldsIter<'a> {
    type Item = (StringWrapper, &'a Field);
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            FieldsIter::Unnamed(vec_iter, i) => { 
                vec_iter.next().map(|v| {
                    let k = format!("{}", i).into();
                    *i += 1;
                    (k, v)
                })
            }
            FieldsIter::Named(values_iter) =>
                values_iter.next().map(|(k,v)| (k.clone(), v)),
            FieldsIter::None => None
        }
    }
}
impl Fields {
    fn iter(&self) -> FieldsIter {
        match self {
            Fields::Unnamed(vec) => FieldsIter::Unnamed(vec.iter(), 0),
            Fields::Named(hm) => FieldsIter::Named(hm.iter()),
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
    pub is_enum_variant: bool,
    pub ty: Rc<Type>,
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
type SelfParam = Option<VarHandling>;
#[derive(PartialEq, Clone, Debug)]
pub struct Method {
    pub name: StringWrapper,
    pub self_param: SelfParam,
    pub lt_generics: Vec<StringWrapper>,
    pub lt_args: Vec<Lifetime>,
    pub type_generics: Generics,
    pub type_args: Vec<Type>,
    pub func: Func
}
#[derive(Clone, Debug)]
pub struct TraitDescription {
    name: StringWrapper,
    trait_generics: Generics
}
#[derive(Clone, Debug)]
pub struct Trait {
    name: StringWrapper,
    type_generics: Generics,
    implementors: Vec<(TraitArgs, Rc<Type>)>
}
#[derive(Clone, Debug)]
pub struct TraitArgs {
    type_generics: Generics,
    type_args: Vec<Type>
}
impl TraitArgs {
    fn matches(&self, args: &Vec<Type>) -> bool {
        self.matches_with_generics(args, &vec![])
    }
    fn matches_with_generics(&self, args: &Vec<Type>, generics: &Generics) -> bool {
        self.type_args.iter().zip(args)
            .all(|(a1, a2)| a1.matches_with_generics(a2, generics))
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum Either<A,B> { Left(A), Right(B) }
use Either::{Left, Right};
#[derive(Clone, Debug, PartialEq)]
pub enum Refs {
    None,
    MutRef(Lifetime),
    Ref(Vec<Lifetime>)
}
#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
    pub ty: Type,
    pub mutability: Mutability,
    pub refs: RefCell<Refs>,
    pub lifetime: Lifetime,
}
impl Variable {
    fn mve(&self, ctx: &Context) {
        use Refs::*;
        end_lifetime(ctx, &self.lifetime);
        match &*self.refs.borrow() {
            None => {},
            Ref(lts) => lts.iter().for_each(|lt| end_lifetime(ctx, lt)),
            MutRef(mut_lt) => end_lifetime(ctx, mut_lt)
        }
    }
    fn borrow(&self, ctx: &Context, lt: Lifetime) {
        self.refs.replace_with(|r| {
            use Refs::*;
            match r {
                None => Ref(vec![lt]),
                // TODO:Is there a way to do this without cloning?
                Ref(lts) => { lts.push(lt); Ref(lts.clone()) },
                MutRef(mut_lt) => {
                    end_lifetime(ctx, mut_lt);
                    Ref(vec![lt])
                }
            }
        });
    }
    fn mut_borrow(&self, ctx: &Context, lt: Lifetime) {
        self.refs.replace_with(|r| {
            use Refs::*;
            match r {
                None => {}
                Ref(lts) => lts.iter().for_each(|lt| end_lifetime(ctx, lt)),
                MutRef(mut_lt) => end_lifetime(ctx, mut_lt)
            }
            MutRef(lt)
        });
    }
    pub fn new(ty: Type, mutability: Mutability, lifetime: Lifetime) -> Self {
        Variable {
            ty, mutability, lifetime,
            refs: RefCell::new(Refs::None),
        }
    }
}
pub type VarScope = HashMap<StringWrapper, Rc<Variable>>;
pub type TypeScope = HashMap<StringWrapper, Kind>;
pub type LtConstraints = (Cell<bool>, Vec<Lifetime>);
pub type LtScope = HashMap<Lifetime, LtConstraints>;
pub type TraitScope = HashMap<StringWrapper, Trait>;
pub type StructScope = HashMap<StringWrapper, Rc<Struct>>;
pub type MacroScope = HashMap<StringWrapper, Rc<Macro>>;
pub type MethodScope = Vec<(Either<Rc<Type>, TraitDescription>, Vec<Rc<Method>>)>;
#[derive(Clone, Debug)]
pub struct Scope {
    pub owned: bool,
    pub vars: VarScope,
    pub types: TypeScope,
    pub lifetimes: LtScope,
    pub traits: TraitScope,
    pub structs: StructScope,
    pub macros: MacroScope,
    pub methods: MethodScope,
    pub by_ty_name: HashMap<StringWrapper, TypeIndexed>
}
#[derive(Clone, Debug, PartialEq)]
pub struct Operator {
    pub type_generics: Generics,
    pub operands: (Type, Option<Type>),
    pub ret_type: Type,
}
impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        self.owned == other.owned &&
        self.vars == other.vars &&
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
            for (name, field) in struc.fields.iter() {
                if !struc.is_enum_variant && !struc.ty.name.as_str().starts_with("#Tuple") {
                    if field.ty.is_generic_with(&struc.ty.type_generics).is_some() {
                        self.by_ty_name.entry("#Generic".into())
                            .or_insert(Default::default())
                            .fields.push((
                                struc.ty.clone(),
                                field.ty.clone(),
                                name.clone()
                            ))
                    } else {
                        self.by_ty_name.entry(field.ty.name.clone())
                            .or_insert(Default::default())
                            .fields.push((struc.ty.clone(), field.ty.clone(), name.clone()))
                    }
                }
            }
        }
        for (name, var) in &self.vars {
            if let Some(func) = &var.ty.func {
                if let Some(_) = func.ret_type.is_generic_with(&var.ty.type_generics) {
                    self.by_ty_name.entry("#Generic".into())
                        .or_insert(Default::default())
                        .vars.insert(name.clone(), var.clone());
                } else {
                    self.by_ty_name.entry(func.ret_type.name.clone())
                        .or_insert(Default::default())
                        .vars.insert(name.clone(), var.clone());
                }
            } else {
                self.by_ty_name.entry(var.ty.name.clone())
                    .or_insert(Default::default())
                    .vars.insert(name.clone(), var.clone());
            }
        }
        for (name, macr) in &self.macros {
            // TODO: if a generic (not never type) macro is added,
            // put it into #Generic
            self.by_ty_name.entry(macr.ty.name.clone())
                .or_insert(Default::default())
                .macros.insert(name.clone(), macr.clone());
        }
        for (assoc_ty, methods) in &self.methods {
            for method in methods {
                if let Some(_) = method.func.ret_type.is_generic_with(&method.type_generics) {
                    self.by_ty_name.entry("#Generic".into())
                        .or_insert(Default::default())
                        .methods.push((assoc_ty.clone(), method.clone()));
                } else {
                    self.by_ty_name.entry(method.func.ret_type.name.clone())
                        .or_insert(Default::default())
                        .methods.push((assoc_ty.clone(), method.clone()));
                }
            }
        }
        for (name, trai) in self.traits.iter() {
            for (args, implementor) in trai.implementors.iter() {
                self.by_ty_name.entry((*implementor).name.clone())
                    .or_insert(Default::default())
                    .trait_impls.entry(name.clone())
                    .or_insert(vec![])
                    .push((args.clone(), implementor.clone()))
            }
        }
        return self;
    }
}
#[derive(Clone, Debug, Default)]
pub struct TypeIndexed {
    pub vars: HashMap<StringWrapper, Rc<Variable>>,
    pub structs: HashMap<StringWrapper, Rc<Struct>>,
    pub macros: HashMap<StringWrapper, Rc<Macro>>,
    pub fields: Vec<(Rc<Type>, Rc<Type>, StringWrapper)>,
    pub methods: Vec<(Either<Rc<Type>, TraitDescription>, Rc<Method>)>,
    pub trait_impls: HashMap<StringWrapper, Vec<(TraitArgs, Rc<Type>)>>
}
#[derive(PartialEq, Clone, Debug)]
pub enum RcList<T> {
    Cons(RefCell<T>, Rc<RcList<T>>),
    Nill
}
pub type Stack<T> = Rc<RcList<T>>;
impl<T> RcList<T> {
    fn iter<'a>(&'a self) -> Box<dyn Iterator<Item=&T> + 'a> {
        match self {
            RcList::Nill => Box::new(iter::empty()),
            RcList::Cons(t, rest) => Box::new(
                iter::once(unsafe {
                    t.try_borrow_unguarded().unwrap()
                }).chain(rest.iter())
            )
        }
    }
    fn sized_iter<'a>(&'a self) -> std::vec::IntoIter<&T> {
        self.iter().collect::<Vec<_>>().into_iter()
    }
    pub fn top_mut(&self) -> Option<RefMut<T>> {
        match self {
            RcList::Nill => None,
            RcList::Cons(t, _rest) => Some(t.borrow_mut())
        }
    }
}
impl<T> FromIterator<T> for RcList<T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(into_iter: Iter) -> Self {
        let mut iter = into_iter.into_iter();
        match iter.next() {
            None => RcList::Nill,
            Some(t) => RcList::Cons(RefCell::new(t), Rc::new(iter.collect()))
        }   
    }
}
#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Lit(StringWrapper, Vec<Type>),
    Struct(StringWrapper, Vec<Type>, HashMap<StringWrapper, Expr>),
    Fn(StringWrapper, Vec<Type>, Vec<Expr>),
    AssocFn(StringWrapper, Vec<Type>, StringWrapper, Vec<Type>, Vec<Expr>),
    Method(Box<Expr>, StringWrapper, Vec<Type>, Vec<Expr>),
    Var(StringWrapper),
    UnOp(&'static str, Box<Expr>),
    BinOp(&'static str, Box<Expr>, Box<Expr>),
    Field(Box<Expr>, StringWrapper),
    Macro(StringWrapper, MacroBody),
    ExactString(String)
}


impl Type {
    pub fn matches(&self, other: &Type) -> bool {
        self.matches_with_generics(other, &self.type_generics)
    }
    pub fn matches_with_generics(&self, other: &Type, generics: &Generics) -> bool {
        //! NOT reflexive: Option{N}[N].matches(Option[i32]) but not
        //! Option[i32].matches(Option{N}[N])

        // The never type can be coerced into any type, but no type can be 
        // coerced into the never type

        // if self.name == "#Ref" && other.name == "#Ref" {
        //     println!("Comparing refs: {} ({:?})\n    {} ({:?})",
        //         self, self.lt_args, other, other.lt_args)
        // }

        // \forall x: !.matches(x)
        // \forall x: x != ! ==> not x.matches(!)
        if self.name == "!" {
            return true;
        } 
        if self.name != other.name &&
            !(self.name == "#RefMut" && other.name == "#Ref") {
            return false;
        }

        if other.lt_args.len() > self.lt_args.len() {
            return false;
        }

        let lt_zipped = self.lt_args.iter().zip(other.lt_args.iter());
        for (self_lt, other_lt) in lt_zipped {
            if !self_lt.matches_with_generics(other_lt, &self.lt_generics) {
               return false;
            }
        }

        // TODO: handle constraints correctly
        let type_zipped = self.type_args.iter().zip(other.type_args.iter());
        for (self_arg, other_arg) in type_zipped {
            if !self_arg.is_generic_with(generics).is_some() &&
               !self_arg.matches_with_generics(other_arg, generics) {
                // println!("Type arguments where different: {:?}, {:?}", self_arg, other_arg);
                return false;
            }
        }
        return true;
    }
    pub fn merge(self, other: Type) -> Option<Type> {
        if self.name != other.name || self.name == "!" {
            return None;
        }
        let mut type_args = vec![];
        let self_len = self.type_args.len();
        let other_len = other.type_args.len();
        let max_len = std::cmp::max(self_len, other_len);

        for i in 0..max_len {
            let self_arg = self.type_args.get(i);
            let other_arg = other.type_args.get(i);
            match (self_arg, other_arg) {
                (Some(self_a), Some(other_a)) => 
                    if let Some(a) = self_a.clone().merge(other_a.clone()) {
                        type_args.push(a);
                    } else {
                        return None;
                    },
                (Some(self_a), None) => type_args.push(self_a.clone()),
                (None, Some(other_a)) => type_args.push(other_a.clone()),
                (None, None) => unreachable!()
            }
        }
        
        let type_generics =
            self.type_generics.into_iter()
                .zip(other.type_generics)
                .skip(max_len)
                .map(|(mut gen, other)| {
                    gen.constraints.extend(other.constraints);
                    Generic {
                        name: gen.name,
                        constraints: gen.constraints,
                        is_arg_for_other: gen.is_arg_for_other && other.is_arg_for_other
                    }
                })
                .collect::<Generics>();

        return Some(Type {
            type_generics,
            type_args,
            ..self
        })

    }
    pub fn is_generic_with<'a>(&self, generics: &'a Generics) -> Option<&'a Vec<Constraint>> {
        generics.iter().find_map(|gen| {
            if gen.name == self.name {
                Some(&gen.constraints)
            } else {
                None
            }
        })
    }
    pub fn type_generic_names<'a>(&'a self) -> Box<dyn Iterator<Item = StringWrapper> + 'a> {
        // Box is unneccesary, just there to avoid typing out the horrible return type
        Box::new(self.type_generics.iter().map(|t| t.name.clone()))
    }
    pub fn lt_generic_names<'a>(&'a self) -> Box<dyn Iterator<Item = StringWrapper> + 'a> {
        // Box is unneccesary, just there to avoid typing out the horrible return type
        Box::new(self.lt_generics.iter().map(|t| t.clone()))
    }
    pub fn contains(&self, name: &StringWrapper) -> bool {
        &self.name == name
        || self.type_args.iter().any(|arg| arg.contains(name))
    }
    pub fn diff<'a>(&'a self, other: &'a Type) -> Box<dyn Iterator<Item = (StringWrapper, &Type)> + 'a> {
        Box::new(self.diff_sym(other).map(|(a,b)| (a.name.clone(), b)))
    }
    pub fn diff_sym<'a>(&'a self, other: &'a Type) -> Box<dyn Iterator<Item = (&Type, &Type)> + 'a> {
        if self.name != other.name {
            return Box::new(iter::once((self, other)));
        } else {
            Box::new(
                self.type_args.iter()
                    .zip(other.type_args.iter())
                    .flat_map(|(a,b)| a.diff_sym(b))
            )
        }
    }
    pub fn replace(&self, name: &StringWrapper, other: &Type) -> Type {
        if &self.name == name {
            return other.clone()
        }
        Type {
            type_generics:
                self.type_generics.iter().cloned()
                .filter(|gen| &gen.name == name).collect(),
            type_args: self.type_args.iter().map(|arg| arg.replace(name, other)).collect(),
            ..self.clone()
        }
    }
    pub fn replace_lts(&self, lts: &Vec<(StringWrapper, &Lifetime)>) -> Type {
        Type {
            lt_args: self.lt_args.iter().map(|other_lt| {
                for (name, lt) in lts.iter() {
                    if other_lt == &Lifetime::Named(name.clone()) {
                        return Lifetime::clone(lt)
                    }
                }
                return other_lt.clone()
            }).collect(),
            type_args: self.type_args.iter().map(|t| t.replace_lts(lts)).collect(),
            ..self.clone()
        }
    }
    pub fn is_sized_by(&self, name: &StringWrapper) -> bool {
        !INDIRECT_TYPES.contains(&name.as_str()) &&
        (&self.name == name
         || self.type_args.iter().any(|arg| arg.is_sized_by(name)))
    }
    pub fn fits_constraints(&self, ctx: &Context, constraints: &Vec<Constraint>) -> bool {
        // TODO: should take trait args as well
        'top: for constraint in constraints {
            for scope in ctx.scopes.iter() {
                if let Some(type_indexed) = scope.by_ty_name.get(&self.name) {
                    if let Some(traits) = type_indexed.trait_impls.get(&constraint.trait_name) {
                        if traits.iter().any(|(args, implementor)| self.matches(implementor) && args.matches(&constraint.trait_args)) {
                            continue 'top;
                        } else {
                            return false;
                        }
                    }
                }
            }
            return false;
        }
        return true;
    }
}

impl Method {
    pub fn type_generic_names<'a>(&'a self) -> Box<dyn Iterator<Item = StringWrapper> + 'a> {
        // Box is unneccesary, just there to avoid typing out the horrible return type
        Box::new(self.type_generics.iter().map(|t| t.name.clone()))
    }
    pub fn lt_generic_names<'a>(&'a self) -> Box<dyn Iterator<Item = StringWrapper> + 'a> {
        // Box is unneccesary, just there to avoid typing out the horrible return type
        Box::new(self.lt_generics.iter().map(|t| t.clone()))
    }
}

const INDIRECT_TYPES: [&str; 4] = ["Rc", "Vec", "Cow", "Arc"];

pub fn push_scope(ctx: &mut Context) {
    ctx.scopes = Rc::new(RcList::Cons(RefCell::new(Scope {
        owned: true,
        macros: HashMap::new(),
        methods: vec![],
        vars: HashMap::new(),
        types: HashMap::new(),
        lifetimes: HashMap::new(),
        traits: HashMap::new(),
        structs: HashMap::new(),
        by_ty_name: HashMap::new()
    }), ctx.scopes.clone()));
}

#[allow(dead_code)]
pub fn pop_scope(ctx: &mut Context) {
    if let RcList::Cons(_, rest) = &*ctx.scopes {
        ctx.scopes = rest.clone()
    }
}

pub fn add_lifetime(ctx: &mut Context, lt: Lifetime) {
    ctx.scopes.top_mut().map(|mut scope| {
        scope.lifetimes.insert(lt, (Cell::new(true), vec![]));
    });
}

pub fn end_lifetime(ctx: &Context, lt: &Lifetime) {
    for scope in ctx.scopes.iter() {
        if let Some((is_valid, _)) = scope.lifetimes.get(lt) {
            is_valid.set(false);
            return;
        }
    }
    panic!("Attemted to remove lifetime {:?}, but it couldn't be found", lt);
}

// macro_rules! with_scope {
//     ($ctx: ident, $expr:expr) => ({
//         push_scope($ctx);
//         let result = $expr;
//         pop_scope($ctx);
//         result
//     })
// }

#[allow(dead_code)]
pub fn add_var<'a>(ctx: &mut Context, name: StringWrapper, var: Variable) {
    add_lifetime(ctx, var.lifetime.clone());
    ctx.scopes.top_mut().map(|mut l| {
        let rc_var = Rc::new(var);
        l.by_ty_name.entry(rc_var.ty.name.clone())
            .or_insert(Default::default())
            .vars.insert(name.clone(), rc_var.clone());
        l.vars.insert(name, rc_var);
    });
}

#[allow(dead_code)]
pub fn add_type<'a>(ctx: &mut Context, name: StringWrapper, ty: Kind) {
    ctx.scopes.top_mut().map(|mut l| l.types.insert(name, ty));
}

pub fn remove_type(ctx: &mut Context, name: StringWrapper) {
    ctx.scopes.top_mut().map(|mut l| l.types.remove(&name));
}

#[allow(dead_code)]
pub fn add_struct(ctx: &mut Context, name: StringWrapper, struc: Struct) {
    ctx.scopes.top_mut().map(|mut l| {
        if !struc.is_enum_variant {
            for (name, field) in struc.fields.iter() {
                // println!("Added field {}: {}", name.as_str(), field.ty);
                l.by_ty_name.entry(field.ty.as_ref().name.clone())
                    .or_insert(Default::default())
                    .fields.push((struc.ty.clone(), field.ty.clone(), name.clone()))
            }
        }
        let rc_struc = Rc::new(struc);
        l.by_ty_name.entry(rc_struc.ty.name.clone())
            .or_insert(Default::default())
            .structs.insert(name.clone(), rc_struc.clone());
        l.structs.insert(name, rc_struc);
    });
}

pub fn make_ref(lt_arg: StringWrapper, ty: Type) -> Type {
    Type {
        name: "#Ref".into(),
        type_args: vec![ty],
        lt_args: vec![Lifetime::Named(lt_arg)],
        type_generics: vec![],
        lt_generics: vec![],
        func: None
    }
}

#[allow(dead_code)]
pub fn lookup_var<'a,'b>(ctx: &'b mut Context, name: &StringWrapper) -> Option<&'b Variable> {
    for scope in ctx.scopes.iter() {
        if let Some(var) = scope.vars.get(name) {
            return Some(var);
        }
    }
    return None;
}

#[allow(dead_code)]
pub fn lookup_type<'a,'b, S: Into<StringWrapper>>(ctx: &'b mut Context, name: S) -> Option<&'b Kind> {
    let wrapped_name = name.into();
    for scope in ctx.scopes.iter() {
        if let Some(kind) = scope.types.get(&wrapped_name) {
            return Some(kind);
        }
    }
    return None;
}

/// Picks a sc at random, then runs the predicate on *all* variables in the scope,
/// then picks a variable from that scope at random. Most likely slower than it has to be.
#[allow(dead_code)]
pub fn pick_var_that<'a, 'b, F>(
    ctx: &'b mut Context,
    u: &'b mut Unstructured<'a>,
    pred: F
) -> context_arbitrary::Result<(&'b StringWrapper, &'b Variable)>
where F: Fn(&StringWrapper, &Variable) -> bool {
    let scope = choose_consume(u, ctx.scopes.sized_iter())?;
    let vars: Vec<(&StringWrapper, &Rc<Variable>)> = scope.vars.iter().filter_map(|(n,var)| {
        if pred(n,var) {
            Some((n,var))
        } else {
            None
        }
    }).collect();
    if vars.is_empty() {
        Err(GenerationError::AppropriateTypeFailure)
    } else {
        let &(name, var) = u.choose(&vars)?;
        Ok((name, var))
    }
}

#[allow(dead_code)]
pub fn pick_var<'a, 'b>(
    ctx: &'b mut Context,
    u: &'b mut Unstructured<'a>,
) -> context_arbitrary::Result<(&'b StringWrapper, &'b Variable)> {
    pick_var_that(ctx, u, |_,_| true)
}


#[allow(dead_code)]
pub fn pick_type<'a>(ctx: &Context, u: &mut Unstructured<'a>) -> arbitrary::Result<Type> {
    Ok(pick_type_with_vis(ctx, u)?.0)
}

pub fn pick_type_with_vis<'a>(ctx: &Context, u: &mut Unstructured<'a>) -> arbitrary::Result<(Type, bool)> {
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
    Ok((Type { 
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
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        func: None
    }, kind.is_visible))
}

pub fn pick_type_impl<'a>(ctx: &'a Context, u: &mut Unstructured, desc: &TraitDescription) -> arbitrary::Result<(&'a TraitArgs, &'a Type)> {
    for scope in ctx.scopes.iter() {
        for (name, trai) in scope.traits.iter() {
            if name == &desc.name {
                let (args, ty) = u.choose(&trai.implementors)?;
                return Ok((args, ty.as_ref()));
            }
        }
    }
    panic!("Couldn't find trait {}", desc.name.as_str());
}

pub fn pick_type_impls(ctx: &Context, u: &mut Unstructured, constraints: &Vec<Constraint>) -> arbitrary::Result<Type> {
    Ok(pick_type_impls_interconnected(ctx, u, constraints, &vec![])?.0)
}

pub fn pick_type_impls_interconnected<'a>(
        ctx: &'a Context, u: &mut Unstructured,
        constraints: &'a Vec<Constraint>,
        determined_by_others: &Generics
    ) -> arbitrary::Result<(Type, Vec<(StringWrapper, &'a Type)>)> {
    
    if constraints.is_empty() {
        return Ok((pick_type(ctx, u)?, vec![]));
    }
    let mut implementors_map = HashMap::new();
    'trai: for constraint in constraints {
        for scope in ctx.scopes.iter() {
            for (name, trai) in scope.traits.iter() {
                if name == &constraint.trait_name {
                    // NOTE: can't handle multiple trait implementations
                    // for one type name
                    'args: for (args, ty) in trai.implementors.iter() {
                        // We have generics on both sides: the trait arg generics in the lhs 
                        // and the determined by other in the rhs
                        let diffs: Box<dyn Iterator<Item=_>> = Box::new(
                            args.type_args.iter().zip(&constraint.trait_args)
                                .flat_map(|(arg, t_arg)| arg.diff_sym(t_arg))
                        );
                        // Because of above, we check that they match in this for loop
                        // insted of with the matches function. We also find others to
                        // put in the others vec
                        let mut others = vec![];
                        for (type_arg, cons_arg) in diffs {
                            if determined_by_others.iter().any(|gen| gen.name == cons_arg.name) {
                                // a diff that is generic in the rhs
                                others.push((cons_arg.name.clone(), type_arg));
                            } else if !args.type_generics.iter().any(|gen| gen.name == type_arg.name) {
                                // a diff that isn't generic in either => doesn't match
                                continue 'args;
                            } 
                            // else: a diff in the lhs
                        }
                        implementors_map.entry(ty.name.clone())
                            .or_insert(vec![]).push((ty, others))
                    }
                    continue 'trai;
                }
            }
        }
        panic!("Couldn't find trait {}", constraint.trait_name.as_str());
    }
    // println!("Implementors map: {:?}", implementors_map.keys().collect::<Vec<_>>());
    type InterIter<'a> = Box<dyn Iterator<Item=(StringWrapper, &'a Type)> + 'a>;
    let (ty, inter) = choose_consume(u,
        implementors_map.into_iter().filter_map(|(_,values)| {
            // these 3 lines could be replaced by a hypothetical
            // values.try_reduce(...)
            let mut values_iter = values.into_iter();
            values_iter.next().and_then(|(first, f_args)| {
                let f_iter: InterIter = Box::new(f_args.into_iter());
                values_iter.try_fold((first.as_ref().clone(), f_iter),
                    |(acc_ty, acc_iter), (v_ty, v_iter)|
                        -> Option<(Type, InterIter)> {
                        println!("Merging");
                        Some((acc_ty.merge(v_ty.as_ref().clone())?,
                             Box::new(acc_iter.into_iter().chain(v_iter))))
                    }
                )
            })
        }).collect::<Vec<_>>().into_iter()
    )?;
    // TODO: handle the implementor having an arg determined by others
    let ty_args = ty.type_generics.clone().into_iter()
        .filter(|gen| ty.contains(&gen.name))
        .map(|gen| Ok((gen.name.clone(), pick_type_impls(ctx, u, &gen.constraints)?)))
        .collect::<arbitrary::Result<Vec<_>>>()?;
    // TODO: replace generics in inter too
    let ty_with_args = ty_args.into_iter().fold(ty, |acc, ty_arg| {
        let (gen_name, arg) = ty_arg;
        let ty = acc;
        ty.replace(&gen_name, &arg)
    });
    return Ok((ty_with_args, inter.collect()))
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VarHandling {
    Borrow,
    MutBorrow,
    Own
}
use VarHandling::*;

pub fn construct_value<'a>(
    ctx: &Context,
    u: &mut Unstructured<'a>,
    ty: Type,
    allow_ambiguity: bool
) -> context_arbitrary::Result<Expr> {
    ctx.size.set(0);
    construct_value_inner(ctx, u, ty, allow_ambiguity, Own)
}

const MAX_SIZE: usize = 30;

fn construct_value_inner<'a>(
    ctx: &Context,
    u: &mut Unstructured<'a>,
    ty: Type,
    allow_ambiguity: bool,
    var_handling: VarHandling
) -> context_arbitrary::Result<Expr> {

    // println!("construct_value called on type {}", ty);

    // NOTE: there is a bias towards expressions further to the left being bigger.
    // Recursive calls should be made in a random order.
    let old_size = ctx.size.get();
    // println!("{}", old_size);
    ctx.size.set(old_size + 1);

    fn handle_field_generics(generics: Vec<(StringWrapper, &Type)>, fields: &HashMap<StringWrapper, Field>) -> HashMap<StringWrapper, Type> {
        // println!("handle_field_generics called on {:?}, {:?}", generics, fields);
        let mut new_map = HashMap::new();
        'outer: for (name, field) in fields.iter() {
            for (ty_name, ty) in generics.iter() {
                if ty_name == &field.ty.name {
                    new_map.insert(name.to_owned(), (*ty).clone() );
                    continue 'outer;
                }
            }
            new_map.insert(name.clone(), field.ty.as_ref().clone());
        }
        return new_map;           
    }

    fn handle_arg_generics(
        ty_generics: Vec<(StringWrapper, &Type)>,
        lt_generics: Vec<(StringWrapper, &Lifetime)>,
        args: &Vec<Type>
    ) -> Vec<Type> {
        // println!("handle_arg_generics called on {:?}, {:?}\n", generics, args);
        let mut new_args = vec![];
        'vec_args: for arg in args {
            for (ty_name, ty) in ty_generics.iter() {
                if arg.contains(&ty_name) {
                    // TODO: replace both in one pass?
                    new_args.push(arg.replace(&ty_name, &ty).replace_lts(&lt_generics));
                    continue 'vec_args;
                }
            }
            new_args.push(arg.replace_lts(&lt_generics));
        }
        return new_args;
    }

    fn handle_single_generic(generics: Vec<(StringWrapper, &Type)>, single: &Type) -> Type {
        for (ty_name, ty) in generics.iter() {
            if single.contains(&ty_name) {
                return single.replace(&ty_name, &ty);
            }
        }
        return single.clone();
        
    }

    fn make_lt_generics(
        output_params: &Vec<StringWrapper>,
        expected_params: &Vec<Lifetime>,
        lt_generics: &Vec<StringWrapper>
    ) -> Vec<Lifetime> {
        let mut result = vec![];
        for (i, gen) in lt_generics.iter().enumerate() {
            if output_params.contains(&gen.clone()) {
                result.push(expected_params[i].clone());
            } else {
                result.push(Lifetime::Any);
            }
        }
        return result;
    }

    fn make_fn_generics(
        ctx: &Context, u: &mut Unstructured,
        allow_ambiguity: bool,
        ty1: &Type, ty2: &Type, 
        args: &Vec<Type>,
        type_generics: &Generics, type_args: &Vec<Type>,
    ) -> arbitrary::Result<(Option<Vec<usize>>, Vec<Type>)> {
    //! The output type is perhaps a bit counter-intuitive,
    //! (None, tys) means that the function *should* be given
    //! type parameters and that all arguments can be ambigous
    //! (Some(determined_args]), tys) mean that the function
    //! *should not* be given type parameters and if the index
    //! of an argument is in determined_args it can't be ambigous.
    //!
    //! (None, vec![Vec<u32>]) ==> drop::<Vec<u32>>(Vec::new())
    //! (Some(vec![0]), vec![Vec<u32>]) ==> drop(Vec::<u32>::new())
        let mut result = vec![];
        let mut args_determined_by_generics = vec![];
        let mut generics_required = false;

        let mut output_diff =
            ty2.diff(ty1)
                .collect::<HashMap<StringWrapper, &Type>>();

        // println!("make_generics called with: {:?} {:?} {:?} {:?} {:?}\n",
        //    ty1, ty2, args, type_generics, type_args);
        // println!("diff is {:?}\n", output_diff);

        // TODO: handle constraints
        for (i, gen) in type_generics.iter().enumerate() {
            if ty2.contains(&gen.name) {
                result.push(
                    type_args.get(i)
                        .or_else(|| output_diff.remove(&gen.name))
                        .map_or_else(
                            || name_to_type(gen.name.clone()),
                            |ty| {
                                generics_required |= !allow_ambiguity;
                                ty.clone()
                            }
                                    
                        )
                );
            } else if args.iter().any(|arg| arg.contains(&gen.name)) {
                // result.push(if let Some(arg) = type_args.get(i) {
                //     arg.clone()
                // } else {
                    result.push(pick_type_impls(ctx, u, &gen.constraints)?);
                // });
                args_determined_by_generics.push(i);
            } else {
                generics_required = true;
                // result.push(if let Some(arg) = type_args.get(i) {
                //     arg.clone()
                // } else {
                    result.push(pick_type_impls(ctx, u, &gen.constraints)?);
                // });
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

        // TODO: handle constraints
        'outer: for gen in ty.type_generics.iter() {
            for (i, field) in fields.iter().enumerate() {
                if field.contains(&gen.name) {
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

    fn generate_free_generics<'a>(
        ctx: &'a Context, u: &mut Unstructured,
        full_generics: &'a Generics,
        so_far: &Vec<(StringWrapper, &Type)>
    ) -> arbitrary::Result<Vec<(StringWrapper, Either<Type, &'a Type>)>> {
        let mut result = vec![];
        let so_far_names: Vec<&StringWrapper> = so_far.iter().map(|(name,_)| name).collect();
        let free_generics: Vec<&Generic> = full_generics.iter()
            .filter(|gen| !so_far_names.contains(&&gen.name))
            .collect();
        let (determined_by_others_ref, normal): (Vec<&Generic>, Vec<&Generic>) =
            free_generics.iter().cloned().partition(|gen| gen.is_arg_for_other);
        let determined_by_others =
            determined_by_others_ref.into_iter().cloned().collect();
        for gen in normal {
            let (gen_ty, interconnected) =
                pick_type_impls_interconnected(
                    ctx, u,
                    &gen.constraints,
                    &determined_by_others
                )?;
            result.push((gen.name.clone(), Left(gen_ty)));
            result.extend(
                interconnected.into_iter()
                    .map(|(name,ty)| (name, Right(ty)))
            );
        }
        return Ok(result);
    }

    enum PossibleExpression<'a> {
        Struct(&'a StringWrapper, &'a Struct),
        Var(&'a StringWrapper, &'a Variable),
        Fn(&'a StringWrapper, &'a Type, &'a Func),
        Method(&'a Method, &'a Either<Rc<Type>, TraitDescription>),
        Field(&'a Type, &'a Type, &'a StringWrapper),
        Macro(&'a StringWrapper, &'a Macro),
        Op(&'static str, &'a Operator)
    }
    let mut possible_exprs: Vec<PossibleExpression> = vec![];

    for scope in ctx.scopes.iter() {

        let by_ty_name = &scope.by_ty_name;

        let matches_ty_iter = 
            by_ty_name.get(&ty.name).map(|cached| {
                (&cached.structs, &cached.vars, &cached.methods, &cached.fields, &cached.macros)
            });
        let matches_never_iter = 
            by_ty_name.get(&"!".into()).map(|cached| {
                (&cached.structs, &cached.vars, &cached.methods, &cached.fields, &cached.macros)
            });

        let matches_generic_iter = 
            by_ty_name.get(&"#Generic".into()).map(|cached| {
                (&cached.structs, &cached.vars, &cached.methods, &cached.fields, &cached.macros)
            });

        type Iterables<'a, A, B, C, D, E> = (
            Box<dyn Iterator<Item=A> + 'a>,
            Box<dyn Iterator<Item=B> + 'a>,
            Box<dyn Iterator<Item=C> + 'a>,
            Box<dyn Iterator<Item=D> + 'a>,
            Box<dyn Iterator<Item=E> + 'a>
        );
        
        let (structs, vars, methods, fields, macros): Iterables<_,_,_,_,_> = 
            matches_ty_iter.into_iter()
                .chain(matches_never_iter)
                .chain(matches_generic_iter)
                .fold(
                    (Box::new(iter::empty()), Box::new(iter::empty()),
                     Box::new(iter::empty()), Box::new(iter::empty()),
                     Box::new(iter::empty())),
                    |(a1,b1,c1,d1,e1), (a2,b2,c2,d2,e2)| {
                        (Box::new(a1.chain(a2)), Box::new(b1.chain(b2)),
                         Box::new(c1.chain(c2)), Box::new(d1.chain(d2)),
                         Box::new(e1.chain(e2)))
                    }
                );

        for (name, struc) in structs {
            // NOTE: structs are always allowed, because otherwise some 
            // things might not be constructable
            if struc.ty.matches(&ty) {
                possible_exprs.push(PossibleExpression::Struct(name, struc));
            }
        }
        for (name, var) in vars {
            let doesnt_move_reserved_vars =
                var_handling != VarHandling::Own ||
                ctx.is_final_stmnt || 
                !ctx.final_stmnt_only.contains(name);
            let doesnt_break_mutability =
                var_handling != VarHandling::MutBorrow ||
                var.mutability == Mutability::Mutable;
            if doesnt_move_reserved_vars &&
                doesnt_break_mutability &&
                var.lifetime.is_valid(ctx) &&
                var.ty.matches(&ty) {
                possible_exprs.push(PossibleExpression::Var(name, var));
            } else if let Some(func) = &var.ty.func {
                if func.ret_type.matches_with_generics(&ty, &var.ty.type_generics) &&
                    (ctx.size.get() < MAX_SIZE || func.args.len() <= 1) {
                    possible_exprs.push(PossibleExpression::Fn(name, &var.as_ref().ty, func));
                }
            }
        }
        for (ty_or_trait, method) in methods {
            let mut ret_gen = Vec::new();
            let (constraints, trai_gen) = match ty_or_trait {
                Right(_) => (vec![], vec![]),
                Left(trai) => (vec![Constraint {
                    trait_name: trai.name.clone(),
                    // TODO: come up with an actual value for args
                    trait_args: vec![]
                }], trai.type_generics.clone())
            };
            // ret_gen.extend(assoc_ty.type_generics.clone()); 
            ret_gen.extend(method.type_generics.clone());
            ret_gen.extend(trai_gen);
            ret_gen.push(Generic {
                name: "Self".into(),
                constraints,
                is_arg_for_other: false
            });
            if method.func.ret_type.matches_with_generics(&ty, &ret_gen) && 
                (ctx.size.get() < MAX_SIZE || method.func.args.len() <= 1) {
                possible_exprs.push(PossibleExpression::Method(method, ty_or_trait));
            }
        }
        for (struc_ty, field_ty, name) in fields {
            if let Some(constraints) = field_ty.is_generic_with(&struc_ty.type_generics) {
                // To prevent the output being flooded by tuples,
                // since getting an argument from a generic tuple
                // works for every type
                if !constraints.is_empty() &&
                    ty.fits_constraints(ctx, constraints) {
                    possible_exprs.push(PossibleExpression::Field(struc_ty, field_ty, name))
                }
            } else {
                // println!("checking if {}.{}: {} matches {}", struc_ty, name.as_str(), field_ty, ty);
                if field_ty.matches(&ty) {
                    possible_exprs.push(PossibleExpression::Field(struc_ty, field_ty, name));
                }
            }
        }
        if allow_ambiguity {
            // TODO: some macros are fine to call if we have hit the max size, others not
            for (name, macr) in macros {
                if macr.ty.matches(&ty) {
                    possible_exprs.push(PossibleExpression::Macro(name, macr));
                }
            }
        }
    }

    for (name, operator) in ctx.operators.iter() {
        if (operator.ret_type.is_generic_with(&operator.type_generics).map_or(false, 
                |con| ty.fits_constraints(ctx, con)) ||
            operator.ret_type.matches_with_generics(&ty, &operator.type_generics)) &&
            (ctx.size.get() < MAX_SIZE || operator.operands.1.is_none()) {
            // println!("Operator matches: {}", name);
            possible_exprs.push(PossibleExpression::Op(name, operator));
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
                struc.fields.iter().map(|(_,f)| f.ty.as_ref()).collect(),
            )?;
            let self_str = "Self".into();
            let generics =
                iter::once((self_str, struc.ty.as_ref()))
                    .chain(struc.ty.type_generics.iter().map(|t| t.name.clone())
                        .zip(ty.type_args.iter()))
                    .collect();
            match &struc.fields {
                Fields::Named(fields) => {
                    let mut args = vec![];
                    for (i, (name, arg)) in handle_field_generics(generics, &fields).into_iter().enumerate() {

                        let arg_ambigous = determined_fields.as_ref().map_or(true,
                            |determined| !determined.contains(&i)
                        );
                        args.push((name, construct_value_inner(ctx, u, arg, arg_ambigous, Own)?));
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
                    let field_types = fields.into_iter().map(|f| f.ty.as_ref().clone()).collect();
                    // FIXME: what are the actual lifetime generics?
                    for (i, arg) in handle_arg_generics(generics, vec![], &field_types).into_iter().enumerate() {

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
                        args.push(construct_value_inner(ctx, u, arg, arg_ambigous, Own)?);
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
        PossibleExpression::Var(name, var) => {
            match var_handling {
                Own => var.mve(ctx),
                Borrow => var.borrow(ctx, Lifetime::Anon(ctx.lifetime)),
                MutBorrow => var.mut_borrow(ctx, Lifetime::Anon(ctx.lifetime))
            }
            Ok(Expr::Var(name.to_owned()))
        }
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
            let ty_generics =
                fn_ty.type_generic_names().zip(generic_params.iter()).collect();
            let lt_params = make_lt_generics(
                &func.ret_type.lt_generics,
                &ty.lt_args,
                &fn_ty.lt_generics
            );
            let lt_generics = 
                fn_ty.lt_generic_names().zip(lt_params.iter()).collect();
            // println!("generics {:?} {:?}", type_generics, generic_params);
            for (i, arg) in handle_arg_generics(ty_generics, lt_generics, &func.args).into_iter().enumerate() {
                let arg_ambigous = determined_args.as_ref().map_or(true,
                    |determined| !determined.contains(&i)
                );
                args.push(construct_value_inner(ctx, u, arg, arg_ambigous, Own)?);
            }
            let given_ty_args = if determined_args.is_none() {
                generic_params
            } else {
                vec![]
            };
            Ok(Expr::Fn(name.clone(), given_ty_args, args))
        }
        PossibleExpression::Field(struc_ty, field_ty, name) => {
            let required_args_map: HashMap<_,_> = field_ty.diff(&ty).collect(); 
            let mut type_args = vec![];
            for gen in struc_ty.type_generics.iter() {
                if let Some(ty) = required_args_map.get(&gen.name) {
                    type_args.push((*ty).clone())
                } else {
                    type_args.push(pick_type_impls(ctx, u, &gen.constraints)?)
                }
            }
            Ok(Expr::Field(Box::new(construct_value_inner(ctx, u, Type {
                type_args,
                ..struc_ty.clone()
            }, false, Own)?), name.clone()))
        }
        PossibleExpression::Method(method, ty_or_trait) => {
            let (trait_args, assoc_ty) = match ty_or_trait {
                Left(assoc_ty) => (vec![], assoc_ty.as_ref()),
                Right(trait_desc) => {
                    let (trait_args, assoc_ty) = pick_type_impl(ctx, u, &trait_desc)?;
                    (
                        trait_desc.trait_generics.iter().cloned()
                            .map(|gen| gen.name.clone())
                            .zip(trait_args.type_args.iter())
                            .collect(),
                        assoc_ty
                    )
                }
            };

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
            let trait_param_determined: Vec<usize> =
                trait_args.iter().flat_map(|(gen,_)| {
                    method.func.args.iter().enumerate()
                        .filter_map(move |(i, arg)| {
                            if arg.contains(&gen) {
                                Some(i)
                            } else {
                                None
                            }
                        })
                }).collect();
            let self_str = "Self".into();
            let generics = 
                iter::once((self_str, assoc_ty))
                    .chain(assoc_ty.type_generic_names()
                        .chain(method.type_generic_names())
                        .zip(disc_ty_args.iter()
                            .chain(method_ty_args.iter())))
                    .chain(trait_args.iter().cloned())
                    .collect();
            let disc_lt_params = make_lt_generics(
                &method.func.ret_type.lt_generics,
                &ty.lt_args,
                &assoc_ty.lt_generics
            );
            let lt_params = make_lt_generics(
                &method.func.ret_type.lt_generics,
                &ty.lt_args,
                &method.lt_generics
            );
            let lt_generics = 
                assoc_ty.lt_generic_names()
                    .zip(disc_lt_params.iter())
                    .chain(method.lt_generic_names()
                        .zip(lt_params.iter()))
                .collect();
            let mut args = vec![];
            // FIXME: what are the actual lifetime generics?
            for (i, arg) in handle_arg_generics(
                generics,
                lt_generics,
                &method.func.args
            ).into_iter().enumerate() {
                let arg_ambigous =
                    !trait_param_determined.contains(&i) &&
                    determined_args.as_ref().map_or(true,
                        |determined| !determined.contains(&i)
                    );
                args.push(construct_value_inner(ctx, u, arg, arg_ambigous, Own)?);
            }
            let given_ty_args = if determined_args.is_none() {
                method_ty_args
            } else {
                vec![]
            };
            if let Some(ownership) = method.self_param {
                Ok(Expr::Method(
                    Box::new(construct_value_inner(ctx, u, Type {
                        type_args: disc_ty_args,
                        ..assoc_ty.clone()
                    }, false, ownership)?),
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
        
        PossibleExpression::Op(name, op) => {
            let diff = op.ret_type.diff(&ty).collect::<Vec<_>>();
            let mut generated_store = vec![];
            // We have to store all the non-reference types in the above vector in order
            // to reference them. This has to be done in two passes to avoid having both
            // mutable and immutable borrows in the loop/iterator
            let generated_either = generate_free_generics(ctx, u, &op.type_generics, &diff)?;
            let generated_either_stored =
                generated_either.into_iter().map(|(name, e)| match e {
                    Right(ty_ref) => (name, Right(ty_ref)),
                    Left(ty) => {
                        generated_store.push(ty);
                        (name, Left(generated_store.len() -1))
                    }
                }).collect::<Vec<_>>();
            let generated =
                generated_either_stored.into_iter().map(|(name, e)| match e {
                    Right(ty_ref) => (name, ty_ref),
                    Left(i) => (name, &generated_store[i])
                });
            let generics: Vec<(StringWrapper, &Type)> =
                diff.into_iter()
                    .chain(generated)
                    .collect();
            // println!("diff: {:?}", generics);
            match &op.operands {
                (lhs, Some(rhs)) => {
                    Ok(Expr::BinOp(name, 
                        Box::new(construct_value_inner(ctx, u, handle_single_generic(generics.clone(), lhs), false, Own)?),
                        Box::new(construct_value_inner(ctx, u, handle_single_generic(generics, rhs), false, Own)?)
                    ))
                }
                (operand, None) => {
                    let ownership = match name {
                        "&" => Borrow,
                        "& mut" => MutBorrow,
                        _ => Own
                    };
                    let val = construct_value_inner(
                        ctx, u,
                        handle_single_generic(generics, operand),
                        allow_ambiguity,
                        ownership
                    )?;
                    Ok(Expr::UnOp(name, Box::new(val)))
                }
            }
        }
    }
}

pub fn kind_to_type<S: Into<StringWrapper>>(name: S, kind: &Kind) -> Type {
    Type {
        name: name.into(),
        lt_generics: GEN_STRING[0..(kind.lifetimes as usize)].iter()
                            .map(|s| (*s).into()).collect(),
        type_generics: GEN_STRING[0..(kind.types as usize)].iter()
                            .map(|s| Generic {
                                    name: (*s).into(), 
                                    is_arg_for_other: false,
                                    constraints: vec![]
                                }).collect(),
        lt_args: vec![],
        type_args: vec![],
        func: None
    }
}

pub fn name_to_type<S: Into<StringWrapper>>(name: S) -> Type {
    Type {
        name: name.into(),
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

macro_rules! make_var {
    ($name:path : $($ty:tt)*) =>
        ((stringify!($name).into(), Rc::new(Variable::new(
            make_type!($($ty)*), crate::semantics::Mutability::Immutable,
            crate::semantics::Lifetime::Named("'static".into())
        ))))
}

#[macro_export]
macro_rules! make_type {
    (!) => (crate::semantics::Type {
        name: "!".into(),
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args: vec![],
        func: None
    });
    (*& $($rest:tt)*) => (crate::semantics::Type {
        name: "#RefMut",
        ..make_type!(& $($rest)*)
    });
    (& #($($ty:tt)*)) => (make_type!(& {'a} 'a $($ty)*));
    (& $name:ident $($rest:tt)*) => (make_type!(& {'a} 'a $name $($rest)*));
    (& $lt:lifetime $($rest:tt)*) => (make_type!(& {} $lt $($rest)*));
    (& %local_ref $($rest:tt)*) => (crate::semantics::Type {
        lt_args: vec![crate::semantics::Lifetime::Named("%local_ref".into())],
        ..make_type!(& 'local_ref $($rest)*)
    });
    (& {$($gen:tt)*} $lt:lifetime $($ty:tt)*) => ({
        let (lt_generics, type_generics): (Vec<StringWrapper>, Vec<Generic>) = parse_generics!([],[];$($gen)*,);
        crate::semantics::Type {
            name: "#Ref".into(),
            lt_generics, type_generics,
            lt_args: vec![crate::semantics::Lifetime::Named(
                stringify!($lt).into()
            )],
            type_args: vec![make_type!($($ty)*)],
            func: None
        }
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
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args: vec![],
        func: None
    });
    (()) => (crate::semantics::Type {
        name: "#Unit".into(),
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
        type_generics: [
            (make_type!($fst), vec![])
            $(, make_type!($rest))*
        ].iter().cloned().collect(),
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
    ([$(,$lt:expr)*],[$(,$ty:expr)*];$(,)?) => ( 
        (vec![$($lt),*], vec![$($ty),*])
    );
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$lt:lifetime, $($arg:tt)*) => {
        parse_generics!([$(,$lts)*,stringify!($lt).into()],[$(,$tys)*];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$name:path, $($arg:tt)*) => {
        parse_generics!([$(,$lts)*],[$(,$tys)*,
            Generic {
                name: stringify!($name).into(), 
                constraints: vec![],
                is_arg_for_other: false
            }
        ];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$name:path : ($($traits:tt)*), $($arg:tt)*) => {
        parse_generics!([$(,$lts)*],[$(,$tys)*,
            Generic {
                name: stringify!($name).into(), 
                constraints: parse_constraints!(;$($traits)*,),
                is_arg_for_other: false
            }
        ];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];?$name:path, $($arg:tt)*) => {
        parse_generics!([$(,$lts)*],[$(,$tys)*,
            Generic {
                name: stringify!($name).into(), 
                constraints: vec![],
                is_arg_for_other: true
            }
        ];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];?$name:path: ($($traits:tt)*), $($arg:tt)*) => {
        parse_generics!([$(,$lts)*],[$(,$tys)*,
            Generic {
                name: stringify!($name).into(), 
                constraints: parse_constraints!(;$($traits)*,),
                is_arg_for_other: true
            }
        ];$($arg)*)
    };

}

macro_rules! parse_trait_desc {
    ($name:path$({$($trait_gen:ident),*})?) => {
        crate::semantics::TraitDescription {
            name: stringify!($name).into(),
            trait_generics: parse_trait_generics!(;$($($trait_gen)*)?,)
        }
    }
}

macro_rules! parse_trait_generics {
    ($($args:tt)*) => ({
        let (_lt_generics, type_generics): (Vec<Lifetime>, _)
        = parse_generics!([],[]$($args)*);
        type_generics
    })
}


macro_rules! parse_constraints {
    ($(,$exprs:expr)*;$(,)?) => (vec![$($exprs),*]);
    ($(,$exprs:expr)*;$name:path$([$($args:tt)*])?, $($rest:tt)*) => ({
        parse_constraints!($(,$exprs)*, parse_constraint!($name$([$($args)*])?); $($rest)*)
    })
}

macro_rules! parse_constraint {
    ($name:path$([$($args:tt)*])?) => ({
        crate::semantics::Constraint {
            trait_name: stringify!($name).into(),
            trait_args: parse_types!(;$($($args)*)?,)
        }
    })
}

macro_rules! make_kind {
    ($name:path$({})?) => (make_kind!($name{0;0}));
        
    ($name:path{$ty:expr}) => {
        (stringify!($name).into(), crate::semantics::Kind {
            is_visible: true,
            lifetimes: 0,
            types: $ty
        })
    };
    (()) => (("#Unit".into(), crate::semantics::Kind {
        is_visible: true,
        lifetimes: 0,
        types: 0
    }));
    (($len:expr;)) => {
        (TUPLE_NAMES[$len-1].into(), crate::semantics::Kind {
            is_visible: true,
            lifetimes: 0,
            types: $len
        })
    };
    ($name:path{$lt:expr; $ty:expr}) => {
        (stringify!($name).into(), Kind {
            is_visible: true,
            lifetimes: $lt,
            types: $ty
        })
    };
}

#[allow(unused_macros)]
macro_rules! make_trait {
    ($name:path$({$($gen:tt)*})? : ($($types:tt)*)) => ({
        let name = StringWrapper::from(stringify!($name));
        let (_lt_generics, type_generics): (Vec<Lifetime>, _)
            = parse_generics!([],[];$($($gen)*,)?);
        (name.clone(), crate::semantics::Trait {
            name, type_generics,
            implementors: parse_implementors!(;$($types)*,)
        })
    })
}

macro_rules! parse_implementors {
    ($(,$exprs:expr)*;$(,)?) => (vec![$($exprs,)*]);
    ($(,$exprs:expr)*; $name:path$({$($gen:tt)*})?$([$($args:tt)*])?, $($rest:tt)*) => {
        parse_implementors!($(,$exprs)*; {}[] : #($name$({$($gen)*})?$([$($args)*])?), $($rest)*)
    };
    ($(,$exprs:expr)*; #($($ty:tt)*), $($rest:tt)*) => {
        parse_implementors!($(,$exprs)*; {}[] : #($($ty)*), $($rest)*)
    };
    ($(,$exprs:expr)*; [$($args:tt)*]: $($rest:tt)*) => {
        parse_implementors!($(,$exprs)*; {}[$($args)*] : $($rest)*)
    };
    ($(,$exprs:expr)*; {$($trait_gen:tt)*}[$($trait_args:tt)*] :
                       $name:path$({$($gen:tt)*})?$([$($args:tt)*])?, $($rest:tt)*) => {
        parse_implementors!($(,$exprs)*; {$($trait_gen)*}[$($trait_args)*] : #($name$({$($gen)*})?$([$($args)*])?), $($rest)*)
    };
    ($(,$exprs:expr)*;{$($gen:tt)*}[$($args:tt)*] : #($($ty:tt)*), $($rest:tt)*) => ({
        let (_lt_generics, type_generics): (Vec<Lifetime>, _)
            = parse_generics!([],[];$($($gen)*,)?);
        parse_implementors!($(,$exprs)*,(crate::semantics::TraitArgs {
            type_generics,
            type_args: parse_types!(;$($args)*,)
        }, Rc::new(make_type!($($ty)*)));$($rest)*)
    })
}

static GEN_STRING: [&'static str; 12] = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"];

macro_rules! make_methods {
    ($name:path$([$($type_args:tt)*])? {$($methods:tt)*}) => 
        (make_methods!(#($name$([$($type_args)*])?){$($methods)*}));
    (% #($($ty:tt)*) {$($methods:tt)*}) => (
        (Right(parse_trait_desc!($($ty)*)), parse_methods!(;$($methods)*,))
    );
    (#($($ty:tt)*){$($methods:tt)*}) => (
        (Left(Rc::new(make_type!($($ty)*))), parse_methods!(;$($methods)*,))
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
            let (self_param, args): (SelfParam, Vec<Type>)
                = parse_method_args!($($args)*,);
            let ret_type = make_type!($($ret)*);
            let (lt_generics, type_generics) = parse_generics!([],[];$($($gen)*)?,);
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
    (self, $($args:tt)*) => ((Some(Own), parse_types!(;$($args)*)));
    (& self, $($args:tt)*) => ((Some(Borrow), parse_types!(;$($args)*)));
    (& mut self, $($args:tt)*) => ((Some(MutBorrow), parse_types!(;$($args)*)));
    (*& self, $($args:tt)*) => ((Some(MutBorrow) parse_types!(;$($args)*)));
    ($($args:tt)*) => ((None, parse_types!(;$($args)*)));
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
        is_enum_variant: false,
        ty: Rc::new(make_type!(())),
        fields: Fields::None
    }));
    (($len:expr;)) => ({
        let len = $len;
        (TUPLE_NAMES[len-1].into(), crate::semantics::Struct {
            is_enum_variant: false,
            ty: Rc::new(Type {
                name: TUPLE_NAMES[len-1].into(),
                lt_args: vec![],
                lt_generics: vec![],
                type_generics: GEN_STRING[0..len].iter()
                    .map(|s| (Generic {
                        name: s.clone().into(), 
                        constraints: vec![],
                        is_arg_for_other: false
                    })).collect(),
                type_args: vec![], //GEN_NAMES[0..len].to_vec(),
                func: None
            }),
            fields: Fields::Unnamed(GEN_STRING[0..len].iter().map(|name| Field {
                visible: true,
                ty: Rc::new(name_to_type(name.to_owned()))
            }).collect())
        })
    });
    ($name:ident$($rest:tt)*) => ({
        let (name, struc) = make_struct!(:$name: $name$($rest)*);
        (name, crate::semantics::Struct {
            is_enum_variant: false, 
            ..struc
        })
    });
    (:$ty:path: $name:path$([$($gen:tt)*])? ) =>
        ((stringify!($name).into(), crate::semantics::Struct {
            is_enum_variant: false,
            ty: Rc::new(make_type!($ty$({$($gen)*})?)),
            fields: Fields::None
        }));
    ($name:ident$([$($gen:tt)*])? ( $($fields:tt)* )) =>
        (make_struct!(:$name:$name$([$($gen)*])? ( $($fields)* )));
    ($name:path$([$($gen:tt)*])? { $($fields:tt)* }) =>
        (make_struct!(:$name:$name$([$($gen)*])? { $($fields)* }));
    (:$ty:path: $name:ident $([$($gen:tt)*])? ( $($fields:tt)* )) =>
        ((StringWrapper::Static(stringify!($name)), crate::semantics::Struct {
            is_enum_variant: true,
            ty: Rc::new(make_type!($ty$({$($gen)*}[$($gen)*])?)),
            fields: parse_unnamed_fields!(;$($fields)*,)
        }));
    (:$ty:path: $name:path $([$($gen:tt)*])? { $($fields:tt)* }) =>
        ((stringify!($name).into(), crate::semantics::Struct {
            is_enum_variant: true,
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
            ty: make_type!($($ty)*)
        }; $($rest)*);
    };
    ($(,$fields:expr)*; #($($ty:tt)*), $($rest:tt)*) => {
        parse_unnamed_fields!($(,$fields)*, crate::semantics::Field {
            visible: false,
            ty: Rc::new(make_type!($($ty)*))
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
            ty: make_type!($($ty)*)
        }; $($rest)*));
    };
    ($(,$fields:expr)*;$name:ident : #($($ty:tt)*), $($rest:tt)*) => {
        parse_named_fields!($(,$fields)*, (stringify!($name).into(), crate::semantics::Field {
            visible: false,
            ty: make_type!($($ty)*)
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


pub fn prelude_scope(use_panics: bool) -> Scope {
    let mut scope = Scope {
        by_ty_name: HashMap::new(),
        owned: false,
        vars: vec![
            make_var!(drop: %Fn{T}(T)),
        ].into_iter().map(|(k,v)| (StringWrapper::clone(&k), v)).collect(), 
        types: [
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
            // make_kind!(Default),
            // make_kind!(Eq),
            // make_kind!(Ord),
            // make_kind!(ParitalOrd),
            // make_kind!(ToOwned),
            make_kind!(Box{1}),
            make_kind!(String),
            make_kind!(Vec{1}),
        ].iter().cloned().collect(),
        lifetimes: HashMap::new(),
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
            }),
            make_methods!(#(Vec{T: (Clone)}) {
                resize(self, usize, T),
            }),
            make_methods!(std::io::Error {
                new{E: (IntoError)}(std::io::ErrorKind, E) -> std::io::Error
            }),
            make_methods!(% #(ToString) {
                to_string(self) -> String 
            }),
            make_methods!(% #(PartialEq{Rhs}) {
                eq{'a}(&self, #(&'a Rhs)) -> bool,
                ne{'a}(&self, #(&'a Rhs)) -> bool
            }),
            make_methods!(% #(PartialOrd) {
                gt{'a}(&self, #(&'a Self)) -> bool,
                ge{'a}(&self, #(&'a Self)) -> bool,
                lt{'a}(&self, #(&'a Self)) -> bool,
                le{'a}(&self, #(&'a Self)) -> bool,
            }),
            make_methods!(% #(std::ops::Add) {
                add(self, Self) -> Self
            }),
            make_methods!(% #(std::ops::Sub) {
                sub(self, Self) -> Self
            }),
            make_methods!(% #(std::ops::Mul) {
                mul(self, Self) -> Self
            }),
        ],
        traits: [
            // make_kind!(Send),
            make_trait!(ToString : (char, String)),
            make_trait!(IntoError : (String)),
            make_trait!(PartialEq{Rhs} : (
                [String]: String,
                [char]: char,
                [u8]: u8,
                [u16]: u16, 
                [u32]: u32, 
                [u128]: u128, 
                [i8]: i8, 
                [i16]: i16, 
                [i32]: i32, 
                [i128]: i128, 
                [bool]: bool, 
                [usize]: usize,
            )),
            make_trait!(Debug : (
                String, char, u8, u16, u32, u128, i8, i16, i32, i128, bool, usize,
                Vec{T: (Clone)}[T], Option{T: (Clone)}[T],
                Result{R: (Clone), E: (Clone)}[R,E]
            )),
            make_trait!(PartialOrd : (
                String, char, u8, u16, u32, u128, i8, i16, i32, i128, bool, usize,
            )),
            make_trait!(std::ops::Add : (
                u8, u16, u32, u128, i8, i16, i32, i128, usize,
            )),
            make_trait!(std::ops::Sub : (
                u8, u16, u32, u128, i8, i16, i32, i128, usize,
            )),
            make_trait!(std::ops::Mul : (
                u8, u16, u32, u128, i8, i16, i32, i128, usize,
            )),
            make_trait!(Clone: (
                String, char, u8, u16, u32, u128, i8, i16, i32, i128, bool, usize,
                Vec{T: (Clone)}[T], Option{T: (Clone)}[T],
                Result{R: (Clone), E: (Clone)}[R,E]
            ))
        ].iter().cloned().collect(),
        structs: [
            make_struct!(:Option:None[T]),
            make_struct!(:Maybe:Some[T](T)),
            make_struct!(:Result:Ok[R,E](R)),
            make_struct!(:Result:Err[R,E](E)),
            make_struct!(:std::io::ErrorKind:std::io::ErrorKind::NotFound),
            make_struct!(:std::io::ErrorKind:std::io::ErrorKind::PermissionDenied),
            make_struct!(:std::io::ErrorKind:std::io::ErrorKind::ConnectionRefused),
        ].iter().cloned().map(|(k,v)| (k, Rc::new(v))).collect(), 
        macros: {
            let mut macros = vec![
                make_macro!(#(Vec{T}) : vec(|ty_args, ctx, u| {
                    Ok(MacroBody {
                        brackets: if Arbitrary::arbitrary(u)? {
                            BracketType::Round
                        } else {
                            BracketType::Square
                        },
                        tokens: c_arbitrary_iter_with_non_mut(ctx, u, |ctx, u| {
                            Ok(Token::Expr(construct_value_inner(ctx, u, ty_args[0].clone(), true, Own)?))
                        }).collect::<context_arbitrary::Result<Vec<Token>>>()?,
                        seperator: Seperator::Comma
                    })
                })),
            ];
            if use_panics {
                macros.push(make_macro!(#(!) : panic(|_, _, u| {
                    Ok(MacroBody {
                        brackets: BracketType::Round,
                        tokens: vec![Token::Expr(Expr::ExactString(Arbitrary::arbitrary(u)?))],
                        seperator: Seperator::Comma
                    })
                })))
            }
            macros
        }.into_iter().map(|(k,v)| (k, Rc::new(v))).collect(), 
    };
    scope.make_ty_index();
    scope
}

pub fn primitive_scope() -> Scope {
    let mut scope = Scope {
        by_ty_name: HashMap::new(),
        owned: false,
        vars: HashMap::new(),
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
            // make_kind!((12;)),
            ("!".into(), Kind { is_visible: true, lifetimes: 0, types: 0 })
        ].iter().cloned().collect(),
        lifetimes: HashMap::new(),
        traits: HashMap::new(),
    };
    scope.make_ty_index();
    scope
}

pub fn operators() -> HashMap<&'static str, Operator> {
    [
        ("&", Operator {
            type_generics: vec![Generic {
                name: "T".into(),
                constraints: vec![],
                is_arg_for_other: false
            }],
            operands: (make_type!(T), None),
            // TODO: the lifetime should be the current scopes lifetime
            ret_type: make_type!(& %local_ref T)
        }),
        ("+", Operator {
            type_generics: make_type!(add{A: (std::ops::Add)}).type_generics,
            operands: (make_type!(A), Some(make_type!(A))),
            ret_type: make_type!(A)
        }),
        ("-", Operator {
            type_generics: make_type!(sub{S: (std::ops::Sub)}).type_generics,
            operands: (make_type!(S), Some(make_type!(S))),
            ret_type: make_type!(S)
        }),
        ("*", Operator {
            type_generics: make_type!(mul{M: (std::ops::Mul)}).type_generics,
            operands: (make_type!(M), Some(make_type!(M))),
            ret_type: make_type!(M)
        }),
        ("==", Operator {
            type_generics: make_type!(eq{?Rhs, E: (PartialEq[Rhs])}).type_generics,
            operands: (make_type!(E), Some(make_type!(Rhs))),
            ret_type: make_type!(bool)
        })
    ].iter().cloned().collect()
}


// #[cfg(test)]
// mod tests {
//     use super::*;
//     #[test]
//     fn matches_generics() {
//         assert!(make_type!(Option{T}[T]).matches(&make_type!(Option[i32])));
//     }
//     #[test]
//     fn not_matches_different_parameters() {
//         assert!(!make_type!(Option[i32]).matches(&make_type!(Option[str])));
//     }
//     #[test]
//     fn different_specificity_matches_correctly() {
//         assert!(!make_type!(Option[i32]).matches(&make_type!(Option{T}[T])));
//         assert!( make_type!(Option{T}[T]).matches(&make_type!(Option[i32])));
//     }
//     #[test]
//     fn fn_return_type_matches() {
//         assert!(make_type!(%Fn{T}(T) -> Box[T]).func.unwrap().ret_type.matches(&make_type!(Box[u16])));
//     }
//     // #[test]
//     // fn assoc_type_generics() {
//     //     assert!(make_methods!(#(Vec{T}) { a(), b() }).0.type_generics.len() == 1)
//     // }
//     #[test]
//     fn struct_type_generics() {
//         assert!(make_struct!(:Result:Ok[R,E](R)).1.ty.type_generics.len() == 2)
//     }
//     #[test]
//     fn type_diff() {
//         assert!(make_type!(A).diff(&make_type!(B)).next().unwrap().1.matches(&make_type!(B)));
//         assert!(make_type!(Vec[A]).diff(&make_type!(Vec[B])).next().unwrap().1.matches(&make_type!(B)));
//         let many_diffs_lhs = make_type!(Result[Vec[Result[A,B]],Box[C]]);
//         let many_diffs_rhs = make_type!(Result[Vec[Result[D,E]],Box[F]]);
//         let many_diffs = many_diffs_lhs.diff(&many_diffs_rhs).collect::<Vec<_>>();
//         let expected = [("A".into(), make_type!(D)), ("B".into(), make_type!(E)), ("C", make_type!(F))];
//         assert!(many_diffs.len() == 3);
//         for (i, diff) in many_diffs.into_iter().enumerate() {
//             // assert!(diff.0 == &expected[i].0);
//             assert!(diff.1.matches(&expected[i].1));
//         }
//     }
//     #[test]
//     fn fits_constraints() {
//         let constraints = &make_type!(add{A: (std::ops::Add)}[A]).type_generics[0].constraints;
//         assert!(!make_type!(&str).fits_constraints(&Context::make_context(true), constraints));
//     }
// }
// // TODO: should take trait args as well
