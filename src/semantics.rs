use std::backtrace::Backtrace;
use std::collections::HashMap;
use std::cell::{self, Cell, RefCell, RefMut};
use std::fmt::Display;
use std::hash::Hash;
use std::iter;
use std::iter::FromIterator;
use std::rc::Rc;
use std::collections::HashSet;
use std::sync::atomic::{self, AtomicUsize};
use arbitrary::{Arbitrary, Unstructured};
use crate::context::{RefType, fresh_lt};
use crate::context_arbitrary::{Result, GenerationError};
use crate::choose::choose_consume;
use crate::ty_macros::GEN_STRING;
use crate::string_wrapper::*;
use crate::branching::{save_var, save_lt};

use super::context::Context;

#[derive(PartialEq, Clone, Debug)]
pub enum Mutability { Immutable, Mutable }
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
        use Lifetime::*;
        if other == &Any ||
            self.name().map_or(false, |name| generics.contains(name)) {
            return true
        // The lifetimes then need to be constrained by eachother
        } else if let (Anon(_), Anon(_)) = (self, other) {
            return true
        } else if let (Any, Anon(_)) = (self, other) {
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
impl Display for Lifetime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lifetime::Named(n) => write!(f, "{}", n.as_str()),
            Lifetime::Anon(a) => write!(f, "#anon({})", a),
            Lifetime::Any => write!(f, "#any"),
        }
    }
}
pub type TypeGenerics = Vec<Generic>;
pub type LtGenerics = Vec<StringWrapper>;
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Generic {
    pub name: StringWrapper,
    pub is_arg_for_other: bool,
    pub constraints: Vec<Constraint>
}
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Constraint {
    pub trait_name: Path,
    pub trait_args: Vec<Type>
}
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Type {
    pub name: Path,
    pub lt_generics: LtGenerics,
    pub type_generics: TypeGenerics,
    pub lt_args: Vec<Lifetime>,
    pub type_args: Vec<Type>,
    pub func: Option<Box<Func>>,
    pub is_visible: bool
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}",
            path_to_string(&self.name),
            if self.type_generics.is_empty() {
                "".to_string()
            } else {
                format!("{{{}}}",
                    self.type_generics.iter()
                        .map(|a| format!("{}", a.name.as_str()))
                        .chain(
                            self.lt_generics.iter().map(StringWrapper::to_string)
                        )
                        .reduce(|acc, gen| format!("{}, {}", acc, gen))
                        .unwrap_or("".to_string())
                )
            },
            if self.type_args.is_empty() {
                "".to_string()
            } else {
                format!("[{}]",
                    self.type_args.iter()
                        .map(|a| format!("{}", a))
                        .chain(
                            self.lt_args.iter().map(|lt| format!("{}", lt))
                        )
                        .reduce(|acc, gen| format!("{}, {}", acc, gen))
                        .unwrap_or("".to_string())
                )
            }
        )
    }
}
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Func {
    pub args: Vec<Type>,
    pub ret_type: Type
}
#[derive(PartialEq, Clone, Debug)]
pub struct Kind { pub is_visible: bool, pub lifetimes: usize, pub types: usize }
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
    pub fn iter(&self) -> FieldsIter {
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
pub type Struct = Rc<Fields>;
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
type MacroConstructor = fn(Vec<Type>, &Context, &mut Unstructured) -> Result<MacroBody>;
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
pub type SelfParam = Option<VarHandling>;
#[derive(PartialEq, Clone, Debug)]
pub struct Method {
    pub name: StringWrapper,
    pub self_param: SelfParam,
    pub lt_generics: LtGenerics,
    pub lt_args: Vec<Lifetime>,
    pub type_generics: TypeGenerics,
    pub type_args: Vec<Type>,
    pub func: Func
}
#[derive(Clone, Debug)]
pub struct TraitDescription {
    pub name: Path,
    pub trait_generics: TypeGenerics
}
#[derive(Clone, Debug)]
pub struct Trait {
    pub name: Path,
    pub type_generics: TypeGenerics,
    pub implementors: Vec<(TraitArgs, Rc<Type>)>
}
#[derive(Clone, Debug)]
pub struct TraitArgs {
    pub type_generics: TypeGenerics,
    pub type_args: Vec<Type>
}
impl TraitArgs {
    pub fn matches(&self, args: &Vec<Type>) -> bool {
        self.matches_with_generics(args, &vec![])
    }
    pub fn matches_with_generics(&self, args: &Vec<Type>, generics: &TypeGenerics) -> bool {
        self.type_args.iter().zip(args)
            .all(|(a1, a2)| a1.matches_with_generics(a2, generics))
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum Either<A,B> { Left(A), Right(B) }
pub use Either::{Left, Right};
#[derive(Clone, Debug, PartialEq)]
pub enum Refs {
    None,
    MutRef(Lifetime),
    Ref(Vec<Lifetime>),
    /// Only possible through branching (e.g. if/else)
    BranchingRefs { mutable: Vec<Lifetime>, immutable: Vec<Lifetime> }
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
        end_lifetime(ctx, &self.lifetime);
    }
    fn borrow(&self, ctx: &Context, lt: Lifetime) {
        self.refs.replace_with(|r| {
            use Refs::*;
            constrain_lt(ctx, &lt, &self.lifetime);
            match r {
                None => Ref(vec![lt]),
                Ref(old_lts) => {
                    let mut lts = std::mem::replace(old_lts, vec![]);
                    lts.push(lt);
                    Ref(lts)
                },
                MutRef(mut_lt) => {
                    end_lifetime(ctx, mut_lt);
                    Ref(vec![lt])
                },
                BranchingRefs { mutable, immutable } => {
                    mutable.into_iter().for_each(|lt| end_lifetime(ctx, lt));
                    immutable.push(lt);
                    Ref(immutable.clone())
                }
            }
        });
    }
    fn mut_borrow(&self, ctx: &Context, lt: Lifetime) {
        self.refs.replace_with(|r| {
            use Refs::*;
            constrain_lt(ctx, &lt, &self.lifetime);
            match r {
                None => {}
                Ref(lts) => lts.iter().for_each(|lt| end_lifetime(ctx, lt)),
                MutRef(mut_lt) => end_lifetime(ctx, mut_lt),
                BranchingRefs { mutable, immutable } => {
                    mutable.into_iter().for_each(|lt| end_lifetime(ctx, lt));
                    immutable.into_iter().for_each(|lt| end_lifetime(ctx, lt));
                }
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
pub type VarScope = HashMap<Path, Rc<Variable>>;
pub type TypeScope = HashMap<Path, Kind>;
pub type LtRefs = (Cell<bool>, RefCell<Vec<Lifetime>>);
pub type LtScope = HashMap<Lifetime, LtRefs>;
pub type TraitScope = HashMap<Path, Trait>;
pub type StructScope = HashMap<Path, (Rc<Type>, Struct)>;
pub type EnumScope = HashMap<Path, (Rc<Type>, HashMap<StringWrapper, Struct>)>;
pub type MacroScope = HashMap<Path, Rc<Macro>>;
pub type MethodScope = Vec<(Either<Rc<Type>, TraitDescription>, Vec<Rc<Method>>)>;
#[derive(Clone, Debug, Default)]
pub struct Reservations {
    pub fns: Vec<StringWrapper>,
    /// The bool being true means that the struct is a tuple struct
    pub structs: Vec<(Type, bool)>,
    pub enums: Vec<(Type, Vec<(StringWrapper, bool)>)>
}
static SCOPE_ID: AtomicUsize = AtomicUsize::new(0);
#[derive(Clone, Debug)]
pub struct Scope {
    pub id: usize,
    pub path_aliases: HashMap<Path, Vec<Path>>,
    pub owned: bool,
    pub vars: VarScope,
    pub types: TypeScope,
    pub lifetimes: LtScope,
    pub traits: TraitScope,
    pub structs: StructScope,
    pub reserved: Reservations,
    pub enums: EnumScope,
    pub macros: MacroScope,
    pub methods: MethodScope,
    pub by_ty_name: HashMap<Path, TypeIndexed>
}
#[derive(Clone, Debug, PartialEq)]
pub struct Operator {
    pub type_generics: TypeGenerics,
    pub operands: (Type, Option<Type>),
    pub ret_type: Type,
}
impl Default for Scope {
    fn default() -> Self {
        // The ordering doesn't currently matter, but it was selected
        // arbitrarily and should maybe be changed if the program does
        // become concurrent in the future.
        let id = SCOPE_ID.fetch_add(1, atomic::Ordering::Relaxed);
        Scope {
            id,
            owned: true,
            path_aliases: Default::default(),
            vars: Default::default(),
            types: Default::default(),
            lifetimes: Default::default(),
            traits: Default::default(),
            structs: Default::default(),
            reserved: Default::default(),
            enums: Default::default(),
            macros: Default::default(),
            methods: Default::default(),
            by_ty_name: Default::default(),
        }
    }
}
impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        //! Scopes are given a unique id on construction but it's possible to break
        //! the uniqueness in varoius ways which also breaks this function.
        self.id == other.id
    }
}
impl Scope {
    pub fn make_ty_index(&mut self) -> &mut Self {
        let enums_and_structs =
            self.structs.iter().map(|(n, (ty, st))| (n.clone(), false, ty, st))
                .chain(self.enums.iter()
                    .flat_map(|(path, (ty, variants))|
                        variants.iter().map(move |(name, variant)| {
                            let mut full_path = path.clone();
                            full_path.push(name.clone());
                            (full_path.to_owned(), true, ty, variant)
                        })));
        for (name, is_enum_variant, struc_ty, struc) in enums_and_structs {
            self.by_ty_name.entry(struc_ty.name.clone())
                .or_insert(Default::default())
                .structs.insert(name.clone(), (struc_ty.clone(), struc.clone()));
            for (field_name, field) in struc.iter() {
                if !is_enum_variant &&
                    !struc_ty.name.last().unwrap().starts_with("#Tuple") {
                    if field.ty.is_generic_with(&struc_ty.type_generics).is_some() {
                        self.by_ty_name.entry(vec!["#Generic".into()])
                            .or_insert(Default::default())
                            .fields.push((
                                struc_ty.clone(),
                                field.ty.clone(),
                                field_name.clone()
                            ))
                    } else {
                        self.by_ty_name.entry(field.ty.name.clone())
                            .or_insert(Default::default())
                            .fields.push((struc_ty.clone(), field.ty.clone(), field_name.clone()))
                    }
                }
            }
        }
        for (name, var) in &self.vars {
            if let Some(func) = &var.ty.func {
                if let Some(_) = func.ret_type.is_generic_with(&var.ty.type_generics) {
                    self.by_ty_name.entry(vec!["#Generic".into()])
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
                    self.by_ty_name.entry(vec!["#Generic".into()])
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
    pub vars: HashMap<Path, Rc<Variable>>,
    pub structs: HashMap<Path, (Rc<Type>, Struct)>,
    pub macros: HashMap<Path, Rc<Macro>>,
    pub fields: Vec<(Rc<Type>, Rc<Type>, StringWrapper)>,
    pub methods: Vec<(Either<Rc<Type>, TraitDescription>, Rc<Method>)>,
    pub trait_impls: HashMap<Path, Vec<(TraitArgs, Rc<Type>)>>
}
#[derive(PartialEq, Clone, Debug)]
pub enum RcList<T> {
    Cons(RefCell<T>, Rc<RcList<T>>),
    Nill
}
pub type Stack<T> = Rc<RcList<T>>;
impl<T> RcList<T> {
    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item=&T> + 'a> {
        //! WARNING: calling while a link is mut_borrowed can result in undefined behavior
        match self {
            RcList::Nill => Box::new(iter::empty()),
            RcList::Cons(t, rest) => Box::new(
                iter::once(unsafe {
                    t.try_borrow_unguarded().unwrap()
                }).chain(rest.iter())
            )
        }
    }
    pub fn sized_iter<'a>(&'a self) -> std::vec::IntoIter<&T> {
        self.iter().collect::<Vec<_>>().into_iter()
    }
    pub fn top_mut(&self) -> Option<RefMut<T>> {
        match self {
            RcList::Nill => None,
            RcList::Cons(t, _rest) => Some(t.borrow_mut())
        }
    }
    pub fn top(&self) -> Option<cell::Ref<T>> {
        match self {
            RcList::Nill => None,
            RcList::Cons(t, _rest) => Some(t.borrow())
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
pub type Path = Vec<StringWrapper>;
pub fn path_to_string(p: &Path) -> String {
    p.iter().map(StringWrapper::to_string).reduce(|acc, a| {
        format!("{}::{}", acc, a)
    }).unwrap_or("".to_string())
}
#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Lit(Path, Vec<Type>),
    Struct(Path, Vec<Type>, HashMap<StringWrapper, Expr>),
    Fn(Path, Vec<Type>, Vec<Expr>),
    AssocFn(Path, Vec<Type>, StringWrapper, Vec<Type>, Vec<Expr>),
    Method(Box<Expr>, StringWrapper, Vec<Type>, Vec<Expr>),
    Var(Path),
    UnOp(&'static str, Box<Expr>),
    BinOp(&'static str, Box<Expr>, Box<Expr>),
    Field(Box<Expr>, StringWrapper),
    Macro(Path, MacroBody),
    ExactString(String),
    AdditionalArg(Type, Mutability)
}


impl Type {
    pub fn sub_tys<'a>(&'a self) -> Box<dyn Iterator<Item=&Type> + 'a> {
        Box::new(iter::once(self).chain(self.type_args.iter().flat_map(Type::sub_tys)))
    }
    pub fn sub_lts<'a>(&'a self) -> Box<dyn Iterator<Item=&Lifetime> + 'a> {
        Box::new(self.sub_tys().flat_map(|ty| ty.lt_args.iter()))
    }
    pub fn matches(&self, other: &Type) -> bool {
        self.matches_with_generics(other, &self.type_generics)
    }
    pub fn matches_with_generics(&self, other: &Type, generics: &TypeGenerics) -> bool {
        //! NOT reflexive: Option{N}[N].matches(Option[i32]) but not
        //! Option[i32].matches(Option{N}[N])

        // if self.name == "#Ref" && other.name == "#Ref" {
        //     println!("Comparing refs: {} ({:?})\n    {} ({:?})",
        //         self, self.lt_args, other, other.lt_args)
        // }

        // The never type can be coerced into any type, but no type can be 
        // coerced into the never type

        // \forall x: !.matches(x)
        // \forall x: x != ! ==> not x.matches(!)
        if self.name.last().unwrap() == &"!" {
            return true;
        } 
        if self.name != other.name /*&&
            !(self.name == "#RefMut" && other.name == "#Ref")*/ {
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
        if self.name != other.name || self.name.last().unwrap() == &"!" {
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
                .collect::<TypeGenerics>();

        return Some(Type {
            type_generics,
            type_args,
            ..self
        })

    }
    pub fn is_generic_with<'a>(&self, generics: &'a TypeGenerics) -> Option<&'a Vec<Constraint>> {
        generics.iter().find_map(|gen| {
            if vec![gen.name.clone()] == self.name {
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
    pub fn contains(&self, name: &Path) -> bool {
        self.name.eq(name)
        || self.type_args.iter().any(|arg| arg.contains(name))
    }
    pub fn contains_lt<S>(&self, name: &S) -> bool
        where StringWrapper: PartialEq<S> {
        self.lt_args.iter().any(|lt| lt.name().map_or(false, |lt_name| lt_name == name))
        || self.type_args.iter().any(|arg| arg.contains_lt(name))
    }
    pub fn diff<'a>(&'a self, other: &'a Type) -> Box<dyn Iterator<Item = (&Path, &Type)> + 'a> {
        Box::new(self.diff_sym(other).map(|(a,b)| (&a.name, b)))
    }
    pub fn diff_sym<'a>(&'a self, other: &'a Type) -> Box<dyn Iterator<Item = (&Type, &Type)> + 'a> {
        if self.name != other.name /*&& 
            !(self.name == "#RefMut" && other.name == "#Ref")*/ {
            return Box::new(iter::once((self, other)));
        } else {
            Box::new(
                self.type_args.iter()
                    .zip(other.type_args.iter())
                    .flat_map(|(a,b)| a.diff_sym(b))
            )
        }
    }
    pub fn replace(&self, name: &Path, other: &Type) -> Type {
        if &self.name == name {
            return other.clone()
        }
        Type {
            type_generics:
                self.type_generics.iter().cloned()
                .filter(|gen| &vec![gen.name.clone()] == name).collect(),
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
    pub fn has_lts(&self) -> bool {
        self.sub_tys().any(|ty| ty.lt_args.len() > 0)
    }
    pub fn has_named_lts(&self) -> bool {
        self.sub_tys().any(|ty| ty.lt_args.iter().any(|lt| lt.name().is_some()))
    }
    pub fn needs_lt(&self) -> bool {
        self.lt_args.iter().any(|lt| lt == &Lifetime::Any) ||
        self.type_args.iter().any(|a| a.needs_lt())
    }
    pub fn assign_lts<F,E>(&self, f: &mut F) -> std::result::Result<Type, E> 
        where F: FnMut() -> std::result::Result<Lifetime, E> {
        Ok(Type {
            lt_args: self.lt_args.iter().map(|lt| {
                if lt == &Lifetime::Any || 
                    lt.name().map_or(false, |n| self.lt_generics.contains(n)) {
                    f()
                } else {
                    Ok(lt.clone())
                }
            }).collect::<std::result::Result<Vec<Lifetime>, E>>()?,
            type_args: self.type_args.iter().map(|t| t.assign_lts(f))
                .collect::<std::result::Result<Vec<_>, E>>()?,
            ..self.clone()
        })
    }
    pub fn assign_lts_vec(&self, u: &mut Unstructured, lts: &Vec<Lifetime>) -> Result<Type> {
        self.assign_lts(&mut || Ok(u.choose(lts)?.clone()))
    }
    pub fn is_sized_by(&self, name: &Path) -> bool {
        !INDIRECT_TYPES.contains(&name.as_slice()) &&
        (&self.name == name
         || self.type_args.iter().any(|arg| arg.is_sized_by(name)))
    }
    pub fn assign_fresh_lt(&self, ctx: &Context) -> (Option<Lifetime>, Type) {
        let mut cached_lt: Option<Lifetime> = None;
        let ty = self.assign_lts(&mut || {
            match &cached_lt {
                Some(lt) => Ok::<Lifetime, !>(lt.clone()),
                None => {
                    let lt = fresh_lt(ctx);
                    cached_lt = Some(lt.clone());
                    Ok(lt)
                }
            }
        }).map_or_else(|a| a, |a| a);
        (cached_lt, ty)
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
    pub fn is_private(&self) -> bool {
        self.sub_tys().any(|t| !t.is_visible)
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

const INDIRECT_TYPES: [&'static [StringWrapper]; 4] = [
    &[StringWrapper::Static("Rc")],
    &[StringWrapper::Static("Vec")],
    &[StringWrapper::Static("Cow")],
    &[StringWrapper::Static("Arc")]
];

pub fn push_scope(ctx: &mut Context) {
    ctx.scopes = Rc::new(RcList::Cons(RefCell::new(Default::default()), ctx.scopes.clone()));
}

#[allow(dead_code)]
pub fn pop_scope(ctx: &mut Context) {
    if let RcList::Cons(_, rest) = &*ctx.scopes {
        ctx.scopes = rest.clone()
    }
}

pub fn add_lifetime(ctx: &mut Context, lt: Lifetime) {
    ctx.scopes.top_mut().map(|mut scope| {
        scope.lifetimes.insert(lt, (Cell::new(true), RefCell::new(vec![])));
    });
}

pub fn end_lifetime(ctx: &Context, lt: &Lifetime) {
    for scope in ctx.scopes.iter() {
        if let Some(data) = scope.lifetimes.get(lt) {
            if ctx.branches.is_some() {
                save_lt(ctx, data, lt.clone());
            }
            let (is_valid, refs) = data;
            if is_valid.get() {
                is_valid.set(false);
                for rf in refs.borrow().iter() {
                    end_lifetime(ctx, rf);
                }
            }
            return;
        }
    }
    // panic!("Attemted to remove lifetime {:?}, but it couldn't be found", lt);
}

pub fn is_alive(ctx: &Context, lt: &Lifetime) -> bool {
    for scope in ctx.scopes.iter() {
        if let Some((is_valid, _)) = scope.lifetimes.get(lt) {
            return is_valid.get();
        }
    }
    return false;
}

pub fn constrain_lt(ctx: &Context, lt: &Lifetime, constraint: &Lifetime) {
    if lt != &Lifetime::Any {
        for scope in ctx.scopes.iter() {
            if let Some(data) = scope.lifetimes.get(constraint) {
                if ctx.branches.is_some() {
                    save_lt(ctx, data, lt.clone());
                }
                let refs = &data.1;
                refs.borrow_mut().push(lt.clone());
            }
        }
    }
}

pub fn reserve_lt(ctx: &Context, lt: Lifetime, reserve_ty: RefType) {
    if lt != Lifetime::Any {

        for scope in ctx.scopes.iter() {
            for (other_lt, (_, refs)) in scope.lifetimes.iter() {
                if refs.borrow().contains(&lt) {
                    reserve_lt(ctx, other_lt.clone(), reserve_ty);
                }
            }
        }
        ctx.reserved_lts.borrow_mut().last_mut().unwrap().insert(lt, reserve_ty);

    }
}

pub fn is_free(ctx: &Context, lt: &Lifetime, ref_ty: RefType) -> bool {
    if lt == &Lifetime::Any {
        true
    } else {
        use RefType::*;
        ctx.reserved_lts.borrow().iter().all(
            |hmap| hmap.get(lt).map_or(true, |other| ref_ty == Borrow && other == &Borrow)
        )
    }
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
pub fn add_var<'a>(ctx: &mut Context, name: Path, var: Variable) {
    add_lifetime(ctx, var.lifetime.clone());
    ctx.scopes.top_mut().map(|mut l| {
        let ty_name = var.ty.name.clone();
        let rc_var = Rc::new(var);
        l.by_ty_name.entry(ty_name.clone())
            .or_insert(Default::default())
            .vars.insert(name.clone(), rc_var.clone());
        let old_var_opt = l.vars.insert(name.clone(), rc_var);
        // If the variable is being shadowed we need to remove the 
        // old variable from by_ty_name
        if let Some(old_var) = old_var_opt {
            if ty_name != old_var.ty.name {
                l.by_ty_name.entry(old_var.ty.name.clone())
                    .or_insert(Default::default())
                    .vars.remove(&name);
            }
        }
        
    });
}

#[allow(dead_code)]
pub fn add_type<'a>(ctx: &mut Context, name: Path, ty: Kind) {
    ctx.scopes.top_mut().map(|mut l| l.types.insert(name, ty));
}

pub fn remove_type(ctx: &mut Context, name: Path) {
    ctx.scopes.top_mut().map(|mut l| l.types.remove(&name));
}

#[allow(dead_code)]
pub fn add_struct(ctx: &mut Context, name: Path, ty: Type, struc: Struct) {
    ctx.scopes.top_mut().map(|mut l| {
        let rc_ty = Rc::new(ty);
        for (name, field) in struc.iter() {
            l.by_ty_name.entry(field.ty.as_ref().name.clone())
                .or_insert(Default::default())
                .fields.push((rc_ty.clone(), field.ty.clone(), name.clone()))
        }
        l.by_ty_name.entry(rc_ty.name.clone())
            .or_insert(Default::default())
            .structs.insert(name.clone(), (rc_ty.clone(), struc.clone()));
        l.structs.insert(name, (rc_ty, struc));
    });
}

pub fn add_enum(ctx: &mut Context, name: Path, ty: Type, strucs: HashMap<StringWrapper, Struct>) {
    ctx.scopes.top_mut().map(|mut l| {
        let rc_ty = Rc::new(ty);
        for (variant_name, struc) in strucs.iter() {
            let mut variant_path = name.clone();
            variant_path.push(variant_name.clone());
            l.by_ty_name.entry(rc_ty.name.clone())
                .or_insert(Default::default())
                .structs.insert(variant_path, (rc_ty.clone(), struc.clone()));
        }
        l.enums.insert(name, (rc_ty, strucs));
    });
}

#[allow(dead_code)]
pub fn lookup_var<'a,'b>(ctx: &'b mut Context, name: &Path) -> Option<&'b Variable> {
    for scope in ctx.scopes.iter() {
        if let Some(var) = scope.vars.get(name) {
            return Some(var);
        }
    }
    return None;
}

#[allow(dead_code)]
pub fn lookup_type<'a,'b>(ctx: &'b mut Context, name: Path) -> Option<&'b Kind> {
    for scope in ctx.scopes.iter() {
        if let Some(kind) = scope.types.get(&name) {
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
) -> Result<(&'b Path, &'b Variable)>
where F: Fn(&Path, &Variable) -> bool {
    let scope = choose_consume(u, ctx.scopes.sized_iter())?;
    let vars: Vec<(&Path, &Rc<Variable>)> = scope.vars.iter().filter_map(|(n,var)| {
        if pred(n,var) {
            Some((n,var))
        } else {
            None
        }
    }).collect();
    if vars.is_empty() {
        Err(GenerationError::AppropriateTypeFailure(Backtrace::capture()))
    } else {
        let &(name, var) = u.choose(&vars)?;
        Ok((name, var))
    }
}

#[allow(dead_code)]
pub fn pick_var<'a, 'b>(
    ctx: &'b mut Context,
    u: &'b mut Unstructured<'a>,
) -> Result<(&'b Path, &'b Variable)> {
    pick_var_that(ctx, u, |_,_| true)
}


#[allow(dead_code)]
pub fn pick_type<'a>(ctx: &Context, u: &mut Unstructured<'a>) -> Result<Type> {
    Ok(pick_type_that(ctx, u, |_,_| true)?)
}

pub fn pick_type_that<'a, F>(ctx: &Context, u: &mut Unstructured<'a>, pred: F)
     -> Result<Type> 
where F: Fn(&Path, &Kind) -> bool + Clone {
    let mut seen = HashSet::new();
    let mut kinds: Vec<(&Path, &Kind)> = vec![];
    for scope in ctx.scopes.iter() {
        for (name, kind) in scope.types.iter() {
            if !seen.contains(name) {
                seen.insert(name);
                if pred(name, kind) {
                    kinds.push((name, kind));
                }
            }
        }
    }
    let &(name, kind) = u.choose(&kinds)?;
    let n_of_args = kind.types;
    // TODO: pick functions
    // TODO: handle not picking ! more elegantly 
    let mut lt_args = vec![];
    for _ in 0..kind.lifetimes {
        lt_args.push(Lifetime::Any);
    }
    Ok(Type { 
        name: if name == &vec!["!"] {
            vec!["#Unit".into()]
        } else {
            name.clone()
        },
        type_args: {
            let mut type_args: Vec<Type> = vec![];
            for _ in 0..n_of_args {
                type_args.push(pick_type_that(ctx, u, pred.clone())?);
            }
            type_args
        },
        type_generics: vec![],
        lt_generics: vec![],
        lt_args,
        func: None,
        is_visible: kind.is_visible
    })
}

pub fn pick_type_impl<'a>(ctx: &'a Context, u: &mut Unstructured, desc: &TraitDescription) -> Result<(&'a TraitArgs, &'a Type)> {
    for scope in ctx.scopes.iter() {
        for (name, trai) in scope.traits.iter() {
            if name == &desc.name {
                let (args, ty) = u.choose(&trai.implementors)?;
                return Ok((args, ty.as_ref()));
            }
        }
    }
    panic!("Couldn't find trait {}", path_to_string(&desc.name));
}

pub fn pick_type_impls(ctx: &Context, u: &mut Unstructured, constraints: &Vec<Constraint>) -> Result<Type> {
    Ok(pick_type_impls_interconnected(ctx, u, constraints, &vec![])?.0)
}

pub fn pick_type_impls_interconnected<'a>(
        ctx: &'a Context, u: &mut Unstructured,
        constraints: &'a Vec<Constraint>,
        determined_by_others: &TypeGenerics
    ) -> Result<(Type, Vec<(Path, &'a Type)>)> {
    
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
                            if determined_by_others.iter().any(|gen| vec![gen.name.clone()] == cons_arg.name) {
                                // a diff that is generic in the rhs
                                others.push((cons_arg.name.clone(), type_arg));
                            } else if !args.type_generics.iter().any(|gen| vec![gen.name.clone()] == type_arg.name) {
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
        panic!("Couldn't find trait {}", path_to_string(&constraint.trait_name));
    }
    // println!("Implementors map: {:?}", implementors_map.keys().collect::<Vec<_>>());
    type InterIter<'a> = Box<dyn Iterator<Item=(Path, &'a Type)> + 'a>;
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
                        Some((acc_ty.merge(v_ty.as_ref().clone())?,
                             Box::new(acc_iter.into_iter().chain(v_iter))))
                    }
                )
            })
        }).collect::<Vec<_>>().into_iter()
    )?;
    // TODO: handle the implementor having an arg determined by others
    let ty_args = ty.type_generics.clone().into_iter()
        .filter(|gen| ty.contains(&vec![gen.name.clone()]))
        .map(|gen| Ok((gen.name.clone(), pick_type_impls(ctx, u, &gen.constraints)?)))
        .collect::<Result<Vec<_>>>()?;
    // TODO: replace generics in inter too
    let ty_with_args = ty_args.into_iter().fold(ty, |acc, ty_arg| {
        let (gen_name, arg) = ty_arg;
        let ty = acc;
        ty.replace(&vec![gen_name], &arg)
    });
    return Ok((ty_with_args, inter.collect()))
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarHandling {
    Borrow(Lifetime),
    MutBorrow(Lifetime),
    Own
}
impl VarHandling {
    #[allow(dead_code)]
    fn is_borrow(&self) -> bool {
        if let Borrow(_) = self {
            true
        } else {
            false
        }
    }
    fn is_mut_borrow(&self) -> bool {
        if let MutBorrow(_) = self {
            true
        } else {
            false
        }
    }
    fn get_lifetime(&self) -> Option<&Lifetime> {
        match self {
            Borrow(lt) => Some(lt),
            MutBorrow(lt) => Some(lt),
            Own => None
        }
    }
    fn map<F: Fn(&Lifetime) -> Lifetime>(&self, f: F) -> Self {
        match self {
            Borrow(lt) => Borrow(f(lt)),
            MutBorrow(lt) => MutBorrow(f(lt)),
            Own => Own
        }
    }
}
use VarHandling::*;
impl From<&VarHandling> for RefType {
    fn from(r: &VarHandling) -> Self {
        match r {
            VarHandling::Borrow(_) => RefType::Borrow,
            _ => RefType::MoveOrMut
        }
    }
}

pub fn construct_value<'a>(
    ctx: &Context,
    u: &mut Unstructured<'a>,
    ty: Type,
    allow_ambiguity: bool
) -> Result<Expr> {
    ctx.size.set(0);
    ctx.reserved_lts.replace(vec![]);
    let result = construct_value_inner(ctx, u, ty, allow_ambiguity, Own);
    let tmps = ctx.temporaries.replace(vec![]);
    tmps.into_iter().for_each(|lt| end_lifetime(ctx, &lt));
    ctx.kill_temps.set(false);
    return result;
}

const MAX_SIZE: usize = 30;

pub fn construct_value_inner<'a>(
    ctx: &Context,
    u: &mut Unstructured<'a>,
    ty: Type,
    allow_ambiguity: bool,
    var_handling: VarHandling
) -> Result<Expr> {

    // println!("construct_value called on type {}", ty);

    // NOTE: there is a bias towards expressions further to the left being bigger.
    // Recursive calls could be made in a random order to adress this.
    let old_size = ctx.size.get();
    // println!("{}", old_size);
    ctx.size.set(old_size + 1);

    fn handle_field_generics(generics: Vec<(StringWrapper, &Type)>, fields: &HashMap<StringWrapper, Field>) -> HashMap<StringWrapper, Type> {
        // println!("handle_field_generics called on {:?}, {:?}", generics, fields);
        let mut new_map = HashMap::new();
        'outer: for (name, field) in fields.iter() {
            for (ty_name, ty) in generics.iter() {
                if vec![ty_name.clone()] == field.ty.name {
                    new_map.insert(name.to_owned(), (*ty).clone() );
                    continue 'outer;
                }
            }
            new_map.insert(name.clone(), field.ty.as_ref().clone());
        }
        return new_map;           
    }

    ctx.reserved_lts.borrow_mut().push(HashMap::new());

    fn handle_arg_generics(
        ty_generics: Vec<(StringWrapper, &Type)>,
        lt_generics: Vec<(StringWrapper, &Lifetime)>,
        args: &Vec<Type>
    ) -> Vec<Type> {
        // println!("handle_arg_generics called on {:?}, {:?}\n", generics, args);
        let mut new_args = vec![];
        'vec_args: for arg in args {
            for (ty_name, ty) in ty_generics.iter() {
                let ty_path = vec![ty_name.clone()];
                if arg.contains(&ty_path) {
                    new_args.push(arg.replace(&ty_path, &ty).replace_lts(&lt_generics));
                    continue 'vec_args;
                }
            }
            new_args.push(arg.replace_lts(&lt_generics));
        }
        return new_args;
    }

    fn handle_single_generic(generics: Vec<(Path, &Type)>, single: &Type) -> Type {
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
        type_generics: &TypeGenerics, type_args: &Vec<Type>,
    ) -> Result<(Option<Vec<usize>>, Vec<Type>)> {
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
                .collect::<HashMap<&Path, &Type>>();

        // println!("make_generics called with: {:?} {:?} {:?} {:?} {:?}\n",
        //    ty1, ty2, args, type_generics, type_args);
        // println!("diff is {:?}\n", output_diff);

        // TODO: handle constraints
        for (i, gen) in type_generics.iter().enumerate() {
            let gen_path = vec![gen.name.clone()];
            if ty2.contains(&gen_path) {
                result.push(
                    type_args.get(i)
                        .or_else(|| output_diff.remove(&gen_path))
                        .map_or_else(
                            || name_to_type(gen.name.clone()),
                            |ty| {
                                generics_required |= !allow_ambiguity;
                                ty.clone()
                            }
                                    
                        )
                );
            } else if args.iter().any(|arg| arg.contains(&gen_path)) {
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
    ) -> Result<Option<Vec<usize>>> {
    //! Similar to make_fn_generics, but much less complicated because the output type
    //! always has the same generics as the struct unlike functions
    
        let mut determined_args = vec![];

        if allow_ambiguity {
            return Ok(Some(vec![]));
        }

        // TODO: handle constraints
        'outer: for gen in ty.type_generics.iter() {
            for (i, field) in fields.iter().enumerate() {
                if field.contains(&vec![gen.name.clone()]) {
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
        full_generics: &'a TypeGenerics,
        so_far: &Vec<(&Path, &Type)>
    ) -> Result<Vec<(Path, Either<Type, &'a Type>)>> {
        let mut result = vec![];
        let so_far_names: Vec<&&Path> = so_far.iter().map(|(name,_)| name).collect();
        let free_generics: Vec<&Generic> = full_generics.iter()
            .filter(|gen| !so_far_names.contains(&&&vec![gen.name.clone()]))
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
            result.push((vec![gen.name.clone()], Left(gen_ty)));
            result.extend(
                interconnected.into_iter()
                    .map(|(name,ty)| (name, Right(ty)))
            );
        }
        return Ok(result);
    }

    enum PossibleExpression<'a> {
        Struct(&'a Path, &'a Rc<Type>, &'a Struct),
        Var(&'a Path, &'a Rc<Variable>),
        Fn(&'a Path, &'a Type, &'a Func),
        Method(&'a Method, &'a Either<Rc<Type>, TraitDescription>),
        Field(&'a Type, &'a Type, &'a StringWrapper),
        Macro(&'a Path, &'a Macro),
        Op(&'static str, &'a Operator),
        AdditionalArg
    }
    let mut possible_exprs: Vec<PossibleExpression> = vec![];

    let mut seen_vars = HashSet::new();
    let (invalid_from, invalid_to) =
        ctx.not_in_use_scopes.as_ref().map_or((None,None), |(a,b)| (Some(a), Some(b)));
    let mut scope_is_valid = false;
    for scope in ctx.scopes.iter() {
        if scope_is_valid && Some(scope.id) ==
                invalid_from.as_ref().and_then(|i| i.top().map(|s| s.id)) {
            scope_is_valid = false;
        }
        if !scope_is_valid && Some(scope.id) ==
                invalid_to.as_ref().and_then(|i| i.top().map(|s| s.id)) {
            scope_is_valid = true;
        }


        let by_ty_name = &scope.by_ty_name;

        let matches_ty_iter = 
            by_ty_name.get(&ty.name).map(|cached| {
                (&cached.structs, &cached.vars, &cached.methods, &cached.fields, &cached.macros)
            });
        let matches_never_iter = 
            by_ty_name.get(&vec!["!".into()]).map(|cached| {
                (&cached.structs, &cached.vars, &cached.methods, &cached.fields, &cached.macros)
            });

        let matches_generic_iter = 
            by_ty_name.get(&vec!["#Generic".into()]).map(|cached| {
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

        for (path, (struc_ty, struc)) in structs {
            // NOTE: structs are always allowed, because otherwise some 
            // things might not be constructable
            if struc_ty.matches(&ty) {
                possible_exprs.push(PossibleExpression::Struct(path, struc_ty, struc));
            }
        }
        for (name, var) in vars {
            let isnt_shadowed = !seen_vars.contains(name);
            seen_vars.insert(name);
            let doesnt_break_mutability =
                !var_handling.is_mut_borrow() ||
                var.mutability == Mutability::Mutable;
            let isnt_reserved = is_free(ctx, &var.lifetime, (&var_handling).into());
            if doesnt_break_mutability &&
                isnt_shadowed &&
                isnt_reserved &&
                var.lifetime.is_valid(ctx) &&
                scope_is_valid &&
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
        if ctx.can_demand_additional_args && ty.has_named_lts() {
            possible_exprs.push(PossibleExpression::AdditionalArg)
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
    let result = match choice {
        PossibleExpression::Struct(path, struc_ty, struc) => {
            if ctx.kill_temps.get() {
                if let Some(lt) = var_handling.get_lifetime() {
                    ctx.temporaries.borrow_mut().push(lt.clone());
                }
            }
            ctx.kill_temps.set(true);
            let determined_fields = make_struct_generics(
                u, 
                allow_ambiguity,
                &struc_ty,
                struc.iter().map(|(_,f)| f.ty.as_ref()).collect(),
            )?;
            let self_str = "Self".into();
            let generics =
                iter::once((self_str, struc_ty.as_ref()))
                    .chain(struc_ty.type_generics.iter().map(|t| t.name.clone())
                        .zip(ty.type_args.iter()))
                    .collect();
            match struc.as_ref() {
                Fields::Named(fields) => {
                    let mut args = vec![];
                    for (i, (name, arg)) in handle_field_generics(generics, &fields).into_iter().enumerate() {

                        let arg_ambigous = determined_fields.as_ref().map_or(true,
                            |determined| !determined.contains(&i)
                        );
                        args.push((name, construct_value_inner(ctx, u, arg, arg_ambigous, Own)?));
                    }
                    Ok(Expr::Struct(
                        path.clone(),
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
                        let arg_ambigous = if path.first().unwrap().starts_with("#Tuple") {
                            allow_ambiguity
                        } else {
                            determined_fields.as_ref().map_or(true,
                                |determined| !determined.contains(&i)
                            )
                        };
                        args.push(construct_value_inner(ctx, u, arg, arg_ambigous, Own)?);
                    }
                    Ok(Expr::Fn(
                        path.clone(),
                        if determined_fields.is_none() {
                            ty.type_args.clone()
                        } else {
                            vec![]
                        },
                        args
                    ))
                }
                Fields::None => Ok(Expr::Lit(path.clone(), ty.type_args.clone()))
            }
        }
        PossibleExpression::Var(name, var) => {
            reserve_lt(ctx, var.lifetime.clone(), (&var_handling).into());
            match var_handling {
                Own => var.mve(ctx),
                Borrow(lt) => {
                    save_var(ctx, var, name.clone());
                    var.borrow(ctx, lt)
                }
                MutBorrow(lt) => {
                    save_var(ctx, var, name.clone());
                    var.mut_borrow(ctx, lt)
                }
            }
            Ok(Expr::Var(name.to_owned()))
        }
        PossibleExpression::Fn(name, fn_ty, func) => {
            if ctx.kill_temps.get() {
                if let Some(lt) = var_handling.get_lifetime() {
                    ctx.temporaries.borrow_mut().push(lt.clone());
                }
            }
            ctx.kill_temps.set(true);
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
            if ctx.kill_temps.get() {
                if let Some(lt) = var_handling.get_lifetime() {
                    ctx.temporaries.borrow_mut().push(lt.clone());
                }
            }
            ctx.kill_temps.set(true);
            let required_args_map: HashMap<_,_> = field_ty.diff(&ty).collect(); 
            let mut type_args = vec![];
            for gen in struc_ty.type_generics.iter() {
                if let Some(ty) = required_args_map.get(&vec![gen.name.clone()]) {
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
            if ctx.kill_temps.get() {
                if let Some(lt) = var_handling.get_lifetime() {
                    ctx.temporaries.borrow_mut().push(lt.clone());
                }
            }
            ctx.kill_temps.set(true);
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
                            if arg.contains(&vec![gen.clone()]) {
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
            if let Some(ownership) = &method.self_param {
                let ownership_no_any = ownership.map(|lt| match lt {
                    Lifetime::Any => fresh_lt(ctx),
                    _ => lt.clone()
                });
                Ok(Expr::Method(
                    Box::new(construct_value_inner(ctx, u, Type {
                        type_args: disc_ty_args,
                        ..assoc_ty.clone()
                    }, false, ownership_no_any.clone())?),
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
            if !name.starts_with("&") {
                if ctx.kill_temps.get() {
                    if let Some(lt) = var_handling.get_lifetime() {
                        ctx.temporaries.borrow_mut().push(lt.clone());
                    }
                }
                ctx.kill_temps.set(true);
            }
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
            let generics: Vec<(Path, &Type)> =
                diff.into_iter().map(|(p,ty)| (p.clone(), ty))
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
                        "&" => Borrow(fresh_lt(ctx)),
                        "&mut" => MutBorrow(fresh_lt(ctx)),
                        _ => Own
                    };
                    let val = construct_value_inner(
                        ctx, u,
                        handle_single_generic(generics, operand),
                        allow_ambiguity,
                        ownership.clone()
                    )?;
                    if let Some(lt) = match ownership {
                        Borrow(lt) => Some(lt),
                        MutBorrow(lt) => Some(lt),
                        _ => None
                    } {
                        constrain_lt(ctx, &lt, &ty.lt_args[0]);
                        constrain_lt(ctx, &ty.lt_args[0], &lt);
                    }
                    Ok(Expr::UnOp(name, Box::new(val)))
                }
            }
        },
        PossibleExpression::AdditionalArg => Ok(Expr::AdditionalArg(
            ty.clone(),
            if var_handling.is_mut_borrow() || Arbitrary::arbitrary(u)? {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            }
        ))
    };
    ctx.reserved_lts.borrow_mut().pop();
    return result;
}

pub fn kind_to_type<S: Into<StringWrapper>>(name: S, kind: &Kind) -> Type {
    Type {
        name: vec![name.into()],
        lt_generics: GEN_STRING[0..(kind.lifetimes as usize)].iter()
                            .map(|s| (*s).into()).collect(),
        lt_args: GEN_STRING[0..(kind.lifetimes as usize)].iter()
                            .map(|s| Lifetime::Named((*s).into())).collect(),
        type_generics: GEN_STRING[0..(kind.types as usize)].iter()
                            .map(|s| Generic {
                                    name: (*s).into(), 
                                    is_arg_for_other: false,
                                    constraints: vec![]
                                }).collect(),
        type_args: vec![],
        func: None,
        is_visible: kind.is_visible
    }
}

pub fn name_to_type<S: Into<StringWrapper>>(name: S) -> Type {
    Type {
        name: vec![name.into()],
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args: vec![],
        func: None,
        is_visible: false
    }
}

fn report_unconstructable_variable(ty: Type) -> Expr {
    println!("Couldn't find value of type {}", ty);
    Expr::Macro(vec!["compile_error".into()], MacroBody {
        brackets: BracketType::Round,
        seperator: Seperator::Comma,
        tokens: vec![Token::Expr(Expr::ExactString(format!("Couldn't find value of type {}", ty)))]
    })
}
