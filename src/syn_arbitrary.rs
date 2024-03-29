// vim: set foldmethod=marker:
use arbitrary::Arbitrary;
use proc_macro2::TokenStream;
use std::{collections::{HashMap, HashSet}, iter, rc::Rc, backtrace::Backtrace};
use syn::{Abi, AngleBracketedGenericArguments, Arm, AttrStyle, Attribute, BareFnArg, BinOp, Binding, Block, ConstParam, Constraint, Expr, ExprArray, ExprAssign, ExprAssignOp, ExprAsync, ExprAwait, ExprBinary, ExprBlock, ExprBox, ExprBreak, ExprCall, ExprCast, ExprClosure, ExprContinue, ExprField, ExprForLoop, ExprGroup, ExprIf, ExprIndex, ExprLet, ExprLit, ExprLoop, ExprMacro, ExprMatch, ExprMethodCall, ExprParen, ExprPath, ExprRange, ExprReference, ExprRepeat, ExprReturn, ExprStruct, ExprTry, ExprTryBlock, ExprTuple, ExprType, ExprUnary, ExprWhile, ExprYield, Field, FieldPat, FieldValue, Fields, FieldsNamed, FieldsUnnamed, File, FnArg, GenericArgument, GenericMethodArgument, GenericParam, Generics, Ident, Index, Item, ItemConst, ItemEnum, ItemFn, ItemStatic, ItemStruct, ItemTrait, ItemType, Label, Lifetime, LifetimeDef, Lit, LitStr, Local, MacroDelimiter, Member, MethodTurbofish, Pat, PatBox, PatIdent, PatLit, PatOr, PatPath, PatRange, PatReference, PatRest, PatSlice, PatStruct, PatTuple, PatTupleStruct, PatType, PatWild, PathArguments, PathSegment, RangeLimits, Receiver, ReturnType, Signature, Stmt, Token, TraitBound, TraitBoundModifier, TraitItem, TraitItemConst, TraitItemMethod, TraitItemType, Type, TypeArray, TypeBareFn, TypeGroup, TypeImplTrait, TypeInfer, TypeNever, TypeParam, TypeParamBound, TypeParen, TypePath, TypePtr, TypeReference, TypeSlice, TypeTraitObject, TypeTuple, UnOp, Variadic, Variant, VisPublic, VisRestricted, Visibility, parse_quote, parse_str, punctuated::Punctuated, token::{Brace, Bracket, Group, Paren}};
use quote::quote;
use lazy_static::lazy_static;
use std::mem;

use crate::{choose::{self, choose_consume}, string_wrapper::*};

use crate::context_arbitrary::*;
use crate::context_arbitrary as context_arbitrary;
use crate::context_arbitrary::Result;

use crate::context::*;

use crate::semantics as sem;
use crate::semantics::*;

use crate::make_type;
use crate::branches;


const MAX_DEPTH: usize = 20;

pub struct WrappedFile(pub File);
unsafe impl Send for WrappedFile {}
#[allow(dead_code)]
pub fn make_wrapped_file<'a>(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<WrappedFile> {
    Ok(WrappedFile(c_arbitrary(ctx, u)?))
}

/// To help with type inference
fn ok<T>(a: T) -> context_arbitrary::Result<T> {
    Ok(a)
}

fn dummy_span() -> proc_macro2::Span {
    let dummy_token: Token![=] = parse_quote!(=);
    return dummy_token.span;
}

macro_rules! lit_of_type {
    ($u:expr, $ty:ty) => ({
        let l: $ty = Arbitrary::arbitrary($u)?;
        parse_quote!(#l)
    })
}

macro_rules! parens_ex {
    ($p:expr, $ctx: ident, $e: expr)
    => (Box::new(parenthesize_expr($p, rhs!($ctx, $e))))
}

// doesn't need to be a macro atm, but keeping it for consistency with parens_ex
macro_rules! parens_ty {
    ($ctx: ident, $e: expr) => (Box::new(parenthesize_type($e)))
}

macro_rules! parens_pat {
    ($ctx: ident, $e: expr) => (Box::new(parenthesize_pat($e)))
}

macro_rules! parens_block {
    ($ctx: ident, $e: expr) => (Box::new(parenthesize_block($e)))
}

fn parenthesize_expr(presc: u8, e: Expr) -> Expr {
    let expr_presc = prescedence(&e);
    if expr_presc < presc {
        return Expr::Paren(ExprParen {
            attrs: vec![],
            paren_token: Paren { span: dummy_span() },
            expr: Box::new(e),
        });
    } else {
        return e;
    }
}

fn prescedence(e: &Expr) -> u8 {
    // https://doc.rust-lang.org/stable/reference/expressions.html#expression-precedence
    // 18: Paths
    // 17: Method calls
    // 16: Field expressions
    // 15: Function calls, array indexing
    // 14: ?
    // 13: Unary - * ! & &mut
    // 12: as
    // 11: * / %
    // 10: + -
    // 9: << >>
    // 8: &
    // 7: ^
    // 6: |
    // 5: == != < > <= >=
    // 4: &&
    // 3: ||
    // 2: .. ..=
    // 1: = += -= *= /= %= &= |= ^= <<= >>=
    // 0: return, break, closures
    match e {
        Expr::Path(_) => 18,
        Expr::MethodCall(_) => 17,
        Expr::Field(_) => 16,
        Expr::Call(_) | Expr::Index(_) => 15,
        // await needs to be lower than call to not be parsed as a method
        Expr::Try(_) | Expr::Await(_) => 14,
        Expr::Unary(_) => 13,
        Expr::Cast(_) => 12,
        Expr::Binary(op) => binary_prescedence(&op.op),
        Expr::Range(_) => 2,
        Expr::Assign(_) | Expr::AssignOp(_) => 1,
        Expr::Break(_) | Expr::Return(_) => 0,
        // TODO: find out what prescedence box should ACTUALLY be
        Expr::Box(_) => 0,
        // Expr::Loop(l) if l.label.is_some() => 0,
        Expr::Group(g) => prescedence(&g.expr),
        // TODO: things with blocks might only need to be parenthesized if they're the lhs 
        Expr::Block(_) | Expr::While(_) | Expr::ForLoop(_) | Expr::Closure(_) | 
            Expr::Match(_) | Expr::Loop(_) | Expr::If(_) | Expr::TryBlock(_) => 0,

        _ => u8::MAX
    }
}

fn binary_prescedence(op: &BinOp) -> u8 {
    use BinOp::*;
    match op {
        Mul(_) | Div(_) | Rem(_) => 11,
        Add(_) | Sub(_) => 10,
        Shl(_) | Shr(_) => 9,
        BitAnd(_) => 8,
        BitXor(_) => 7,
        BitOr(_) => 6,
        Lt(_) | Le(_) | Gt(_) | Ge(_) | Ne(_) | Eq(_) => 5,
        And(_) => 4,
        Or(_) => 3,
        AddEq(_) | SubEq(_) | MulEq(_) | DivEq(_) | RemEq(_) |
        BitXorEq(_) | BitAndEq(_) | BitOrEq(_) | ShlEq(_) | ShrEq(_) => 1
    }
}

fn binary_associativity(op: &BinOp) -> (u8, u8) {
    //! The point of the return type is that you can add it to the 
    //! left and right hand side in order to require things to be 
    //! parenthesized based on their associativity.
    //!
    //! (1,0) means right associative, (0,1) left associative
    //! and (1,1) means no associativity (which == and ranges have)
    
    use BinOp::*;
    match op {
        // No associativity
        Lt(_) | Le(_) | Gt(_) | Ge(_) | Ne(_) | Eq(_) => (1,1),
        // Right associativity
        AddEq(_) | SubEq(_) | MulEq(_) | DivEq(_) | RemEq(_) |
        BitXorEq(_) | BitAndEq(_) | BitOrEq(_) | ShlEq(_) | ShrEq(_) => (1,0),
        _ => (0,1)
    }

}

fn parenthesize_block(e: Expr) -> Expr {
    match e { //{{{
        Expr::Assign(ass) => {
            Expr::Assign(ExprAssign {
                attrs: ass.attrs,
                left: Box::new(parenthesize_block(*ass.left)),
                eq_token: ass.eq_token,
                right: Box::new(parenthesize_block(*ass.right))
            })
        }
        Expr::AssignOp(ass) => {
            Expr::AssignOp(ExprAssignOp {
                attrs: ass.attrs,
                left: Box::new(parenthesize_block(*ass.left)),
                op: ass.op,
                right: Box::new(parenthesize_block(*ass.right))
            })
        }
        Expr::Await(aw) => {
            Expr::Await(ExprAwait {
                attrs: aw.attrs,
                base: Box::new(parenthesize_block(*aw.base)),
                dot_token: aw.dot_token,
                await_token: aw.await_token
            })
        }
        Expr::Binary(bin) => {
            Expr::Binary(ExprBinary {
                attrs: bin.attrs,
                left: Box::new(parenthesize_block(*bin.left)),
                op: bin.op,
                right: Box::new(parenthesize_block(*bin.right))
            })
        }
        Expr::Box(b) => {
            Expr::Box(ExprBox {
                attrs: b.attrs,
                box_token: b.box_token,
                expr: Box::new(parenthesize_block(*b.expr))
            })
        }
        Expr::Break(b) => {
            Expr::Break(ExprBreak {
                attrs: b.attrs,
                expr: b.expr.map(|expr| Box::new(parenthesize_block(*expr))),
                break_token: b.break_token,
                label: b.label
            })
        }
        Expr::Call(c) => {
            Expr::Call(ExprCall {
                attrs: c.attrs,
                func: Box::new(parenthesize_block(*c.func)),
                paren_token: Paren { span: dummy_span() },
                args: c.args
            })
        }
        Expr::Cast(c) => {
            Expr::Cast(ExprCast {
                attrs: c.attrs,
                expr: Box::new(parenthesize_block(*c.expr)),
                as_token: c.as_token,
                ty: c.ty
            })
        }
        Expr::Continue(c) => Expr::Continue(c),
        Expr::Field(f) => {
            Expr::Field(ExprField {
                attrs: f.attrs,
                base: Box::new(parenthesize_block(*f.base)),
                dot_token: f.dot_token,
                member: f.member
            })
        }
        Expr::Group(g) => {
            Expr::Group(ExprGroup {
                attrs: g.attrs,
                expr: Box::new(parenthesize_block(*g.expr)),
                group_token: g.group_token
            })
        }
        Expr::Index(i) => {
            Expr::Index(ExprIndex {
                attrs: i.attrs,
                expr: Box::new(parenthesize_block(*i.expr)),
                bracket_token: i.bracket_token,
                index: i.index
            })
        }
        Expr::Let(l) => {
            Expr::Let(ExprLet {
                attrs: l.attrs,
                expr: Box::new(parenthesize_block(*l.expr)),
                let_token: l.let_token,
                pat: l.pat,
                eq_token: l.eq_token
            })
        }
        Expr::MethodCall(m) => {
            Expr::MethodCall(ExprMethodCall {
                attrs: m.attrs,
                paren_token: m.paren_token,
                receiver: Box::new(parenthesize_block(*m.receiver)),
                dot_token: m.dot_token,
                method: m.method,
                turbofish: m.turbofish,
                args: m.args
            })
        }
        Expr::Range(r) => {
            Expr::Range(ExprRange {
                attrs: r.attrs,
                from: r.from.map(|from| Box::new(parenthesize_block(*from))),
                limits: r.limits,
                to: r.to.map(|to| Box::new(parenthesize_block(*to))),
            })
        }
        Expr::Reference(r) => {
            Expr::Reference(ExprReference {
                attrs: r.attrs,
                expr: Box::new(parenthesize_block(*r.expr)),
                and_token: r.and_token,
                raw: r.raw,
                mutability: r.mutability
            })
        }
        Expr::Try(t) => {
            Expr::Try(ExprTry {
                attrs: t.attrs,
                expr: Box::new(parenthesize_block(*t.expr)),
                question_token: t.question_token
            })
        }
        Expr::Type(t) => {
            Expr::Type(ExprType {
                attrs: t.attrs,
                expr: Box::new(parenthesize_block(*t.expr)),
                colon_token: t.colon_token,
                ty: t.ty
            })
        }
        Expr::Unary(u) => {
            Expr::Unary(ExprUnary {
                attrs: u.attrs,
                expr: Box::new(parenthesize_block(*u.expr)),
                op: u.op
            })
        }
        // Yield is not in the language yet
        // Expr::Yield(_) => {}
        Expr::Async(_) | Expr::Block(_) | Expr::Closure(_) | Expr::ForLoop(_) |
        Expr::If(_) | Expr::Loop(_) | Expr::Match(_) | Expr::Return(_) |
        Expr::Unsafe(_) | Expr::Struct(_) | Expr::TryBlock(_) | Expr::While(_)
            => in_parens(e),
        _ => e
    } //}}}
}

fn in_parens(e: Expr) -> Expr {
    Expr::Paren(ExprParen {
        attrs: vec![],
        paren_token: Paren { span: dummy_span() },
        expr: Box::new(e)
    })
}

// TODO: is this actually a bug?
fn parenthesize_label(e: Expr) -> Expr {
    match e {
        Expr::Block(b) if b.label.is_some() => in_parens(Expr::Block(b)),
        Expr::ForLoop(b) if b.label.is_some() => in_parens(Expr::ForLoop(b)),
        Expr::Loop(b) if b.label.is_some() => in_parens(Expr::Loop(b)),
        Expr::While(b) if b.label.is_some() => in_parens(Expr::While(b)),
        _ => e
    }
}

fn parenthesize_type(ty: Type) -> Type {
    match ty {
        Type::Array(_) => ty,
        Type::Infer(_) => ty,
        Type::Never(_) => ty,
        Type::Paren(_) => ty,
        Type::Path(_) => ty,
        Type::Slice(_) => ty,
        Type::Tuple(_) => ty,
        _ => Type::Paren(TypeParen {
            paren_token: Paren { span: dummy_span() },
            elem: Box::new(ty),
        }),
    }
}

fn parenthesize_pat(pat: Pat) -> Pat {
    match pat {
        Pat::Ident(_) => pat,
        Pat::Lit(_) => pat,
        Pat::Path(_) => pat,
        Pat::Rest(_) => pat,
        Pat::Slice(_) => pat,
        Pat::Struct(_) => pat,
        Pat::Tuple(_) => pat,
        Pat::TupleStruct(_) => pat,
        Pat::Type(_) => pat,
        Pat::Wild(_) => pat,
        // There isn't a paren type for patterns, so we need to use Verbatim
        _ => Pat::Verbatim(quote!((#pat))),
    }
}

impl From<sem::Type> for syn::Type {
    fn from(ty: sem::Type) -> Self {
        if ty.name.last().unwrap().starts_with("#Tuple") {
            syn::Type::Tuple(TypeTuple {
                paren_token: Paren { span: dummy_span() },
                elems: ty.type_args.into_iter().map::<syn::Type,_>(From::from).collect()
            })
        } else if let Some(func) = ty.func {
            syn::Type::BareFn(TypeBareFn {
                lifetimes: None,
                unsafety: None,
                abi: None,
                fn_token: parse_quote!(fn),
                paren_token: Paren { span: dummy_span() },
                inputs: func.args.iter().map(|arg| {
                    BareFnArg {
                        attrs: vec![],
                        name: None,
                        // name: Some((parse_str(arg.name.to_str()), parse_quote!(:))),
                        ty: arg.clone().into()
                    }
                }).collect(),
                variadic: None,
                output: ReturnType::Type(parse_quote!(->), Box::new(func.ret_type.clone().into()))
            })
        } else if ty.name == vec![StringWrapper::from("#Unit")] {
            syn::Type::Tuple(TypeTuple {
                paren_token: Paren { span: dummy_span() },
                elems: iter::empty::<syn::Type>().collect()
            })
        } else if let Some(lit) = match path_to_string(&ty.name).as_str() {
            "u8" | "u16" | "u32" | "u64" | "u128" |
            "i8" | "i16" | "i32" | "i64" | "i128" |
            "usize" | "char" | "str" => Some(parse_str(path_to_string(&ty.name).as_str()).unwrap()),
            _ => None
        } {
            lit
        } else if ty.name == vec![StringWrapper::from("!")] {
            syn::Type::Never(TypeNever { bang_token: parse_quote!(!) })
        } else if ty.name.last().unwrap().starts_with("#Ref") {
            syn::Type::Reference(TypeReference {
                and_token: parse_quote!(&),
                lifetime: ty.lt_args.first()
                            .and_then(sem::Lifetime::name)
                            .filter(|lt| !ty.lt_generics.contains(lt))
                            .map(|lt| {
                                syn::Lifetime {
                                    apostrophe: dummy_span(),
                                    ident: name_to_ident_det(lt.as_str())
                                }
                            }),
                mutability: if ty.name == vec!["#RefMut"] {
                    parse_quote!(mut)
                } else {
                    None
                },
                elem: Box::new(ty.type_args[0].clone().into())
            })
        } else if ty.name.last().unwrap().starts_with("#") {
            panic!("Unhandled special type: {:?}", ty);
        } else {
            // println!("Type path: {:?}", ty);
            let ty_path = make_type_path(ty);
            syn::Type::Path(TypePath {
                qself: None,
                path: ty_path
            })
        }
    }
}

impl From<sem::Fields> for syn::Fields {
    fn from(fields: sem::Fields) -> Self {
        // TODO: it can sometimes be okay to give a struct the "wrong" kind of
        // fields if it doesn't have any fields, e.g. None can be written as None{}
        match fields {
            sem::Fields::Unnamed(unnamed) => syn::Fields::Unnamed(FieldsUnnamed {
                paren_token: Paren { span: dummy_span() },
                unnamed: unnamed.into_iter().map(|field| {
                    Field {
                        attrs: vec![],
                        vis: if field.visible {
                            Visibility::Public(VisPublic { pub_token: parse_quote!(pub) })
                        } else {
                            Visibility::Inherited
                        },
                        ident: None,
                        colon_token: parse_quote!(:),
                        ty: Rc::as_ref(&field.ty).clone().into()
                    }
                }).collect()
            }),
            sem::Fields::Named(named) => syn::Fields::Named(FieldsNamed {
                brace_token: Brace { span: dummy_span() },
                named: named.into_iter().map(|(name, field)| {
                    Field {
                        attrs: vec![],
                        vis: if field.visible {
                            Visibility::Public(VisPublic { pub_token: parse_quote!(pub) })
                        } else {
                            Visibility::Inherited
                        },
                        ident: Some(name_to_ident_det(name.as_str())),
                        colon_token: parse_quote!(:),
                        ty: Rc::as_ref(&field.ty).clone().into()
                    }
                }).collect(),
            }),
            sem::Fields::None => syn::Fields::Unit
        }
    }
}

fn from_sem_expr(ctx: &mut Context, u: &mut Unstructured, ex: &sem::Expr) -> Result<Expr> {
    Ok(match ex {
        sem::Expr::Lit(path, type_args) => {
            let name = path.last().expect("Empty path");
            match name.as_str() {
                "usize" => lit_of_type!(u, usize),
                "u8"    => lit_of_type!(u, u8),
                "u16"   => lit_of_type!(u, u16),
                "u32"   => lit_of_type!(u, u32),
                "u64"   => lit_of_type!(u, u64),
                "u128"  => lit_of_type!(u, u128),
                "i8"    => lit_of_type!(u, i8),
                "i16"   => lit_of_type!(u, i16),
                "i32"   => lit_of_type!(u, i32),
                "i64"   => lit_of_type!(u, i64),
                "i128"  => lit_of_type!(u, i128),
                "char"  => lit_of_type!(u, char),
                "str"   => lit_of_type!(u, &str),
                "bool"  => lit_of_type!(u, bool),
                "#Unit" => Expr::Tuple(ExprTuple {
                    attrs: vec![],
                    paren_token: Paren { span: dummy_span() },
                    elems: iter::empty::<Expr>().collect()
                }),
                s if s.starts_with("#") => panic!("Unhandled special type {}", s),
                _ => Expr::Path(ExprPath {
                    attrs: vec![],
                    qself: None,
                    path: make_turbofish_path(&path, type_args.clone())
                })
            }
        }
        sem::Expr::Struct(path, type_args, fields) => {
            Expr::Struct(ExprStruct {
                attrs: vec![],
                path: make_turbofish_path(&path, type_args.clone()),
                brace_token: Brace { span: dummy_span() },
                fields: fields.iter().map(|(name, val)| {
                    Ok(FieldValue {
                        attrs: vec![],
                        member: Member::Named(name_to_ident_det(name.as_str())),
                        colon_token: parse_quote!(:),
                        expr: from_sem_expr(ctx, u, val)?
                    })
                }).collect::<Result<Punctuated<FieldValue, Token![,]>>>()?,
                dot2_token: None,
                rest: None
            })
        }
        sem::Expr::Field(reciever, name) => {
            let member = name.as_str().parse::<u32>().map_or_else(
                |_| Member::Named(name_to_ident_det(name.as_str())),
                |n| Member::Unnamed(Index {
                        index: n,
                        span: dummy_span()
                    })
            );
            // println!("Field access!");
            syn::Expr::Field(ExprField {
                attrs: vec![],
                base: Box::new(parenthesize_expr(16, from_sem_expr(ctx, u, reciever)?)),
                dot_token: parse_quote!(.),
                member
            })
        }
        sem::Expr::Method(reciever, name, type_args, args) => {
            Expr::MethodCall(ExprMethodCall {
                attrs: vec![],
                receiver: Box::new(parenthesize_expr(17, from_sem_expr(ctx, u, &*reciever)?)),
                dot_token: parse_quote!(.),
                method: name_to_ident(u, name.as_str())?,
                turbofish: if type_args.len() == 0 && Arbitrary::arbitrary(u)? {
                    None
                } else {
                    Some(MethodTurbofish {
                        colon2_token: parse_quote!(::),
                        lt_token: parse_quote!(<),
                        args: type_args.into_iter()
                            .map(|arg| GenericMethodArgument::Type(arg.clone().into()))
                            .collect(),
                        gt_token: parse_quote!(>)
                    })
                },
                paren_token: Paren { span: dummy_span() },
                args: args.iter().map(|arg| from_sem_expr(ctx, u, arg))
                    .collect::<Result<Punctuated<Expr, Token![,]>>>()?
            })
        }
        sem::Expr::AssocFn(ty, ty_args, name, fn_type_args, args) => {
            Expr::Call(ExprCall {
                attrs: vec![],
                func: {
                    let ty_path = make_turbofish_path(&ty, ty_args.clone());
                    let fn_path = make_turbofish_path(&vec![name.clone()], fn_type_args.clone());
                    Box::new(Expr::Path(ExprPath{
                        qself: None,
                        attrs: vec![],
                        path: syn::Path {
                            leading_colon: ty_path.leading_colon,
                            segments: ty_path.segments.into_iter()
                                .chain(fn_path.segments.into_iter())
                                .collect()
                        }
                    }))
                },
                paren_token: Paren { span: dummy_span() },
                args: args.iter().map(|a| from_sem_expr(ctx, u, a))
                    .collect::<Result<Punctuated<Expr, Token![,]>>>()?
            })
        }
        sem::Expr::Macro(name, body) => {
            let exprs = body.tokens.iter().map(|t| {
                match t {
                    Token::Expr(e) => {
                        let syn_e = from_sem_expr(ctx, u, &e)?;
                        Ok(quote!(#syn_e))
                    }
                    Token::Ident(s) => {
                        let ident = name_to_ident_det(s.as_str());
                        Ok(quote!(#ident))
                    }
                    Token::Type(ty) => {
                        let syn_ty: syn::Type = ty.clone().into();
                        Ok(quote!(#syn_ty))
                    }
                }
            });
            let content = match body.seperator {
                Seperator::Comma => {
                    let cont = exprs.collect::<Result<Punctuated<TokenStream, Token![,]>>>()?;
                    quote!(#cont)
                }
                Seperator::Semi => {
                    let cont = exprs.collect::<Result<Punctuated<TokenStream, Token![;]>>>()?;
                    quote!(#cont)
                }
            };
            syn::Expr::Macro(ExprMacro {
                attrs: vec![],
                mac: syn::Macro {
                    path: from_sem_path(ctx, u, &name)?,
                    bang_token: parse_quote!(!),
                    delimiter: match body.brackets {
                        BracketType::Round => MacroDelimiter::Paren(Default::default()),
                        BracketType::Square => MacroDelimiter::Bracket(Default::default()),
                        BracketType::Curly => MacroDelimiter::Brace(Default::default())
                    },
                    tokens: content
                }
            })
        }
        sem::Expr::Fn(path, type_args, args) => {
            let name = path.first().expect("Empty path");
            if name.starts_with("#Tuple") {
                // println!("{:?}", args);
                Expr::Tuple(ExprTuple {
                    attrs: vec![],
                    paren_token: Paren { span: dummy_span() },
                    elems: args.iter().map(|a| from_sem_expr(ctx, u, a))
                        .collect::<Result<Punctuated<Expr, Token![,]>>>()?
                })
            } else {
                Expr::Call(ExprCall {
                    attrs: vec![],
                    func: Box::new(Expr::Path(ExprPath {
                        attrs: vec![],
                        path: make_turbofish_path(&path, type_args.clone()),
                        qself: None
                    })),
                    paren_token: Paren { span: dummy_span() },
                    args: args.iter().map(|a| from_sem_expr(ctx, u, a))
                        .collect::<Result<Punctuated<Expr, Token![,]>>>()?
                })
            }
        }
        sem::Expr::Var(path) => Expr::Path(ExprPath {
            attrs: vec![],
            qself: None,
            path: from_sem_path(ctx, u, &path)?,
        }),
        sem::Expr::ExactString(str) => Expr::Lit(ExprLit {
            attrs: vec![],
            lit: Lit::Str(LitStr::new(str.as_str(), dummy_span()))
        }),
        sem::Expr::UnOp(op, val) => match *op {
            "&" => Expr::Reference(ExprReference {
                attrs: vec![],
                and_token: parse_quote!(&),
                raw: Default::default(),
                mutability: None,
                expr: Box::new(parenthesize_expr(13, from_sem_expr(ctx, u, val.as_ref())?))
            }),
            "&mut" => Expr::Reference(ExprReference {
                attrs: vec![],
                and_token: parse_quote!(&),
                raw: Default::default(),
                mutability: parse_quote!(mut),
                expr: Box::new(parenthesize_expr(13, from_sem_expr(ctx, u, val.as_ref())?))
            }),
            _ => Expr::Unary(ExprUnary {
                attrs: vec![],
                op: parse_str(op).expect(format!("Failed parsing operator {}", op).as_str()),
                expr: Box::new(parenthesize_expr(13, from_sem_expr(ctx, u, val.as_ref())?))
            })
        }
        sem::Expr::BinOp(op, lhs, rhs) => {
            let syn_op = parse_str(op).expect(format!("Couldn't parse binary operator {}", op).as_str());
            let presc = binary_prescedence(&syn_op);
            let (left_assoc, right_assoc) = binary_associativity(&syn_op);
            Expr::Binary(ExprBinary {
                attrs: vec![],
                op: parse_str(op).expect(format!("Failed parsing operator {}", op).as_str()),
                left: Box::new(parenthesize_expr(presc+left_assoc, from_sem_expr(ctx, u, lhs.as_ref())?)),
                right: Box::new(parenthesize_expr(presc+right_assoc, from_sem_expr(ctx, u, rhs.as_ref())?))
            })
        }
        sem::Expr::AdditionalArg(_ty, _mt) => parse_quote!(compile_error!("Illegal demand for additional arg"))
    })
}

fn extract_additional_args(ctx: &mut Context, u: &mut Unstructured, e: sem::Expr)
    -> Result<(Vec<(Ident, sem::Mutability, sem::Type)>, sem::Expr)> {
    Ok(match e {
        sem::Expr::AdditionalArg(ty, mt) => {
            let ident = c_arbitrary(ctx, u)?;
            // we don't want the argument to be shadowed by a local variable
            ctx.reserved_names.insert(ident_to_name(&ident).into());
            let name = ident_to_name(&ident).into();
            (vec![(ident, mt, ty)], sem::Expr::Var(vec![name]))
        }
        sem::Expr::Struct(name, ty_args, fields) => {
            let init: Result<(_, Vec<(StringWrapper, sem::Expr)>)> = 
                Ok((vec![], vec![]));
            let (extra_args, kvs_vec) = 
                fields.into_iter()
                    .fold(init, |acc, (k, v)| {
                        let (mut e_args, mut kvs) = acc?;
                        let (new_args, processed_v) = extract_additional_args(ctx, u, v)?;
                        kvs.push((k, processed_v));
                        e_args.extend(new_args);
                        Ok((e_args, kvs))
                    })?;
            let processed_fields: HashMap<_,_> = kvs_vec.into_iter().collect();
            (extra_args, sem::Expr::Struct(name, ty_args, processed_fields))
        }
        sem::Expr::Fn(name, ty_args, args) => {
            let init: Result<(_, Vec<sem::Expr>)> = 
                Ok((vec![], vec![]));
            let (extra_args, processed_args) = 
                args.into_iter()
                    .fold(init, |acc, a| {
                        let (mut e_as, mut proc_as) = acc?;
                        let (new_args, processed_arg) = extract_additional_args(ctx, u, a)?;
                        e_as.extend(new_args);
                        proc_as.push(processed_arg);
                        Ok((e_as, proc_as))
                    })?;
            (extra_args, sem::Expr::Fn(name, ty_args, processed_args))
        },
        sem::Expr::AssocFn(ty_name, ty_args, fn_name, fn_ty_args, args) => {
            let init: Result<(_, Vec<sem::Expr>)> = 
                Ok((vec![], vec![]));
            let (extra_args, processed_args) = 
                args.into_iter()
                    .fold(init, |acc, a| {
                        let (mut e_as, mut proc_as) = acc?;
                        let (new_args, processed_arg) = extract_additional_args(ctx, u, a)?;
                        e_as.extend(new_args);
                        proc_as.push(processed_arg);
                        Ok((e_as, proc_as))
                    })?;
            (extra_args, sem::Expr::AssocFn(
                ty_name,
                ty_args,
                fn_name,
                fn_ty_args,
                processed_args
            ))
        }
        sem::Expr::Method(disc, name, ty_args, args) => {
            let (first_extra_args, processed_disc) =
                extract_additional_args(ctx, u, *disc)?;
            let init: Result<(_, Vec<sem::Expr>)> = 
                Ok((first_extra_args, vec![]));
            let (extra_args, processed_args) = 
                args.into_iter()
                    .fold(init, |acc, a| {
                        let (mut e_as, mut proc_as) = acc?;
                        let (new_args, processed_arg) = extract_additional_args(ctx, u, a)?;
                        e_as.extend(new_args);
                        proc_as.push(processed_arg);
                        Ok((e_as, proc_as))
                    })?;
            (extra_args, sem::Expr::Method(
                Box::new(processed_disc),
                name,
                ty_args,
                processed_args
            ))
        }
        sem::Expr::UnOp(name, op) => {
            let (extra_args, processed_op) = extract_additional_args(ctx, u, *op)?;
            (extra_args, sem::Expr::UnOp(name, Box::new(processed_op)))
        }
        sem::Expr::BinOp(name, lhs, rhs) => {
            let (mut lhs_extra_args, processed_lhs) = extract_additional_args(ctx, u, *lhs)?;
            let (rhs_extra_args, processed_rhs) = extract_additional_args(ctx, u, *rhs)?;
            lhs_extra_args.extend(rhs_extra_args);
            (lhs_extra_args, sem::Expr::BinOp(
                name,
                Box::new(processed_lhs),
                Box::new(processed_rhs)
            ))
        }
        sem::Expr::Field(disc, name) => {
            let (extra_args, processed_disc) = extract_additional_args(ctx, u, *disc)?;
            (extra_args, sem::Expr::Field(Box::new(processed_disc), name))
        }
        sem::Expr::Macro(name, body) => {
            let init: Result<(_, Vec<sem::Token>)> = Ok((vec![], vec![]));
            let (extra_args, processed_tokens) = 
                body.tokens.iter()
                    .fold(init, |acc, token| {
                        let (mut e_as, mut proc_body) = acc?;
                        let (new_args, processed_token) = match token {
                            sem::Token::Expr(e) => {
                                let (new_args, proc_e) = extract_additional_args(ctx, u, e.clone())?;
                                (new_args, sem::Token::Expr(proc_e))
                            }
                            _ => (vec![], token.clone())
                        };
                        e_as.extend(new_args);
                        proc_body.push(processed_token);
                        Ok((e_as, proc_body))
                    })?;
            (extra_args, sem::Expr::Macro(name, sem::MacroBody {
                tokens: processed_tokens,
                ..body
            }))
        }
        _ => (vec![], e)
    })
}

fn from_sem_generics(
    ctx: &mut Context, u: &mut Unstructured,
    lt_generics: &Vec<StringWrapper>,
    type_generics: &sem::TypeGenerics
) -> Result<Generics> {
    let is_empty = 
        lt_generics.len() + type_generics.len() == 0 &&
        Arbitrary::arbitrary(u)?;
    Ok(Generics {
        lt_token: if is_empty {
            None
        } else {
            parse_quote!(<)
        },
        params: lt_generics.iter().map(|lt| Ok(GenericParam::Lifetime(LifetimeDef {
            attrs: vec![],
            lifetime: Lifetime {
                apostrophe: dummy_span(),
                ident: name_to_ident(u, lt.as_str())?,
            },
            colon_token: parse_quote!(:),
            // TODO: add lifetime bounds
            bounds: iter::empty::<Lifetime>().collect()
        }))).collect::<Vec<_>>().into_iter().chain(type_generics.iter().map(|ty_gen| Ok(GenericParam::Type(TypeParam {
            attrs: vec![],
            ident: name_to_ident_det(ty_gen.name.as_str()),
            colon_token: parse_quote!(:),
            bounds: ty_gen.constraints.iter().map(|c|
                Ok(TypeParamBound::Trait(TraitBound {
                    // TODO: when is the paren token needed?
                    paren_token: Some(Paren { span: dummy_span() }),
                    modifier: TraitBoundModifier::None,
                    lifetimes: None,
                    path: from_sem_path(ctx, u, &c.trait_name)?
                })
            )).collect::<Result<_>>()?,
            // TODO: defaults
            eq_token: None,
            default: None
        })))).collect::<Result<_>>()?,
        gt_token: if is_empty {
            None
        } else {
            parse_quote!(>)
        },
        // TODO: where clause
        where_clause: None
    })
}

fn from_sem_path(ctx: &Context, u: &mut Unstructured, path: &sem::Path) -> Result<syn::Path> {
    let mut aliases = vec![path];
    for scope in ctx.scopes.iter() {
        if let Some(found_aliases) = scope.path_aliases.get(path) {
            aliases.extend(found_aliases)
        }
    }
    let path_suffix = choose_consume(u, aliases.into_iter())?;
    Ok(make_path(path_suffix, PathArguments::None))
}

fn name_to_ident(u: &mut Unstructured, name: &str) -> Result<Ident> {
    if name.starts_with("r#") {
        Ok(parse_str(name).expect(format!("Unable to parse {} as ident", name).as_str()))
    } else if KWDS_STRICT.contains(name) || Arbitrary::arbitrary(u)? {
        Ok(
            parse_str(format!("r#{}", name).as_str())
                .expect(format!("Unable to parse r#{} as ident", name).as_str())
        )
    } else {
        Ok(Ident::new(name, dummy_span()))
    }
}

fn name_to_ident_det(name: &str) -> Ident {
    if name.starts_with("r#") {
        parse_str(name).expect(format!("Unable to parse {} as ident", name).as_str())
    } else if KWDS_STRICT.contains(name) {
        parse_str(format!("r#{}", name).as_str())
            .expect(format!("Unable to parse r#{} as ident", name).as_str())
    } else {
        Ident::new(name, dummy_span())
    }
}

fn ident_to_name(ident: &Ident) -> String {
    let str = ident.to_string();
    if str.starts_with("r#") {
        return str[2..].to_string();
    } else {
        return str;
    }
}

fn make_path(name: &sem::Path, path_args: PathArguments) -> syn::Path {
    let mut segments_vec = vec![];
    let mut segments_iter = name.into_iter();
    let last_segment = PathSegment {
        ident: name_to_ident_det(segments_iter.next_back().unwrap().as_str()),
        arguments: path_args
    };
    for seg in segments_iter {
        segments_vec.push(PathSegment {
            ident: name_to_ident_det(seg.as_str()),
            arguments: PathArguments::None
        })
    }
    let segments = segments_vec.into_iter()
        .chain(iter::once(last_segment))
        .collect::<Punctuated<PathSegment, Token![::]>>();

    syn::Path {
        leading_colon: None,
        segments, 
    }

}

fn make_turbofish_path(name: &sem::Path, args: Vec<sem::Type>) -> syn::Path {
    let path_args = if args.is_empty() {
        PathArguments::None
    } else {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: parse_quote!(::),
            lt_token: parse_quote!(<),
            args: args.into_iter().map(|ty| GenericArgument::Type(ty.into())).collect(),
            gt_token: parse_quote!(>)
        })
    };
    make_path(name, path_args)
}

fn make_type_path(ty: sem::Type) -> syn::Path {
    let name = ty.name;
    let path_args =
    if ty.type_args.is_empty() && ty.lt_args.is_empty() {
        PathArguments::None
    } else {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: None,
            lt_token: parse_quote!(<),
            args: ty.lt_args.iter().filter_map(sem::Lifetime::name).map(|lt| {
                let lt_str = lt.as_str();
                GenericArgument::Lifetime(syn::Lifetime {
                    apostrophe: dummy_span(),
                    ident: parse_quote!(#lt_str),
                })
            }).chain(ty.type_args.into_iter().map(|ty_arg| {
                    GenericArgument::Type(ty_arg.into())
                }))
                .collect(),
            gt_token: parse_quote!(>),
        })

    };
    make_path(&name, path_args)

}

impl<'a, T, P, Ctx> ContextArbitrary<'a, Ctx> for Punctuated<T, P>
    where
        T: ContextArbitrary<'a, Ctx>,
        P: Default,
    {
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self> {
        let iter: ContextArbitraryIter<T, Ctx> = c_arbitrary_iter(ctx, u);
        return iter.collect();
    }
    }

struct NEPunctuated<T, P>(pub Punctuated<T, P>);
impl<'a, T, P, Ctx> ContextArbitrary<'a, Ctx> for NEPunctuated<T, P>
    where
        T: ContextArbitrary<'a, Ctx>,
        P: Default
    {
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self> {
        let first: T = c_arbitrary(ctx, u)?;
        let rest: ContextArbitraryIter<T, Ctx> = c_arbitrary_iter(ctx, u);
        let iter = iter::once(Ok(first)).chain(rest);
        return Ok(NEPunctuated(iter.collect::<Result<Punctuated<T, P>>>()?));
    }
    }

fn unwrap_nep<T, P>(NEPunctuated(contents): NEPunctuated<T, P>) -> Punctuated<T, P> {
    contents
}

impl<'a> ContextArbitrary<'a, Context> for File {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        // println!("New file!\n--------------------");
        let mut items: Vec<syn::Item> = vec![];
        let reserved = c_arbitrary(ctx, u)?;
        let mut scope = ctx.scopes.top_mut().unwrap();
        scope.reserved = reserved;
        drop(scope);
        // println!("New reserved: {:?}", ctx.scopes.top_mut().unwrap().reserved);
        let attrs = crate_attrs(ctx, u)?;
        reserve_names!(ctx, {
            items.extend(from_reservations(ctx, u)?);
            // if ctx.has_main {
                let main = Item::Fn(make_main(ctx, u)?);
                items.push(main);
                ctx.reserved_names.insert("main".into());
            // }
            // items.extend(c_arbitrary_iter(ctx, u).collect::<Result<Vec<Item>>>()?);
            // if items.is_empty() {
            //     items.push(c_arbitrary(ctx, u)?);
            // }
        });
        choose::shuffle(u, &mut items)?;
        Ok(File {
            shebang: None,
            attrs,
            items,
        })
    }
}


fn crate_attrs<'a>(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Vec<Attribute>> {
    fn wrap_attr(path: syn::Path) -> Attribute {
        Attribute {
            pound_token: parse_quote!(#),
            style: AttrStyle::Inner(parse_quote!(!)),
            bracket_token: Bracket { span: dummy_span() },
            path,
            tokens: quote!(),
        }
    }
    let mut attrs = vec![];
    if ctx.regard_semantics {
        if !ctx.options.runnable {
            lazy_maybe!(u, {
                ctx.has_main = false;
                attrs.push(wrap_attr(parse_quote!(no_main)))
            });
        }
        lazy_maybe!(u, {
            ctx.non_ascii = true;
            attrs.push(parse_quote!(#![feature(non_ascii_idents)]))
        });
    }
    return Ok(attrs);
}

fn make_main<'a>(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<ItemFn> {
let (ret_ty, output) = lazy_choose!(u,  {
    (make_type!(()), ReturnType::Default),
    (make_type!(()), ReturnType::Type(parse_quote!(->), parse_quote!(()))),
    (make_type!(Result[#(()), std::io::Error]),
        ReturnType::Type(parse_quote!(->), parse_quote!(Result<(), std::io::Error>))),
    })?;
    let sig = Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: parse_quote!(fn),
        ident: parse_quote!(main),
        paren_token: Paren { span: dummy_span() },
        inputs: Punctuated::new(),
        variadic: None,
        output, 
        generics: Generics {
            lt_token: None,
            params: Punctuated::new(),
            gt_token: None,
            where_clause: None
        }
    };


    Ok(ItemFn {
        attrs: vec![],
        vis: Visibility::Inherited,
        sig,
        block: with_attrs!(ctx {
            allow_ambiguity = true,
            expected_type = ret_ty,
            is_fn_block = true
        }, c_arbitrary(ctx, u)?)
        // block: ty_ambigious!(ctx, with_type!(ctx, ret_ty, c_arbitrary(ctx, u)?))
    })
}

impl<'a> ContextArbitrary<'a, Context> for Item {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        // let top_lev = !ctx.regard_semantics || ctx.is_top_level;
        #![allow(unreachable_code)]
        not_top_level!(ctx, guarded_lazy_choose!(u,  {
            ctx.regard_semantics => panic!("Use `from_reserved` instead!"),
            !ctx.regard_semantics => Item::Type(c_arbitrary(ctx, u)?),
            !ctx.regard_semantics => Item::Trait(c_arbitrary(ctx, u)?),
            !ctx.regard_semantics => Item::Const(c_arbitrary(ctx, u)?),
            !ctx.regard_semantics => Item::Static(c_arbitrary(ctx, u)?),
            // Enums, Fns, and structs are created by first reserving them
            !ctx.regard_semantics => Item::Fn(c_arbitrary(ctx, u)?),
            !ctx.regard_semantics => Item::Enum(c_arbitrary(ctx, u)?),
            !ctx.regard_semantics => Item::Struct(c_arbitrary(ctx, u)?),
            // TODO:
            // Impl(ItemImpl),
            // Mod(ItemMod),
            // Static(ItemStatic),
            // TraitAlias(ItemTraitAlias),
            // Union(ItemUnion),
            // Use(ItemUse),
            // Intentionally omitted: ExternCrate, ForeignMod, Macro, Verbatim
        }))
    }
}

impl <'a> ContextArbitrary<'a, Context> for sem::Reservations {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        enum ResType {
            Enum(sem::Type, Vec<(StringWrapper, bool)>),
            Fn(StringWrapper),
            Struct(sem::Type, bool)
        }
        let reservations = c_arbitrary_iter_with(ctx, u, |ctx, u| {
            // We're creating all 4 in the same iter_with to avoid a disproportionate amount 
            // of fns, enums and structs being created
            lazy_choose!(u, {
                {
                    // Enum
                    let name: StringWrapper = ident_to_name(&c_arbitrary(ctx, u)?).into();
                    ctx.reserved_names.insert(name.clone());
                    ResType::Enum(sem::Type {
                        name: vec![name], 
                        ..make_type!(a)
                    }, reserve_names!(ctx, c_arbitrary_iter_with(ctx, u, |ctx, u| {
                        let ident = c_arbitrary(ctx, u)?;
                        let name: StringWrapper = ident_to_name(&ident).into();
                        ctx.reserved_names.insert(name.clone());
                        Ok((name, Arbitrary::arbitrary(u)?))
                    }).collect::<Result<Vec<_>>>()?))
                }, {
                    // Fn
                    let name: StringWrapper = ident_to_name(&c_arbitrary(ctx, u)?).into();
                    ctx.reserved_names.insert(name.clone());
                    ResType::Fn(name)
                }, {
                    // Struct
                    // We don't want structs shadowing each other since this can lead to 
                    // unconstructable types
                    let defined_structs = ctx.scopes.iter()
                        .flat_map(|s| s.structs.keys().filter_map(|k| {
                            if k.len() == 1 {
                                k.last().cloned()
                            } else {
                                None
                            }
                        }));
                    let mut new_reserved = ctx.reserved_names.clone();
                    new_reserved.extend(defined_structs);
                    let ident: Ident = reserve_names!(ctx, new_reserved, c_arbitrary(ctx, u)?);
                    let name: StringWrapper = ident_to_name(&ident).into();
                    // println!("Reserved struct: {}", name.as_str());
                    ctx.reserved_names.insert(name.clone());
                    ResType::Struct(sem::Type {
                        name: vec![name],
                        ..make_type!(a)
                    }, Arbitrary::arbitrary(u)?)
                }
            })
        }).collect::<Result<Vec<_>>>()?;
        let (enums, fns, structs) = reservations.into_iter().fold(
            (vec![],vec![],vec![]),
            |(mut enums, mut fns, mut structs), res| {
                use ResType::*;
                match res {
                    Enum(ty, variants) => enums.push((ty, variants)),
                    Fn(name) => fns.push(name),
                    Struct(ty, tuple_struct) => structs.push((ty, tuple_struct))
                }
                (enums, fns, structs)
            }
        );
        Ok(Reservations {
            enums,
            fns,
            structs,
        })
    }
}

fn from_reservations(ctx: &mut Context, u: &mut Unstructured) -> Result<Vec<Item>> {
    let mut result = vec![];
    let mut scope = ctx.scopes.top_mut().unwrap();
    let reservations = mem::replace(&mut scope.reserved, Default::default());
    // Why is this needed??
    drop(scope);
    for (ty, is_tuple_struct) in reservations.structs {
        result.push(Item::Struct(from_reserved_struct(ctx, u, ty, is_tuple_struct)?));
    }
    for (ty, variants) in reservations.enums {
        result.push(Item::Enum(from_reserved_enum(ctx, u, ty, variants)?));
    }
    for name in reservations.fns {
        result.push(Item::Fn(from_reserved_fn(ctx, u, name)?));
    }
    return Ok(result);
}

impl<'a> ContextArbitrary<'a, Context> for ItemStruct {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        if ctx.regard_semantics {
            panic!("Create this from a reservation instead");
        } else {
            Ok(ItemStruct {
                attrs: vec![],
                vis: c_arbitrary(ctx, u)?,
                struct_token: parse_quote!(struct),
                ident: c_arbitrary(ctx, u)?,
                generics: c_arbitrary(ctx, u)?,
                fields: c_arbitrary(ctx, u)?,
                semi_token: parse_quote!(;)
            })
        }
    }
}

fn from_reserved_struct(
    ctx: &mut Context,
    u: &mut Unstructured,
    ty: sem::Type,
    is_tuple: bool
) -> Result<ItemStruct> {
    let ident = name_to_ident(u, ty.name.last().unwrap().as_str())?;
    ctx.reserved_names.insert(ty.name.last().unwrap().clone());
    let generics = from_sem_generics(ctx, u, &ty.lt_generics, &ty.type_generics)?;
    let vis = c_arbitrary(ctx, u)?;
    let kind = sem::Kind {
        is_visible: is_pub(&vis),
        lifetimes: ty.lt_generics.len(),
        types: ty.type_generics.len()
    };
    let ty = kind_to_type(ident_to_name(&ident), &kind);
    let sem_fields = generate_sem_fields(
        ctx, u,
        is_pub(&vis), &ty,
        &ty.lt_generics, &ty.type_generics,
        is_tuple
    )?;
    add_struct(ctx, vec![ident_to_name(&ident).into()], ty, Rc::new(sem_fields.clone()));
    let fields = sem_fields.into();
    Ok(ItemStruct {
        attrs: if ctx.options.print_vars {
            vec![parse_quote!(#[derive(Debug)])]
        } else {
            vec![]
        },
        vis, ident, fields, generics,
        struct_token: parse_quote!(struct),
        semi_token: parse_quote!(;)
    })
}

impl<'a> ContextArbitrary<'a, Context> for ItemEnum {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ItemEnum {
            attrs: vec![],
            vis: c_arbitrary(ctx, u)?,
            enum_token: parse_quote!(enum),
            ident: c_arbitrary(ctx, u)?,
            generics: c_arbitrary(ctx, u)?,
            brace_token: Brace { span: dummy_span() },
            variants: c_arbitrary(ctx, u)?
        })

    }
}

fn from_reserved_enum(
    ctx: &mut Context,
    u: &mut Unstructured,
    ty: sem::Type,
    reserved_variants: Vec<(StringWrapper, bool)>
) -> Result<ItemEnum> {
    let vis = c_arbitrary(ctx, u)?;
    let name = ty.name.last().unwrap();
    let ident = name_to_ident(u, name.as_str())?;
    ctx.reserved_names.insert(name.clone());
    let generics = from_sem_generics(ctx, u, &ty.lt_generics, &ty.type_generics)?;
    let kind = Kind {
        is_visible: is_pub(&vis),
        types: ty.type_generics.len(),
        lifetimes: ty.lt_generics.len()
    };
    // If the enum doesn't have any variants, don't add it to the types
    // because it won't be possible to construct it without cheating
    let mut type_added = false;
    let sem_variants = reserve_names!(ctx, reserved_variants.into_iter().map(|(name, is_tuple)| {
        if !type_added {
            type_added = true;
            add_type(ctx, vec![ident_to_name(&ident).into()], kind.clone());
        }
        ctx.reserved_names.insert(name.clone());
        let fields = generate_sem_fields(
            ctx, u, is_pub(&vis), &ty,
            &ty.lt_generics, &ty.type_generics,
            is_tuple
        )?;
        Ok((name, fields))
    }).collect::<Result<Vec<(StringWrapper, sem::Fields)>>>()?);
    let mut variants = vec![];
    let mut struc_map = HashMap::new();
    for (enum_name, fields) in sem_variants {
        struc_map.insert(enum_name.clone(), Rc::new(fields.clone()));
        variants.push(syn::Variant {
            fields: remove_visibility(fields.into()),
            ident: name_to_ident(u, enum_name.as_str())?,
            // TODO: add discriminants
            discriminant: None,
            attrs: vec![],
        });
    }
    add_enum(ctx, vec![ident_to_name(&ident).into()], ty.clone(), struc_map);
    Ok(ItemEnum {
        attrs: if ctx.options.print_vars {
            vec![parse_quote!(#[derive(Debug)])]
        } else {
            vec![]
        },
        vis,
        enum_token: parse_quote!(enum),
        brace_token: Brace { span: dummy_span() },
        ident, generics,
        variants: variants.into_iter().collect()
    })
}

fn remove_visibility(fields: Fields) -> Fields {
    match fields {
        Fields::Named(named) => {
            Fields::Named(FieldsNamed {
                brace_token: named.brace_token,
                named: named.named.into_iter().map(|field| {
                    Field {
                        vis: Visibility::Inherited,
                        ..field
                    }
                }).collect()
            })
        }
        Fields::Unnamed(unnamed) => {
            Fields::Unnamed(FieldsUnnamed {
                paren_token: unnamed.paren_token,
                unnamed: unnamed.unnamed.into_iter().map(|field| {
                    Field {
                        vis: Visibility::Inherited,
                        ..field
                    }
                }).collect()
            })
        }
        Fields::Unit => Fields::Unit
    }
}

fn generate_sem_fields(
    ctx: &mut Context,
    u: &mut Unstructured,
    vis: bool,
    ty: &sem::Type,
    lts: &Vec<StringWrapper>,
    _tys: &Vec<sem::Generic>,
    is_tuple: bool
) -> Result<sem::Fields> {
    // TODO: make sure that all tys and lts are used
    let fields = c_arbitrary_iter_with(ctx, u, |ctx, u| {
        let mut field_ty = pick_type_that(
            ctx, u,
            |_,k| lts.len() > 0 || k.lifetimes == 0 && (!vis || k.is_visible)
        )?;
        let vis = !field_ty.is_private();
        if field_ty.is_sized_by(&ty.name) {
            field_ty = guarded_lazy_choose!(u, {
                true => sem::Type {
                    type_args: vec![field_ty],
                    ..make_type!(Box)
                },
                // true => sem::Type {
                //    type_args: vec![field_ty],
                //    ..make_type!(std::rc::Rc)
                // },
                true => sem::Type {
                    type_args: vec![field_ty],
                    ..make_type!(Vec)
                },
                lts.len() > 0 => sem::Type {
                    type_args: vec![field_ty],
                    ..make_type!(& field_ty)
                }
            })?;
        }
        if lts.len() > 0 {
            let wrapped_lts = lts.iter().map(|n| sem::Lifetime::Named(n.clone())).collect();
            field_ty = field_ty.assign_lts_vec(u, &wrapped_lts)?;
        }
        Ok(sem::Field {
            ty: Rc::new(field_ty),
            visible: vis,
        })
    }).collect::<Result<Vec<sem::Field>>>()?;
    Ok(if !is_tuple {
        let fields = reserve_names!(ctx, sem::Fields::Named(fields.into_iter().map(|field| {
            let name: StringWrapper = ident_to_name(&c_arbitrary(ctx, u)?).into();
            ctx.reserved_names.insert(name.clone());
            Ok((name, field))
        }).collect::<Result<HashMap<StringWrapper, sem::Field>>>()?));
        fields
    } else {
        sem::Fields::Unnamed(fields)
    })
}

impl<'a> ContextArbitrary<'a, Context> for Variant {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Variant {
            attrs: vec![],
            ident: c_arbitrary(ctx, u)?,
            fields: c_arbitrary(ctx, u)?,
            discriminant: c_arbitrary::<Context, Option<Expr>>(ctx, u)?
                .map(|e| (parse_quote!(=), e)),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Fields {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
lazy_choose!(u, {
    Fields::Named(c_arbitrary(ctx, u)?),
    Fields::Unnamed(c_arbitrary(ctx, u)?),
    Fields::Unit,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for FieldsNamed {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(FieldsNamed {
            brace_token: Brace { span: dummy_span() },
            named: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for FieldsUnnamed {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(FieldsUnnamed {
            paren_token: Paren { span: dummy_span() },
            unnamed: name_fields!(ctx, false, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Field {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let ident: Option<Ident>;
        let colon_token;
        if ctx.named_fields {
            ident = Some(c_arbitrary(ctx, u)?);
            colon_token = Some(parse_quote!(:));
        } else {
            ident = None;
            colon_token = None;
        }
        Ok(Field {
            attrs: vec![],
            vis: c_arbitrary(ctx, u)?,
            ident,
            colon_token,
            ty: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemTrait {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ItemTrait {
            attrs: vec![],
            vis: c_arbitrary(ctx, u)?,
            unsafety: maybe(u, parse_quote!(unsafe)),
            // auto token is not implemented yet
            auto_token: None,
            trait_token: parse_quote!(trait),
            ident: c_arbitrary(ctx, u)?,
            generics: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            supertraits: c_arbitrary(ctx, u)?,
            brace_token: Brace { span: dummy_span() },
            items: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TraitItem {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        lazy_choose!(u, {
            TraitItem::Const(c_arbitrary(ctx, u)?),
            TraitItem::Method(c_arbitrary(ctx, u)?),
            TraitItem::Type(c_arbitrary(ctx, u)?),
            // intentionally ommited: Macro, Verbatim
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TraitItemMethod {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TraitItemMethod {
            attrs: vec![],
            sig: c_arbitrary(ctx, u)?,
            default: c_arbitrary(ctx, u)?,
            semi_token: parse_quote!(;),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TraitItemType {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TraitItemType {
            attrs: vec![],
            type_token: parse_quote!(type),
            ident: c_arbitrary(ctx, u)?,
            generics: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            bounds: unwrap_nep(c_arbitrary(ctx, u)?),
            default: c_arbitrary::<Context, Option<Type>>(ctx, u)?.map(|t| (parse_quote!(=), t)),
            semi_token: parse_quote!(;),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TraitItemConst {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TraitItemConst {
            attrs: vec![],
            const_token: parse_quote!(const),
            ident: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            ty: c_arbitrary(ctx, u)?,
            default: c_arbitrary::<Context, Option<Expr>>(ctx, u)?.map(|e| (parse_quote!(=), e)),
            semi_token: parse_quote!(;),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Visibility {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        guarded_lazy_choose!(u, {
            true => Visibility::Public(VisPublic{ pub_token: parse_quote!(pub), }),
            // Still experimental
            // |_, _| Ok(Visibility::Crate(VisCrate { crate_token: parse_quote!(crate), })),
            // Relies on accessing crates
            !ctx.regard_semantics => {
                Visibility::Restricted(VisRestricted {
                    pub_token: parse_quote!(pub),
                    paren_token: Paren { span: dummy_span() },
                    in_token: parse_quote!(in),
                    path: crate_path!(ctx, c_arbitrary(ctx, u)?),
                })
            },
            true => Visibility::Inherited,
        })
    }
}

fn is_pub(vis: &Visibility) -> bool {
    if let Visibility::Public(_) = vis {
        true 
    } else {
        false
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemStatic {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ItemStatic {
            attrs: vec![],
            vis: c_arbitrary(ctx, u)?,
            static_token: parse_quote!(static),
            mutability: maybe(u, parse_quote!(mut)),
            ident: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            ty: c_arbitrary(ctx, u)?,
            eq_token: parse_quote!(=),
            expr: rhs!(ctx, c_arbitrary(ctx, u)?),
            semi_token: parse_quote!(;),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemConst {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ItemConst {
            attrs: vec![],
            vis: Visibility::Inherited,
            const_token: parse_quote!(const),
            ident: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            ty: c_arbitrary(ctx, u)?,
            eq_token: parse_quote!(=),
            expr: rhs!(ctx, c_arbitrary(ctx, u)?),
            semi_token: parse_quote!(;),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemFn {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let sig = c_arbitrary(ctx, u)?;
        let block = no_block_labels!(ctx, c_arbitrary(ctx, u)?);
        Ok(ItemFn {
            attrs: vec![],
            vis: Visibility::Inherited,
            sig,
            block,
        })
    }
}

fn from_reserved_fn(ctx: &mut Context, u: &mut Unstructured, name: StringWrapper) -> Result<ItemFn> {
    let ident = name_to_ident(u, name.as_str())?;
    let mut reserved = ctx.reserved_names.clone();
    ctx.not_in_use_scopes = Some((ctx.scopes.clone(), ctx.basic_scopes.clone()));
    push_scope(ctx);
    let (fn_type, final_expr, additional_arg_names, sig) = type_with_sig(ctx, u, ident)?;
    // println!("Additional arg names: {:?}", additional_arg_names);
    reserved.extend(additional_arg_names);
    let ret_type = fn_type.func.clone().unwrap().ret_type;
    add_var(ctx, vec![ident_to_name(&sig.ident).into()], Variable::new(
        fn_type.clone(),
        sem::Mutability::Immutable,
        sem::Lifetime::Named("'static".into())
    ));
    let block = with_attrs!(ctx {
        needs_new_scope = false,
        allow_ambiguity = true,
        expected_type = ret_type,
        precomputed_final = final_expr,
        reserved_names = reserved
    }, c_arbitrary(ctx, u)?);
    pop_scope(ctx);
    add_var(ctx, vec![ident_to_name(&sig.ident).into()], Variable::new(
        fn_type,
        sem::Mutability::Immutable,
        sem::Lifetime::Named("'static".into())
    ));
    Ok(ItemFn {
        attrs: vec![],
        vis: Visibility::Inherited,
        sig,
        block,
    })
}

impl<'a> ContextArbitrary<'a, Context> for ItemType {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let ident: Ident = c_arbitrary(ctx, u)?;
        // TODO: Figure out how to deal with type aliases
        Ok(ItemType {
            attrs: vec![],
            vis: Visibility::Inherited,
            type_token: parse_quote!(type),
            generics: c_arbitrary(ctx, u)?,
            eq_token: parse_quote!(=),
            ty: if ctx.regard_semantics {
                // TODO: assign lifetimes
                let ty = pick_type(ctx, u)?;
                // add_type(ctx, ident.to_string().into(), sem::Kind {
                //     lifetimes: 0,
                //     types: 0,
                // });
                Box::new(ty.into())
            } else {
                c_arbitrary(ctx, u)?
            },
            ident,
            semi_token: parse_quote!(;),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Type {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        lazy_choose!(u, {
            Type::Slice(c_arbitrary(ctx, u)?),
            Type::Array(c_arbitrary(ctx, u)?),
            Type::Ptr(c_arbitrary(ctx, u)?),
            Type::Reference(c_arbitrary(ctx, u)?),
            Type::BareFn(c_arbitrary(ctx, u)?),
            // Type::Never(syn::TypeNever{bang_token: parse_quote!(!)}),
            Type::Tuple(c_arbitrary(ctx, u)?),
            Type::Path(c_arbitrary(ctx, u)?),
            Type::TraitObject(c_arbitrary(ctx, u)?),
            Type::ImplTrait(c_arbitrary(ctx, u)?),
            Type::Paren(c_arbitrary(ctx, u)?),
            Type::Group(c_arbitrary(ctx, u)?),
            Type::Infer(TypeInfer { underscore_token: parse_quote!(_) }),
            // Intentionally omitted: Macro, Verbatim
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeSlice {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeSlice {
            bracket_token: Bracket { span: dummy_span() },
            elem: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeArray {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeArray {
            bracket_token: Bracket { span: dummy_span() },
            elem: c_arbitrary(ctx, u)?,
            semi_token: parse_quote!(;),
            len: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeTuple {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeTuple {
            paren_token: Paren { span: dummy_span() },
            elems: unwrap_nep(c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypePtr {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypePtr {
            star_token: parse_quote!(*),
            const_token: parse_quote!(const),
            mutability: maybe(u, parse_quote!(mut)),
            elem: parens_ty!(ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeReference {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeReference {
            and_token: parse_quote!(&),
            lifetime: c_arbitrary(ctx, u)?,
            mutability: maybe(u, parse_quote!(mut)),
            elem: parens_ty!(ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeBareFn {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeBareFn {
            lifetimes: None,
            unsafety: maybe(u, parse_quote!(unsafe)),
            // TODO: Abi
            abi: None,
            fn_token: parse_quote!(fn),
            paren_token: syn::token::Paren { span: dummy_span() },
            inputs: c_arbitrary(ctx, u)?,
            variadic: c_arbitrary(ctx, u)?,
            output: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypePath {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypePath {
            // TODO: maybe add qualified self type?
            qself: None,
            path: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeTraitObject {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let first_bound = TypeParamBound::Trait(c_arbitrary(ctx, u)?);
        let iter: ContextArbitraryIter<TypeParamBound, Context> = c_arbitrary_iter(ctx, u);
        // bounds need at least one trait
        let bounds = iter::once(Ok(first_bound))
            .chain(iter)
            .collect::<Result<Punctuated<TypeParamBound, Token![+]>>>()?;
        Ok(TypeTraitObject {
            dyn_token: maybe(u, parse_quote!(dyn)),
            bounds,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeImplTrait {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeImplTrait {
            impl_token: parse_quote!(impl),
            bounds: unwrap_nep(c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeParen {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeParen {
            paren_token: Paren { span: dummy_span() },
            elem: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeGroup {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeGroup {
            group_token: syn::token::Group { span: dummy_span() },
            elem: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeParamBound {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        lazy_choose!(u, {
            TypeParamBound::Trait(c_arbitrary(ctx, u)?),
            TypeParamBound::Lifetime(c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ReturnType {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        lazy_choose!(u, {
            ReturnType::Default,
            ReturnType::Type(
                parse_quote!(->),
                parens_ty!(ctx, c_arbitrary(ctx, u)?),
            )
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TraitBound {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TraitBound {
            paren_token: None,
            // TODO: potentially add questionmark modifier
            modifier: TraitBoundModifier::None,
            lifetimes: None,
            path: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for BareFnArg {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let opt_name: Option<Ident> = c_arbitrary(ctx, u)?;
        Ok(BareFnArg {
            attrs: vec![],
            name: opt_name.map(|name| (name, parse_quote!(:))),
            ty: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Variadic {
    fn c_arbitrary(_ctx: &mut Context, _u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Variadic {
            attrs: vec![],
            dots: parse_quote!(...),
        })
    }
}

fn type_with_sig(ctx: &mut Context, u: &mut Unstructured, ident: Ident)
-> Result<(sem::Type, Option<sem::Expr>, Vec<StringWrapper>, Signature)> {

    let inputs: Punctuated<FnArg, Token![,]>;
    let output: ReturnType;
    let final_expr;
    let mut input_vec = vec![];
    let mut input_tys: Vec<sem::Type> = vec![];
    let old_reserved = mem::replace(&mut ctx.reserved_names, HashSet::new());
    let mut lt_generics: Vec<StringWrapper> = c_arbitrary_iter_with(ctx, u, |ctx, u| {
        let name: StringWrapper =
            ident_to_name(&c_arbitrary::<Context,Lifetime>(ctx, u)?.ident).into();
        ctx.reserved_names.insert(name.clone());
        Ok(name)
    }).collect::<Result<Vec<_>>>()?;
    // needed later in case the output contains an lt
    let reserved_lts = mem::replace(&mut ctx.reserved_names, old_reserved);
    let mut output_ty;
    // TODO: handle constraints
    // Generics can't shadow structs or enums because then they would be unconstructable
    let mut structs_and_enums = HashSet::new();
    for scope in ctx.scopes.iter() {
        for (name, _) in scope.structs.iter() {
            if name.len() == 1 {
                structs_and_enums.insert(name[0].clone());
            }
        }
        for (ref name, _) in scope.enums.iter() {
            if name.len() == 1 {
                structs_and_enums.insert(name[0].clone());
            }
        }
    }
    let type_generics = reserve_names!(ctx, structs_and_enums, c_arbitrary_iter_with(ctx, u, |ctx, u| {
        let name: StringWrapper =
            ident_to_name(&c_arbitrary::<Context, Ident>(ctx, u)?).into();
        ctx.reserved_names.insert(name.clone());
        Ok(Generic {
            name,
            constraints: vec![],
            is_arg_for_other: false 
        })
    }).collect::<Result<sem::TypeGenerics>>()?);
    for gen in type_generics.iter() {
        add_type(ctx, vec![gen.name.clone()], sem::Kind {
            is_visible: false,
            lifetimes: 0,
            types: 0
        })
    }

    // We only want types that are possible to construct in the body/return type
    for gen in type_generics.iter() {
        if input_tys.iter().all(|ty| !ty.contains(&vec![gen.name.clone()])) {
            remove_type(ctx, vec![gen.name.clone()]);
        }
    }


    // TODO: possible self argument where apropriate
    let len = c_arbitrary_iter::<(), Context>(ctx, u)
        .collect::<Result<Vec<()>>>()?
        .len();
    
    let old_reserved = mem::replace(&mut ctx.reserved_names, HashSet::new());
    for _ in 0..len {
        let ty = if lt_generics.len() == 0 {
            pick_type_that(
                ctx, u,
                |_,k| k.lifetimes == 0
            )?
        } else {
            pick_type(ctx, u)?.assign_lts(&mut ||
                -> arbitrary::Result<sem::Lifetime> {
                Ok(sem::Lifetime::Named(u.choose(&lt_generics)?.clone()))
            })?
        };
        input_tys.push(ty.clone());
        let (pat, vars) = sub_pattern!(ctx, irrefutable!(ctx, pattern_of_type(ctx, u, &ty)?));
        for (name, var) in vars {
            add_var(ctx, vec![name], var);
        }
        input_vec.push(FnArg::Typed(PatType {
            attrs: vec![],
            colon_token: parse_quote!(:),
            pat: Box::new(pat),
            ty: Box::new(ty.into())
        }));
    }
    let reserved_param_names = mem::replace(&mut ctx.reserved_names, old_reserved);

    let mut additional_arg_names = vec![];
    output_ty = pick_type(ctx, u)?;
    if output_ty.needs_lt() {
        let lt_ident: Ident = reserve_names!(
            ctx, reserved_lts,
            lifetime!(ctx, c_arbitrary(ctx, u)?)
        );
        let ident_name: StringWrapper = ident_to_name(&lt_ident).into();
        lt_generics.push(ident_name.clone());
        let wrapped_lts = lt_generics.iter().map(|n| sem::Lifetime::Named(n.clone())).collect();
        output_ty = output_ty.assign_lts_vec(u, &wrapped_lts)?;
        let raw_final_expr =
            can_demand_args!(ctx, construct_value(ctx, u, output_ty.clone(), true)?);
        let (additional_args, final_expr_tmp) = reserve_names!(
            ctx, reserved_param_names,
            extract_additional_args(ctx, u, raw_final_expr)?
        );
        final_expr = Some(final_expr_tmp);
        for (ident, mt, ty) in additional_args {
            let assigned_ty = ty.assign_lts_vec(u, &wrapped_lts)?;
            additional_arg_names.push(ident_to_name(&ident).into());
            input_tys.push(assigned_ty.clone());
            input_vec.push(FnArg::Typed(PatType {
                attrs: vec![],
                colon_token: parse_quote!(:),
                ty: Box::new(assigned_ty.into()),
                pat: Box::new(Pat::Ident(PatIdent {
                    attrs: vec![],
                    ident,
                    mutability: if mt == sem::Mutability::Mutable {
                        parse_quote!(mut)
                    } else {
                        None
                    },
                    by_ref: None,
                    subpat: None,
                }))
            }));
        }
    } else {
        final_expr = None;
    };
    inputs = input_vec.into_iter().collect();
    if output_ty.matches(&make_type!(())) && Arbitrary::arbitrary(u)? {
        output = ReturnType::Default;
    } else {
        output = ReturnType::Type(parse_quote!(->), Box::new(output_ty.clone().into()));
    }


    let generics = Generics {
        lt_token: parse_quote!(<),
        params: type_generics.iter().map(|gen| {
            GenericParam::Type(TypeParam {
                attrs: vec![],
                ident: name_to_ident_det(gen.name.as_str()),
                colon_token: parse_quote!(:),
                bounds: iter::empty::<TypeParamBound>().collect(),
                eq_token: None,
                default: None,

            })
        }).chain(lt_generics.iter().map(|lt| {
            GenericParam::Lifetime(LifetimeDef {
                attrs: vec![],
                colon_token: parse_quote!(:),
                lifetime: Lifetime {
                    apostrophe: dummy_span(),
                    ident: name_to_ident_det(lt.as_str())
                },
                // TODO: what should the bounds be
                bounds: iter::empty::<syn::Lifetime>().collect()
            })
        })).collect::<Punctuated<GenericParam, Token![,]>>(),
        gt_token: parse_quote!(>),
        where_clause: None
    };

    if ctx.regard_semantics {
        ctx.reserved_names.insert(ident_to_name(&ident).into());
    }

    Ok((sem::Type {
        name: vec!["#Fn".into()],
        // TODO: generate lifetimes
        lt_generics,
        type_generics,
        lt_args: vec![],
        type_args: vec![],
        func: Some(Box::new(Func {
            args: input_tys,
            ret_type: output_ty
        })),
        is_visible: true
    }, final_expr, additional_arg_names, Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: parse_quote!(fn),
        ident,
        generics,
        paren_token: Paren { span: dummy_span() },
        inputs,
        variadic: None,
        output
    }))
}

impl<'a> ContextArbitrary<'a, Context> for Signature {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        // Only first arg can be a reciever
        let inputs: Punctuated<FnArg, Token![,]>;
        let output: ReturnType;
irrefutable!(ctx, {
    let first_arg:Option<Receiver> = None;
    let other_args = c_arbitrary_iter::<FnArg, Context>(ctx, u);
    inputs = match first_arg {
        Some(r) => iter::once(Ok(FnArg::Receiver(r)))
            .chain(other_args)
            .collect::<Result<Punctuated<FnArg, Token![,]>>>()?,
        None => other_args.collect::<Result<Punctuated<FnArg, Token![,]>>>()?,
    };
    output = c_arbitrary(ctx, u)?;
        });
        Ok(Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: parse_quote!(fn),
            ident: c_arbitrary(ctx, u)?,
            generics: c_arbitrary(ctx, u)?,
            paren_token: Paren { span: dummy_span() },
            inputs,
            variadic: c_arbitrary(ctx, u)?,
            output
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Abi {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let name = lazy_choose!(u, {
            None,
            Some(parse_quote!("Rust")),
            Some(parse_quote!("C")),
            {
                let name: String = Arbitrary::arbitrary(u)?;
                Some(parse_quote!(#name))
            }
        })?;
        Ok(Abi {
            extern_token: parse_quote!(extern),
            name
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for FnArg {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
lazy_choose!(u, {
    // only the first token can be a reciever
    // FnArg::Receiver(c_arbitrary(ctx, u)?) ,
    FnArg::Typed(irrefutable!(ctx, c_arbitrary(ctx, u)?)),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatType {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatType {
            attrs: vec![],
            pat: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            ty: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Pat {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let irrefut = !ctx.regard_semantics || !ctx.is_refutable;
        let range = irrefut && (!ctx.regard_semantics || ctx.allow_range);
        guarded_lazy_choose!(u, {
            true => Pat::Box(c_arbitrary(ctx, u)?),
            true => Pat::Ident(c_arbitrary(ctx, u)?),
            true => Pat::Path(c_arbitrary(ctx, u)?),
            true => Pat::Reference(c_arbitrary(ctx, u)?),
            true => Pat::Rest(PatRest {
                attrs: vec![],
                dot2_token: parse_quote!(..),
            }),
            true => Pat::Slice(c_arbitrary(ctx, u)?),
            true => Pat::Tuple(c_arbitrary(ctx, u)?),
            // TODO: figure out when type patterns make sense (they often don't)
            // Box::new(|ctx, u| Ok(Pat::Type(c_arbitrary(ctx, u)?)) ),
            true => Pat::Wild(PatWild {
                attrs: vec![],
                underscore_token: parse_quote!(_),
            }),
            // intentionally omitted: Macro, Verbatim
            // NOTE: Unit literal can be refutable, needs a special case
            irrefut => Pat::Lit(c_arbitrary(ctx, u)?),
            // Not even syntactically allowed
            ctx.is_refutable => Pat::Or(c_arbitrary(ctx, u)?),
            // NOTE: can be irrefutable, depending on if it's a struct or an enum
            irrefut => Pat::Struct(c_arbitrary(ctx, u)?),
            irrefut => Pat::TupleStruct(c_arbitrary(ctx, u)?),
            range => Pat::Range(c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatBox {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatBox {
            attrs: vec![],
            box_token: parse_quote!(box),
            pat: parens_pat!(ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatIdent {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatIdent {
            attrs: vec![],
            by_ref: guarded_lazy_choose!(u, {
                true => None,
                !ctx.regard_semantics => parse_quote!(ref)
            })?,
            mutability: maybe(u, parse_quote!(mut)),
            ident: c_arbitrary(ctx, u)?,
            subpat: guarded_lazy_choose!(u, {
                true => None,
                !ctx.regard_semantics => Some((parse_quote!(@), c_arbitrary(ctx, u)?))
            })?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatLit {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatLit {
            attrs: vec![],
            // NOTE: the documentation says that negative numbers are represented
            //       as Unary(-, Lit(num)) but it seems like Lit can contain negative
            //       numbers, meaning that constructing negative numbers in that way
            //       is unneccesary.
            expr: Box::new(Expr::Lit(c_arbitrary(ctx, u)?)),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatOr {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatOr {
            attrs: vec![],
            // TODO: look into if/when a leading vert should be there
            leading_vert: None,
            // TODO: these should possibly be paranthesized
            cases: unwrap_nep(c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatPath {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatPath {
            attrs: vec![],
            qself: None,
            path: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatRange {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatRange {
            attrs: vec![],
            // TODO: lo and hi need to be valid patterns, not just any expr.
            //       Lit might be too restrictive of a type.
            lo: Box::new(Expr::Lit(c_arbitrary(ctx, u)?)),
            // Pattern ranges can only include ..=, .. is used for rest patterns
            // https://doc.rust-lang.org/stable/reference/patterns.html#range-patterns
            limits: parse_quote!(..=),
            hi: Box::new(Expr::Lit(c_arbitrary(ctx, u)?)),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatReference {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatReference {
            attrs: vec![],
            and_token: parse_quote!(&),
            mutability: None,
            pat: parens_pat!(ctx, no_range!(ctx, c_arbitrary(ctx, u)?)),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatSlice {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatSlice {
            attrs: vec![],
            bracket_token: Bracket { span: dummy_span() },
            elems: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatStruct {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatStruct {
            attrs: vec![],
            path: c_arbitrary(ctx, u)?,
            brace_token: Brace { span: dummy_span() },
            fields: c_arbitrary(ctx, u)?,
            dot2_token: parse_quote!(..),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for FieldPat {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(FieldPat {
            attrs: vec![],
            member: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            pat: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatTuple {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatTuple {
            attrs: vec![],
            paren_token: Paren { span: dummy_span() },
            elems: unwrap_nep(c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatTupleStruct {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatTupleStruct {
            attrs: vec![],
            path: c_arbitrary(ctx, u)?,
            pat: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Index {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Index {
            index: Arbitrary::arbitrary(u)?,
            span: dummy_span(),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for RangeLimits {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
lazy_choose!(u, {
    RangeLimits::HalfOpen(parse_quote!(..)),
    RangeLimits::Closed(parse_quote!(..=)),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for syn::Receiver {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(syn::Receiver {
            attrs: vec![],
            reference: lazy_maybe!(u, (parse_quote!(&), c_arbitrary(ctx, u)?)),
            mutability: None,
            self_token: parse_quote!(self),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Block {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> { 
        let stmts: Vec<Stmt>;
        if ctx.regard_semantics  {
            // TODO: if there is a (guaranteed) return/break somewhere, the last
            //       statement can be skipped
            let mut scope_added = false;
            let precomputed = mem::replace(&mut ctx.precomputed_final, None);
            if ctx.needs_new_scope {
                push_scope(ctx);
                scope_added = true;
            }
            ctx.needs_new_scope = true;
            let old_final_stmnt = ctx.is_final_stmnt;
            ctx.is_final_stmnt = false;
            ctx.scopes.top_mut().unwrap().reserved = c_arbitrary(ctx, u)?;
            let mut reserved_stmts = from_reservations(ctx, u)?.into_iter()
                .map(Stmt::Item).collect::<Vec<_>>();
            choose::shuffle(u, &mut reserved_stmts)?;
            let mut init_stmts
                = c_arbitrary_iter_with(ctx, u, |ctx, u| {
                    Ok(c_arbitrary(ctx, u)?)
                }).collect::<Result<Vec<Stmt>>>()?;
            init_stmts = choose::interleave(u,
                reserved_stmts.into_iter(),
                init_stmts.into_iter()
            )?;
            if ctx.options.print_vars {
                for (name, var) in ctx.scopes.top().iter().flat_map(|s| s.vars.iter()) {
                    // if ty.fits_constraints(ctx, &vec![parse_constraint!(Debug)]) {
                    if var.lifetime.is_valid(ctx) && var.ty.func.is_none() {
                        init_stmts.push(make_print(ctx, u, name.clone(), &var.ty)?);
                    }
                }
            }
            ctx.is_final_stmnt = true;
            if let Some(ref final_raw) = precomputed {
                // TODO: the final stmnt could be part of e.g. a block or an if
                init_stmts.push(Stmt::Expr(from_sem_expr(ctx, u, final_raw)?));
            } else {
                guarded_lazy_choose!(u, {
                    ctx.expected_type.matches(&make_type!(())) => (),
                    true => init_stmts.push(Stmt::Expr(c_arbitrary(ctx, u)?))
                })?;
            }
            stmts = init_stmts;
            if scope_added {
                pop_scope(ctx);
            }
            ctx.is_final_stmnt = old_final_stmnt;
        } else {
            let init_stmts = c_arbitrary_iter(ctx, u).collect::<Result<Vec<Stmt>>>()?;
            let final_stmnt = lazy_maybe!(u, Stmt::Expr(c_arbitrary(ctx, u)?));
            stmts = init_stmts.into_iter().chain(final_stmnt).collect();
        }
        Ok(Block {
            brace_token: Brace { span: dummy_span() },
            stmts,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Stmt {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        guarded_lazy_choose!(u, {
            true => Stmt::Local(c_arbitrary(ctx, u)?),
            !ctx.regard_semantics => Stmt::Item(c_arbitrary(ctx, u)?),
            true => Stmt::Semi({
                if ctx.regard_semantics {
                    let ty = pick_type(ctx, u)?;
                    ty_unambigous!(ctx, with_type!(ctx, ty, c_arbitrary(ctx, u)?))
                } else {
                    c_arbitrary(ctx, u)?
                }
            }, parse_quote!(;)),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Local {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let pat;
        let init;
        // TODO: allow uninitialized variables
        if ctx.regard_semantics {
            let type_specified = Arbitrary::arbitrary(u)?;
            let (maybe_lt, ty) = pick_type(ctx, u)?.assign_fresh_lt(ctx);
            let (pat_tmp, vars) = with_attrs!(ctx {
                allow_ambiguity = type_specified,
                is_refutable = false,
                is_top_pattern = true
                // reserved_names = HashSet::new()
            }, pattern_of_type(ctx, u, &ty)?);
            pat = pat_tmp;
            init = Some((parse_quote!(=), with_attrs!(ctx {
                expected_type = ty.clone(),
                allow_ambiguity = type_specified
            }, c_arbitrary(ctx, u)?)));
            for (name, var) in vars {
                maybe_lt.as_ref().map(|lt| constrain_lt(ctx, &var.lifetime, &lt));
                let lts = var.ty.sub_lts().cloned().collect::<Vec<_>>();
                let var_lt = var.lifetime.clone();
                add_var(ctx, vec![name], var);
                lts.into_iter().for_each(|lt| {
                    if is_alive(ctx, &lt) {
                        constrain_lt(ctx, &var_lt, &lt)
                    } else {
                        end_lifetime(ctx, &var_lt);
                    }
                });
            }
        } else {
            pat = irrefutable!(ctx, c_arbitrary(ctx, u)?);
            init = lazy_maybe!(u, (parse_quote!(=), c_arbitrary(ctx, u)?));
        }
        Ok(Local {
            attrs: vec![],
            pat,
            let_token: parse_quote!(let),
            init,
            semi_token: parse_quote!(;),
        })
    }
}

fn pattern_of_type<'a, 'b>(
    ctx: &mut Context,
    u: &mut Unstructured<'a>,
    ty: &'b sem::Type
) -> Result<(Pat, Vec<(StringWrapper, Variable)>)> {
    //! NOTE: adds pattern names to reserved_variables
    let needs_type_pat = ctx.allow_ambiguity && ctx.is_top_pattern;
    guarded_lazy_choose!(u, {
        ctx.is_top_pattern => {
            let (pat, sub_vars) = sub_pattern!(ctx, pattern_of_type(ctx, u, ty)?);
            (Pat::Type(PatType {
                attrs: vec![],
                colon_token: parse_quote!(:),
                pat: Box::new(pat),
                ty: Box::new(ty.clone().into())
            }), sub_vars)
        },
        !needs_type_pat => {
            let pat: PatIdent = c_arbitrary(ctx, u)?;
            let name: StringWrapper = ident_to_name(&pat.ident).into();
            ctx.reserved_names.insert(name.clone());
            let mutability = match pat.mutability {
                Some(_) => sem::Mutability::Mutable,
                None => sem::Mutability::Immutable,
            };
            (Pat::Ident(pat), vec![(name, Variable::new(
                ty.clone(), mutability, fresh_lt(ctx)
            ))])
        },
        !needs_type_pat => (Pat::Wild(PatWild{
            attrs:vec![],
            underscore_token:parse_quote!(_)
        }), vec![]),
        !needs_type_pat && ty.name.last().unwrap().starts_with("#Tuple") => {
            // TODO: it's currently possible to have tuple fields shadow eachother
            let mut fields = vec![];
            let mut sub_vars = vec![];
            // TODO: do we need to end the lifetime of the thing being destructured?
            for type_arg in &ty.type_args {
                let (field, sub_var) = sub_pattern!(ctx, pattern_of_type(ctx, u, type_arg)?);
                fields.push(field);
                sub_vars.extend(sub_var);
            }
            let tuple = Pat::Tuple(PatTuple {
                attrs: vec![],
                paren_token: Paren { span: dummy_span() },
                elems: fields.into_iter().collect()
            });
            (tuple, sub_vars)
        },
        // !needs_type_pat && ty.name == "#Ref" => {
        //     let (inner, sub_vars) = sub_pattern!(ctx, pattern_of_type(ctx, u, &ty.type_args[0])?);
        //     (Pat::Reference(PatReference{
        //         attrs: vec![],
        //         and_token: parse_quote!(&),
        //         // TODO: check if this can be mutable
        //         mutability: None,
        //         pat: Box::new(inner)
        //     }), sub_vars)
        // }
    })
    // TODO: add more patterns
}

impl<'a> ContextArbitrary<'a, Context> for Expr {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        if ctx.regard_semantics {
            // println!("Expected type: {:?}", ctx.expected_type);
            // Blocks can't contain references that outlive them
            if ctx.expected_type.has_lts() {
                let expr = construct_value(ctx, u, ctx.expected_type.clone(), ctx.allow_ambiguity)?;
                from_sem_expr(ctx, u, &expr)
            } else { 
                weighted_lazy_choose!(u, {
                    10 => {
                        let expr = construct_value(
                            ctx, u,
                            ctx.expected_type.clone(),
                            ctx.allow_ambiguity
                        )?;
                        from_sem_expr(ctx, u, &expr)?
                    },
                    1 => Expr::If(c_arbitrary(ctx, u)?),
                    1 => Expr::Block(c_arbitrary(ctx, u)?)
                })
            }
        } else {
            let place_expr = !ctx.regard_semantics || ctx.is_place_expression;
            let within_depth = ctx.depth < MAX_DEPTH;
            ctx.depth += 1;
        let expr = guarded_lazy_choose!(u, {
            within_depth => Expr::Array(c_arbitrary(ctx, u)?),
            within_depth => Expr::Tuple(c_arbitrary(ctx, u)?),
            within_depth => Expr::Path(c_arbitrary(ctx, u)?),
            within_depth => Expr::Binary(c_arbitrary(ctx, u)?),
            within_depth => Expr::Assign(c_arbitrary(ctx, u)?),
            within_depth => Expr::AssignOp(c_arbitrary(ctx, u)?),
            within_depth => Expr::Await(c_arbitrary(ctx, u)?),
            within_depth => Expr::Block(c_arbitrary(ctx, u)?),
            within_depth => Expr::Box(c_arbitrary(ctx, u)?),
            within_depth => Expr::Call(c_arbitrary(ctx, u)?),
            within_depth => Expr::Cast(c_arbitrary(ctx, u)?),
            within_depth => Expr::Closure(c_arbitrary(ctx, u)?),
            within_depth => Expr::Group(c_arbitrary(ctx, u)?),
            within_depth => Expr::If(c_arbitrary(ctx, u)?),
            within_depth => Expr::Loop(c_arbitrary(ctx, u)?),
            within_depth => Expr::Match(c_arbitrary(ctx, u)?),
            within_depth => Expr::MethodCall(c_arbitrary(ctx, u)?),
            within_depth => Expr::Paren(c_arbitrary(ctx, u)?),
            within_depth => Expr::Range(c_arbitrary(ctx, u)?),
            within_depth => Expr::Reference(c_arbitrary(ctx, u)?),
            within_depth => Expr::Repeat(c_arbitrary(ctx, u)?),
            within_depth && !ctx.regard_semantics => Expr::Struct(c_arbitrary(ctx, u)?),
            within_depth => Expr::Try(c_arbitrary(ctx, u)?),
            within_depth => Expr::Unary(c_arbitrary(ctx, u)?),
            within_depth => Expr::Async(c_arbitrary(ctx, u)?),
            within_depth => Expr::Break(c_arbitrary(ctx, u)?),
            within_depth => Expr::Continue(c_arbitrary(ctx, u)?),
            within_depth => Expr::ForLoop(c_arbitrary(ctx, u)?),
            // only used in if let and while let expressions
            // true => Expr::Let(c_arbitrary(ctx, u)?),
            within_depth => Expr::TryBlock(c_arbitrary(ctx, u)?),
            within_depth => Expr::While(c_arbitrary(ctx, u)?),
            // Generators are still an unstable feature
            // true => Expr::Yield(c_arbitrary(ctx, u)?),
            within_depth => Expr::Return(c_arbitrary(ctx, u)?),
        
            // Unsafe(ExprUnsafe),
            within_depth && place_expr => Expr::Index(c_arbitrary(ctx, u)?),
            within_depth && place_expr => Expr::Field(c_arbitrary(ctx, u)?),
            // deref
            within_depth && place_expr => Expr::Unary(ExprUnary {
                attrs: vec!(),
                op: parse_quote!(*),
                // In case this is selected when regard semantics is false
                expr: place_expression!(ctx, c_arbitrary(ctx, u)?)
            }),
            true => Expr::Lit(c_arbitrary(ctx, u)?),
            place_expr => local_var_expr(ctx, u)?,
            })?;
            ctx.depth -= 1;
            // Place expressions do not need to be paranthesized ordinarily,
            // but since we can generate arbitrary expressions when we disregard 
            // semantics we need to parenthesize them in that case
            if !ctx.regard_semantics && ctx.is_place_expression {
                return Ok(parenthesize_expr(12, expr));
            } else {
                return Ok(expr);
            }
        }
    }
}

fn local_var_expr<'a>(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Expr> {

    let path = ExprPath {
        attrs: vec!(),
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments: iter::once(Ok(c_arbitrary::<Context, PathSegment>(ctx, u)?))
                .collect::<Result<Punctuated<PathSegment, Token![::]>>>()?
        }
    };
    Ok(Expr::Path(path))
}

fn let_guard<'a>(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Box<Expr>> {
    Ok(parens_block!(ctx, Expr::Let(c_arbitrary(ctx, u)?)))
}

impl<'a> ContextArbitrary<'a, Context> for ExprYield {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprYield {
            attrs: vec![],
            yield_token: parse_quote!(yield),
            expr: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprWhile {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprWhile {
            attrs: vec![],
            label: c_arbitrary(ctx, u)?,
            while_token: parse_quote!(while),
            cond: if Arbitrary::arbitrary(u)? {
                parens_block!(ctx, c_arbitrary(ctx, u)?)
            } else {
                let_guard(ctx, u)?
            },
            body: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprParen {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprParen {
            attrs: vec![],
            paren_token: Paren { span: dummy_span() },
            expr: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprUnary {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let op = c_arbitrary(ctx, u)?;
        let expr = if op == parse_quote!(*) {
            place_expression!(ctx, c_arbitrary(ctx, u)?)
        } else {
            parens_ex!(13, ctx, c_arbitrary(ctx, u)?)
        };
        Ok(ExprUnary {
            attrs: vec![],
            op,
            expr
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for UnOp {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let ops: [UnOp; 3] = [parse_quote!(!), parse_quote!(-), parse_quote!(*)];
        let op = u.choose(&ops)?;
        return Ok(*op);
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprType {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprType {
            attrs: vec![],
            expr: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            ty: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprTryBlock {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprTryBlock {
            attrs: vec![],
            try_token: parse_quote!(try),
            block: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprTry {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprTry {
            attrs: vec![],
            expr: parens_ex!(14, ctx, c_arbitrary(ctx, u)?),
            question_token: parse_quote!(?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprStruct {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        if ctx.regard_semantics {
            // TODO: add unnamed fields
            todo!();
            // let (path, fields) = pick_stuct_that();
            // Ok(ExprStruct {
            //     attrs: vec![],
            //     path,
            //     brace_token: Brace { span: dummy_span() },
            //     fields: (),
            //     dot2_token: (),
            //     rest: (),
            // })
        } else {
            let named_fields: bool = Arbitrary::arbitrary(u)?;
            let rest: Option<Box<Expr>> = place_expression!(ctx, c_arbitrary(ctx, u)?);
            let dot2_token = if rest.is_some() { parse_quote!(..) } else { None };
            Ok(ExprStruct {
                attrs: vec![],
                path: c_arbitrary(ctx, u)?,
                brace_token: Brace { span: dummy_span() },
                fields: name_fields!(ctx, named_fields, c_arbitrary(ctx, u)?),
                dot2_token,
                rest
            })
        }
    }
}

impl<'a> ContextArbitrary<'a, Context> for FieldValue {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(FieldValue {
            attrs: vec![],
            member: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            expr: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprReturn {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprReturn {
            attrs: vec![],
            return_token: parse_quote!(return),
            expr: rhs!(ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprRepeat {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        // TOOD: substitute this with a propper way to generate static expressions
        let enum_choices = [
        |ctx, u| ok(Expr::Lit(c_arbitrary(ctx, u)?)),
        |ctx, u| ok(Expr::Path(c_arbitrary(ctx, u)?)),
        ];
        let choice = u.choose(&enum_choices)?;
        let len = Box::new(choice(ctx, u)?);

        Ok(ExprRepeat {
            attrs: vec![],
            bracket_token: Bracket { span: dummy_span() },
            expr: c_arbitrary(ctx, u)?,
            semi_token: parse_quote!(;),
            len
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprReference {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprReference {
            attrs: vec![],
            and_token: parse_quote!(&),
            raw: Default::default(),
            mutability: maybe(u, parse_quote!(mut)),
            expr: parens_ex!(13, ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprRange {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        // TODO: what prescedence do the left and right arms really need?
        let from = lazy_maybe!(u, parens_ex!(3, ctx, c_arbitrary(ctx, u)?));
        let limits = c_arbitrary(ctx, u)?;
        // Closed ranges must have a `to`
        let to = match limits {
            RangeLimits::Closed(_) => Some(parens_ex!(3, ctx, c_arbitrary(ctx, u)?)),
            RangeLimits::HalfOpen(_) => lazy_maybe!(u, parens_ex!(2, ctx, c_arbitrary(ctx, u)?))
        };
        Ok(ExprRange {
            attrs: vec![],
            from,
            limits,
            to,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprMethodCall {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprMethodCall {
            attrs: vec![],
            receiver: parens_ex!(17, ctx, c_arbitrary(ctx, u)?),
            dot_token: parse_quote!(.),
            method: c_arbitrary(ctx, u)?,
            turbofish: c_arbitrary(ctx, u)?,
            paren_token: Paren { span: dummy_span() },
            args: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for MethodTurbofish {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(MethodTurbofish {
            colon2_token: parse_quote!(::),
            lt_token: parse_quote!(<),
            args: unwrap_nep(c_arbitrary(ctx, u)?),
            gt_token: parse_quote!(>),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for GenericMethodArgument {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
lazy_choose!(u, {
    // https://github.com/rust-lang/rfcs/blob/master/text/2000-const-generics.md
    // const generics arguments need to be either 
    //  * variables,
    //  * literals,
    //  * blocks (w/o labels)
    GenericMethodArgument::Const(is_const!(ctx, lazy_choose!(u, {
        Expr::Path(c_arbitrary(ctx, u)?),
        Expr::Lit(c_arbitrary(ctx, u)?),
        Expr::Block(no_block_labels!(ctx, c_arbitrary(ctx, u)?))
    })?)),
    GenericMethodArgument::Type(c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprMatch {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let mut arms = unwrap_nev(c_arbitrary(ctx, u)?);
        // remove trailing comma
        if Arbitrary::arbitrary(u)? {
            arms.last_mut().map(|a: &mut Arm| a.comma = None);
        }
        Ok(ExprMatch {
            attrs: vec![],
            match_token: parse_quote!(match),
            expr: parens_block!(ctx, c_arbitrary(ctx, u)?),
            brace_token: Brace { span: dummy_span() },
            arms
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Arm {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let body = c_arbitrary(ctx, u)?;
        let comma = if match body {
            // things ending in blocks don't need a comma
            Expr::Block(_) | Expr::Loop(_) | Expr::ForLoop(_) | Expr::If(_) |
                Expr::TryBlock(_) => true,
            _ => false
        } {
            maybe(u, parse_quote!(,))
        } else {
            Some(parse_quote!(,))
        };
        Ok(Arm {
            attrs: vec![],
            pat: c_arbitrary(ctx, u)?,
            // TODO: Just a guess, but the prescedence should be higher than the binaryor operator
            guard: lazy_maybe!(u, (parse_quote!(if), parens_ex!(8, ctx, c_arbitrary(ctx, u)?))),
            fat_arrow_token: parse_quote!(=>),
            body: Box::new(body),
            comma
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprLoop {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprLoop {
            attrs: vec![],
            label: c_arbitrary(ctx, u)?,
            loop_token: parse_quote!(loop),
            body: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprLet {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprLet {
            attrs: vec![],
            let_token: parse_quote!(let),
            pat: c_arbitrary(ctx, u)?,
            eq_token: parse_quote!(=),
            expr: rhs!(ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprIndex {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprIndex {
            attrs: vec![],
            expr: parens_ex!(15, ctx, c_arbitrary(ctx, u)?),
            bracket_token: Bracket { span: dummy_span() },
            index: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprIf {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let then_branch;
        let else_branch;
        let cond = if !ctx.regard_semantics && Arbitrary::arbitrary(u)? {
            let_guard(ctx, u)?
        } else {
            with_type!(ctx, make_type!(bool), parens_block!(ctx, c_arbitrary(ctx, u)?))
        };
        branches!(ctx, {
            then_branch = not_fn_block!(ctx, no_block_labels!(ctx, c_arbitrary(ctx, u)?));
            ctx.branches.as_ref().unwrap().borrow_mut().new_branch();
            else_branch = guarded_lazy_choose!(u, {
                !ctx.regard_semantics ||
                    ctx.expected_type.name == vec![StringWrapper::from("#Unit")] => None,
                true => Some((
                    parse_quote!(else),
                    Box::new(not_fn_block!(ctx, no_block_labels!(ctx, lazy_choose!(u, {
                        Expr::Block(c_arbitrary(ctx, u)?),
                        Expr::If(c_arbitrary(ctx, u)?)
                    })?)))
                ))
            })?
        });
        Ok(ExprIf {
            attrs: vec![],
            if_token: parse_quote!(if),
            cond,
            then_branch, 
            else_branch,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprGroup {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprGroup {
            attrs: vec![],
            group_token: Group { span: dummy_span() },
            expr: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprForLoop {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprForLoop {
            attrs: vec![],
            label: c_arbitrary(ctx, u)?,
            for_token: parse_quote!(for),
            pat: irrefutable!(ctx, c_arbitrary(ctx, u)?),
            in_token: parse_quote!(in),
            // NOTE: expr can't contain a type
            expr: parens_block!(ctx, no_annotations!(ctx, c_arbitrary(ctx, u)?)),
            body: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprField {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprField {
            attrs: vec![],
            base: parens_ex!(16, ctx, c_arbitrary(ctx, u)?),
            dot_token: parse_quote!(.),
            member: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Member {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
lazy_choose!(u,  {
    Member::Named(c_arbitrary(ctx, u)?),
    Member::Unnamed(c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprContinue {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprContinue {
            attrs: vec![],
            continue_token: parse_quote!(continue),
            label: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprClosure {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let output = c_arbitrary(ctx, u)?;
        Ok(ExprClosure {
            attrs: vec![],
            asyncness: maybe(u, parse_quote!(async)),
            // It's unclear if this feature is actually implemented, dissabling for now
            movability: None, // maybe(u, parse_quote!(static)),
            capture: maybe(u, parse_quote!(move)),
            or1_token: parse_quote!(|),
            inputs: irrefutable!(ctx, c_arbitrary(ctx, u)?),
            or2_token: parse_quote!(|),
            // body needs to be a block if the closure has an output type
            body: if ReturnType::Default == output {
                no_block_labels!(ctx, c_arbitrary(ctx, u)?)
            } else {
                Box::new(Expr::Block(no_block_labels!(ctx, c_arbitrary(ctx, u)?)))
            },
            output
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprCast {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprCast {
            attrs: vec![],
            expr: parens_ex!(13, ctx, c_arbitrary(ctx, u)?),
            as_token: parse_quote!(as),
            ty: parens_ty!(ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprCall {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprCall {
            attrs: vec![],
            func: parens_ex!(15, ctx, c_arbitrary(ctx, u)?),
            paren_token: Paren { span: dummy_span() },
            args: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprBreak {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprBreak {
            attrs: vec![],
            break_token: parse_quote!(break),
            label: c_arbitrary(ctx, u)?,
            expr: lazy_maybe!(u, Box::new(parenthesize_label(c_arbitrary(ctx, u)?))),
        })
    }
}

fn make_print(ctx: &Context, u: &mut Unstructured, name: Path, _ty: &sem::Type) -> Result<Stmt> {
    let ident = from_sem_path(ctx, u, &name)?;
    Ok(Stmt::Semi(Expr::Macro(ExprMacro {
        attrs: vec![],
        mac: syn::Macro {
            path: parse_quote!(println),
            bang_token: parse_quote!(!),
            delimiter: MacroDelimiter::Paren(Paren { span: dummy_span() }),
            tokens: quote!("{:?}", #ident)
        }
    }), parse_quote!(;)))
}

impl<'a> ContextArbitrary<'a, Context> for ExprBlock {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprBlock {
            attrs: vec![],
            label: guarded_lazy_choose!(u, {
                true => None,
                // requires #![feature(label_break_value)]
                !ctx.regard_semantics && ctx.allow_block_labels => c_arbitrary(ctx, u)?
            })?,
            block: not_fn_block!(ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Label {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Label {
            name: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprBox {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprBox {
            attrs: vec![],
            box_token: parse_quote!(box),
            // TODO: what is the prescedence of box?
            expr: parens_ex!(u8::MAX, ctx, c_arbitrary(ctx, u)?),
        })
    }
}
impl<'a> ContextArbitrary<'a, Context> for ExprAwait {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprAwait {
            attrs: vec![],
            await_token: parse_quote!(await),
            dot_token: parse_quote!(.),
            base: parens_ex!(14, ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprAsync {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprAsync {
            attrs: vec![],
            async_token: parse_quote!(async),
            capture: maybe(u, parse_quote!(move)),
            block: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprAssign {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprAssign {
            attrs: vec![],
            left: place_expression!(ctx, c_arbitrary(ctx, u)?),
            eq_token: parse_quote!(=),
            right: parens_ex!(2, ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprAssignOp {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
let op = lazy_choose!(u,  {
    BinOp::AddEq(parse_quote!(+=)),
    BinOp::SubEq(parse_quote!(-=)),
    BinOp::MulEq(parse_quote!(*=)),
    BinOp::DivEq(parse_quote!(/=)),
        })?;
        Ok(ExprAssignOp {
            attrs: vec![],
            left: place_expression!(ctx, c_arbitrary(ctx, u)?),
            right: c_arbitrary(ctx, u)?,
            op,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprBinary {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let op = c_arbitrary(ctx, u)?;
        let (left_assoc, right_assoc) = binary_associativity(&op);
        Ok(ExprBinary {
            attrs: vec![],
            left: parens_ex!(binary_prescedence(&op) + left_assoc, ctx, c_arbitrary(ctx, u)?),
            op,
            right: parens_ex!(binary_prescedence(&op) + right_assoc, ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for BinOp {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        lazy_choose!(u,  {
            parse_quote!(+),
            parse_quote!(*),
            parse_quote!(/),
            parse_quote!(%),
            parse_quote!(-),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprLit {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprLit {
            attrs: vec![],
            lit: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprArray {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprArray {
            attrs: vec![],
            bracket_token: Bracket { span: dummy_span() },
            elems: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprTuple {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprTuple {
            attrs: vec![],
            elems: unwrap_nep(c_arbitrary(ctx, u)?),
            paren_token: Paren { span: dummy_span() },
        })
    }
}


impl<'a> ContextArbitrary<'a, Context> for ExprPath {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let path; 
        if ctx.regard_semantics {
            let expected_type = ctx.expected_type.clone();
            let (var, _) = sem::pick_var_that(
                ctx, u,
                |_,v| v.ty.matches(&expected_type)
            )?;
            let var = var.clone();
            path = from_sem_path(ctx, u, &var)?;
        } else {
            path = c_arbitrary(ctx, u)?;
        }
        Ok(ExprPath {
            attrs: vec![],
            qself: None,
            path
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Lit {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
    lazy_choose!(u,  {
        { let data: u32 = Arbitrary::arbitrary(u)?;
            parse_quote!(#data) },
        { let data: String = Arbitrary::arbitrary(u)?;
            parse_quote!(#data) },
        { let data: f64 = unwrap_finite_f64(Arbitrary::arbitrary(u)?);
            // parse_quote! requires that the float is finite
            parse_quote!(#data) },
        { let data: bool = Arbitrary::arbitrary(u)?;
            parse_quote!(#data) },
            })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Generics {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Generics {
            lt_token: parse_quote!(<),
            params: c_arbitrary(ctx, u)?,
            gt_token: parse_quote!(>),
            where_clause: None,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for GenericParam {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        guarded_lazy_choose!(u, {
            true => GenericParam::Type(c_arbitrary(ctx, u)?),
            true => GenericParam::Lifetime(c_arbitrary(ctx, u)?),
            !ctx.regard_semantics => GenericParam::Const(c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeParam {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let bounds: Punctuated<TypeParamBound, Token![+]> = guarded_lazy_choose!(u, {
            !ctx.regard_semantics => c_arbitrary(ctx, u)?,
            true => iter::empty::<TypeParamBound>().collect()
        })?;
        let r#default = guarded_lazy_choose!(u, {
            !ctx.regard_semantics => Some(c_arbitrary(ctx, u)?),
            true => None
        })?;

        Ok(TypeParam {
            attrs: vec![],
            ident: c_arbitrary(ctx, u)?,
            colon_token: if bounds.len() > 0 { parse_quote!(=) } else { None }, 
            bounds,
            eq_token: default.as_ref().map(|_| parse_quote!(=)),
            r#default,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for LifetimeDef {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let bounds: Punctuated<Lifetime, Token![+]> = guarded_lazy_choose!(u, {
            !ctx.regard_semantics => c_arbitrary(ctx, u)?,
            true => iter::empty::<Lifetime>().collect()
        })?;
        Ok(LifetimeDef {
            attrs: vec![],
            lifetime: c_arbitrary(ctx, u)?,
            colon_token: if bounds.len() > 0 { parse_quote!(=) } else { None }, 
            bounds,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ConstParam {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ConstParam {
            attrs: vec![],
            const_token: parse_quote!(const),
            ident: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            ty: c_arbitrary(ctx, u)?,
            eq_token: parse_quote!(=),
            default: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Lifetime {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Lifetime {
            apostrophe: dummy_span(),
            ident: lifetime!(ctx, c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for syn::Path {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(syn::Path {
            leading_colon: maybe(u, parse_quote!(::)),
            segments: unwrap_nep(c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PathSegment {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PathSegment {
            ident: c_arbitrary(ctx, u)?,
arguments: guarded_lazy_choose!(u, {
    true => PathArguments::None,
    !ctx.no_generics => PathArguments::AngleBracketed(c_arbitrary(ctx, u)?),
    // TODO: make sure that Fn(..) and FnOnce(..) type paths can be 
    // generated where appropriate
    // PathArguments::Parenthesized(c_arbitrary(ctx, u)?)
            })?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for AngleBracketedGenericArguments {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(AngleBracketedGenericArguments {
            // NOTE: always generates turbofish arguments
            colon2_token: parse_quote!(::),
            lt_token: parse_quote!(<),
            args: c_arbitrary(ctx, u)?,
            gt_token: parse_quote!(>)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for GenericArgument {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
lazy_choose!(u, {
    GenericArgument::Lifetime(c_arbitrary(ctx, u)?),
    GenericArgument::Type(c_arbitrary(ctx, u)?),
    GenericArgument::Binding(c_arbitrary(ctx, u)?),
    GenericArgument::Constraint(c_arbitrary(ctx, u)?),
    GenericArgument::Const(is_const!(ctx, lazy_choose!(u, {
        Expr::Path(c_arbitrary(ctx, u)?),
        Expr::Lit(c_arbitrary(ctx, u)?),
        Expr::Block(no_block_labels!(ctx, c_arbitrary(ctx, u)?))
    })?)),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Binding {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Binding {
            ident: c_arbitrary(ctx, u)?,
            eq_token: parse_quote!(=),
            ty: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Constraint {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Constraint {
            ident: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            bounds: c_arbitrary(ctx, u)?
        })
    }
}

lazy_static! {

    static ref KWDS_STRICT: HashSet<&'static str> = ["as", "break", "const", "continue", "crate", "else", "enum", "extern", "crate", "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return", "self", "Self", "static", "struct", "trait", "true", "type", "unsafe", "use", "where", "while", "await", "dyn", "abstract", "become", "box", "do", "final", "macro", "override", "priv", "typeof", "unsized", "virtual", "yield"].iter().cloned().collect();

    static ref U_XID_START: Vec<char> =
    String::from_utf8(std::fs::read("./unicode/xid_start.txt").expect("Error reading file of unicode characters"))
        .expect("Error parsing file of unicode characters")
        .chars()
        .filter(|c| c != &'\n')
        .collect();
    static ref U_XID_CONTINUE: Vec<char> =
    String::from_utf8(std::fs::read("./unicode/xid_continue.txt").expect("Error reading file of unicode characters"))
        .expect("Error parsing file of unicode characters")
        .chars()
        .filter(|c| c != &'\n')
        .collect();
    // For testing shadowing-related bugs
    // static ref ALPHA_: Vec<char>  = "O".chars().collect();
    // static ref ALPHA_NUM: Vec<char> = "k".chars().collect();
    static ref ALPHA_: Vec<char>  = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_".chars().collect();
    static ref ALPHA_NUM: Vec<char> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789".chars().collect();

}

// TODO: handle the "union" soft keyword
impl<'a> ContextArbitrary<'a, Context> for Ident {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {

        /// You are usually not allowed to shadow tuple structs. We are only interested
        /// in single-segment tuple structs since you can't shadow qualified paths
        fn get_tuple_structs(ctx: &Context) -> Vec<&str> {
            let mut one_segment_aliases = HashMap::new();
            for scope in ctx.scopes.iter() {
                for (path, aliases) in scope.path_aliases.iter() {
                    one_segment_aliases.insert(
                        path,
                        aliases.iter().filter(|a| a.len() == 1).collect::<Vec<_>>()
                    );
                }
            }
            let mut tuple_structs: Vec<&str> = vec![];
            for scope in ctx.scopes.iter() {
                // println!("Reserved: {:?}", scope.reserved);
                for (name, (_ty, fields)) in scope.structs.iter() {
                    if let sem::Fields::Unnamed(_) = fields.as_ref() {
                        if name.len() == 1 {
                            tuple_structs.push(name[0].as_str());
                        }
                        if let Some(aliases) = one_segment_aliases.get(name) {
                            tuple_structs.extend(aliases.iter().map(|a| a[0].as_str()));
                        }
                    }
                }
                for (ty, is_tuple_struct) in scope.reserved.structs.iter() {
                    // println!("Reserved struct found! {},{}", ty, is_tuple_struct);
                    if *is_tuple_struct {
                        let name = &ty.name;
                        if name.len() == 1 {
                            tuple_structs.push(name[0].as_str());
                        }
                        if let Some(aliases) = one_segment_aliases.get(name) {
                            tuple_structs.extend(aliases.iter().map(|a| a[0].as_str()));
                        }
                    }
                }
                for (name, (_ty, variants)) in scope.enums.iter() {
                    for (var_name, fields) in variants {
                        if let sem::Fields::Unnamed(_) = fields.as_ref() {
                            let mut full_path = name.clone();
                            full_path.push(var_name.clone());
                            if let Some(aliases) = one_segment_aliases.get(name) {
                                tuple_structs.extend(aliases.iter().map(|a| a[0].as_str()));
                            }
                        }
                    }
                }
                for (ty, variants) in scope.reserved.enums.iter() {
                    let name = &ty.name;
                    for (var_name, is_tuple_struct) in variants {
                        if *is_tuple_struct {
                            let mut full_path = name.clone();
                            full_path.push(var_name.clone());
                            if let Some(aliases) = one_segment_aliases.get(name) {
                                tuple_structs.extend(aliases.iter().map(|a| a[0].as_str()));
                            }
                        }
                    }
                }
            }
            tuple_structs
        }

        let mut name: String = String::new();
        let fst_chars: &Vec<char>;
        let rest_chars: &Vec<char>;
        if ctx.regard_semantics && !ctx.non_ascii {
            fst_chars = &*ALPHA_;
            rest_chars = &*ALPHA_NUM;
        } else {
            fst_chars = &*U_XID_START;
            rest_chars = &*U_XID_CONTINUE;
        }
        let fst_char: char = *u.choose(fst_chars)?;
        name.push(fst_char);
        // let min_l = if fst_char == '_' { 1 } else { 0 };
        let min_l = 2;
        let max_l = 2;
        for _ in 0..(u.int_in_range(min_l..=max_l)?) {
            let char = *u.choose(rest_chars)?;
            name.push(char);
        }
        let tuple_structs = if ctx.is_lifetime {
            vec![]
        } else {
            // vec![]
            get_tuple_structs(ctx)
        };
        while ctx.reserved_names.contains(&name.clone().into()) || 
            // Tuple structs can't be shadowed
            (!ctx.is_lifetime && ctx.regard_semantics && tuple_structs.contains(&name.as_str())) || 
            // Lifetimes can't have raw variables, so we can't just make
            // it a raw variable if it is a keyword
            ctx.is_lifetime && (KWDS_STRICT.contains(&&name[..]) || name == "static") {
            let char = *u.choose(rest_chars)?;
            name.push(char);
        }
        let name_token: Ident = if !ctx.is_lifetime && (KWDS_STRICT.contains(&&name[..]) || Arbitrary::arbitrary(u)?) {
            parse_str(&format!("r#{}", name)).expect(format!("Failed parsing raw identifier {}", name).as_str())
        } else {
            name_to_ident(u, &name)?
        };
        return Ok(name_token);
    }
}
