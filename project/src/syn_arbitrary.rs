use arbitrary::Arbitrary;
use proc_macro2::TokenStream;
use std::{collections::{HashMap, HashSet}, iter::{self, empty}, rc::Rc};
use syn::{Abi, AngleBracketedGenericArguments, Arm, AttrStyle, Attribute, BareFnArg, BinOp, Binding, Block, Constraint, Expr, ExprArray, ExprAssign, ExprAssignOp, ExprAsync, ExprAwait, ExprBinary, ExprBlock, ExprBox, ExprBreak, ExprCall, ExprCast, ExprClosure, ExprContinue, ExprField, ExprForLoop, ExprGroup, ExprIf, ExprIndex, ExprLet, ExprLit, ExprLoop, ExprMacro, ExprMatch, ExprMethodCall, ExprParen, ExprPath, ExprRange, ExprReference, ExprRepeat, ExprReturn, ExprStruct, ExprTry, ExprTryBlock, ExprTuple, ExprType, ExprUnary, ExprWhile, ExprYield, Field, FieldPat, FieldValue, Fields, FieldsNamed, FieldsUnnamed, File, FnArg, GenericArgument, GenericMethodArgument, GenericParam, Generics, Ident, Index, Item, ItemConst, ItemEnum, ItemFn, ItemStatic, ItemStruct, ItemTrait, ItemType, Label, Lifetime, LifetimeDef, Lit, LitStr, Local, MacroDelimiter, Member, MethodTurbofish, Pat, PatBox, PatIdent, PatLit, PatOr, PatPath, PatRange, PatReference, PatRest, PatSlice, PatStruct, PatTuple, PatTupleStruct, PatType, PatWild, PathArguments, PathSegment, RangeLimits, Receiver, ReturnType, Signature, Stmt, Token, TraitBound, TraitBoundModifier, TraitItem, TraitItemConst, TraitItemMethod, TraitItemType, Type, TypeArray, TypeBareFn, TypeGroup, TypeImplTrait, TypeInfer, TypeNever, TypeParam, TypeParamBound, TypeParen, TypePath, TypePtr, TypeReference, TypeSlice, TypeTraitObject, TypeTuple, UnOp, Variadic, Variant, VisPublic, VisRestricted, Visibility, parse_quote, parse_str, punctuated::Punctuated, token::{Brace, Bracket, Group, Paren}};
use quote::quote;
use lazy_static::lazy_static;
use std::mem;

use super::context_arbitrary::*;
use super::context_arbitrary as context_arbitrary;
use super::context_arbitrary::Result;

use super::context::*;

use super::semantics as semantics;
use super::semantics::*;



const MAX_DEPTH: usize = 20;

pub struct WrappedFile(pub File);
unsafe impl Send for WrappedFile {}
pub fn make_wrapped_file<'a>(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<WrappedFile> {
    Ok(WrappedFile(c_arbitrary(ctx, u)?))
}

// to help with type inference
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

macro_rules! guarded_lazy_choose {
    ($u:ident, { $($guard:expr => $choice:expr),*$(,)? }) => {
        (|| {
            let mut len = 0;
            let mut conds = vec![];
            $( 
                conds.push($guard);
                if *conds.last().unwrap() {
                    len += 1;
                }
            )*
            let choice = $u.int_in_range(0..=(len-1))?;
            let mut clause = 0;
            let mut i = 0;
            $(
                if conds[i] && choice == clause {
                    return Ok($choice); 
                }
                if conds[i] {
                    clause += 1;
                }
                i += 1;
            )*
            // purely to supress warnings about the variables being set but never read
            std::mem::drop(clause);
            std::mem::drop(i);
            return Err(GenerationError::ArbitraryError(arbitrary::Error::EmptyChoose))
        })()
    };
}

macro_rules! weighted_lazy_choose {
    ($u:ident, { $($weight:expr => $choice:expr),*$(,)? }) => {
        (|| {
            let mut ranges = vec![];
            let mut sum = 0;

            $({
                let old_sum = sum;
                sum += $weight;
                ranges.push(old_sum..sum)
            })*

            let num = $u.int_in_range(0..=sum-1)?;

            let mut i = 0;
            $(
                if ranges[i].contains(&num) {
                    return Ok($choice);
                }
                i += 1;
            )*
            std::mem::drop(i);
            return Err(GenerationError::ArbitraryError(arbitrary::Error::EmptyChoose))
        })()
    }
}

macro_rules! lazy_choose {
    ($u:ident, { $($choice:expr),*$(,)? })
    => (guarded_lazy_choose!($u, { $(true => $choice),*}))
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
    // TODO: Make the code deal with left and right associative operators propperly
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
        Expr::Binary(op) => binary_prescedence(op.op),
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

fn binary_prescedence(op: BinOp) -> u8 {
    match op {
        BinOp::Mul(_) | BinOp::Div(_) | BinOp::Rem(_) => 11,
        BinOp::Add(_) | BinOp::Sub(_) => 10,
        BinOp::Shl(_) | BinOp::Shr(_) => 9,
        BinOp::BitAnd(_) => 8,
        BinOp::BitXor(_) => 7,
        BinOp::BitOr(_) => 6,
        BinOp::Lt(_) | BinOp::Le(_) | BinOp::Gt(_) | BinOp::Ge(_) | BinOp::Ne(_) | BinOp::Eq(_) => 5,
        BinOp::And(_) => 4,
        BinOp::Or(_) => 3,
        BinOp::AddEq(_) | BinOp::SubEq(_) | BinOp::MulEq(_) | BinOp::DivEq(_) | BinOp::RemEq(_) | BinOp::BitXorEq(_) | BinOp::BitAndEq(_) | BinOp::BitOrEq(_) | BinOp::ShlEq(_) | BinOp::ShrEq(_) => 1
    }
}


fn parenthesize_block(e: Expr) -> Expr {
    match e {
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
    }
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

impl From<semantics::Type> for syn::Type {
    fn from(ty: semantics::Type) -> Self {
        if ty.name.as_str().starts_with("#Tuple") {
            syn::Type::Tuple(TypeTuple {
                paren_token: Paren { span: dummy_span() },
                elems: ty.type_args.into_iter().map::<syn::Type,_>(From::from).collect()
            })
        } else if let Some(func) = ty.func {
            syn::Type::BareFn(TypeBareFn {
                // TODO: add more fields
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
        } else if ty.name == "#Unit" {
            syn::Type::Tuple(TypeTuple {
                paren_token: Paren { span: dummy_span() },
                elems: iter::empty::<syn::Type>().collect()
            })
        } else if let Some(lit) = match ty.name.as_str() {
            "u8" | "u16" | "u32" | "u64" | "u128" |
            "i8" | "i16" | "i32" | "i64" | "i128" |
            "usize" | "char" | "str" => Some(parse_str(ty.name.as_str()).unwrap()),
            _ => None
        } {
            lit
        } else if ty.name == "!" {
            syn::Type::Never(TypeNever { bang_token: parse_quote!(!) })
        } else if ty.name.as_str().starts_with("#Ref") {
            syn::Type::Reference(TypeReference {
                and_token: parse_quote!(&),
                lifetime: ty.lt_args.first().and_then(semantics::Lifetime::name).map(|lt| {
                    syn::Lifetime {
                        apostrophe: dummy_span(),
                        ident: name_to_ident_det(lt.as_str())
                    }
                }),
                mutability: if ty.name == "#RefMut" {
                    parse_quote!(mut)
                } else {
                    None
                },
                elem: Box::new(ty.type_args[0].clone().into())
            })
        } else if ty.name.as_str().starts_with("#") {
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

impl From<semantics::Fields> for syn::Fields {
    fn from(fields: semantics::Fields) -> Self {
        // TODO: it can sometimes be okay to give a struct the "wrong" kind of
        // fields if it doesn't have any fields, e.g. None can be written as None{}
        match fields {
            semantics::Fields::Unnamed(unnamed) => syn::Fields::Unnamed(FieldsUnnamed {
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
            semantics::Fields::Named(named) => syn::Fields::Named(FieldsNamed {
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
            semantics::Fields::None => syn::Fields::Unit
        }
    }
}

fn from_sem_expr(ctx: &Context, u: &mut Unstructured, ex: &semantics::Expr) -> Result<Expr> {
    Ok(match ex {
        semantics::Expr::Lit(name, type_args) => match name.as_str() {
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
                path: make_turbofish_path(name.as_str(), type_args.clone())
            })
        }
        semantics::Expr::Struct(name, type_args, fields) => {
            Expr::Struct(ExprStruct {
                attrs: vec![],
                path: make_turbofish_path(name.as_str(), type_args.clone()),
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
        semantics::Expr::Field(reciever, name) => {
            println!("Field access!");
            syn::Expr::Field(ExprField {
                attrs: vec![],
                base: Box::new(parenthesize_expr(16, from_sem_expr(ctx, u, reciever)?)),
                dot_token: parse_quote!(.),
                member: Member::Named(name_to_ident_det(name.as_str()))
            })
        }
        semantics::Expr::Method(reciever, name, type_args, args) => {
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
        semantics::Expr::AssocFn(ty, ty_args, name, fn_type_args, args) => {
            Expr::Call(ExprCall {
                attrs: vec![],
                func: {
                    let ty_path = make_turbofish_path(ty.as_str(), ty_args.clone());
                    let fn_path = make_turbofish_path(name.as_str(), fn_type_args.clone());
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
        semantics::Expr::Macro(name, body) => {
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
                    path: parse_str(name.as_str()).unwrap(),
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
        semantics::Expr::Fn(name, type_args, args) => {
            if name.as_str().starts_with("#Tuple") {
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
                        path: make_turbofish_path(name.as_str(), type_args.clone()),
                        qself: None
                    })),
                    paren_token: Paren { span: dummy_span() },
                    args: args.iter().map(|a| from_sem_expr(ctx, u, a))
                        .collect::<Result<Punctuated<Expr, Token![,]>>>()?
                })
            }
        }
        semantics::Expr::Var(name) => Expr::Path(parse_str(name.as_str()).expect(format!("Couldn't parse {}", name.as_str()).as_str())),
        semantics::Expr::ExactString(str) => Expr::Lit(ExprLit {
            attrs: vec![],
            lit: Lit::Str(LitStr::new(str.as_str(), dummy_span()))
        }),
        semantics::Expr::UnOp(op, val) => match *op {
            "&" => Expr::Reference(ExprReference {
                attrs: vec![],
                and_token: parse_quote!(&),
                raw: Default::default(),
                mutability: None,
                expr: Box::new(parenthesize_expr(13, from_sem_expr(ctx, u, val.as_ref())?))
            }),
            "& mut" => Expr::Reference(ExprReference {
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
        semantics::Expr::BinOp(op, lhs, rhs) => {
            let presc = binary_prescedence(parse_str(op).expect(format!("Couldn't parse binary operator {}", op).as_str()));
            Expr::Binary(ExprBinary {
                attrs: vec![],
                op: parse_str(op).expect(format!("Failed parsing operator {}", op).as_str()),
                // We assume that binary operators are left associative, so the rhs needs
                // a higher prescedence
                left: Box::new(parenthesize_expr(presc, from_sem_expr(ctx, u, lhs.as_ref())?)),
                right: Box::new(parenthesize_expr(presc+1, from_sem_expr(ctx, u, rhs.as_ref())?))
            })
        }
    })
}
// TODO: add the possibility for things like assignments in the middle of expressions
fn name_to_ident(u: &mut Unstructured, name: &str) -> Result<Ident> {
    if name.starts_with("r#") {
        Ok(parse_str(name).unwrap())
    } else if KWDS_STRICT.contains(name) || Arbitrary::arbitrary(u)? {
        Ok(parse_str(format!("r#{}", name).as_str()).unwrap())
    } else {
        Ok(Ident::new(name, dummy_span()))
    }
}

fn name_to_ident_det(name: &str) -> Ident {
    if name.starts_with("r#") {
        parse_str(name).unwrap()
    } else if KWDS_STRICT.contains(name) {
        parse_str(format!("r#{}", name).as_str()).unwrap()
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

fn make_path(name: &str, path_args: PathArguments) -> syn::Path {
    let mut segments_vec = vec![];
    let mut segments_iter = name.split("::").collect::<Vec<&str>>().into_iter();
    let last_segment = PathSegment {
        ident: name_to_ident_det(segments_iter.next_back().unwrap()),
        arguments: path_args
    };
    for seg in segments_iter {
        segments_vec.push(PathSegment {
            ident: name_to_ident_det(seg),
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

fn make_turbofish_path(name: &str, args: Vec<semantics::Type>) -> syn::Path {
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

fn make_type_path(ty: semantics::Type) -> syn::Path {
    let name = ty.name;
    let path_args =
    if ty.type_args.is_empty() && ty.lt_args.is_empty() {
        PathArguments::None
    } else {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: None,
            lt_token: parse_quote!(<),
            args: ty.lt_args.iter().filter_map(semantics::Lifetime::name).map(|lt| {
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
    make_path(name.as_str(), path_args)

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
        let items: Vec<syn::Item>;
        let attrs = crate_attrs(ctx, u)?;
        if ctx.regard_semantics && ctx.has_main {
            let main = Item::Fn(make_main(ctx, u)?);
            items = iter::once(Ok(main))
                .chain(c_arbitrary_iter(ctx, u))
                .collect::<Result<Vec<syn::Item>>>()?;
        } else {
            items = unwrap_nev(c_arbitrary(ctx, u)?);
        }
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
lazy_maybe!(u, {
    ctx.has_main = false;
    attrs.push(wrap_attr(parse_quote!(no_main)))
        });
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
        let top_lev = !ctx.regard_semantics || ctx.is_top_level;
not_top_level!(ctx, guarded_lazy_choose!(u,  {
    true => Item::Fn(c_arbitrary(ctx, u)?),
    true => Item::Type(c_arbitrary(ctx, u)?),
    !ctx.regard_semantics => Item::Trait(c_arbitrary(ctx, u)?),
    true => Item::Enum(c_arbitrary(ctx, u)?),
    true => Item::Struct(c_arbitrary(ctx, u)?),
    // TODO:
    // Impl(ItemImpl),
    // Mod(ItemMod),
    // Static(ItemStatic),
    // TraitAlias(ItemTraitAlias),
    // Union(ItemUnion),
    // Use(ItemUse),
    // Intentionally omitted: ExternCrate, ForeignMod, Macro, Verbatim
    top_lev => Item::Static(c_arbitrary(ctx, u)?),
    top_lev => Item::Const(c_arbitrary(ctx, u)?),
        }))
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemStruct {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let ident: Ident = c_arbitrary(ctx, u)?;
        let generics = c_arbitrary(ctx, u)?;
        let vis = c_arbitrary(ctx, u)?;
        let fields: Fields;
        if ctx.regard_semantics {
            let kind = gen_to_kind(&generics, is_pub(&vis));
            let ty = kind_to_type(ident_to_name(&ident), &kind);
            let sem_fields = generate_sem_fields(ctx, u, &ty)?;
            add_struct(ctx, ident_to_name(&ident).into(), semantics::Struct {
                is_enum_variant: false,
                ty: Rc::new(ty),
                fields: sem_fields.clone()
            });
            fields = sem_fields.into();
        } else {
            fields = c_arbitrary(ctx, u)?;
        }
        Ok(ItemStruct {
            attrs: if ctx.options.print_vars {
                vec![parse_quote!(#[derive(Debug)])]
            } else {
                vec![]
            },
            vis,
            struct_token: parse_quote!(struct),
            ident, generics, fields,
            semi_token: parse_quote!(;)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemEnum {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let ident: Ident = c_arbitrary(ctx, u)?;
        // TODO: the enum can't be public if one of it's members aren't. Right now enums
        // are conservatively set to private, but ideally they should be able to be public
        // when possible.
        // let vis: Visibility = c_arbitrary(ctx, u)?;
        let vis = Visibility::Inherited;
        let generics = c_arbitrary(ctx, u)?;
        let variants: Punctuated<Variant, Token![,]>;
        if ctx.regard_semantics {
            let kind = gen_to_kind(&generics, is_pub(&vis));
            let ty = kind_to_type(ident_to_name(&ident), &kind);
            // If the enum doesn't have any variants, don't add it to the types
            // because it won't be possible to construct it without cheating
            let mut type_added = false;
            let fields = c_arbitrary_iter_with(ctx, u, |ctx, u| {
                if !type_added {
                    type_added = true;
                    add_type(ctx, ident_to_name(&ident).into(), kind.clone());
                }
                let name: Ident = c_arbitrary(ctx, u)?;
                let fields = generate_sem_fields(ctx, u, &ty)?;
                Ok((name, fields))
            }).collect::<Result<Vec<(Ident, semantics::Fields)>>>()?;
            let mut local_variants = vec![];
            for (enum_ident, fields) in fields {
                let name = format!("{}::{}", ident, enum_ident).into();
                local_variants.push(syn::Variant {
                    fields: remove_visibility(fields.clone().into()),
                    ident: enum_ident,
                    // TODO: add discriminants
                    discriminant: None,
                    attrs: vec![],
                });
                add_struct(ctx, name, semantics::Struct {
                    is_enum_variant: true,
                    ty: Rc::new(ty.clone()),
                    fields
                });
            }
            variants = local_variants.into_iter().collect();
        } else {
            variants = c_arbitrary(ctx, u)?;
        }
        Ok(ItemEnum {
            attrs: if ctx.options.print_vars {
                vec![parse_quote!(#[derive(Debug)])]
            } else {
                vec![]
            },
            vis,
            enum_token: parse_quote!(enum),
            brace_token: Brace { span: dummy_span() },
            ident, generics, variants
        })
    }
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

fn generate_sem_fields(ctx: &mut Context, u: &mut Unstructured, ty: &semantics::Type) -> Result<semantics::Fields> {
    Ok(if Arbitrary::arbitrary(u)? {
        semantics::Fields::Named(c_arbitrary_iter_with(ctx, u, |ctx, u| {
            let ident: Ident = c_arbitrary(ctx, u)?;
            let (field_ty, vis) = pick_type_with_vis(ctx, u)?;
            Ok((ident_to_name(&ident).into(), semantics::Field {
                visible: vis,
                ty: Rc::new(if field_ty.is_sized_by(&ty.name) {
                    lazy_choose!(u, {
                        semantics::Type {
                            type_args: vec![field_ty],
                            ..make_type!(Box)
                        },
                        // semantics::Type {
                        //    type_args: vec![field_ty],
                        //    ..make_type!(Rc)
                        // },
                        semantics::Type {
                            type_args: vec![field_ty],
                            ..make_type!(Vec)
                        },
                        // Reference
                    })?
                } else {
                    field_ty
                })
            }))
        }).collect::<Result<HashMap<StringWrapper, semantics::Field>>>()?)
    } else {
        semantics::Fields::Unnamed(c_arbitrary_iter_with(ctx, u, |ctx, u| {
            let (field_ty, vis) = pick_type_with_vis(ctx, u)?;
            Ok(semantics::Field {
                visible: vis,
                ty: Rc::new(if field_ty.is_sized_by(&ty.name) {
                        lazy_choose!(u, {
                            semantics::Type {
                                type_args: vec![field_ty],
                                ..make_type!(Box)
                            },
                            // semantics::Type {
                            //    type_args: vec![field_ty],
                            //    ..make_type!(Rc)
                            // },
                            semantics::Type {
                                type_args: vec![field_ty],
                                ..make_type!(Vec)
                            },
                            // Reference
                        })?
                    } else {
                        field_ty
                    })

                })
            }).collect::<Result<Vec<semantics::Field>>>()?)
        })
}

fn gen_to_kind(generics: &Generics, is_visible: bool) -> Kind {
    let mut lifetimes = 0;
    let mut types = 0;
    let mut _consts = 0;
    for param in generics.params.iter() {
        match param {
            GenericParam::Type(_) => types += 1,
            GenericParam::Lifetime(_) => lifetimes += 1,
            GenericParam::Const(_) => _consts += 1
        }
    }
    semantics::Kind {
        is_visible,
        lifetimes,
        types
    }
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
            // TODO: different visibilities
            vis: Visibility::Inherited,
            static_token: parse_quote!(static),
            // TODO: possible mutability
            mutability: None,
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
        let sig;
        let block;
        if ctx.regard_semantics {
            let old_final_only = mem::replace(&mut ctx.final_stmnt_only, vec![]);
            let new_scope = Rc::clone(&ctx.basic_scopes);
            let old_scope = mem::replace(&mut ctx.scopes, new_scope);
            push_scope(ctx);
            let (fn_type, sig_tmp) = type_with_sig(ctx, u)?;
            // TODO: the types in the generics should be visible inside the functions scope
            sig = sig_tmp;
            let ret_type = fn_type.func.clone().unwrap().ret_type;
            add_var(ctx, ident_to_name(&sig.ident).into(), Variable::new(
                fn_type.clone(),
                semantics::Mutability::Immutable,
                semantics::Lifetime::Named("'static".into())
            ));
            block = with_attrs!(ctx {
                needs_new_scope = false,
                allow_ambiguity = true,
                expected_type = ret_type
            }, c_arbitrary(ctx, u)?);
            ctx.scopes = old_scope;
            ctx.final_stmnt_only = old_final_only;
            add_var(ctx, ident_to_name(&sig.ident).into(), Variable::new(
                fn_type,
                semantics::Mutability::Immutable,
                semantics::Lifetime::Named("'static".into())
            ));
        } else {
            sig = c_arbitrary(ctx, u)?;
            block = no_block_labels!(ctx, c_arbitrary(ctx, u)?);
        }
        let result = Ok(ItemFn {
            attrs: vec![],
            vis: Visibility::Inherited,
            sig,
            block,
        });
        return result;
    }
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
                let ty = pick_type(ctx, u)?;
                // add_type(ctx, ident.to_string().into(), semantics::Kind {
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

fn type_with_sig(ctx: &mut Context, u: &mut Unstructured)
-> Result<(semantics::Type, Signature)> {

    let inputs: Punctuated<FnArg, Token![,]>;
    let output: ReturnType;
    let mut result = vec![];
    let mut input_tys: Vec<semantics::Type> = vec![];
    let mut lt_generics = vec![];
    let output_ty;
    // TODO: handle constraints
    let type_generics = c_arbitrary_iter_with(ctx, u, |ctx, u| {
        Ok(Generic {
            name: ident_to_name(&c_arbitrary::<Context, Ident>(ctx, u)?).into(),
            constraints: vec![],
            is_arg_for_other: false 
        })
    }).collect::<Result<semantics::Generics>>()?;
    for gen in type_generics.iter() {
        add_type(ctx, gen.name.clone(), semantics::Kind {
            is_visible: false,
            lifetimes: 0,
            types: 0
        })
    }

    // We only want types that are possible to construct in the body/return type
    for gen in type_generics.iter() {
        if input_tys.iter().all(|ty| ty.name != gen.name) {
            remove_type(ctx, gen.name.clone());
        }
    }


    irrefutable!(ctx, {

        // TODO: possible self argument where apropriate
        let len = c_arbitrary_iter::<(), Context>(ctx, u)
            .collect::<Result<Vec<()>>>()?
            .len();
        for _ in 0..len {
            let ty = pick_type(ctx, u)?;
            input_tys.push(ty.clone());
            // TODO: allow mutable arguments
            let (pat, vars) = sub_pattern!(ctx, irrefutable!(ctx, pattern_of_type(ctx, u, &ty)?));
            for (name, var) in vars {
                add_var(ctx, name, var);
            }
            result.push(FnArg::Typed(PatType {
                attrs: vec![],
                colon_token: parse_quote!(:),
                pat: Box::new(pat),
                ty: Box::new(ty.into())
            }));
        }
        let (reserved_input, output_ty_tmp) = lazy_choose!(u, {
            (None, pick_type(ctx, u)?),
            {
                let lt_ident: Ident = lifetime!(ctx, c_arbitrary(ctx, u)?);
                let ident_name: StringWrapper = ident_to_name(&lt_ident).into();
                lt_generics.push(ident_name.clone());
                let output_ty = make_ref(ident_name, pick_type(ctx, u)?);
                // TODO: allow borrowing e.g. structure fields
                let input_ty = output_ty.clone();
                let input_ident: Ident = c_arbitrary(ctx, u)?;
                let input_name: StringWrapper = ident_to_name(&input_ident).into();
                // the lt of the variable holding the reference, not the thing the
                // reference points to
                let ref_lt = fresh_lt(ctx);
                add_var(ctx, input_name.clone(), Variable::new(
                    input_ty.clone(),
                    semantics::Mutability::Immutable,
                    ref_lt
                ));
                result.push(FnArg::Typed(PatType {
                    attrs: vec![],
                    colon_token: parse_quote!(:),
                    pat: Box::new(Pat::Ident(PatIdent {
                        attrs: vec![],
                        by_ref: None,
                        mutability: None,
                        ident: input_ident,
                        subpat: None
                    })),
                    ty: Box::new(input_ty.clone().into())
                }));
                input_tys.push(input_ty.clone());
                (Some(input_name), output_ty) }
        })?;
        output_ty = output_ty_tmp;
        reserved_input.map(|i| reserve_var(ctx, i));
        inputs = result.into_iter().collect();
        if output_ty.matches(&make_type!(())) && Arbitrary::arbitrary(u)? {
            output = ReturnType::Default;
        } else {
            output = ReturnType::Type(parse_quote!(->), Box::new(output_ty.clone().into()));
        }

    });

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

    Ok((semantics::Type {
        name: "#Fn".into(),
        // TODO: generate lifetimes
        lt_generics,
        type_generics,
        lt_args: vec![],
        type_args: vec![],
        func: Some(Box::new(Func {
            args: input_tys,
            ret_type: output_ty
        }))
    }, Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: parse_quote!(fn),
            ident: c_arbitrary(ctx, u)?,
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
            { let name: String = Arbitrary::arbitrary(u)?;
                Some(parse_quote!(#name)) }
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
            if ctx.needs_new_scope {
                push_scope(ctx);
                scope_added = true;
            } else {
                ctx.needs_new_scope = true;
            }
            let old_final_stmnt = ctx.is_final_stmnt;
            ctx.is_final_stmnt = false;
            let mut init_stmts
                = c_arbitrary_iter_with(ctx, u, |ctx, u| {
                    let ty = pick_type(ctx, u)?;
                    Ok(with_type!(ctx, ty, c_arbitrary(ctx, u)?))
                }).collect::<Result<Vec<Stmt>>>()?;
            if ctx.options.print_vars {
                for (name, var) in ctx.scopes.top_mut().iter().flat_map(|s| s.vars.iter()) {
                    // if ty.fits_constraints(ctx, &vec![parse_constraint!(Debug)]) {
                    if var.ty.func.is_none() {
                        init_stmts.push(make_print(name.clone(), &var.ty));
                    }
                }
            }
            ctx.is_final_stmnt = true;
            let final_stmt = guarded_lazy_choose!(u, {
                ctx.expected_type.matches(&make_type!(())) => None,
                true => Some(Stmt::Expr(c_arbitrary(ctx, u)?))
            })?;
            stmts = init_stmts.into_iter()
                .chain(final_stmt.into_iter())
                .collect::<Vec<Stmt>>();
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
lazy_choose!(u, {
    Stmt::Local(c_arbitrary(ctx, u)?),
    Stmt::Item(c_arbitrary(ctx, u)?),
    Stmt::Semi({
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
            let ty: semantics::Type = pick_type(ctx, u)?;
            let pat_and_vars = with_attrs!(ctx {
                allow_ambiguity = type_specified,
                is_refutable = false,
                is_top_pattern = true
            }, pattern_of_type(ctx, u, &ty)?);
                pat = pat_and_vars.0;
                let vars = pat_and_vars.1;
            init = Some((parse_quote!(=), with_attrs!(ctx {
                expected_type = ty.clone(),
                allow_ambiguity = type_specified
            }, c_arbitrary(ctx, u)?)));
            for (name, var) in vars {
                add_var(ctx, name, var);
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
ty: &'b semantics::Type
) -> Result<(Pat, Vec<(StringWrapper, Variable)>)> {
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
        let name = ident_to_name(&pat.ident).into();
        let mutability = match pat.mutability {
            Some(_) => semantics::Mutability::Mutable,
            None => semantics::Mutability::Immutable,
        };
        (Pat::Ident(pat), vec![(name, Variable::new(
            ty.clone(), mutability, fresh_lt(ctx)
        ))])
    },
    !needs_type_pat => (Pat::Wild(PatWild{
        attrs:vec![],
        underscore_token:parse_quote!(_)
    }), vec![]),
    !needs_type_pat && ty.name.as_str().starts_with("#Tuple") => {
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
    !needs_type_pat &&  ty.name == "#Ref" => {
        let (inner, sub_vars) = sub_pattern!(ctx, pattern_of_type(ctx, u, &ty.type_args[0])?);
        (Pat::Reference(PatReference{
            attrs: vec![],
            and_token: parse_quote!(&),
            // TODO: check if this can be mutable
            mutability: None,
            pat: Box::new(inner)
        }), sub_vars)
    }
    })
    // TODO: add more patterns
}

impl<'a> ContextArbitrary<'a, Context> for Expr {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        if ctx.regard_semantics {
            // println!("Expected type: {:?}", ctx.expected_type);
            weighted_lazy_choose!(u, {
                10 => {
                    let expr = construct_value(ctx, u, ctx.expected_type.clone(), ctx.allow_ambiguity)?;
                    from_sem_expr(ctx, u, &expr)?
                },
                // 1 => Expr::If(c_arbitrary(ctx, u)?)
                1 => Expr::Block(c_arbitrary(ctx, u)?)
            })
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
        Ok(ExprIf {
            attrs: vec![],
            if_token: parse_quote!(if),
            cond: if !ctx.regard_semantics && Arbitrary::arbitrary(u)? {
                let_guard(ctx, u)?
            } else {
                with_type!(ctx, make_type!(bool), parens_block!(ctx, c_arbitrary(ctx, u)?))
            },
            then_branch: not_fn_block!(ctx, no_block_labels!(ctx, c_arbitrary(ctx, u)?)),
            else_branch: guarded_lazy_choose!(u, {
                !ctx.regard_semantics || ctx.expected_type.name == "#Unit" => None,
                true => Some((
                    parse_quote!(else),
                    Box::new(not_fn_block!(ctx, no_block_labels!(ctx, lazy_choose!(u, {
                        Expr::Block(c_arbitrary(ctx, u)?),
                        Expr::If(c_arbitrary(ctx, u)?)
                    })?)))
                ))
            })?
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

fn make_print(name: StringWrapper, _ty: &semantics::Type) -> Stmt {
    let ident = name_to_ident_det(name.as_str());
    Stmt::Semi(Expr::Macro(ExprMacro {
        attrs: vec![],
        mac: syn::Macro {
            path: parse_quote!(println),
            bang_token: parse_quote!(!),
            delimiter: MacroDelimiter::Paren(Paren { span: dummy_span() }),
            tokens: quote!("{:?}", #ident)
        }
    }), parse_quote!(;))
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
        Ok(ExprBinary {
            attrs: vec![],
            left: parens_ex!(binary_prescedence(op), ctx, c_arbitrary(ctx, u)?),
            op,
            right: parens_ex!(binary_prescedence(op), ctx, c_arbitrary(ctx, u)?),
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
            let (var, _) = semantics::pick_var_that(
                ctx, u,
                |_,v| v.ty.matches(&expected_type)
            )?;
            let var_str = var.as_str();
            path = syn::Path {
                leading_colon: None,
                segments: vec![
                    PathSegment {
                        ident: parse_quote!(#var_str),
                        arguments: PathArguments::None
                    }
                ].into_iter().collect()
            };
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
    fn c_arbitrary(_ctx: &mut Context, _u: &mut Unstructured<'a>) -> Result<Self> {
        // TEMPORARY
        Ok(Generics {
            lt_token: parse_quote!(<),
            params: empty::<GenericParam>().collect(),
            gt_token: parse_quote!(>),
            where_clause: None,
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
    static ref ALPHA_: Vec<char>  = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_".chars().collect();
    static ref ALPHA_NUM: Vec<char> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789".chars().collect();

}

// TODO: handle the "union" soft keyword
impl<'a> ContextArbitrary<'a, Context> for Ident {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
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
        let min_l = if fst_char == '_' { 1 } else { 0 };
        for _ in 0..(u.int_in_range(min_l..=10)?) {
            let char = *u.choose(rest_chars)?;
            name.push(char);
        }
        // Lifetimes can't have raw variables, so we can't just make it a raw variable if it is a keyword
        while ctx.is_lifetime && (KWDS_STRICT.contains(&&name[..]) || name == "static") {
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
