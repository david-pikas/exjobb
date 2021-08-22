use std::collections::HashMap;
use std::rc::Rc;

use arbitrary::Arbitrary;

use crate::context_arbitrary::c_arbitrary_iter_with_non_mut;
use crate::semantics::*;
use crate::string_wrapper::*;
use crate::{make_var, make_type, make_kind, make_methods, make_trait, make_struct, make_enum, make_macro};



pub fn prelude_scope(use_panics: bool) -> Scope {
    let mut scope = Scope {
        by_ty_name: HashMap::new(),
        owned: false,
        vars: vec![
            make_var!(drop: %Fn{T}(T)),
        ].into_iter().collect(), 
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
                capacity(&self) -> usize,
                reserve(&mut self, usize),
                reserve_exact(&mut self, usize),
                shrink_to_fit(&mut self),
                // into_boxed_slice(self) -> Box{[T]},
                truncate(&mut self, usize),
                // set_len(self, new_len),
                swap_remove(&mut self, usize) -> T,
                insert(&mut self, usize, T),
                remove(&mut self, usize) -> T,
                // retain{F}(self, f),
                push(&mut self, T),
                pop(&mut self) -> Option[T],
                // append(self, &Vec{T})
                len(&self) -> usize,
                is_empty(&self) -> bool,
                split_off(&mut self, usize) -> Vec[T]
            }),
            make_methods!(#(Vec{T: (Clone)}) {
                resize(&mut self, usize, T),
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
        structs: HashMap::new(),
        enums: [
            make_enum!(Result{R,E}: (
                Ok(R),
                Err(E)
            )),
            make_enum!(Option{T}: (
                Some(T),
                None
            )),
            make_enum!(std::io::ErrorKind: (
                NotFound,
                PermissionDenied,
                ConnectionRefused,
            ))
        ].iter().cloned().collect(), 
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
                            Ok(Token::Expr(construct_value_inner(ctx, u, ty_args[0].clone(), true, crate::semantics::VarHandling::Own)?))
                        }).collect::<crate::context_arbitrary::Result<Vec<Token>>>()?,
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
        }.into_iter().collect(), 
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
        ].iter().cloned().collect(), 
        enums: [

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
            make_kind!(&),
            make_kind!(&mut),
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
        ("&mut", Operator {
            type_generics: vec![Generic {
                name: "T".into(),
                constraints: vec![],
                is_arg_for_other: false
            }],
            operands: (make_type!(T), None),
            // TODO: the lifetime should be the current scopes lifetime
            ret_type: make_type!(*& %local_ref T)
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
