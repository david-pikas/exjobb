
pub static TUPLE_NAMES: [&'static str; 12] = [
    "#Tuple1", "#Tuple2" , "#Tuple3" , "#Tuple4" ,
    "#Tuple5", "#Tuple6" , "#Tuple7" , "#Tuple8" ,
    "#Tuple9", "#Tuple10", "#Tuple11", "#Tuple12",
];

#[macro_export]
macro_rules! make_var {
    ($name:path : $($ty:tt)*) =>
        ((crate::parse_path!($name), Rc::new(Variable::new(
            crate::make_type!($($ty)*), crate::semantics::Mutability::Immutable,
            crate::semantics::Lifetime::Named("'static".into())
        ))))
}

#[macro_export]
macro_rules! make_type {
    (!) => (crate::semantics::Type {
        name: vec!["!".into()],
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args: vec![],
        func: None,
        is_visible: true
    });
    (*& $($rest:tt)*) => (crate::semantics::Type {
        name: vec!["#RefMut".into()],
        ..crate::make_type!(& $($rest)*)
    });
    (& #($($ty:tt)*)) => (crate::make_type!(& {'a} 'a $($ty)*));
    (& $name:ident $($rest:tt)*) => (crate::make_type!(& {'a} 'a $name $($rest)*));
    (& $lt:lifetime $($rest:tt)*) => (crate::make_type!(& {} $lt $($rest)*));
    (& %local_ref $($rest:tt)*) => (crate::semantics::Type {
        lt_args: vec![crate::semantics::Lifetime::Any],
        ..crate::make_type!(& 'local_ref $($rest)*)
    });
    (& {$($gen:tt)*} $lt:lifetime $($ty:tt)*) => ({
        let (lt_generics, type_generics): (Vec<StringWrapper>, Vec<Generic>) = crate::parse_generics!([],[];$($gen)*,);
        crate::semantics::Type {
            name: vec!["#Ref".into()],
            lt_generics, type_generics,
            lt_args: vec![crate::semantics::Lifetime::Named(
                stringify!($lt).into()
            )],
            type_args: vec![make_type!($($ty)*)],
            func: None,
            is_visible: true
        }
    });
    (%Fn($($args:tt)*) $($ret:tt)*) =>
        (make_type!(%Fn{}($($args)*) $($ret)*));
    (%Fn{$($gen:tt)*}($($args:tt)*)) => 
        (crate::make_type!(%Fn{$($gen)*}($($args)*) -> ()));
    (%Fn{$($gen:tt)*}($($args:tt)*) -> $($ret:tt)*) => {
        {
            let (lt_generics, type_generics) = crate::parse_generics!([],[];$($gen)*,);
            let mut ret_type = crate::make_type!($($ret)*);
            let (_, mut args): (Vec<Lifetime>, Vec<Type>) = crate::parse_args!([],[];$($args)*,);
            ret_type.lt_generics.extend(lt_generics.clone());
            ret_type.type_generics.extend(type_generics.clone());
            for arg in &mut args {
                arg.lt_generics.extend(lt_generics.clone());
                arg.type_generics.extend(type_generics.clone());
            }
            crate::semantics::Type {
                name: vec!["#Fn".into()],
                lt_generics, type_generics,
                lt_args: vec![],
                type_args: vec![],
                func: Some(Box::new(Func {
                    args,
                    ret_type
                })),
                is_visible: true,
            }
        }
    };
    ($name:path$({})?$([])?) => (crate::semantics::Type {
        name: crate::parse_path!($name),
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args: vec![],
        func: None,
        is_visible: true,
    });
    (()) => (crate::semantics::Type {
        name: vec!["#Unit".into()],
        lt_generics: vec![],
        type_generics: vec![],
        lt_args: vec![],
        type_args: vec![],
        func: None,
        is_visible: true
    });
    (($len:expr; $fst:expr $(,$rest:expr)*)) => (crate::semantics::Type {
        name: vec![crate::ty_macros::TUPLE_NAMES[len-1].into()],
        mutability: crate::semantics::Mutability::Immutable,
        lt_generics: vec![],
        type_generics: [
            (crate::make_type!($fst), vec![])
            $(, crate::make_type!($rest))*
        ].iter().cloned().collect(),
        lt_args: vec![],
        type_args: vec![],
        func: None
    });
    ($name:path[$($args:tt)*]) => (crate::make_type!($name{}[$($args)*]));
    ($name:path{$($args:tt)*}) => (crate::make_type!($name{$($args)*}[]));
    ($name:path{$($gen:tt)*}[$($args:tt)*]) => {
        {
            let (lt_generics, type_generics) = crate::parse_generics!([],[];$($gen)*,);
            let (lt_args, type_args) = crate::parse_args!([],[];$($args)*,);
            crate::semantics::Type {
                name: crate::parse_path!($name),
                lt_generics, type_generics,
                lt_args, type_args,
                func: None,
                is_visible: true
            }
        }
    };
}

#[macro_export]
macro_rules! parse_path {
    ($p:path) => (stringify!($p).split("::").map(StringWrapper::from).collect::<Vec<_>>())
    // ($init:ident$(::$segment:ident)*) =>
    //     (vec![
    //         StringWrapper::from(stringify!($init))
    //         $(StringWrapper::from(stringify!($segment)),)*
    //     ])
}

#[macro_export]
macro_rules! parse_args {
    ([$(,$lt:expr)*],[$(,$ty:expr)*];$(,)?) => ( (vec![$($lt),*],vec![$($ty),*]) );
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$lt:lifetime, $($arg:tt)*) => {
        crate::parse_args!([$(,$lts)*,Lifetime(stringify!($lt).into())],[$(,$tys)*];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$name:path$([$($args:tt)*])?, $($arg:tt)*) => {
        crate::parse_args!([$(,$lts)*],[$(,$tys)*, make_type!($name$([$($args)*])?)];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];#($($quoted:tt)*), $($arg:tt)*) => {
        crate::parse_args!([$(,$lts)*],[$(,$tys)*, make_type!($($quoted)*)];$($arg)*)
    };
}

#[macro_export]
macro_rules! parse_generics {
    ([$(,$lt:expr)*],[$(,$ty:expr)*];$(,)?) => ( 
        (vec![$($lt),*], vec![$($ty),*])
    );
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$lt:lifetime, $($arg:tt)*) => {
        crate::parse_generics!([$(,$lts)*,stringify!($lt).into()],[$(,$tys)*];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$name:path, $($arg:tt)*) => {
        crate::parse_generics!([$(,$lts)*],[$(,$tys)*,
            Generic {
                name: stringify!($name).into(), 
                constraints: vec![],
                is_arg_for_other: false
            }
        ];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];$name:path : ($($traits:tt)*), $($arg:tt)*) => {
        crate::parse_generics!([$(,$lts)*],[$(,$tys)*,
            Generic {
                name: stringify!($name).into(), 
                constraints: crate::parse_constraints!(;$($traits)*,),
                is_arg_for_other: false
            }
        ];$($arg)*)
    };
    ([$(,$lts:expr)*],[$(,$tys:expr)*];?$name:path, $($arg:tt)*) => {
        crate::parse_generics!([$(,$lts)*],[$(,$tys)*,
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
                constraints: crate::parse_constraints!(;$($traits)*,),
                is_arg_for_other: true
            }
        ];$($arg)*)
    };

}

#[macro_export]
macro_rules! parse_trait_desc {
    ($name:path$({$($trait_gen:ident),*})?) => {
        crate::semantics::TraitDescription {
            name: crate::parse_path!($name),
            trait_generics: crate::parse_trait_generics!(;$($($trait_gen)*)?,)
        }
    }
}

#[macro_export]
macro_rules! parse_trait_generics {
    ($($args:tt)*) => ({
        let (_lt_generics, type_generics): (Vec<Lifetime>, _)
        = crate::parse_generics!([],[]$($args)*);
        type_generics
    })
}


#[macro_export]
macro_rules! parse_constraints {
    ($(,$exprs:expr)*;$(,)?) => (vec![$($exprs),*]);
    ($(,$exprs:expr)*;$name:path$([$($args:tt)*])?, $($rest:tt)*) => ({
        crate::parse_constraints!($(,$exprs)*, crate::parse_constraint!($name$([$($args)*])?); $($rest)*)
    })
}

#[macro_export]
macro_rules! parse_constraint {
    ($name:path$([$($args:tt)*])?) => ({
        crate::semantics::Constraint {
            trait_name: crate::parse_path!($name),
            trait_args: crate::parse_types!(;$($($args)*)?,)
        }
    })
}

#[macro_export]
macro_rules! make_kind {
    ($name:path$({})?) => (make_kind!($name{0;0}));
        
    ($name:path{$ty:expr}) => {
        (crate::parse_path!($name), crate::semantics::Kind {
            is_visible: true,
            lifetimes: 0,
            types: $ty
        })
    };
    (&mut) => ((vec![StringWrapper::from("#RefMut")], crate::semantics::Kind {
        is_visible: true,
        lifetimes: 1,
        types: 1
    }));
    (&) => ((vec![StringWrapper::from("#Ref")], crate::semantics::Kind {
        is_visible: true,
        lifetimes: 1,
        types: 1
    }));
    (()) => ((vec![StringWrapper::from("#Unit")], crate::semantics::Kind {
        is_visible: true,
        lifetimes: 0,
        types: 0
    }));
    (($len:expr;)) => {
        (vec![StringWrapper::from(crate::ty_macros::TUPLE_NAMES[$len-1])], crate::semantics::Kind {
            is_visible: true,
            lifetimes: 0,
            types: $len
        })
    };
    ($name:path{$lt:expr; $ty:expr}) => {
        (crate::parse_path!($name), Kind {
            is_visible: true,
            lifetimes: $lt,
            types: $ty
        })
    };
}

#[macro_export]
macro_rules! make_trait {
    ($name:path$({$($gen:tt)*})? : ($($types:tt)*)) => ({
        let name = crate::parse_path!($name);
        let (_lt_generics, type_generics): (Vec<Lifetime>, _)
            = crate::parse_generics!([],[];$($($gen)*,)?);
        (name.clone(), crate::semantics::Trait {
            name, type_generics,
            implementors: crate::parse_implementors!(;$($types)*,)
        })
    })
}

#[macro_export]
macro_rules! parse_implementors {
    ($(,$exprs:expr)*;$(,)?) => (vec![$($exprs,)*]);
    ($(,$exprs:expr)*; $name:path$({$($gen:tt)*})?$([$($args:tt)*])?, $($rest:tt)*) => {
        crate::parse_implementors!($(,$exprs)*; {}[] : #($name$({$($gen)*})?$([$($args)*])?), $($rest)*)
    };
    ($(,$exprs:expr)*; #($($ty:tt)*), $($rest:tt)*) => {
        crate::parse_implementors!($(,$exprs)*; {}[] : #($($ty)*), $($rest)*)
    };
    ($(,$exprs:expr)*; [$($args:tt)*]: $($rest:tt)*) => {
        crate::parse_implementors!($(,$exprs)*; {}[$($args)*] : $($rest)*)
    };
    ($(,$exprs:expr)*; {$($trait_gen:tt)*}[$($trait_args:tt)*] :
                       $name:path$({$($gen:tt)*})?$([$($args:tt)*])?, $($rest:tt)*) => {
        crate::parse_implementors!($(,$exprs)*; {$($trait_gen)*}[$($trait_args)*] : #($name$({$($gen)*})?$([$($args)*])?), $($rest)*)
    };
    ($(,$exprs:expr)*;{$($gen:tt)*}[$($args:tt)*] : #($($ty:tt)*), $($rest:tt)*) => ({
        let (_lt_generics, type_generics): (Vec<Lifetime>, _)
            = crate::parse_generics!([],[];$($($gen)*,)?);
        crate::parse_implementors!($(,$exprs)*,(crate::semantics::TraitArgs {
            type_generics,
            type_args: crate::parse_types!(;$($args)*,)
        }, std::rc::Rc::new(make_type!($($ty)*)));$($rest)*)
    })
}

pub static GEN_STRING: [&'static str; 12] = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"];

#[macro_export]
macro_rules! make_methods {
    ($name:path$([$($type_args:tt)*])? {$($methods:tt)*}) => 
        (crate::make_methods!(#($name$([$($type_args)*])?){$($methods)*}));
    (% #($($ty:tt)*) {$($methods:tt)*}) => (
        (Right(crate::parse_trait_desc!($($ty)*)), crate::parse_methods!(;$($methods)*,))
    );
    (#($($ty:tt)*){$($methods:tt)*}) => (
        (Left(Rc::new(crate::make_type!($($ty)*))), crate::parse_methods!(;$($methods)*,))
    );
}

#[macro_export]
macro_rules! parse_methods {
    ($(,$expr:expr)*;$(,)?) => (vec![$(Rc::new($expr)),*]);
    ($(,$expr:expr)*;
        $name:ident$({$($gen:tt)*})?($($args:tt)*) 
            -> $ret:path$([$($ret_arg:tt)*])?, $($rest:tt)*) => {
        crate::parse_methods!($(,$expr)*; $name$({$($gen)*})?($($args)*)
            -> #($ret$([$($ret_arg)*])?), $($rest)*)
    };
    ($(,$expr:expr)*;
        $name:ident$({$($gen:tt)*})?($($args:tt)*), $($rest:tt)*) => {
        crate::parse_methods!($(,$expr)*; $name$({$($gen)*})?($($args)*) -> #(()), $($rest)*)
    };
    ($(,$expr:expr)*;
        $name:ident$({$($gen:tt)*})?($($args:tt)*) -> #($($ret:tt)*), $($rest:tt)*) => {
        crate::parse_methods!($(,$expr)*, {
            let (self_param, args): (crate::semantics::SelfParam, Vec<Type>)
                = crate::parse_method_args!($($args)*,);
            let ret_type = make_type!($($ret)*);
            let (lt_generics, type_generics) = crate::parse_generics!([],[];$($($gen)*)?,);
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

#[macro_export]
macro_rules! parse_method_args {
    (self, $($args:tt)*) => ((Some(crate::semantics::VarHandling::Own), crate::parse_types!(;$($args)*)));
    (&self, $($args:tt)*) => ((Some(crate::semantics::VarHandling::Borrow(Lifetime::Any)), crate::parse_types!(;$($args)*)));
    (&mut self, $($args:tt)*) => ((Some(crate::semantics::VarHandling::MutBorrow(Lifetime::Any)), crate::parse_types!(;$($args)*)));
    (*& self, $($args:tt)*) => ((Some(crate::semantics::VarHandling::MutBorrow(Lifetime::Any)) crate::parse_types!(;$($args)*)));
    ($($args:tt)*) => ((None, crate::parse_types!(;$($args)*)));
}

#[macro_export]
macro_rules! parse_types {
    ($(,$tys:expr)*;$(,)?) => (vec![$($tys),*]);
    ($(,$tys:expr)*;$name:path$({$($gen:tt)*})?$([$($type_args:tt)*])?, $($rest:tt)*) => 
        (crate::parse_types!($(,$tys)*;#($name$({$($gen)*})?$([$($type_args)*])?), $($rest)*));
    ($(,$tys:expr)*;#($($ty:tt)*), $($rest:tt)*) => 
        (crate::parse_types!($(,$tys)*,make_type!($($ty)*);$($rest)*));
}

#[macro_export]
macro_rules! make_enum {
    ($name:path$({$($gen:tt)*})?: ($($structs:tt)*)) => (
        (crate::parse_path!($name), (
            Rc::new(make_type!($name$({$($gen)*})?)),
            crate::parse_structs!(;$($structs)*,)
        ))
    )
}

#[macro_export]
macro_rules! parse_structs {
    ($(,$structs:expr)*;$(,)?) =>
        (vec![$($structs,)*].into_iter().collect::<HashMap<_,_>>());
    ($(,$structs:expr)*;$name:ident, $($rest:tt)*) => {
        crate::parse_structs!($(,$structs)*, (
            StringWrapper::from(stringify!($name)),
            Rc::new(crate::semantics::Fields::None)
        );$($rest)*)
    };
    ($(,$structs:expr)*;$name:ident($($args:tt)*), $($rest:tt)*) => {
        crate::parse_structs!($(,$structs)*, (
            StringWrapper::from(stringify!($name)),
            Rc::new(crate::parse_unnamed_fields!(;$($args)*,))
        );$($rest)*)
    }
}

#[macro_export]
macro_rules! make_struct {
    (()) => ((vec![StringWrapper::from("#Unit")], (
        Rc::new(make_type!(())),
        Rc::new(Fields::None)
    )));
    (($len:expr;)) => ({
        let len = $len;
        (vec![StringWrapper::from(crate::ty_macros::TUPLE_NAMES[len-1])], (
            Rc::new(Type {
                name: vec![crate::ty_macros::TUPLE_NAMES[len-1].into()],
                lt_args: vec![],
                lt_generics: vec![],
                type_generics: crate::ty_macros::GEN_STRING[0..len].iter()
                    .map(|s| (Generic {
                        name: s.clone().into(), 
                        constraints: vec![],
                        is_arg_for_other: false
                    })).collect(),
                type_args: vec![], //GEN_NAMES[0..len].to_vec(),
                func: None,
                is_visible: true,
            }),
            Rc::new(Fields::Unnamed(crate::ty_macros::GEN_STRING[0..len].iter().map(|name| Field {
                visible: true,
                ty: Rc::new(name_to_type(name.to_owned()))
            }).collect()))
        ))
    });
    ($name:path$({$($gen:tt)*})? ) =>
        ((crate::parse_path!($name), (
            Rc::new(make_type!($name$({$($gen)*})?)),
            Rc::new(Fields::None)
        )));
    ($name:ident $({$($gen:tt)*})? ( $($fields:tt)* )) =>
        ((StringWrapper::Static(stringify!($name)), (
            Rc::new(make_type!($name$({$($gen)*}[$($gen)*])?)),
            Rc::new(crate::parse_unnamed_fields!(;$($fields)*,))
        )));
    ($name:path $({$($gen:tt)*})? { $($fields:tt)* }) =>
        ((StringWrapper::Static(stringify!($name)), (
            Rc::new(make_type!($name$({$($gen)*}[$($gen)*])?)),
            Rc::new(parse_named_fields!(;$($fields)*))
        )))
}

#[macro_export]
macro_rules! parse_unnamed_fields {
    ($(,$fields:expr)*;$(,)?) => (Fields::Unnamed(vec![$($fields),*]));
    ($(,$fields:expr)*; $ty:path$({$($gen:tt)*})? $([$($arg:tt)*])?, $($rest:tt)*) => {
        crate::parse_unnamed_fields!($(,$fields)*; #($ty$({$($gen)*})?$([$($arg)*])?), $($rest)*)
    };
    ($(,$fields:expr)*;pub #($($ty:tt)*), $($rest:tt)*) => {
        parse_unnamed_fields!($(,$fields)*, crate::semantics::Field {
            visible: true,
            ty: make_type!($($ty)*)
        }; $($rest)*);
    };
    ($(,$fields:expr)*; #($($ty:tt)*), $($rest:tt)*) => {
        crate::parse_unnamed_fields!($(,$fields)*, crate::semantics::Field {
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
        parse_named_fields!($(,$fields)*, (crate::parse_path!($name), crate::semantics::Field {
            visible: true,
            ty: make_type!($($ty)*)
        }; $($rest)*));
    };
    ($(,$fields:expr)*;$name:ident : #($($ty:tt)*), $($rest:tt)*) => {
        parse_named_fields!($(,$fields)*, (crate::parse_path!($name), crate::semantics::Field {
            visible: false,
            ty: make_type!($($ty)*)
        }; $($rest)*));
    };
}

#[macro_export]
macro_rules! make_macro {
    ($ty:path$({$($gen:tt)*})?$([$($arg:tt)*])? : $name:ident($fn:expr)) => 
        (#($ty{$($gen)*}[$($arg)*]) : $name($fn));
    (#($($ty:tt)*) : $name:ident($fn:expr)) => {
        (crate::parse_path!($name), Rc::new(Macro {
            constructor: $fn,
            ty: crate::make_type!($($ty)*)
        }))
    }
}


