File {
    shebang: None,
    attrs: [],
    items: [
        Fn(
            ItemFn {
                attrs: [],
                vis: Inherited,
                sig: Signature {
                    constness: None,
                    asyncness: None,
                    unsafety: None,
                    abi: None,
                    fn_token: Fn,
                    ident: Ident(
                        quux,
                    ),
                    generics: Generics {
                        lt_token: Some(
                            Lt,
                        ),
                        params: [],
                        gt_token: Some(
                            Gt,
                        ),
                        where_clause: None,
                    },
                    paren_token: Paren,
                    inputs: [
                        Receiver(
                            Receiver {
                                attrs: [],
                                reference: Some(
                                    (
                                        And,
                                        Some(
                                            Lifetime {
                                                apostrophe: Span,
                                                ident: Ident(
                                                    ipsum,
                                                ),
                                            },
                                        ),
                                    ),
                                ),
                                mutability: None,
                                self_token: SelfValue,
                            },
                        ),
                    ],
                    variadic: Some(
                        Variadic {
                            attrs: [],
                            dots: Dot3,
                        },
                    ),
                    output: Default,
                },
                block: Block {
                    brace_token: Brace,
                    stmts: [
                        Local(
                            Local {
                                attrs: [],
                                let_token: Let,
                                pat: Wild(
                                    PatWild {
                                        attrs: [],
                                        underscore_token: Underscore,
                                    },
                                ),
                                init: Some(
                                    (
                                        Eq,
                                        If(
                                            ExprIf {
                                                attrs: [],
                                                if_token: If,
                                                cond: Paren(
                                                    ExprParen {
                                                        attrs: [],
                                                        paren_token: Paren,
                                                        expr: Group(
                                                            ExprGroup {
                                                                attrs: [],
                                                                group_token: Group,
                                                                expr: Cast(
                                                                    ExprCast {
                                                                        attrs: [],
                                                                        expr: Let(
                                                                            ExprLet {
                                                                                attrs: [],
                                                                                let_token: Let,
                                                                                pat: Path(
                                                                                    PatPath {
                                                                                        attrs: [],
                                                                                        qself: None,
                                                                                        path: Path {
                                                                                            leading_colon: None,
                                                                                            segments: [
                                                                                                PathSegment {
                                                                                                    ident: Ident(
                                                                                                        baz,
                                                                                                    ),
                                                                                                    arguments: None,
                                                                                                },
                                                                                            ],
                                                                                        },
                                                                                    },
                                                                                ),
                                                                                eq_token: Eq,
                                                                                expr: Cast(
                                                                                    ExprCast {
                                                                                        attrs: [],
                                                                                        expr: Closure(
                                                                                            ExprClosure {
                                                                                                attrs: [],
                                                                                                asyncness: None,
                                                                                                movability: None,
                                                                                                capture: Some(
                                                                                                    Move,
                                                                                                ),
                                                                                                or1_token: Or,
                                                                                                inputs: [],
                                                                                                or2_token: Or,
                                                                                                output: Type(
                                                                                                    RArrow,
                                                                                                    Slice(
                                                                                                        TypeSlice {
                                                                                                            bracket_token: Bracket,
                                                                                                            elem: Reference(
                                                                                                                TypeReference {
                                                                                                                    and_token: And,
                                                                                                                    lifetime: None,
                                                                                                                    mutability: Some(
                                                                                                                        Mut,
                                                                                                                    ),
                                                                                                                    elem: Array(
                                                                                                                        TypeArray {
                                                                                                                            bracket_token: Bracket,
                                                                                                                            elem: Paren(
                                                                                                                                TypeParen {
                                                                                                                                    paren_token: Paren,
                                                                                                                                    elem: Tuple(
                                                                                                                                        TypeTuple {
                                                                                                                                            paren_token: Paren,
                                                                                                                                            elems: [
                                                                                                                                                BareFn(
                                                                                                                                                    TypeBareFn {
                                                                                                                                                        lifetimes: None,
                                                                                                                                                        unsafety: Some(
                                                                                                                                                            Unsafe,
                                                                                                                                                        ),
                                                                                                                                                        abi: None,
                                                                                                                                                        fn_token: Fn,
                                                                                                                                                        paren_token: Paren,
                                                                                                                                                        inputs: [
                                                                                                                                                            BareFnArg {
                                                                                                                                                                attrs: [],
                                                                                                                                                                name: None,
                                                                                                                                                                ty: ImplTrait(
                                                                                                                                                                    TypeImplTrait {
                                                                                                                                                                        impl_token: Impl,
                                                                                                                                                                        bounds: [
                                                                                                                                                                            Trait(
                                                                                                                                                                                TraitBound {
                                                                                                                                                                                    paren_token: None,
                                                                                                                                                                                    modifier: None,
                                                                                                                                                                                    lifetimes: None,
                                                                                                                                                                                    path: Path {
                                                                                                                                                                                        leading_colon: None,
                                                                                                                                                                                        segments: [
                                                                                                                                                                                            PathSegment {
                                                                                                                                                                                                ident: Ident(
                                                                                                                                                                                                    bar,
                                                                                                                                                                                                ),
                                                                                                                                                                                                arguments: None,
                                                                                                                                                                                            },
                                                                                                                                                                                        ],
                                                                                                                                                                                    },
                                                                                                                                                                                },
                                                                                                                                                                            ),
                                                                                                                                                                            Add,
                                                                                                                                                                            Trait(
                                                                                                                                                                                TraitBound {
                                                                                                                                                                                    paren_token: None,
                                                                                                                                                                                    modifier: None,
                                                                                                                                                                                    lifetimes: None,
                                                                                                                                                                                    path: Path {
                                                                                                                                                                                        leading_colon: None,
                                                                                                                                                                                        segments: [
                                                                                                                                                                                            PathSegment {
                                                                                                                                                                                                ident: Ident(
                                                                                                                                                                                                    dolor,
                                                                                                                                                                                                ),
                                                                                                                                                                                                arguments: None,
                                                                                                                                                                                            },
                                                                                                                                                                                        ],
                                                                                                                                                                                    },
                                                                                                                                                                                },
                                                                                                                                                                            ),
                                                                                                                                                                            Add,
                                                                                                                                                                            Trait(
                                                                                                                                                                                TraitBound {
                                                                                                                                                                                    paren_token: None,
                                                                                                                                                                                    modifier: None,
                                                                                                                                                                                    lifetimes: None,
                                                                                                                                                                                    path: Path {
                                                                                                                                                                                        leading_colon: None,
                                                                                                                                                                                        segments: [
                                                                                                                                                                                            PathSegment {
                                                                                                                                                                                                ident: Ident(
                                                                                                                                                                                                    bar,
                                                                                                                                                                                                ),
                                                                                                                                                                                                arguments: None,
                                                                                                                                                                                            },
                                                                                                                                                                                        ],
                                                                                                                                                                                    },
                                                                                                                                                                                },
                                                                                                                                                                            ),
                                                                                                                                                                        ],
                                                                                                                                                                    },
                                                                                                                                                                ),
                                                                                                                                                            },
                                                                                                                                                            Comma,
                                                                                                                                                            BareFnArg {
                                                                                                                                                                attrs: [],
                                                                                                                                                                name: None,
                                                                                                                                                                ty: Slice(
                                                                                                                                                                    TypeSlice {
                                                                                                                                                                        bracket_token: Bracket,
                                                                                                                                                                        elem: Ptr(
                                                                                                                                                                            TypePtr {
                                                                                                                                                                                star_token: Star,
                                                                                                                                                                                const_token: Some(
                                                                                                                                                                                    Const,
                                                                                                                                                                                ),
                                                                                                                                                                                mutability: Some(
                                                                                                                                                                                    Mut,
                                                                                                                                                                                ),
                                                                                                                                                                                elem: Infer(
                                                                                                                                                                                    TypeInfer {
                                                                                                                                                                                        underscore_token: Underscore,
                                                                                                                                                                                    },
                                                                                                                                                                                ),
                                                                                                                                                                            },
                                                                                                                                                                        ),
                                                                                                                                                                    },
                                                                                                                                                                ),
                                                                                                                                                            },
                                                                                                                                                        ],
                                                                                                                                                        variadic: Some(
                                                                                                                                                            Variadic {
                                                                                                                                                                attrs: [],
                                                                                                                                                                dots: Dot3,
                                                                                                                                                            },
                                                                                                                                                        ),
                                                                                                                                                        output: Default,
                                                                                                                                                    },
                                                                                                                                                ),
                                                                                                                                            ],
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                },
                                                                                                                            ),
                                                                                                                            semi_token: Semi,
                                                                                                                            len: AssignOp(
                                                                                                                                ExprAssignOp {
                                                                                                                                    attrs: [],
                                                                                                                                    left: Unary(
                                                                                                                                        ExprUnary {
                                                                                                                                            attrs: [],
                                                                                                                                            op: Deref(
                                                                                                                                                Star,
                                                                                                                                            ),
                                                                                                                                            expr: ForLoop(
                                                                                                                                                ExprForLoop {
                                                                                                                                                    attrs: [],
                                                                                                                                                    label: Some(
                                                                                                                                                        Label {
                                                                                                                                                            name: Lifetime {
                                                                                                                                                                apostrophe: Span,
                                                                                                                                                                ident: Ident(
                                                                                                                                                                    foo,
                                                                                                                                                                ),
                                                                                                                                                            },
                                                                                                                                                            colon_token: Colon,
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                    for_token: For,
                                                                                                                                                    pat: Ident(
                                                                                                                                                        PatIdent {
                                                                                                                                                            attrs: [],
                                                                                                                                                            by_ref: None,
                                                                                                                                                            mutability: None,
                                                                                                                                                            ident: Ident(
                                                                                                                                                                dolor,
                                                                                                                                                            ),
                                                                                                                                                            subpat: None,
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                    in_token: In,
                                                                                                                                                    expr: Path(
                                                                                                                                                        ExprPath {
                                                                                                                                                            attrs: [],
                                                                                                                                                            qself: None,
                                                                                                                                                            path: Path {
                                                                                                                                                                leading_colon: None,
                                                                                                                                                                segments: [
                                                                                                                                                                    PathSegment {
                                                                                                                                                                        ident: Ident(
                                                                                                                                                                            foo,
                                                                                                                                                                        ),
                                                                                                                                                                        arguments: None,
                                                                                                                                                                    },
                                                                                                                                                                ],
                                                                                                                                                            },
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                    body: Block {
                                                                                                                                                        brace_token: Brace,
                                                                                                                                                        stmts: [],
                                                                                                                                                    },
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                    op: Sub(
                                                                                                                                        Sub,
                                                                                                                                    ),
                                                                                                                                    right: Cast(
                                                                                                                                        ExprCast {
                                                                                                                                            attrs: [],
                                                                                                                                            expr: Assign(
                                                                                                                                                ExprAssign {
                                                                                                                                                    attrs: [],
                                                                                                                                                    left: Call(
                                                                                                                                                        ExprCall {
                                                                                                                                                            attrs: [],
                                                                                                                                                            func: Paren(
                                                                                                                                                                ExprParen {
                                                                                                                                                                    attrs: [],
                                                                                                                                                                    paren_token: Paren,
                                                                                                                                                                    expr: Loop(
                                                                                                                                                                        ExprLoop {
                                                                                                                                                                            attrs: [],
                                                                                                                                                                            label: Some(
                                                                                                                                                                                Label {
                                                                                                                                                                                    name: Lifetime {
                                                                                                                                                                                        apostrophe: Span,
                                                                                                                                                                                        ident: Ident(
                                                                                                                                                                                            foo,
                                                                                                                                                                                        ),
                                                                                                                                                                                    },
                                                                                                                                                                                    colon_token: Colon,
                                                                                                                                                                                },
                                                                                                                                                                            ),
                                                                                                                                                                            loop_token: Loop,
                                                                                                                                                                            body: Block {
                                                                                                                                                                                brace_token: Brace,
                                                                                                                                                                                stmts: [],
                                                                                                                                                                            },
                                                                                                                                                                        },
                                                                                                                                                                    ),
                                                                                                                                                                },
                                                                                                                                                            ),
                                                                                                                                                            paren_token: Paren,
                                                                                                                                                            args: [],
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                    eq_token: Eq,
                                                                                                                                                    right: Await(
                                                                                                                                                        ExprAwait {
                                                                                                                                                            attrs: [],
                                                                                                                                                            base: Path(
                                                                                                                                                                ExprPath {
                                                                                                                                                                    attrs: [],
                                                                                                                                                                    qself: None,
                                                                                                                                                                    path: Path {
                                                                                                                                                                        leading_colon: None,
                                                                                                                                                                        segments: [
                                                                                                                                                                            PathSegment {
                                                                                                                                                                                ident: Ident(
                                                                                                                                                                                    ipsum,
                                                                                                                                                                                ),
                                                                                                                                                                                arguments: None,
                                                                                                                                                                            },
                                                                                                                                                                        ],
                                                                                                                                                                    },
                                                                                                                                                                },
                                                                                                                                                            ),
                                                                                                                                                            dot_token: Dot,
                                                                                                                                                            await_token: Await,
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                            as_token: As,
                                                                                                                                            ty: TraitObject(
                                                                                                                                                TypeTraitObject {
                                                                                                                                                    dyn_token: None,
                                                                                                                                                    bounds: [
                                                                                                                                                        Trait(
                                                                                                                                                            TraitBound {
                                                                                                                                                                paren_token: None,
                                                                                                                                                                modifier: None,
                                                                                                                                                                lifetimes: None,
                                                                                                                                                                path: Path {
                                                                                                                                                                    leading_colon: None,
                                                                                                                                                                    segments: [
                                                                                                                                                                        PathSegment {
                                                                                                                                                                            ident: Ident(
                                                                                                                                                                                lorem,
                                                                                                                                                                            ),
                                                                                                                                                                            arguments: None,
                                                                                                                                                                        },
                                                                                                                                                                    ],
                                                                                                                                                                },
                                                                                                                                                            },
                                                                                                                                                        ),
                                                                                                                                                    ],
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                },
                                                                                                                            ),
                                                                                                                        },
                                                                                                                    ),
                                                                                                                },
                                                                                                            ),
                                                                                                        },
                                                                                                    ),
                                                                                                ),
                                                                                                body: Block(
                                                                                                    ExprBlock {
                                                                                                        attrs: [],
                                                                                                        label: None,
                                                                                                        block: Block {
                                                                                                            brace_token: Brace,
                                                                                                            stmts: [],
                                                                                                        },
                                                                                                    },
                                                                                                ),
                                                                                            },
                                                                                        ),
                                                                                        as_token: As,
                                                                                        ty: ImplTrait(
                                                                                            TypeImplTrait {
                                                                                                impl_token: Impl,
                                                                                                bounds: [
                                                                                                    Trait(
                                                                                                        TraitBound {
                                                                                                            paren_token: None,
                                                                                                            modifier: None,
                                                                                                            lifetimes: None,
                                                                                                            path: Path {
                                                                                                                leading_colon: None,
                                                                                                                segments: [
                                                                                                                    PathSegment {
                                                                                                                        ident: Ident(
                                                                                                                            foo,
                                                                                                                        ),
                                                                                                                        arguments: None,
                                                                                                                    },
                                                                                                                ],
                                                                                                            },
                                                                                                        },
                                                                                                    ),
                                                                                                ],
                                                                                            },
                                                                                        ),
                                                                                    },
                                                                                ),
                                                                            },
                                                                        ),
                                                                        as_token: As,
                                                                        ty: Path(
                                                                            TypePath {
                                                                                qself: None,
                                                                                path: Path {
                                                                                    leading_colon: None,
                                                                                    segments: [
                                                                                        PathSegment {
                                                                                            ident: Ident(
                                                                                                lorem,
                                                                                            ),
                                                                                            arguments: None,
                                                                                        },
                                                                                    ],
                                                                                },
                                                                            },
                                                                        ),
                                                                    },
                                                                ),
                                                            },
                                                        ),
                                                    },
                                                ),
                                                then_branch: Block {
                                                    brace_token: Brace,
                                                    stmts: [],
                                                },
                                                else_branch: Some(
                                                    (
                                                        Else,
                                                        Let(
                                                            ExprLet {
                                                                attrs: [],
                                                                let_token: Let,
                                                                pat: Box(
                                                                    PatBox {
                                                                        attrs: [],
                                                                        box_token: Box,
                                                                        pat: Path(
                                                                            PatPath {
                                                                                attrs: [],
                                                                                qself: None,
                                                                                path: Path {
                                                                                    leading_colon: None,
                                                                                    segments: [
                                                                                        PathSegment {
                                                                                            ident: Ident(
                                                                                                ipsum,
                                                                                            ),
                                                                                            arguments: None,
                                                                                        },
                                                                                    ],
                                                                                },
                                                                            },
                                                                        ),
                                                                    },
                                                                ),
                                                                eq_token: Eq,
                                                                expr: Cast(
                                                                    ExprCast {
                                                                        attrs: [],
                                                                        expr: Match(
                                                                            ExprMatch {
                                                                                attrs: [],
                                                                                match_token: Match,
                                                                                expr: Array(
                                                                                    ExprArray {
                                                                                        attrs: [],
                                                                                        bracket_token: Bracket,
                                                                                        elems: [],
                                                                                    },
                                                                                ),
                                                                                brace_token: Brace,
                                                                                arms: [
                                                                                    Arm {
                                                                                        attrs: [],
                                                                                        pat: Box(
                                                                                            PatBox {
                                                                                                attrs: [],
                                                                                                box_token: Box,
                                                                                                pat: Ident(
                                                                                                    PatIdent {
                                                                                                        attrs: [],
                                                                                                        by_ref: Some(
                                                                                                            Ref,
                                                                                                        ),
                                                                                                        mutability: None,
                                                                                                        ident: Ident(
                                                                                                            dolor,
                                                                                                        ),
                                                                                                        subpat: Some(
                                                                                                            (
                                                                                                                At,
                                                                                                                Box(
                                                                                                                    PatBox {
                                                                                                                        attrs: [],
                                                                                                                        box_token: Box,
                                                                                                                        pat: Wild(
                                                                                                                            PatWild {
                                                                                                                                attrs: [],
                                                                                                                                underscore_token: Underscore,
                                                                                                                            },
                                                                                                                        ),
                                                                                                                    },
                                                                                                                ),
                                                                                                            ),
                                                                                                        ),
                                                                                                    },
                                                                                                ),
                                                                                            },
                                                                                        ),
                                                                                        guard: None,
                                                                                        fat_arrow_token: FatArrow,
                                                                                        body: Path(
                                                                                            ExprPath {
                                                                                                attrs: [],
                                                                                                qself: None,
                                                                                                path: Path {
                                                                                                    leading_colon: None,
                                                                                                    segments: [
                                                                                                        PathSegment {
                                                                                                            ident: Ident(
                                                                                                                baz,
                                                                                                            ),
                                                                                                            arguments: None,
                                                                                                        },
                                                                                                    ],
                                                                                                },
                                                                                            },
                                                                                        ),
                                                                                        comma: Some(
                                                                                            Comma,
                                                                                        ),
                                                                                    },
                                                                                ],
                                                                            },
                                                                        ),
                                                                        as_token: As,
                                                                        ty: Ptr(
                                                                            TypePtr {
                                                                                star_token: Star,
                                                                                const_token: Some(
                                                                                    Const,
                                                                                ),
                                                                                mutability: None,
                                                                                elem: Array(
                                                                                    TypeArray {
                                                                                        bracket_token: Bracket,
                                                                                        elem: Array(
                                                                                            TypeArray {
                                                                                                bracket_token: Bracket,
                                                                                                elem: Path(
                                                                                                    TypePath {
                                                                                                        qself: None,
                                                                                                        path: Path {
                                                                                                            leading_colon: None,
                                                                                                            segments: [
                                                                                                                PathSegment {
                                                                                                                    ident: Ident(
                                                                                                                        dolor,
                                                                                                                    ),
                                                                                                                    arguments: None,
                                                                                                                },
                                                                                                            ],
                                                                                                        },
                                                                                                    },
                                                                                                ),
                                                                                                semi_token: Semi,
                                                                                                len: Block(
                                                                                                    ExprBlock {
                                                                                                        attrs: [],
                                                                                                        label: Some(
                                                                                                            Label {
                                                                                                                name: Lifetime {
                                                                                                                    apostrophe: Span,
                                                                                                                    ident: Ident(
                                                                                                                        quux,
                                                                                                                    ),
                                                                                                                },
                                                                                                                colon_token: Colon,
                                                                                                            },
                                                                                                        ),
                                                                                                        block: Block {
                                                                                                            brace_token: Brace,
                                                                                                            stmts: [],
                                                                                                        },
                                                                                                    },
                                                                                                ),
                                                                                            },
                                                                                        ),
                                                                                        semi_token: Semi,
                                                                                        len: Continue(
                                                                                            ExprContinue {
                                                                                                attrs: [],
                                                                                                continue_token: Continue,
                                                                                                label: None,
                                                                                            },
                                                                                        ),
                                                                                    },
                                                                                ),
                                                                            },
                                                                        ),
                                                                    },
                                                                ),
                                                            },
                                                        ),
                                                    ),
                                                ),
                                            },
                                        ),
                                    ),
                                ),
                                semi_token: Semi,
                            },
                        ),
                    ],
                },
            },
        ),
    ],
}