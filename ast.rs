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
                        main,
                    ),
                    generics: Generics {
                        lt_token: None,
                        params: [],
                        gt_token: None,
                        where_clause: None,
                    },
                    paren_token: Paren,
                    inputs: [],
                    variadic: None,
                    output: Type(
                        RArrow,
                        Tuple(
                            TypeTuple {
                                paren_token: Paren,
                                elems: [],
                            },
                        ),
                    ),
                },
                block: Block {
                    brace_token: Brace,
                    stmts: [
                        Semi(
                            Return(
                                ExprReturn {
                                    attrs: [],
                                    return_token: Return,
                                    expr: Some(
                                        Unary(
                                            ExprUnary {
                                                attrs: [],
                                                op: Deref(
                                                    Star,
                                                ),
                                                expr: Field(
                                                    ExprField {
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
                                                                                foo,
                                                                            ),
                                                                            arguments: None,
                                                                        },
                                                                    ],
                                                                },
                                                            },
                                                        ),
                                                        dot_token: Dot,
                                                        member: Named(
                                                            Ident(
                                                                ipsum,
                                                            ),
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    ),
                                },
                            ),
                            Semi,
                        ),
                    ],
                },
            },
        ),
        Trait(
            ItemTrait {
                attrs: [],
                vis: Public(
                    VisPublic {
                        pub_token: Pub,
                    },
                ),
                unsafety: None,
                auto_token: None,
                trait_token: Trait,
                ident: Ident(
                    foo,
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
                colon_token: Some(
                    Colon,
                ),
                supertraits: [],
                brace_token: Brace,
                items: [],
            },
        ),
    ],
}