File {
    shebang: None,
    attrs: [
        Attribute {
            pound_token: Pound,
            style: Inner(
                Bang,
            ),
            bracket_token: Bracket,
            path: Path {
                leading_colon: None,
                segments: [
                    PathSegment {
                        ident: Ident(
                            no_main,
                        ),
                        arguments: None,
                    },
                ],
            },
            tokens: TokenStream [],
        },
        Attribute {
            pound_token: Pound,
            style: Inner(
                Bang,
            ),
            bracket_token: Bracket,
            path: Path {
                leading_colon: None,
                segments: [
                    PathSegment {
                        ident: Ident(
                            feature,
                        ),
                        arguments: None,
                    },
                ],
            },
            tokens: TokenStream [
                Group {
                    delimiter: Parenthesis,
                    stream: TokenStream [
                        Ident {
                            sym: non_ascii_idents,
                        },
                    ],
                },
            ],
        },
    ],
    items: [
        Type(
            ItemType {
                attrs: [],
                vis: Inherited,
                type_token: Type,
                ident: Ident(
                    r#ȣq,
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
                eq_token: Eq,
                ty: ImplTrait(
                    TypeImplTrait {
                        impl_token: Impl,
                        bounds: [
                            Lifetime(
                                Lifetime {
                                    apostrophe: Span,
                                    ident: Ident(
                                        r#ÓXUrL91CW,
                                    ),
                                },
                            ),
                        ],
                    },
                ),
                semi_token: Semi,
            },
        ),
    ],
}