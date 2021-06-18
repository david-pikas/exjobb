File {
    shebang: None,
    attrs: [],
    items: [
        Static(
            ItemStatic {
                attrs: [],
                vis: Inherited,
                static_token: Static,
                mutability: None,
                ident: Ident(
                    ᘼ𘠾,
                ),
                colon_token: Colon,
                ty: Ptr(
                    TypePtr {
                        star_token: Star,
                        const_token: Some(
                            Const,
                        ),
                        mutability: Some(
                            Mut,
                        ),
                        elem: Path(
                            TypePath {
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                r#𒁺𖧯ﬥﴀ𑆴𛄘,
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
                expr: Group(
                    ExprGroup {
                        attrs: [],
                        group_token: Group,
                        expr: While(
                            ExprWhile {
                                attrs: [],
                                label: None,
                                while_token: While,
                                cond: Let(
                                    ExprLet {
                                        attrs: [],
                                        let_token: Let,
                                        pat: Range(
                                            PatRange {
                                                attrs: [],
                                                lo: Lit(
                                                    ExprLit {
                                                        attrs: [],
                                                        lit: Int(
                                                            LitInt {
                                                                token: 945438884u32,
                                                            },
                                                        ),
                                                    },
                                                ),
                                                limits: Closed(
                                                    DotDotEq,
                                                ),
                                                hi: Lit(
                                                    ExprLit {
                                                        attrs: [],
                                                        lit: Bool(
                                                            LitBool {
                                                                value: false,
                                                            },
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                        eq_token: Eq,
                                        expr: AssignOp(
                                            ExprAssignOp {
                                                attrs: [],
                                                left: Paren(
                                                    ExprParen {
                                                        attrs: [],
                                                        paren_token: Paren,
                                                        expr: MethodCall(
                                                            ExprMethodCall {
                                                                attrs: [],
                                                                receiver: Paren(
                                                                    ExprParen {
                                                                        attrs: [],
                                                                        paren_token: Paren,
                                                                        expr: Break(
                                                                            ExprBreak {
                                                                                attrs: [],
                                                                                break_token: Break,
                                                                                label: None,
                                                                                expr: Some(
                                                                                    Block(
                                                                                        ExprBlock {
                                                                                            attrs: [],
                                                                                            label: Some(
                                                                                                Label {
                                                                                                    name: Lifetime {
                                                                                                        apostrophe: Span,
                                                                                                        ident: Ident(
                                                                                                            ﾭ႐Ꞛ𓀼ᒵ,
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
                                                                                ),
                                                                            },
                                                                        ),
                                                                    },
                                                                ),
                                                                dot_token: Dot,
                                                                method: Ident(
                                                                    𓅔ϪԐ𖼶𒎈ῴỿڢ𒇗ﶇ𐌲,
                                                                ),
                                                                turbofish: Some(
                                                                    MethodTurbofish {
                                                                        colon2_token: Colon2,
                                                                        lt_token: Lt,
                                                                        args: [
                                                                            Type(
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
                                                                                                elem: Path(
                                                                                                    TypePath {
                                                                                                        qself: None,
                                                                                                        path: Path {
                                                                                                            leading_colon: None,
                                                                                                            segments: [
                                                                                                                PathSegment {
                                                                                                                    ident: Ident(
                                                                                                                        r#ளℌฃ𓇇ꬮ𞠈ൕꩇᨚ,
                                                                                                                    ),
                                                                                                                    arguments: None,
                                                                                                                },
                                                                                                                Colon2,
                                                                                                                PathSegment {
                                                                                                                    ident: Ident(
                                                                                                                        𒁓𐤒ꢶ𐘗Ǔ𛁓𑂆,
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
                                                                            ),
                                                                        ],
                                                                        gt_token: Gt,
                                                                    },
                                                                ),
                                                                paren_token: Paren,
                                                                args: [],
                                                            },
                                                        ),
                                                    },
                                                ),
                                                op: MulEq(
                                                    MulEq,
                                                ),
                                                right: Array(
                                                    ExprArray {
                                                        attrs: [],
                                                        bracket_token: Bracket,
                                                        elems: [],
                                                    },
                                                ),
                                            },
                                        ),
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
                semi_token: Semi,
            },
        ),
    ],
}