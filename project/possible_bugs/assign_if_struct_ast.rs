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
                    foo,
                ),
                colon_token: Colon,
                ty: Ptr(
                    TypePtr {
                        star_token: Star,
                        const_token: Some(
                            Const,
                        ),
                        mutability: None,
                        elem: Tuple(
                            TypeTuple {
                                paren_token: Paren,
                                elems: [
                                    Paren(
                                        TypeParen {
                                            paren_token: Paren,
                                            elem: Slice(
                                                TypeSlice {
                                                    bracket_token: Bracket,
                                                    elem: Path(
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
                                ],
                            },
                        ),
                    },
                ),
                eq_token: Eq,
                expr: Array(
                    ExprArray {
                        attrs: [],
                        bracket_token: Bracket,
                        elems: [
                            If(
                                ExprIf {
                                    attrs: [],
                                    if_token: If,
                                    cond: Paren(
                                        ExprParen {
                                            attrs: [],
                                            paren_token: Paren,
                                            expr: While(
                                                ExprWhile {
                                                    attrs: [],
                                                    label: None,
                                                    while_token: While,
                                                    cond: Paren(
                                                        ExprParen {
                                                            attrs: [],
                                                            paren_token: Paren,
                                                            expr: If(
                                                                ExprIf {
                                                                    attrs: [],
                                                                    if_token: If,
                                                                    cond: Paren(
                                                                        ExprParen {
                                                                            attrs: [],
                                                                            paren_token: Paren,
                                                                            expr: Assign(
                                                                                ExprAssign {
                                                                                    attrs: [],
                                                                                    left: Unary(
                                                                                        ExprUnary {
                                                                                            attrs: [],
                                                                                            op: Deref(
                                                                                                Star,
                                                                                            ),
                                                                                            expr: AssignOp(
                                                                                                ExprAssignOp {
                                                                                                    attrs: [],
                                                                                                    left: AssignOp(
                                                                                                        ExprAssignOp {
                                                                                                            attrs: [],
                                                                                                            left: Lit(
                                                                                                                ExprLit {
                                                                                                                    attrs: [],
                                                                                                                    lit: Float(
                                                                                                                        LitFloat {
                                                                                                                            token: 0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003706486043333249f64,
                                                                                                                        },
                                                                                                                    ),
                                                                                                                },
                                                                                                            ),
                                                                                                            op: Add(
                                                                                                                Add,
                                                                                                            ),
                                                                                                            right: Loop(
                                                                                                                ExprLoop {
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
                                                                                                                    loop_token: Loop,
                                                                                                                    body: Block {
                                                                                                                        brace_token: Brace,
                                                                                                                        stmts: [],
                                                                                                                    },
                                                                                                                },
                                                                                                            ),
                                                                                                        },
                                                                                                    ),
                                                                                                    op: Div(
                                                                                                        Div,
                                                                                                    ),
                                                                                                    right: Unary(
                                                                                                        ExprUnary {
                                                                                                            attrs: [],
                                                                                                            op: Deref(
                                                                                                                Star,
                                                                                                            ),
                                                                                                            expr: Path(
                                                                                                                ExprPath {
                                                                                                                    attrs: [],
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
                                                                                    eq_token: Eq,
                                                                                    right: Path(
                                                                                        ExprPath {
                                                                                            attrs: [],
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
                                                                    then_branch: Block {
                                                                        brace_token: Brace,
                                                                        stmts: [],
                                                                    },
                                                                    else_branch: None,
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
                                    then_branch: Block {
                                        brace_token: Brace,
                                        stmts: [],
                                    },
                                    else_branch: Some(
                                        (
                                            Else,
                                            Index(
                                                ExprIndex {
                                                    attrs: [],
                                                    expr: Paren(
                                                        ExprParen {
                                                            attrs: [],
                                                            paren_token: Paren,
                                                            expr: Let(
                                                                ExprLet {
                                                                    attrs: [],
                                                                    let_token: Let,
                                                                    pat: Tuple(
                                                                        PatTuple {
                                                                            attrs: [],
                                                                            paren_token: Paren,
                                                                            elems: [
                                                                                Ident(
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
                                                                                                Tuple(
                                                                                                    PatTuple {
                                                                                                        attrs: [],
                                                                                                        paren_token: Paren,
                                                                                                        elems: [
                                                                                                            Box(
                                                                                                                PatBox {
                                                                                                                    attrs: [],
                                                                                                                    box_token: Box,
                                                                                                                    pat: Slice(
                                                                                                                        PatSlice {
                                                                                                                            attrs: [],
                                                                                                                            bracket_token: Bracket,
                                                                                                                            elems: [
                                                                                                                                Box(
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
                                                                                                                                                                dolor,
                                                                                                                                                            ),
                                                                                                                                                            arguments: None,
                                                                                                                                                        },
                                                                                                                                                    ],
                                                                                                                                                },
                                                                                                                                            },
                                                                                                                                        ),
                                                                                                                                    },
                                                                                                                                ),
                                                                                                                                Comma,
                                                                                                                                Box(
                                                                                                                                    PatBox {
                                                                                                                                        attrs: [],
                                                                                                                                        box_token: Box,
                                                                                                                                        pat: Slice(
                                                                                                                                            PatSlice {
                                                                                                                                                attrs: [],
                                                                                                                                                bracket_token: Bracket,
                                                                                                                                                elems: [],
                                                                                                                                            },
                                                                                                                                        ),
                                                                                                                                    },
                                                                                                                                ),
                                                                                                                            ],
                                                                                                                        },
                                                                                                                    ),
                                                                                                                },
                                                                                                            ),
                                                                                                        ],
                                                                                                    },
                                                                                                ),
                                                                                            ),
                                                                                        ),
                                                                                    },
                                                                                ),
                                                                                Comma,
                                                                                Rest(
                                                                                    PatRest {
                                                                                        attrs: [],
                                                                                        dot2_token: Dot2,
                                                                                    },
                                                                                ),
                                                                            ],
                                                                        },
                                                                    ),
                                                                    eq_token: Eq,
                                                                    expr: Lit(
                                                                        ExprLit {
                                                                            attrs: [],
                                                                            lit: Int(
                                                                                LitInt {
                                                                                    token: 3582019037u32,
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            ),
                                                        },
                                                    ),
                                                    bracket_token: Bracket,
                                                    index: Tuple(
                                                        ExprTuple {
                                                            attrs: [],
                                                            paren_token: Paren,
                                                            elems: [
                                                                Let(
                                                                    ExprLet {
                                                                        attrs: [],
                                                                        let_token: Let,
                                                                        pat: Slice(
                                                                            PatSlice {
                                                                                attrs: [],
                                                                                bracket_token: Bracket,
                                                                                elems: [],
                                                                            },
                                                                        ),
                                                                        eq_token: Eq,
                                                                        expr: Struct(
                                                                            ExprStruct {
                                                                                attrs: [],
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
                                                                                brace_token: Brace,
                                                                                fields: [
                                                                                    FieldValue {
                                                                                        attrs: [],
                                                                                        member: Unnamed(
                                                                                            Index {
                                                                                                index: 796176504,
                                                                                                span: Span,
                                                                                            },
                                                                                        ),
                                                                                        colon_token: Some(
                                                                                            Colon,
                                                                                        ),
                                                                                        expr: Try(
                                                                                            ExprTry {
                                                                                                attrs: [],
                                                                                                expr: Paren(
                                                                                                    ExprParen {
                                                                                                        attrs: [],
                                                                                                        paren_token: Paren,
                                                                                                        expr: Block(
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
                                                                                                question_token: Question,
                                                                                            },
                                                                                        ),
                                                                                    },
                                                                                ],
                                                                                dot2_token: Some(
                                                                                    Dot2,
                                                                                ),
                                                                                rest: Some(
                                                                                    Assign(
                                                                                        ExprAssign {
                                                                                            attrs: [],
                                                                                            left: If(
                                                                                                ExprIf {
                                                                                                    attrs: [],
                                                                                                    if_token: If,
                                                                                                    cond: Paren(
                                                                                                        ExprParen {
                                                                                                            attrs: [],
                                                                                                            paren_token: Paren,
                                                                                                            expr: Reference(
                                                                                                                ExprReference {
                                                                                                                    attrs: [],
                                                                                                                    and_token: And,
                                                                                                                    raw: Reserved,
                                                                                                                    mutability: None,
                                                                                                                    expr: Paren(
                                                                                                                        ExprParen {
                                                                                                                            attrs: [],
                                                                                                                            paren_token: Paren,
                                                                                                                            expr: Binary(
                                                                                                                                ExprBinary {
                                                                                                                                    attrs: [],
                                                                                                                                    left: Paren(
                                                                                                                                        ExprParen {
                                                                                                                                            attrs: [],
                                                                                                                                            paren_token: Paren,
                                                                                                                                            expr: Let(
                                                                                                                                                ExprLet {
                                                                                                                                                    attrs: [],
                                                                                                                                                    let_token: Let,
                                                                                                                                                    pat: Rest(
                                                                                                                                                        PatRest {
                                                                                                                                                            attrs: [],
                                                                                                                                                            dot2_token: Dot2,
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                    eq_token: Eq,
                                                                                                                                                    expr: While(
                                                                                                                                                        ExprWhile {
                                                                                                                                                            attrs: [],
                                                                                                                                                            label: Some(
                                                                                                                                                                Label {
                                                                                                                                                                    name: Lifetime {
                                                                                                                                                                        apostrophe: Span,
                                                                                                                                                                        ident: Ident(
                                                                                                                                                                            lorem,
                                                                                                                                                                        ),
                                                                                                                                                                    },
                                                                                                                                                                    colon_token: Colon,
                                                                                                                                                                },
                                                                                                                                                            ),
                                                                                                                                                            while_token: While,
                                                                                                                                                            cond: Paren(
                                                                                                                                                                ExprParen {
                                                                                                                                                                    attrs: [],
                                                                                                                                                                    paren_token: Paren,
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
                                                                                                                                                                            expr: Try(
                                                                                                                                                                                ExprTry {
                                                                                                                                                                                    attrs: [],
                                                                                                                                                                                    expr: Paren(
                                                                                                                                                                                        ExprParen {
                                                                                                                                                                                            attrs: [],
                                                                                                                                                                                            paren_token: Paren,
                                                                                                                                                                                            expr: Break(
                                                                                                                                                                                                ExprBreak {
                                                                                                                                                                                                    attrs: [],
                                                                                                                                                                                                    break_token: Break,
                                                                                                                                                                                                    label: None,
                                                                                                                                                                                                    expr: Some(
                                                                                                                                                                                                        Paren(
                                                                                                                                                                                                            ExprParen {
                                                                                                                                                                                                                attrs: [],
                                                                                                                                                                                                                paren_token: Paren,
                                                                                                                                                                                                                expr: Range(
                                                                                                                                                                                                                    ExprRange {
                                                                                                                                                                                                                        attrs: [],
                                                                                                                                                                                                                        from: None,
                                                                                                                                                                                                                        limits: HalfOpen(
                                                                                                                                                                                                                            Dot2,
                                                                                                                                                                                                                        ),
                                                                                                                                                                                                                        to: Some(
                                                                                                                                                                                                                            Paren(
                                                                                                                                                                                                                                ExprParen {
                                                                                                                                                                                                                                    attrs: [],
                                                                                                                                                                                                                                    paren_token: Paren,
                                                                                                                                                                                                                                    expr: ForLoop(
                                                                                                                                                                                                                                        ExprForLoop {
                                                                                                                                                                                                                                            attrs: [],
                                                                                                                                                                                                                                            label: None,
                                                                                                                                                                                                                                            for_token: For,
                                                                                                                                                                                                                                            pat: Reference(
                                                                                                                                                                                                                                                PatReference {
                                                                                                                                                                                                                                                    attrs: [],
                                                                                                                                                                                                                                                    and_token: And,
                                                                                                                                                                                                                                                    mutability: None,
                                                                                                                                                                                                                                                    pat: Reference(
                                                                                                                                                                                                                                                        PatReference {
                                                                                                                                                                                                                                                            attrs: [],
                                                                                                                                                                                                                                                            and_token: And,
                                                                                                                                                                                                                                                            mutability: None,
                                                                                                                                                                                                                                                            pat: Ident(
                                                                                                                                                                                                                                                                PatIdent {
                                                                                                                                                                                                                                                                    attrs: [],
                                                                                                                                                                                                                                                                    by_ref: None,
                                                                                                                                                                                                                                                                    mutability: Some(
                                                                                                                                                                                                                                                                        Mut,
                                                                                                                                                                                                                                                                    ),
                                                                                                                                                                                                                                                                    ident: Ident(
                                                                                                                                                                                                                                                                        lorem,
                                                                                                                                                                                                                                                                    ),
                                                                                                                                                                                                                                                                    subpat: Some(
                                                                                                                                                                                                                                                                        (
                                                                                                                                                                                                                                                                            At,
                                                                                                                                                                                                                                                                            Reference(
                                                                                                                                                                                                                                                                                PatReference {
                                                                                                                                                                                                                                                                                    attrs: [],
                                                                                                                                                                                                                                                                                    and_token: And,
                                                                                                                                                                                                                                                                                    mutability: None,
                                                                                                                                                                                                                                                                                    pat: Rest(
                                                                                                                                                                                                                                                                                        PatRest {
                                                                                                                                                                                                                                                                                            attrs: [],
                                                                                                                                                                                                                                                                                            dot2_token: Dot2,
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
                                                                                                                                                                                                                                                },
                                                                                                                                                                                                                                            ),
                                                                                                                                                                                                                                            in_token: In,
                                                                                                                                                                                                                                            expr: Array(
                                                                                                                                                                                                                                                ExprArray {
                                                                                                                                                                                                                                                    attrs: [],
                                                                                                                                                                                                                                                    bracket_token: Bracket,
                                                                                                                                                                                                                                                    elems: [],
                                                                                                                                                                                                                                                },
                                                                                                                                                                                                                                            ),
                                                                                                                                                                                                                                            body: Block {
                                                                                                                                                                                                                                                brace_token: Brace,
                                                                                                                                                                                                                                                stmts: [
                                                                                                                                                                                                                                                    Local(
                                                                                                                                                                                                                                                        Local {
                                                                                                                                                                                                                                                            attrs: [],
                                                                                                                                                                                                                                                            let_token: Let,
                                                                                                                                                                                                                                                            pat: Reference(
                                                                                                                                                                                                                                                                PatReference {
                                                                                                                                                                                                                                                                    attrs: [],
                                                                                                                                                                                                                                                                    and_token: And,
                                                                                                                                                                                                                                                                    mutability: None,
                                                                                                                                                                                                                                                                    pat: Tuple(
                                                                                                                                                                                                                                                                        PatTuple {
                                                                                                                                                                                                                                                                            attrs: [],
                                                                                                                                                                                                                                                                            paren_token: Paren,
                                                                                                                                                                                                                                                                            elems: [
                                                                                                                                                                                                                                                                                Path(
                                                                                                                                                                                                                                                                                    PatPath {
                                                                                                                                                                                                                                                                                        attrs: [],
                                                                                                                                                                                                                                                                                        qself: None,
                                                                                                                                                                                                                                                                                        path: Path {
                                                                                                                                                                                                                                                                                            leading_colon: None,
                                                                                                                                                                                                                                                                                            segments: [
                                                                                                                                                                                                                                                                                                PathSegment {
                                                                                                                                                                                                                                                                                                    ident: Ident(
                                                                                                                                                                                                                                                                                                        quux,
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
                                                                                                                                                                                                                                                            init: None,
                                                                                                                                                                                                                                                            semi_token: Semi,
                                                                                                                                                                                                                                                        },
                                                                                                                                                                                                                                                    ),
                                                                                                                                                                                                                                                ],
                                                                                                                                                                                                                                            },
                                                                                                                                                                                                                                        },
                                                                                                                                                                                                                                    ),
                                                                                                                                                                                                                                },
                                                                                                                                                                                                                            ),
                                                                                                                                                                                                                        ),
                                                                                                                                                                                                                    },
                                                                                                                                                                                                                ),
                                                                                                                                                                                                            },
                                                                                                                                                                                                        ),
                                                                                                                                                                                                    ),
                                                                                                                                                                                                },
                                                                                                                                                                                            ),
                                                                                                                                                                                        },
                                                                                                                                                                                    ),
                                                                                                                                                                                    question_token: Question,
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
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                    op: Sub(
                                                                                                                                        Sub,
                                                                                                                                    ),
                                                                                                                                    right: Field(
                                                                                                                                        ExprField {
                                                                                                                                            attrs: [],
                                                                                                                                            base: Paren(
                                                                                                                                                ExprParen {
                                                                                                                                                    attrs: [],
                                                                                                                                                    paren_token: Paren,
                                                                                                                                                    expr: Box(
                                                                                                                                                        ExprBox {
                                                                                                                                                            attrs: [],
                                                                                                                                                            box_token: Box,
                                                                                                                                                            expr: Try(
                                                                                                                                                                ExprTry {
                                                                                                                                                                    attrs: [],
                                                                                                                                                                    expr: Path(
                                                                                                                                                                        ExprPath {
                                                                                                                                                                            attrs: [],
                                                                                                                                                                            qself: None,
                                                                                                                                                                            path: Path {
                                                                                                                                                                                leading_colon: None,
                                                                                                                                                                                segments: [
                                                                                                                                                                                    PathSegment {
                                                                                                                                                                                        ident: Ident(
                                                                                                                                                                                            quux,
                                                                                                                                                                                        ),
                                                                                                                                                                                        arguments: None,
                                                                                                                                                                                    },
                                                                                                                                                                                ],
                                                                                                                                                                            },
                                                                                                                                                                        },
                                                                                                                                                                    ),
                                                                                                                                                                    question_token: Question,
                                                                                                                                                                },
                                                                                                                                                            ),
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                            dot_token: Dot,
                                                                                                                                            member: Unnamed(
                                                                                                                                                Index {
                                                                                                                                                    index: 2886770147,
                                                                                                                                                    span: Span,
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
                                                                                                    then_branch: Block {
                                                                                                        brace_token: Brace,
                                                                                                        stmts: [],
                                                                                                    },
                                                                                                    else_branch: None,
                                                                                                },
                                                                                            ),
                                                                                            eq_token: Eq,
                                                                                            right: Repeat(
                                                                                                ExprRepeat {
                                                                                                    attrs: [],
                                                                                                    bracket_token: Bracket,
                                                                                                    expr: ForLoop(
                                                                                                        ExprForLoop {
                                                                                                            attrs: [],
                                                                                                            label: None,
                                                                                                            for_token: For,
                                                                                                            pat: Slice(
                                                                                                                PatSlice {
                                                                                                                    attrs: [],
                                                                                                                    bracket_token: Bracket,
                                                                                                                    elems: [],
                                                                                                                },
                                                                                                            ),
                                                                                                            in_token: In,
                                                                                                            expr: Paren(
                                                                                                                ExprParen {
                                                                                                                    attrs: [],
                                                                                                                    paren_token: Paren,
                                                                                                                    expr: Box(
                                                                                                                        ExprBox {
                                                                                                                            attrs: [],
                                                                                                                            box_token: Box,
                                                                                                                            expr: Paren(
                                                                                                                                ExprParen {
                                                                                                                                    attrs: [],
                                                                                                                                    paren_token: Paren,
                                                                                                                                    expr: ForLoop(
                                                                                                                                        ExprForLoop {
                                                                                                                                            attrs: [],
                                                                                                                                            label: Some(
                                                                                                                                                Label {
                                                                                                                                                    name: Lifetime {
                                                                                                                                                        apostrophe: Span,
                                                                                                                                                        ident: Ident(
                                                                                                                                                            baz,
                                                                                                                                                        ),
                                                                                                                                                    },
                                                                                                                                                    colon_token: Colon,
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                            for_token: For,
                                                                                                                                            pat: Tuple(
                                                                                                                                                PatTuple {
                                                                                                                                                    attrs: [],
                                                                                                                                                    paren_token: Paren,
                                                                                                                                                    elems: [
                                                                                                                                                        Box(
                                                                                                                                                            PatBox {
                                                                                                                                                                attrs: [],
                                                                                                                                                                box_token: Box,
                                                                                                                                                                pat: Reference(
                                                                                                                                                                    PatReference {
                                                                                                                                                                        attrs: [],
                                                                                                                                                                        and_token: And,
                                                                                                                                                                        mutability: None,
                                                                                                                                                                        pat: Box(
                                                                                                                                                                            PatBox {
                                                                                                                                                                                attrs: [],
                                                                                                                                                                                box_token: Box,
                                                                                                                                                                                pat: Reference(
                                                                                                                                                                                    PatReference {
                                                                                                                                                                                        attrs: [],
                                                                                                                                                                                        and_token: And,
                                                                                                                                                                                        mutability: None,
                                                                                                                                                                                        pat: Reference(
                                                                                                                                                                                            PatReference {
                                                                                                                                                                                                attrs: [],
                                                                                                                                                                                                and_token: And,
                                                                                                                                                                                                mutability: None,
                                                                                                                                                                                                pat: Reference(
                                                                                                                                                                                                    PatReference {
                                                                                                                                                                                                        attrs: [],
                                                                                                                                                                                                        and_token: And,
                                                                                                                                                                                                        mutability: None,
                                                                                                                                                                                                        pat: Ident(
                                                                                                                                                                                                            PatIdent {
                                                                                                                                                                                                                attrs: [],
                                                                                                                                                                                                                by_ref: Some(
                                                                                                                                                                                                                    Ref,
                                                                                                                                                                                                                ),
                                                                                                                                                                                                                mutability: Some(
                                                                                                                                                                                                                    Mut,
                                                                                                                                                                                                                ),
                                                                                                                                                                                                                ident: Ident(
                                                                                                                                                                                                                    quux,
                                                                                                                                                                                                                ),
                                                                                                                                                                                                                subpat: Some(
                                                                                                                                                                                                                    (
                                                                                                                                                                                                                        At,
                                                                                                                                                                                                                        Tuple(
                                                                                                                                                                                                                            PatTuple {
                                                                                                                                                                                                                                attrs: [],
                                                                                                                                                                                                                                paren_token: Paren,
                                                                                                                                                                                                                                elems: [
                                                                                                                                                                                                                                    Reference(
                                                                                                                                                                                                                                        PatReference {
                                                                                                                                                                                                                                            attrs: [],
                                                                                                                                                                                                                                            and_token: And,
                                                                                                                                                                                                                                            mutability: None,
                                                                                                                                                                                                                                            pat: Wild(
                                                                                                                                                                                                                                                PatWild {
                                                                                                                                                                                                                                                    attrs: [],
                                                                                                                                                                                                                                                    underscore_token: Underscore,
                                                                                                                                                                                                                                                },
                                                                                                                                                                                                                                            ),
                                                                                                                                                                                                                                        },
                                                                                                                                                                                                                                    ),
                                                                                                                                                                                                                                ],
                                                                                                                                                                                                                            },
                                                                                                                                                                                                                        ),
                                                                                                                                                                                                                    ),
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
                                                                                                                                                                    },
                                                                                                                                                                ),
                                                                                                                                                            },
                                                                                                                                                        ),
                                                                                                                                                    ],
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                            in_token: In,
                                                                                                                                            expr: Paren(
                                                                                                                                                ExprParen {
                                                                                                                                                    attrs: [],
                                                                                                                                                    paren_token: Paren,
                                                                                                                                                    expr: Closure(
                                                                                                                                                        ExprClosure {
                                                                                                                                                            attrs: [],
                                                                                                                                                            asyncness: Some(
                                                                                                                                                                Async,
                                                                                                                                                            ),
                                                                                                                                                            movability: None,
                                                                                                                                                            capture: Some(
                                                                                                                                                                Move,
                                                                                                                                                            ),
                                                                                                                                                            or1_token: Or,
                                                                                                                                                            inputs: [],
                                                                                                                                                            or2_token: Or,
                                                                                                                                                            output: Type(
                                                                                                                                                                RArrow,
                                                                                                                                                                Array(
                                                                                                                                                                    TypeArray {
                                                                                                                                                                        bracket_token: Bracket,
                                                                                                                                                                        elem: ImplTrait(
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
                                                                                                                                                                        semi_token: Semi,
                                                                                                                                                                        len: Paren(
                                                                                                                                                                            ExprParen {
                                                                                                                                                                                attrs: [],
                                                                                                                                                                                paren_token: Paren,
                                                                                                                                                                                expr: Paren(
                                                                                                                                                                                    ExprParen {
                                                                                                                                                                                        attrs: [],
                                                                                                                                                                                        paren_token: Paren,
                                                                                                                                                                                        expr: Binary(
                                                                                                                                                                                            ExprBinary {
                                                                                                                                                                                                attrs: [],
                                                                                                                                                                                                left: Path(
                                                                                                                                                                                                    ExprPath {
                                                                                                                                                                                                        attrs: [],
                                                                                                                                                                                                        qself: None,
                                                                                                                                                                                                        path: Path {
                                                                                                                                                                                                            leading_colon: None,
                                                                                                                                                                                                            segments: [
                                                                                                                                                                                                                PathSegment {
                                                                                                                                                                                                                    ident: Ident(
                                                                                                                                                                                                                        bar,
                                                                                                                                                                                                                    ),
                                                                                                                                                                                                                    arguments: None,
                                                                                                                                                                                                                },
                                                                                                                                                                                                                Colon2,
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
                                                                                                                                                                                                op: Sub(
                                                                                                                                                                                                    Sub,
                                                                                                                                                                                                ),
                                                                                                                                                                                                right: Lit(
                                                                                                                                                                                                    ExprLit {
                                                                                                                                                                                                        attrs: [],
                                                                                                                                                                                                        lit: Bool(
                                                                                                                                                                                                            LitBool {
                                                                                                                                                                                                                value: true,
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
                                                                                                    semi_token: Semi,
                                                                                                    len: Path(
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
                                                                                                },
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                ),
                                                                            },
                                                                        ),
                                                                    },
                                                                ),
                                                            ],
                                                        },
                                                    ),
                                                },
                                            ),
                                        ),
                                    ),
                                },
                            ),
                        ],
                    },
                ),
                semi_token: Semi,
            },
        ),
        Static(
            ItemStatic {
                attrs: [],
                vis: Inherited,
                static_token: Static,
                mutability: None,
                ident: Ident(
                    ipsum,
                ),
                colon_token: Colon,
                ty: Infer(
                    TypeInfer {
                        underscore_token: Underscore,
                    },
                ),
                eq_token: Eq,
                expr: Paren(
                    ExprParen {
                        attrs: [],
                        paren_token: Paren,
                        expr: If(
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
                                                expr: Assign(
                                                    ExprAssign {
                                                        attrs: [],
                                                        left: Async(
                                                            ExprAsync {
                                                                attrs: [],
                                                                async_token: Async,
                                                                capture: None,
                                                                block: Block {
                                                                    brace_token: Brace,
                                                                    stmts: [],
                                                                },
                                                            },
                                                        ),
                                                        eq_token: Eq,
                                                        right: Loop(
                                                            ExprLoop {
                                                                attrs: [],
                                                                label: None,
                                                                loop_token: Loop,
                                                                body: Block {
                                                                    brace_token: Brace,
                                                                    stmts: [],
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
                                else_branch: None,
                            },
                        ),
                    },
                ),
                semi_token: Semi,
            },
        ),
        Static(
            ItemStatic {
                attrs: [],
                vis: Inherited,
                static_token: Static,
                mutability: None,
                ident: Ident(
                    dolor,
                ),
                colon_token: Colon,
                ty: Reference(
                    TypeReference {
                        and_token: And,
                        lifetime: None,
                        mutability: Some(
                            Mut,
                        ),
                        elem: Paren(
                            TypeParen {
                                paren_token: Paren,
                                elem: Reference(
                                    TypeReference {
                                        and_token: And,
                                        lifetime: Some(
                                            Lifetime {
                                                apostrophe: Span,
                                                ident: Ident(
                                                    foo,
                                                ),
                                            },
                                        ),
                                        mutability: None,
                                        elem: Array(
                                            TypeArray {
                                                bracket_token: Bracket,
                                                elem: BareFn(
                                                    TypeBareFn {
                                                        lifetimes: None,
                                                        unsafety: Some(
                                                            Unsafe,
                                                        ),
                                                        abi: None,
                                                        fn_token: Fn,
                                                        paren_token: Paren,
                                                        inputs: [],
                                                        variadic: None,
                                                        output: Type(
                                                            RArrow,
                                                            Array(
                                                                TypeArray {
                                                                    bracket_token: Bracket,
                                                                    elem: Array(
                                                                        TypeArray {
                                                                            bracket_token: Bracket,
                                                                            elem: Reference(
                                                                                TypeReference {
                                                                                    and_token: And,
                                                                                    lifetime: None,
                                                                                    mutability: Some(
                                                                                        Mut,
                                                                                    ),
                                                                                    elem: Paren(
                                                                                        TypeParen {
                                                                                            paren_token: Paren,
                                                                                            elem: ImplTrait(
                                                                                                TypeImplTrait {
                                                                                                    impl_token: Impl,
                                                                                                    bounds: [
                                                                                                        Lifetime(
                                                                                                            Lifetime {
                                                                                                                apostrophe: Span,
                                                                                                                ident: Ident(
                                                                                                                    dolor,
                                                                                                                ),
                                                                                                            },
                                                                                                        ),
                                                                                                    ],
                                                                                                },
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                            semi_token: Semi,
                                                                            len: Range(
                                                                                ExprRange {
                                                                                    attrs: [],
                                                                                    from: None,
                                                                                    limits: Closed(
                                                                                        DotDotEq,
                                                                                    ),
                                                                                    to: Some(
                                                                                        Path(
                                                                                            ExprPath {
                                                                                                attrs: [],
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
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                    semi_token: Semi,
                                                                    len: AssignOp(
                                                                        ExprAssignOp {
                                                                            attrs: [],
                                                                            left: Lit(
                                                                                ExprLit {
                                                                                    attrs: [],
                                                                                    lit: Float(
                                                                                        LitFloat {
                                                                                            token: 0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000015868696090656947f64,
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                            op: Mul(
                                                                                Star,
                                                                            ),
                                                                            right: Block(
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
                                                                },
                                                            ),
                                                        ),
                                                    },
                                                ),
                                                semi_token: Semi,
                                                len: AssignOp(
                                                    ExprAssignOp {
                                                        attrs: [],
                                                        left: MethodCall(
                                                            ExprMethodCall {
                                                                attrs: [],
                                                                receiver: Unary(
                                                                    ExprUnary {
                                                                        attrs: [],
                                                                        op: Deref(
                                                                            Star,
                                                                        ),
                                                                        expr: Unary(
                                                                            ExprUnary {
                                                                                attrs: [],
                                                                                op: Neg(
                                                                                    Sub,
                                                                                ),
                                                                                expr: Tuple(
                                                                                    ExprTuple {
                                                                                        attrs: [],
                                                                                        paren_token: Paren,
                                                                                        elems: [
                                                                                            Break(
                                                                                                ExprBreak {
                                                                                                    attrs: [],
                                                                                                    break_token: Break,
                                                                                                    label: Some(
                                                                                                        Lifetime {
                                                                                                            apostrophe: Span,
                                                                                                            ident: Ident(
                                                                                                                foo,
                                                                                                            ),
                                                                                                        },
                                                                                                    ),
                                                                                                    expr: Some(
                                                                                                        Paren(
                                                                                                            ExprParen {
                                                                                                                attrs: [],
                                                                                                                paren_token: Paren,
                                                                                                                expr: Struct(
                                                                                                                    ExprStruct {
                                                                                                                        attrs: [],
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
                                                                                                                        brace_token: Brace,
                                                                                                                        fields: [],
                                                                                                                        dot2_token: Some(
                                                                                                                            Dot2,
                                                                                                                        ),
                                                                                                                        rest: Some(
                                                                                                                            Tuple(
                                                                                                                                ExprTuple {
                                                                                                                                    attrs: [],
                                                                                                                                    paren_token: Paren,
                                                                                                                                    elems: [
                                                                                                                                        While(
                                                                                                                                            ExprWhile {
                                                                                                                                                attrs: [],
                                                                                                                                                label: None,
                                                                                                                                                while_token: While,
                                                                                                                                                cond: Paren(
                                                                                                                                                    ExprParen {
                                                                                                                                                        attrs: [],
                                                                                                                                                        paren_token: Paren,
                                                                                                                                                        expr: Continue(
                                                                                                                                                            ExprContinue {
                                                                                                                                                                attrs: [],
                                                                                                                                                                continue_token: Continue,
                                                                                                                                                                label: Some(
                                                                                                                                                                    Lifetime {
                                                                                                                                                                        apostrophe: Span,
                                                                                                                                                                        ident: Ident(
                                                                                                                                                                            dolor,
                                                                                                                                                                        ),
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
                                                                                                                                    ],
                                                                                                                                },
                                                                                                                            ),
                                                                                                                        ),
                                                                                                                    },
                                                                                                                ),
                                                                                                            },
                                                                                                        ),
                                                                                                    ),
                                                                                                },
                                                                                            ),
                                                                                        ],
                                                                                    },
                                                                                ),
                                                                            },
                                                                        ),
                                                                    },
                                                                ),
                                                                dot_token: Dot,
                                                                method: Ident(
                                                                    quux,
                                                                ),
                                                                turbofish: Some(
                                                                    MethodTurbofish {
                                                                        colon2_token: Colon2,
                                                                        lt_token: Lt,
                                                                        args: [
                                                                            Type(
                                                                                Group(
                                                                                    TypeGroup {
                                                                                        group_token: Group,
                                                                                        elem: Paren(
                                                                                            TypeParen {
                                                                                                paren_token: Paren,
                                                                                                elem: Reference(
                                                                                                    TypeReference {
                                                                                                        and_token: And,
                                                                                                        lifetime: None,
                                                                                                        mutability: Some(
                                                                                                            Mut,
                                                                                                        ),
                                                                                                        elem: Paren(
                                                                                                            TypeParen {
                                                                                                                paren_token: Paren,
                                                                                                                elem: ImplTrait(
                                                                                                                    TypeImplTrait {
                                                                                                                        impl_token: Impl,
                                                                                                                        bounds: [
                                                                                                                            Lifetime(
                                                                                                                                Lifetime {
                                                                                                                                    apostrophe: Span,
                                                                                                                                    ident: Ident(
                                                                                                                                        bar,
                                                                                                                                    ),
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
                                                                            ),
                                                                            Comma,
                                                                            Type(
                                                                                Infer(
                                                                                    TypeInfer {
                                                                                        underscore_token: Underscore,
                                                                                    },
                                                                                ),
                                                                            ),
                                                                            Comma,
                                                                            Type(
                                                                                TraitObject(
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
                                                                            ),
                                                                            Comma,
                                                                            Type(
                                                                                Path(
                                                                                    TypePath {
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
                                                                                                Colon2,
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
                                                                            ),
                                                                            Comma,
                                                                            Type(
                                                                                Tuple(
                                                                                    TypeTuple {
                                                                                        paren_token: Paren,
                                                                                        elems: [
                                                                                            Infer(
                                                                                                TypeInfer {
                                                                                                    underscore_token: Underscore,
                                                                                                },
                                                                                            ),
                                                                                            Comma,
                                                                                            Tuple(
                                                                                                TypeTuple {
                                                                                                    paren_token: Paren,
                                                                                                    elems: [
                                                                                                        Ptr(
                                                                                                            TypePtr {
                                                                                                                star_token: Star,
                                                                                                                const_token: Some(
                                                                                                                    Const,
                                                                                                                ),
                                                                                                                mutability: Some(
                                                                                                                    Mut,
                                                                                                                ),
                                                                                                                elem: Tuple(
                                                                                                                    TypeTuple {
                                                                                                                        paren_token: Paren,
                                                                                                                        elems: [
                                                                                                                            Path(
                                                                                                                                TypePath {
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
                                                                                                                                            Colon2,
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
                                                                                                                            Comma,
                                                                                                                            Array(
                                                                                                                                TypeArray {
                                                                                                                                    bracket_token: Bracket,
                                                                                                                                    elem: BareFn(
                                                                                                                                        TypeBareFn {
                                                                                                                                            lifetimes: None,
                                                                                                                                            unsafety: Some(
                                                                                                                                                Unsafe,
                                                                                                                                            ),
                                                                                                                                            abi: None,
                                                                                                                                            fn_token: Fn,
                                                                                                                                            paren_token: Paren,
                                                                                                                                            inputs: [],
                                                                                                                                            variadic: Some(
                                                                                                                                                Variadic {
                                                                                                                                                    attrs: [],
                                                                                                                                                    dots: Dot3,
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                            output: Default,
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                    semi_token: Semi,
                                                                                                                                    len: Lit(
                                                                                                                                        ExprLit {
                                                                                                                                            attrs: [],
                                                                                                                                            lit: Int(
                                                                                                                                                LitInt {
                                                                                                                                                    token: 123650538374584850000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f64,
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                },
                                                                                                                            ),
                                                                                                                            Comma,
                                                                                                                            Group(
                                                                                                                                TypeGroup {
                                                                                                                                    group_token: Group,
                                                                                                                                    elem: Infer(
                                                                                                                                        TypeInfer {
                                                                                                                                            underscore_token: Underscore,
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                },
                                                                                                                            ),
                                                                                                                            Comma,
                                                                                                                            TraitObject(
                                                                                                                                TypeTraitObject {
                                                                                                                                    dyn_token: Some(
                                                                                                                                        Dyn,
                                                                                                                                    ),
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
                                                                                                                        ],
                                                                                                                    },
                                                                                                                ),
                                                                                                            },
                                                                                                        ),
                                                                                                    ],
                                                                                                },
                                                                                            ),
                                                                                            Comma,
                                                                                            Group(
                                                                                                TypeGroup {
                                                                                                    group_token: Group,
                                                                                                    elem: Slice(
                                                                                                        TypeSlice {
                                                                                                            bracket_token: Bracket,
                                                                                                            elem: TraitObject(
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
                                                                                                                                                dolor,
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
                                                                                        ],
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
                                                        op: Mul(
                                                            Star,
                                                        ),
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
                                                                                        foo,
                                                                                    ),
                                                                                    arguments: None,
                                                                                },
                                                                                Colon2,
                                                                                PathSegment {
                                                                                    ident: Ident(
                                                                                        quux,
                                                                                    ),
                                                                                    arguments: None,
                                                                                },
                                                                                Colon2,
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
                                                                await_token: Await,
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
                eq_token: Eq,
                expr: Unary(
                    ExprUnary {
                        attrs: [],
                        op: Neg(
                            Sub,
                        ),
                        expr: Try(
                            ExprTry {
                                attrs: [],
                                expr: Repeat(
                                    ExprRepeat {
                                        attrs: [],
                                        bracket_token: Bracket,
                                        expr: TryBlock(
                                            ExprTryBlock {
                                                attrs: [],
                                                try_token: Try,
                                                block: Block {
                                                    brace_token: Brace,
                                                    stmts: [],
                                                },
                                            },
                                        ),
                                        semi_token: Semi,
                                        len: Lit(
                                            ExprLit {
                                                attrs: [],
                                                lit: Int(
                                                    LitInt {
                                                        token: -3575747252861803000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f64,
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                ),
                                question_token: Question,
                            },
                        ),
                    },
                ),
                semi_token: Semi,
            },
        ),
        Enum(
            ItemEnum {
                attrs: [],
                vis: Restricted(
                    VisRestricted {
                        pub_token: Pub,
                        paren_token: Paren,
                        in_token: Some(
                            In,
                        ),
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
                enum_token: Enum,
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
                brace_token: Brace,
                variants: [
                    Variant {
                        attrs: [],
                        ident: Ident(
                            quux,
                        ),
                        fields: Unit,
                        discriminant: Some(
                            (
                                Eq,
                                While(
                                    ExprWhile {
                                        attrs: [],
                                        label: Some(
                                            Label {
                                                name: Lifetime {
                                                    apostrophe: Span,
                                                    ident: Ident(
                                                        bar,
                                                    ),
                                                },
                                                colon_token: Colon,
                                            },
                                        ),
                                        while_token: While,
                                        cond: Path(
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
                                        body: Block {
                                            brace_token: Brace,
                                            stmts: [],
                                        },
                                    },
                                ),
                            ),
                        ),
                    },
                    Comma,
                    Variant {
                        attrs: [],
                        ident: Ident(
                            ipsum,
                        ),
                        fields: Unit,
                        discriminant: Some(
                            (
                                Eq,
                                ForLoop(
                                    ExprForLoop {
                                        attrs: [],
                                        label: None,
                                        for_token: For,
                                        pat: Wild(
                                            PatWild {
                                                attrs: [],
                                                underscore_token: Underscore,
                                            },
                                        ),
                                        in_token: In,
                                        expr: Lit(
                                            ExprLit {
                                                attrs: [],
                                                lit: Int(
                                                    LitInt {
                                                        token: 3010805549u32,
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
                            ),
                        ),
                    },
                ],
            },
        ),
    ],
}