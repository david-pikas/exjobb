use arbitrary::{Arbitrary, Result, Unstructured};
use syn::{Arm, BareFnArg, BinOp, Block, Expr, ExprArray, ExprAssign, ExprAssignOp, ExprAsync, ExprAwait, ExprBinary, ExprBlock, ExprBox, ExprBreak, ExprCall, ExprCast, ExprClosure, ExprContinue, ExprField, ExprForLoop, ExprGroup, ExprIf, ExprIndex, ExprLet, ExprLit, ExprLoop, ExprMatch, ExprMethodCall, ExprParen, ExprPath, ExprRange, ExprReference, ExprRepeat, ExprReturn, ExprStruct, ExprTry, ExprTryBlock, ExprTuple, ExprType, ExprUnary, ExprWhile, ExprYield, Field, FieldPat, FieldValue, Fields, FieldsNamed, FieldsUnnamed, File, FnArg, GenericMethodArgument, GenericParam, Generics, Ident, Index, Item, ItemConst, ItemEnum, ItemFn, ItemStatic, ItemTrait, ItemType, Label, Lifetime, Lit, Local, Member, MethodTurbofish, Pat, PatBox, PatIdent, PatLit, PatOr, PatPath, PatRange, PatReference, PatRest, PatSlice, PatStruct, PatTuple, PatTupleStruct, PatType, PatWild, PathArguments, PathSegment, RangeLimits, ReturnType, Signature, Stmt, TraitBound, TraitBoundModifier, TraitItem, TraitItemConst, TraitItemMethod, TraitItemType, Type, TypeArray, TypeBareFn, TypeGroup, TypeImplTrait, TypeInfer, TypeParamBound, TypeParen, TypePath, TypePtr, TypeReference, TypeSlice, TypeTraitObject, TypeTuple, UnOp, Variadic, Variant, VisCrate, VisPublic, VisRestricted, Visibility, parse_quote, parse_str, punctuated::Punctuated, token::{Brace, Bracket, Group, Paren}};
use std::iter::{self, empty};

#[path = "context_arbitrary.rs"]
mod context_arbitrary;
use context_arbitrary::*;

#[path = "context.rs"]
#[macro_use]
mod context;
use context::*;

pub struct WrappedFile(pub File);
unsafe impl Send for WrappedFile {}


fn dummy_span() -> proc_macro2::Span {
    let dummy_token: syn::token::Eq = parse_quote!(=);
    return dummy_token.span;
}

macro_rules! parens_ex {
   ($ctx: ident, $e: expr) => (Box::new(parenthesize_expr(rhs!($ctx, $e))))
}

// doesn't need to be a macro atm, but keeping it for consistency with parens_ex
macro_rules! parens_ty {
   ($ctx: ident, $e: expr) => (Box::new(parenthesize_type($e)))
}

fn parenthesize_expr(e: Expr) -> Expr {
   match e {
       Expr::Array(_) => e,
       Expr::Block(_) => e,
       Expr::Call(_) => e,
       Expr::Closure(_) => e,
       Expr::Field(_) => e,
       Expr::Index(_) => e,
       Expr::Lit(_) => e,
       Expr::Match(_) => e,
       Expr::MethodCall(_) => e,
       Expr::Paren(_) => e,
       Expr::Path(_) => e, 
       Expr::Repeat(_) => e,
       Expr::Struct(_) => e,
       Expr::Try(_) => e,
       Expr::Tuple(_) => e,
       Expr::Unary(_) => e,
       _ => Expr::Paren(ExprParen {
                attrs: vec!(),
                paren_token: Paren { span: dummy_span() },
                expr: Box::new(e)
            })
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
                elem: Box::new(ty)
            })
   }
}

impl<'a, T, P, Ctx> ContextArbitrary<'a, Ctx> for Punctuated<T, P>
where T: ContextArbitrary<'a, Ctx>, P: Default {
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self> {
        let iter: ContextArbitraryIter<T, Ctx> = c_arbitrary_iter(ctx, u);
        return iter.collect()
    }
}

struct NEPunctuated<T, P>(pub Punctuated<T, P>);
impl<'a, T, P, Ctx> ContextArbitrary<'a, Ctx> for NEPunctuated<T, P> 
where T: ContextArbitrary<'a, Ctx>, P: Default {
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
impl<'a> Arbitrary<'a> for WrappedFile {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {   
        let mut ctx = Default::default();
        Ok(WrappedFile(File {
            shebang: None,
            // TODO: Possibly add attributes?
            attrs: vec!(),
            items: unwrap_nev(c_arbitrary(&mut ctx, u)?)
        }))
    }
}

impl<'a> ContextArbitrary<'a, Context> for Item {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            |ctx, u| Ok(Item::Static(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Item::Const(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Item::Fn(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Item::Type(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Item::Trait(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Item::Enum(c_arbitrary(ctx, u)?)) ,
            // TODO:
            // Impl(ItemImpl),
            // Mod(ItemMod),
            // Static(ItemStatic),
            // Struct(ItemStruct),
            // TraitAlias(ItemTraitAlias),
            // Union(ItemUnion),
            // Use(ItemUse),

            // Intentionally omitted: ExternCrate, ForeignMod, Macro, Verbatim
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemEnum {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ItemEnum {
            attrs: vec!(),
            vis: c_arbitrary(ctx, u)?,
            enum_token: parse_quote!(enum),
            ident: c_arbitrary(ctx, u)?,
            generics: c_arbitrary(ctx, u)?,
            brace_token: Brace { span: dummy_span() },
            variants: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Variant {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Variant {
            attrs: vec!(),
            ident: c_arbitrary(ctx, u)?,
            fields: c_arbitrary(ctx, u)?,
            discriminant: c_arbitrary::<Context, Option<Expr>>(ctx, u)?
                .map(|e| (parse_quote!(=), e)),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Fields {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            |ctx, u| Ok(Fields::Named(c_arbitrary(ctx, u)?)),
            |ctx, u| Ok(Fields::Unnamed(c_arbitrary(ctx, u)?)),
            |_  , _| Ok(Fields::Unit)
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}

impl<'a> ContextArbitrary<'a, Context> for FieldsNamed {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(FieldsNamed {
            brace_token: Brace { span: dummy_span() },
            named: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for FieldsUnnamed {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(FieldsUnnamed {
            paren_token: Paren { span: dummy_span() },
            unnamed: name_fields!(ctx, false, c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Field {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let ident: Option<Ident>;
        let colon_token;
        if ctx.named_fields { 
            ident = c_arbitrary(ctx, u)?;
            colon_token = Some(parse_quote!(:));
        } else {
            ident = None;
            colon_token = None;
        }
        Ok(Field {
            attrs: vec!(),
            vis: c_arbitrary(ctx, u)?,
            ident, colon_token,
            ty: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemTrait {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ItemTrait {
            attrs: vec!(),
            vis: c_arbitrary(ctx, u)?,
            unsafety: maybe(u, parse_quote!(unsafe)),
            auto_token: maybe(u, parse_quote!(auto)),
            trait_token: parse_quote!(trait),
            ident: c_arbitrary(ctx, u)?,
            generics: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            supertraits: c_arbitrary(ctx, u)?,
            brace_token: Brace { span: dummy_span() },
            items: c_arbitrary(ctx, u)?
         })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TraitItem {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
           |ctx, u| Ok(TraitItem::Const(c_arbitrary(ctx, u)?)),
           |ctx, u| Ok(TraitItem::Method(c_arbitrary(ctx, u)?)),
           |ctx, u| Ok(TraitItem::Type(c_arbitrary(ctx, u)?)),
           // intentionally ommited: Macro, Verbatim
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}

impl<'a> ContextArbitrary<'a, Context> for TraitItemMethod {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TraitItemMethod {
            attrs: vec!(),
            sig: c_arbitrary(ctx, u)?,
            default: c_arbitrary(ctx, u)?,
            semi_token: parse_quote!(;),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TraitItemType {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TraitItemType {
            attrs: vec!(),
            type_token: parse_quote!(type),
            ident: c_arbitrary(ctx, u)?,
            generics: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            bounds: unwrap_nep(c_arbitrary(ctx, u)?),
            default: c_arbitrary::<Context, Option<Type>>(ctx, u)?
                         .map(|t| (parse_quote!(=), t)),
            semi_token: parse_quote!(;)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TraitItemConst {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TraitItemConst {
            attrs: vec!(),
            const_token: parse_quote!(const),
            ident: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            ty: c_arbitrary(ctx, u)?,
            default: c_arbitrary::<Context, Option<Expr>>(ctx, u)?
                         .map(|e| (parse_quote!(=), e)),
            semi_token: parse_quote!(;),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Visibility {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            |_  , _| Ok(Visibility::Public(VisPublic{pub_token: parse_quote!(pub)})),
            |_  , _| Ok(Visibility::Crate(VisCrate{crate_token: parse_quote!(crate)})),
            |ctx, u| {
               Ok(Visibility::Restricted(VisRestricted{
                   pub_token: parse_quote!(pub),
                   paren_token: Paren { span: dummy_span() },
                   in_token: parse_quote!(in),
                   path: c_arbitrary(ctx, u)?
               }))
            },
            |_  , _| Ok(Visibility::Inherited)
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);

    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemStatic {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ItemStatic {
            attrs: vec!(),
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
            semi_token: parse_quote!(;)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemConst {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ItemConst {
            attrs: vec!(),
            vis: Visibility::Inherited, 
            const_token: parse_quote!(const),
            ident: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            ty: c_arbitrary(ctx, u)?,
            eq_token: parse_quote!(=),
            expr: rhs!(ctx, c_arbitrary(ctx, u)?),
            semi_token: parse_quote!(;)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemFn {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ItemFn {
            attrs: vec!(),
            vis: Visibility::Inherited,
            sig: c_arbitrary(ctx, u)?,
            block: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemType {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
         Ok(ItemType {
             attrs: vec!(),
             vis: Visibility::Inherited,
             type_token: parse_quote!(type),
             ident: c_arbitrary(ctx, u)?,
             generics: c_arbitrary(ctx, u)?,
             eq_token: parse_quote!(=),
             ty: c_arbitrary(ctx, u)?,
             semi_token: parse_quote!(;)
         })

    }
}

impl<'a> ContextArbitrary<'a, Context> for Type {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            |ctx, u| Ok(Type::Slice(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Type::Array(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Type::Ptr(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Type::Reference(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Type::BareFn(c_arbitrary(ctx, u)?)) ,
            // |_  , _| Ok(Type::Never(syn::TypeNever{bang_token: parse_quote!(!)})),
            |ctx, u| Ok(Type::Tuple(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Type::Path(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Type::TraitObject(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Type::ImplTrait(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Type::Paren(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(Type::Group(c_arbitrary(ctx, u)?)) ,
            |_  , _| Ok(Type::Infer(TypeInfer {underscore_token: parse_quote!(_)})),
            // Intentionally omitted: Macro, Verbatim
            // TODO:
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
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
            len: c_arbitrary(ctx, u)?
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
            elem: parens_ty!(ctx, c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeReference {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeReference {
            and_token: parse_quote!(&),
            lifetime: c_arbitrary(ctx, u)?,
            mutability: maybe(u, parse_quote!(mut)),
            elem: parens_ty!(ctx, c_arbitrary(ctx, u)?)
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
            output: c_arbitrary(ctx, u)?
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
        let bounds = iter::once(Ok(first_bound)).chain(iter)
                     .collect::<Result<Punctuated<TypeParamBound, syn::token::Add>>>()?;
        Ok(TypeTraitObject {
            dyn_token: None,
            bounds 
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeImplTrait {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeImplTrait {
            impl_token: parse_quote!(impl),
            bounds: unwrap_nep(c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeParen {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeParen {
            paren_token: Paren { span: dummy_span() },
            elem: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeGroup {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TypeGroup {
            group_token: syn::token::Group { span: dummy_span() },
            elem: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for TypeParamBound {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            |ctx, u| Ok(TypeParamBound::Trait(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(TypeParamBound::Lifetime(c_arbitrary(ctx, u)?)) ,
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}

impl<'a> ContextArbitrary<'a, Context> for ReturnType {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            |_  , _| Ok(ReturnType::Default) ,
            |ctx, u| Ok(ReturnType::Type(parse_quote!(->),
                                         parens_ty!(ctx, c_arbitrary(ctx, u)?))),
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}

impl<'a> ContextArbitrary<'a, Context> for TraitBound {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(TraitBound {
            paren_token: None,
            // TODO: potentially add questionmark modifier
            modifier: TraitBoundModifier::None,
            lifetimes: None,
            path: c_arbitrary(ctx, u)?
         
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for BareFnArg {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let opt_name: Option<Ident> = c_arbitrary(ctx, u)?;
        Ok(BareFnArg {
            attrs: vec!(),
            name: opt_name.map(|name| (name, parse_quote!(:))),
            ty: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Variadic {
    fn c_arbitrary(_ctx: &mut Context, _u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Variadic {
            attrs: vec!(),
            dots: parse_quote!(...)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Signature {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: parse_quote!(fn),
            ident: c_arbitrary(ctx, u)?,
            generics: c_arbitrary(ctx, u)?,
            paren_token: Paren { span: dummy_span() },
            inputs: c_arbitrary(ctx, u)?,
            variadic: c_arbitrary(ctx, u)?,
            output: c_arbitrary(ctx, u)?

        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for FnArg {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            |ctx, u| Ok(FnArg::Receiver(c_arbitrary(ctx, u)?)) ,
            |ctx, u| Ok(FnArg::Typed(c_arbitrary(ctx, u)?)) ,
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);

    }
}

impl<'a> ContextArbitrary<'a, Context> for PatType {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatType {
            attrs: vec!(),
            pat: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            ty: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Pat {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
           |ctx, u| Ok(Pat::Box(c_arbitrary(ctx, u)?)) ,
           |ctx, u| Ok(Pat::Ident(c_arbitrary(ctx, u)?)) ,
           |ctx, u| Ok(Pat::Lit(c_arbitrary(ctx, u)?)) ,
           |ctx, u| Ok(Pat::Or(c_arbitrary(ctx, u)?)) ,
           |ctx, u| Ok(Pat::Path(c_arbitrary(ctx, u)?)) ,
           |ctx, u| Ok(Pat::Range(c_arbitrary(ctx, u)?)) ,
           |ctx, u| Ok(Pat::Reference(c_arbitrary(ctx, u)?)) ,
           |_  , _| Ok(Pat::Rest(PatRest{attrs: vec!(), dot2_token: parse_quote!(..)})),
           |ctx, u| Ok(Pat::Slice(c_arbitrary(ctx, u)?)) ,
           |ctx, u| Ok(Pat::Struct(c_arbitrary(ctx, u)?)) ,
           |ctx, u| Ok(Pat::Tuple(c_arbitrary(ctx, u)?)) ,
           |ctx, u| Ok(Pat::TupleStruct(c_arbitrary(ctx, u)?)) ,
           |ctx, u| Ok(Pat::Type(c_arbitrary(ctx, u)?)) ,
           |_  , _| Ok(Pat::Wild(PatWild{attrs: vec!(), underscore_token: parse_quote!(_)})),
           // intentionally omitted: Macro, Verbatim
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatBox {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatBox {
            attrs: vec!(),
            box_token: parse_quote!(box),
            pat: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatIdent {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let opt_pat: Option<Box<Pat>> = c_arbitrary(ctx, u)?;
        Ok(PatIdent {
            attrs: vec!(),
            // TODO: ref
            by_ref: maybe(u, parse_quote!(ref)),
            mutability: maybe(u, parse_quote!(mut)),
            ident: c_arbitrary(ctx, u)?,
            subpat: opt_pat.map(|pat| (parse_quote!(@), pat))
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatLit {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatLit {
            attrs: vec!(),
            // TODO: Pattern can either be a Lit or a Unary(-, Lit)
            expr: Box::new(Expr::Lit(c_arbitrary(ctx, u)?))
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatOr {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatOr {
            attrs: vec!(),
            // TODO: look into if/when a leading vert should be there
            leading_vert: None,
            cases: unwrap_nep(c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatPath {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatPath {
            attrs: vec!(),
            qself: None,
            path: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatRange {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatRange {
            attrs: vec!(),
            // TODO: lo and hi need to be valid patterns, not just any expr.
            //       Lit might be too restrictive of a type.
            lo: Box::new(Expr::Lit(c_arbitrary(ctx, u)?)),
            limits: c_arbitrary(ctx, u)?,
            hi: Box::new(Expr::Lit(c_arbitrary(ctx, u)?))
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatReference {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatReference {
            attrs: vec!(),
            and_token: parse_quote!(&),
            mutability: None,
            pat: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatSlice {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatSlice {
            attrs: vec!(),
            bracket_token: Bracket { span: dummy_span() },
            elems: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatStruct {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatStruct {
            attrs: vec!(),
            path: c_arbitrary(ctx, u)?,
            brace_token: Brace { span: dummy_span() },
            fields: c_arbitrary(ctx, u)?,
            dot2_token: parse_quote!(..)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for FieldPat {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(FieldPat {
            attrs: vec!(),
            member: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            pat: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatTuple {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatTuple {
            attrs: vec!(),
            paren_token: Paren { span: dummy_span() },
            elems: unwrap_nep(c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PatTupleStruct {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PatTupleStruct {
            attrs: vec!(),
            path: c_arbitrary(ctx, u)?,
            pat: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Index {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Index {
            index: Arbitrary::arbitrary(u)?,
            span: dummy_span()
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for RangeLimits {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            RangeLimits::HalfOpen(parse_quote!(..)),
            RangeLimits::Closed(parse_quote!(..=))
        ];
        return Ok(*u.choose(&enum_choices)?)
    }
}

impl<'a> ContextArbitrary<'a, Context> for syn::Receiver {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let opt_ref: Option<Option<Lifetime>> = c_arbitrary(ctx, u)?;
        Ok(syn::Receiver {
            attrs: vec!(),
            reference: opt_ref.map(|ref_| (parse_quote!(&), ref_)),
            mutability: None,
            self_token: parse_quote!(self)
        })
    }
}


impl<'a> ContextArbitrary<'a, Context> for Block {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Block {
            brace_token: Brace { span: dummy_span() },
            stmts: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Stmt {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
          |ctx, u| Ok(Stmt::Local(c_arbitrary(ctx, u)?)) ,
          |ctx, u| Ok(Stmt::Item(c_arbitrary(ctx, u)?)) ,
          // TODO: expr should only be last in a block
          // |ctx, u| Ok(Stmt::Expr(c_arbitrary(ctx, u)?)) ,
          |ctx, u| Ok(Stmt::Semi(c_arbitrary(ctx, u)?,
                                parse_quote!(;))),
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}

impl<'a> ContextArbitrary<'a, Context> for Local {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let init: Option<Box<Expr>> = c_arbitrary(ctx, u)?;
        Ok(Local {
            attrs: vec!(),
            let_token: parse_quote!(let),
            pat: c_arbitrary(ctx, u)?,
            init: init.map(|expr| (parse_quote!(=), expr)),
            semi_token: parse_quote!(;)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Expr {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let mut enum_choices: Vec<Box<dyn Fn(_,_) -> Result<Expr>>> = vec!(
           Box::new(|ctx, u| Ok(Expr::Array(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Tuple(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Path(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Lit(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Binary(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Assign(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::AssignOp(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Await(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Block(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Box(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Call(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Cast(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Closure(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Field(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Group(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::If(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Index(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Loop(c_arbitrary(ctx, u)?))),
           // Macro(ExprMacro),
           Box::new(|ctx, u| Ok(Expr::Match(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::MethodCall(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Paren(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Range(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Reference(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Repeat(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Return(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Struct(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Try(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Unary(c_arbitrary(ctx, u)?))),
           // Unsafe(ExprUnsafe),
        );
        let valueless_choices: Vec<Box<dyn Fn(_,_) -> Result<Expr>>> = vec!(
           Box::new(|ctx, u| Ok(Expr::Async(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Break(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Continue(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::ForLoop(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Let(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::TryBlock(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::While(c_arbitrary(ctx, u)?))),
           Box::new(|ctx, u| Ok(Expr::Yield(c_arbitrary(ctx, u)?))),
        );
        if !ctx.has_value {
            enum_choices.extend(valueless_choices);
        }
        if ctx.allow_type_annotations {
           enum_choices.push(Box::new(|ctx, u| Ok(Expr::Type(c_arbitrary(ctx, u)?))));
        }
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprYield {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprYield {
            attrs: vec!(),
            yield_token: parse_quote!(yield),
            expr: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprWhile {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprWhile {
            attrs: vec!(),
            label: c_arbitrary(ctx, u)?,
            while_token: parse_quote!(while),
            cond: c_arbitrary(ctx, u)?,
            body: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprParen {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprParen {
            attrs: vec!(),
            paren_token: Paren { span: dummy_span() },
            expr: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprUnary {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprUnary {
            attrs: vec!(),
            op: c_arbitrary(ctx, u)?,
            expr: parens_ex!(ctx, c_arbitrary(ctx, u)?)
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
            attrs: vec!(),
            expr: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            ty: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprTryBlock {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprTryBlock {
            attrs: vec!(),
            try_token: parse_quote!(try),
            block: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprTry {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprTry {
            attrs: vec!(),
            expr: parens_ex!(ctx, c_arbitrary(ctx, u)?),
            question_token: parse_quote!(?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprStruct {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let named_fields: bool = Arbitrary::arbitrary(u)?;
        Ok(ExprStruct {
            attrs: vec!(),
            path: c_arbitrary(ctx, u)?,
            brace_token: Brace { span: dummy_span() },
            fields: name_fields!(ctx, named_fields, c_arbitrary(ctx, u)?),
            dot2_token: parse_quote!(..),
            rest: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for FieldValue {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(FieldValue {
            attrs: vec!(),
            member: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:),
            expr: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprReturn {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprReturn {
            attrs: vec!(),
            return_token: parse_quote!(return),
            expr: c_arbitrary(ctx, u)?,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprRepeat {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprRepeat {
            attrs: vec!(),
            bracket_token: Bracket { span: dummy_span() },
            expr: c_arbitrary(ctx, u)?,
            semi_token: parse_quote!(;),
            len: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprReference {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprReference {
            attrs: vec!(),
            and_token: parse_quote!(&),
            raw: Default::default(),
            mutability: maybe(u, parse_quote!(mut)),
            expr: parens_ex!(ctx, c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprRange {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let from = rhs!(ctx, c_arbitrary(ctx, u)?);
        let limits = c_arbitrary(ctx, u)?;
        // Closed ranges must have a `to`
        let to = match limits {
            RangeLimits::Closed(_) => Some(rhs!(ctx, c_arbitrary(ctx, u)?)),
            RangeLimits::HalfOpen(_) => rhs!(ctx, c_arbitrary(ctx, u)?)
        };
        Ok(ExprRange {
            attrs: vec!(),
            from, limits, to
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprMethodCall {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprMethodCall {
            attrs: vec!(),
            receiver: parens_ex!(ctx, c_arbitrary(ctx, u)?),
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
        let enum_choices = [
            // TODO: find out exactly what goes in the argument as Expr is surely to general
            // |ctx, u| Ok(GenericMethodArgument::Const(c_arbitrary(ctx, u)?)),
            |ctx, u| Ok(GenericMethodArgument::Type(c_arbitrary(ctx, u)?)),
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprMatch {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprMatch {
            attrs: vec!(),
            match_token: parse_quote!(match),
            expr: rhs!(ctx, c_arbitrary(ctx, u)?),
            brace_token: Brace { span: dummy_span() },
            arms: unwrap_nev(c_arbitrary(ctx, u)?),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Arm {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Arm {
            attrs: vec!(),
            pat: c_arbitrary(ctx, u)?,
            guard: c_arbitrary::<Context, Option<Box<Expr>>>(ctx, u)?
                       .map(|e| (parse_quote!(if), e)),
            fat_arrow_token: parse_quote!(=>),
            body: c_arbitrary(ctx, u)?,
            comma: maybe(u, parse_quote!(,))
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprLoop {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprLoop {
            attrs: vec!(),
            label: c_arbitrary(ctx, u)?,
            loop_token: parse_quote!(loop),
            body: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprLet {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprLet {
            attrs: vec!(),
            let_token: parse_quote!(let),
            pat: c_arbitrary(ctx, u)?,
            eq_token: parse_quote!(=),
            expr: rhs!(ctx, c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprIndex {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprIndex {
            attrs: vec!(),
            expr: parens_ex!(ctx, c_arbitrary(ctx, u)?),
            bracket_token: Bracket { span: dummy_span() },
            index: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprIf {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprIf {
            attrs: vec!(),
            if_token: parse_quote!(if),
            cond: parens_ex!(ctx, c_arbitrary(ctx, u)?),
            then_branch: c_arbitrary(ctx, u)?,
            else_branch: c_arbitrary::<Context, Option<Box<Expr>>>(ctx, u)?
                             .map(|e| (parse_quote!(else), e)),
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprGroup {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprGroup {
            attrs: vec!(),
            group_token: Group { span: dummy_span() },
            expr: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprForLoop {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprForLoop {
            attrs: vec!(),
            label: c_arbitrary(ctx, u)?,
            for_token: parse_quote!(for),
            pat: c_arbitrary(ctx, u)?,
            in_token: parse_quote!(in),
            // NOTE: expr can't contain a type
            expr: parens_ex!(ctx, no_annotations!(ctx, c_arbitrary(ctx, u)?)),
            body: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprField {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprField {
            attrs: vec!(),
            base: parens_ex!(ctx, c_arbitrary(ctx, u)?),
            dot_token: parse_quote!(.),
            member: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Member {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            |ctx, u| Ok(Member::Named(c_arbitrary(ctx, u)?)),
            |ctx, u| Ok(Member::Unnamed(c_arbitrary(ctx, u)?)),
        ];
        let choice = u.choose(&enum_choices)?;
        return choice(ctx, u);
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprContinue {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprContinue {
            attrs: vec!(),
            continue_token: parse_quote!(continue),
            label: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprClosure {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprClosure {
            attrs: vec!(),
            // it appears that this feature hasn't actually propery been added to the language
            asyncness: None, // maybe(u, parse_quote!(async)),
            movability: maybe(u, parse_quote!(static)),
            capture: maybe(u, parse_quote!(move)),
            or1_token: parse_quote!(|),
            inputs: c_arbitrary(ctx, u)?,
            or2_token: parse_quote!(|),
            output: c_arbitrary(ctx, u)?,
            body: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprCast {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprCast {
            attrs: vec!(),
            expr: c_arbitrary(ctx, u)?,
            as_token: parse_quote!(as),
            ty: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprCall {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprCall {
            attrs: vec!(),
            func: parens_ex!(ctx, c_arbitrary(ctx, u)?),
            paren_token: Paren { span: dummy_span() },
            args: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprBreak {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprBreak {
            attrs: vec!(),
            break_token: parse_quote!(break),
            label: c_arbitrary(ctx, u)?,
            expr: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprBlock {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprBlock {
            attrs: vec!(),
            label: c_arbitrary(ctx, u)?,
            block: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Label {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Label {
            name: c_arbitrary(ctx, u)?,
            colon_token: parse_quote!(:)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprBox {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprBox {
            attrs: vec!(),
            box_token: parse_quote!(box),
            expr: c_arbitrary(ctx, u)?
        })
    }
}
impl<'a> ContextArbitrary<'a, Context> for ExprAwait {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprAwait {
            attrs: vec!(),
            await_token: parse_quote!(await),
            dot_token: parse_quote!(.),
            base: parens_ex!(ctx, c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprAsync {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprAsync {
            attrs: vec!(),
            async_token: parse_quote!(async),
            capture: maybe(u, parse_quote!(move)),
            block: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprAssign {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprAssign {
            attrs: vec!(),
            // TODO: what exactly can the lhs in an assignment be?
            left: Box::new(Expr::Path(c_arbitrary(ctx, u)?)),
            eq_token: parse_quote!(=),
            right: rhs!(ctx, c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprAssignOp {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            BinOp::Add(parse_quote!(+)),
            BinOp::Sub(parse_quote!(-)),
            BinOp::Mul(parse_quote!(*)),
            BinOp::Div(parse_quote!(/))
        ];
        let choice = u.choose(&enum_choices)?;
        let op = *choice;
        Ok(ExprAssignOp {
            attrs: vec!(),
            left: Box::new(Expr::Path(c_arbitrary(ctx, u)?)),
            right: c_arbitrary(ctx, u)?,
            op
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprBinary {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprBinary {
            attrs: vec!(),
            left: parens_ex!(ctx, c_arbitrary(ctx, u)?),
            op: c_arbitrary(ctx, u)?,
            right: parens_ex!(ctx, c_arbitrary(ctx, u)?)
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for BinOp {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let ops = [parse_quote!(+), parse_quote!(*), parse_quote!(/), parse_quote!(%), parse_quote!(-)];
        let op = u.choose(&ops)?;
        return Ok(*op);
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprLit {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprLit {
            attrs: vec!(),
            lit: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprArray {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprArray {
            attrs: vec!(),
            bracket_token: Bracket { span: dummy_span() },
            elems: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprTuple {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprTuple {
            attrs: vec!(),
            elems: unwrap_nep(c_arbitrary(ctx, u)?),
            paren_token: Paren { span: dummy_span() }
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ExprPath {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(ExprPath {
            attrs: vec!(),
            qself: None,
            path: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Lit {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let data_choices = [
            |u| { let data: u32 = Arbitrary::arbitrary(u)?;
                  Ok(parse_quote!(#data)) },
            |u| { let data: String = Arbitrary::arbitrary(u)?;
                  Ok(parse_quote!(#data)) },
            |u| { let data: f64 = unwrap_finite_f64(Arbitrary::arbitrary(u)?);
                  // parse_quote! requires that the float is finite
                  Ok(parse_quote!(#data)) },
            |u| { let data: bool = Arbitrary::arbitrary(u)?;
                  Ok(parse_quote!(#data)) },
        ];
        let choice = u.choose(&data_choices)?;
        return choice(u);
    }
}

impl<'a> ContextArbitrary<'a, Context> for Generics {
    fn c_arbitrary(_ctx: &mut Context, _u: &mut Unstructured<'a>) -> Result<Self> {
        // TEMPORARY
        Ok(Generics {
            lt_token: parse_quote!(<),
            params: empty::<GenericParam>().collect(),
            gt_token: parse_quote!(>),
            where_clause: None
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Lifetime {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Lifetime {
            apostrophe: dummy_span(),
            ident: c_arbitrary(ctx, u)?
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for syn::Path {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
         Ok(syn::Path {
             // TODO: look into when there should be a leading colon
             leading_colon: None,
             segments: unwrap_nep(c_arbitrary(ctx, u)?)
         })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PathSegment {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PathSegment {
            ident: c_arbitrary(ctx, u)?,
            // TODO: add path arguments
            arguments: PathArguments::None
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Ident {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let names = ["foo", "bar", "baz", "quux", "lorem", "ipsum", "dolor" ];
        let name = *u.choose(&names)?;
        let name_token: Ident = parse_str(name).expect("Invalid identifier");
        return Ok(name_token);
    }
}
