use arbitrary::Arbitrary;
use rustc_ap_rustc_ast::{
    ast::{Attribute, Item},
    ptr::P,
    Defaultness, Expr, ExprKind, ItemKind, NodeId, Path, PathSegment, Ty, TyKind, Visibility,
    VisibilityKind,
};
use rustc_ap_rustc_span::{
    hygiene::SyntaxContext, symbol::Ident, with_default_session_globals, BytePos, Span, Symbol,
};
use std::iter;

#[path = "context_arbitrary.rs"]
mod context_arbitrary;
use context_arbitrary::*;

struct Context {
    next_id: u32,
}

fn attrs<'a, V>(_ctx: &mut Context, _u: &mut Unstructured<'a>) -> V
where
    V: iter::FromIterator<Attribute>,
{
    iter::empty().collect()
}

fn id(ctx: &mut Context) -> NodeId {
    let node = NodeId::from_u32(ctx.next_id);
    ctx.next_id += 1;
    return node;
}

fn span(_ctx: &mut Context) -> Span {
    Span::new(BytePos(0), BytePos(0), SyntaxContext::root())
}

impl<'a, T, Ctx> ContextArbitrary<'a, Ctx> for P<T>
where
    T: ContextArbitrary<'a, Ctx>,
    T: 'static,
{
    fn c_arbitrary(ctx: &mut Ctx, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(P(c_arbitrary(ctx, u)?))
    }
}

pub struct WrappedFile(pub Item);
unsafe impl Send for WrappedFile {}
impl<'a> Arbitrary<'a> for WrappedFile {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(WrappedFile(c_arbitrary(&mut Context { next_id: 0 }, u)?))
    }
}

impl<'a> ContextArbitrary<'a, Context> for Item {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Item {
            attrs: attrs(ctx, u),
            id: id(ctx),
            span: span(ctx),
            vis: c_arbitrary(ctx, u)?,
            ident: c_arbitrary(ctx, u)?,
            kind: c_arbitrary(ctx, u)?,
            tokens: None,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for ItemKind {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        // let enum_choices = [
        //     |ctx, u| {
        let defaultness = c_arbitrary(ctx, u)?;
        let ty = c_arbitrary(ctx, u)?;
        let expr = c_arbitrary(ctx, u)?;
        Ok(ItemKind::Const(defaultness, ty, expr))
        // }
        // ];
        // let choice = u.choose(&enum_choices)?;
        // return choice(ctx, u);
        // TODO:
        // ExternCrate(Option<Symbol>),
        // Use(UseTree),
        // Static(P<Ty>, Mutability, Option<P<Expr>>),
        // Fn(Box<FnKind>),
        // Mod(Unsafe, ModKind),
        // ForeignMod(ForeignMod),
        // GlobalAsm(GlobalAsm),
        // TyAlias(Box<TyAliasKind>),
        // Enum(EnumDef, Generics),
        // Struct(VariantData, Generics),
        // Union(VariantData, Generics),
        // Trait(Box<TraitKind>),
        // TraitAlias(Generics, GenericBounds),
        // Impl(Box<ImplKind>),
        // MacCall(MacCall),
        // MacroDef(MacroDef),
    }
}

impl<'a> ContextArbitrary<'a, Context> for Expr {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [|ctx, u| Ok(ExprKind::Path(None, c_arbitrary(ctx, u)?))];
        let choice = u.choose(&enum_choices)?;
        let kind = choice(ctx, u)?;
        Ok(Expr {
            id: id(ctx),
            kind,
            span: span(ctx),
            attrs: attrs(ctx, u),
            tokens: None,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Ty {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            // TODO: QSelf
            |ctx, u| Ok(TyKind::Path(None, c_arbitrary(ctx, u)?)),
        ];
        let choice = u.choose(&enum_choices)?;
        let kind = choice(ctx, u)?;
        Ok(Ty {
            id: id(ctx),
            kind,
            span: span(ctx),
            tokens: None,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Path {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(Path {
            span: span(ctx),
            segments: unwrap_nev(c_arbitrary(ctx, u)?),
            tokens: None,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for PathSegment {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        Ok(PathSegment {
            ident: c_arbitrary(ctx, u)?,
            id: id(ctx),
            // TODO: PathSegment args
            args: None,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Defaultness {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [
            |ctx| Defaultness::Default(span(ctx)),
            |_| Defaultness::Final,
        ];
        let choice = u.choose(&enum_choices)?;
        return Ok(choice(ctx));
    }
}

impl<'a> ContextArbitrary<'a, Context> for Visibility {
    fn c_arbitrary(ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let enum_choices = [|| VisibilityKind::Public, || VisibilityKind::Inherited];
        let choice = u.choose(&enum_choices)?;
        let kind = choice();
        Ok(Visibility {
            kind,
            span: span(ctx),
            tokens: None,
        })
    }
}

impl<'a> ContextArbitrary<'a, Context> for Ident {
    fn c_arbitrary(_ctx: &mut Context, u: &mut Unstructured<'a>) -> Result<Self> {
        let names = ["foo", "bar", "baz", "quux", "lorem", "ipsum", "dolor"];
        let name = *u.choose(&names)?;
        let name_token =
            with_default_session_globals(|| Ident::with_dummy_span(Symbol::intern(name)));
        // let name_token = Ident::from_str(name);
        // let name_token = Ident::with_dummy_span(Symbol::new(Arbitrary::arbitrary(u)?));
        return Ok(name_token);
    }
}
