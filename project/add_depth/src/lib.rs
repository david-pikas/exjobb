use proc_macro::TokenStream;
use quote::quote;
use syn::{/*Ident,*/ ItemFn, parse, parse_quote};

#[proc_macro_attribute]
pub fn add_depth(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let func: ItemFn = parse(item).unwrap();
    // let ctx: Ident = parse(attr).unwrap();
    let block = func.block;
    let outer_func = ItemFn {
        block: Box::new(parse_quote!({
            let mut __inner__func = (move |ctx: &mut Context| #block);
            ctx.depth += 1;
            let result = __inner__func(ctx);
            ctx.depth -= 1;
            return result;
        })),
        attrs: func.attrs,
        vis: func.vis,
        sig: func.sig,
    };
    return quote!(#outer_func).into();
}
