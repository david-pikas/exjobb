use proc_macro::TokenStream;
use syn;

#[proc_macro]
pub fn make_double(input: TokenStream) -> TokenStream {
    let struct: syn::ItemStruct = syn::parse(input);
    unimplemented!();
}
