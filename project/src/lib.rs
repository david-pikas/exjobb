#![feature(once_cell)]
#![feature(cell_update)]
#![feature(backtrace)]
#![feature(never_type)]

#[macro_use]
pub mod parser_wrapper;

pub use parser_wrapper::parse;

#[macro_use]
pub mod context_arbitrary;

#[macro_use]
mod choose;

#[macro_use]
pub mod context;
pub use context::Context;

#[macro_use]
mod semantics;

mod string_wrapper;

mod scopes;

mod ty_macros;
pub mod syn_arbitrary;

#[macro_use]
mod branching;

pub use syn_arbitrary::make_wrapped_file;
pub use context_arbitrary::*;
pub use syn_arbitrary::*;

pub fn make_program_str(
    data: &[u8],
    use_semantics: bool,
    use_panics: bool,
    print_vars: bool
) -> context_arbitrary::Result<String> {
    let mut u = Unstructured::new(&data);
    let mut ctx = Context::make_context(context::Options{ use_semantics, use_panics, print_vars, runnable: false });
    let WrappedFile(ast) = make_wrapped_file(&mut ctx, &mut u)?;
    Ok(quote::quote!(#ast).to_string())
}
