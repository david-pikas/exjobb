use std::error::Error;
use std::{env, fs, thread};
use std::io::{self, Read};

use arbitrary::Unstructured;

use rustsmith::context::Options;

use rustsmith::make_wrapped_file;
use rustsmith::Context;
use rustsmith::GenerationError;
use rustsmith::syn_arbitrary::WrappedFile;


#[allow(dead_code)]
fn main() -> Result<(), Box<dyn Error>> {
    let filename = env::args().nth(1).expect("Expected filename as arg");
    let mut buffer = vec![];
    io::stdin().read_to_end(&mut buffer)?;
    let ast = gen_code(buffer, Options::import_env())?;
    let ast_str = quote::quote!(#ast).to_string();
    fs::write(filename, ast_str)?;
    Ok(())
}


fn gen_code(data: Vec<u8>, options: Options) -> Result<syn::File, GenerationError> {
    let WrappedFile(ast): WrappedFile =
        with_stack_space_of(64 * 1024 * 1024, move || {
            let mut u = Unstructured::new(&data);
            let mut ctx = Context::make_context(options);
            make_wrapped_file(&mut ctx, &mut u)
        })?;
    return Ok(ast);
}

fn with_stack_space_of<T, F>(stack_space: usize, closure: F) -> T
where
    F: FnOnce() -> T + 'static + Send,
    T: 'static + Send,
{
    let builder = thread::Builder::new().stack_size(stack_space);
    let handler = builder.spawn(closure).expect("Error creating thread");
    return handler.join().expect("Error executing thread");
}
