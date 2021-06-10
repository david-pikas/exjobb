#![no_main]
use std::fs;

use libfuzzer_sys::fuzz_target;
use arbitrary::Unstructured;
use quote::quote;
extern crate rustsmith;
use rustsmith::{make_wrapped_file, WrappedFile, Context};

fuzz_target!(|data: &[u8]| {
    let mut ctx = Context::make_context(false);
    let mut u = Unstructured::new(&data);
    let wrapped_ast = make_wrapped_file(&mut ctx, &mut u).unwrap();
    let WrappedFile(ast) = wrapped_ast;
    let code_str = quote!(#ast).to_string();
    let filename = "./output_files/generated_code.rs";
    fs::write(filename, &code_str).expect("Failed writing file");
    rustsmith::parse(filename).unwrap();
});
