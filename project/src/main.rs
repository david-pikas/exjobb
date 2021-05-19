use arbitrary::{Arbitrary, Unstructured, Result};
use quote::quote;
use rand::{Rng, distributions, prelude::Distribution, thread_rng};
use rustc_ap_rustc_ast_pretty::pprust::item_to_string;
use std::{env, fs, str, thread};
use std::process::Command;

mod syn_arbitrary;
mod rustc_ast_arbitrary;

const USE_RUST_AST: bool = false;

fn main() {
    let compile: bool = Some(String::from("compile")) == env::args().nth(1);
    let code_str: String;
    if USE_RUST_AST {
            let wrapped_ast: Result<rustc_ast_arbitrary::WrappedFile> =
                with_stack_space_of(32 * 1024 * 1024, move || {
                    let mut rng = thread_rng();
                    let len = rng.gen_range(1000000..10000000);
                    let data: Vec<u8> = distributions::Standard.sample_iter(&mut rng).take(len).collect();
                    let mut u = Unstructured::new(&data);
                    Arbitrary::arbitrary(&mut u)
                });
        let ast: rustc_ap_rustc_ast::Item = match wrapped_ast {
            Ok(rustc_ast_arbitrary::WrappedFile(a)) => a,
            Err(e) => panic!("{:?}", e)
        };
        // println!("{:?}", ast);
        code_str = item_to_string(&ast);
    } else {
        let wrapped_ast: Result<syn_arbitrary::WrappedFile> =
            with_stack_space_of(64 * 1024 * 1024, move || {
                    let mut rng = thread_rng();
                    let len = rng.gen_range(100000..1000000);
                    let data: Vec<u8> = distributions::Standard.sample_iter(&mut rng).take(len).collect();
                    let mut u = Unstructured::new(&data);
                    Arbitrary::arbitrary(&mut u)
                });
        let ast: syn::File = match wrapped_ast {
            Ok(syn_arbitrary::WrappedFile(a)) => a,
            Err(e) => panic!("{:?}", e)
        };
        // println!("{:?}", ast);
        code_str = quote!(#ast).to_string();
   }
    println!("{}", code_str);
    let filename = "generated_code.rs";
    fs::write(filename, &code_str).expect("Failed writing file");
    if compile {
        let compilation_output =
            Command::new("rustc")
                .arg(filename)
                .arg("--allow").arg("warnings")
                .output()
                .expect("Error executing compile command");
        println!("{}", str::from_utf8(&compilation_output.stdout).unwrap());
        println!("{}", str::from_utf8(&compilation_output.stderr).unwrap());
    }
}

fn with_stack_space_of<T, F>(stack_space: usize, closure: F) -> T 
where F: FnOnce() -> T + 'static + Send, T: 'static + Send {
    let builder = thread::Builder::new().stack_size(stack_space);
    let handler = builder.spawn(closure).unwrap();
    return handler.join().unwrap();
}
