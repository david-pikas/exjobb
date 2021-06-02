use arbitrary::{Result, Unstructured};
use quote::quote;
use rand::{distributions, prelude::Distribution, thread_rng, Rng};
use std::process::Command;
use std::{env, fs, str, thread};

#[macro_use]
mod context_arbitrary;
use context::Context;

#[macro_use]
mod context;

#[macro_use]
mod semantics;

mod syn_arbitrary;
use syn_arbitrary::make_wrapped_file;

use crate::syn_arbitrary::WrappedFile;

fn main() -> Result<()> {
    let semantically_correct: bool = Some(String::from("syntax")) != env::args().nth(1);
    let code_str: String;
    
    let wrapped_ast: syn_arbitrary::WrappedFile =
        with_stack_space_of(64 * 1024 * 1024, move || {
            let mut rng = thread_rng();
            let len = rng.gen_range(100000..1000000);
            let data: Vec<u8> = distributions::Standard
                .sample_iter(&mut rng)
                .take(len)
                .collect();
            
            let mut u = Unstructured::new(&data);
            let mut ctx = Context::make_context(semantically_correct);
            make_wrapped_file(&mut ctx, &mut u)
        })?;
    let WrappedFile(ast) = wrapped_ast;
    let ast_string = format!("{:#?}", ast);
    let ast_filename = "./output_files/ast.rs";
    fs::write(ast_filename, &ast_string).expect("Failed writing file");
    code_str = quote!(#ast).to_string();
    println!("{}", code_str);
    let filename = "./output_files/generated_code.rs";
    fs::write(filename, &code_str).expect("Failed writing file");
    let compilation_output = if semantically_correct {
        Command::new("rustc")
            .arg(filename)
            .arg("--allow").arg("warnings")
            .arg("--edition").arg("2018")
            .arg("--out-dir").arg("./output_files/")
            .output().expect("Error executing compile command")
    } else {
        Command::new("rustfmt").arg(filename)
            .output().expect("Error executing compile command")
                
    };
    println!("{}", str::from_utf8(&compilation_output.stdout).unwrap());
    println!("{}", str::from_utf8(&compilation_output.stderr).unwrap());
    return Ok(())
}

fn with_stack_space_of<T, F>(stack_space: usize, closure: F) -> T
where
    F: FnOnce() -> T + 'static + Send,
    T: 'static + Send,
{
    let builder = thread::Builder::new().stack_size(stack_space);
    let handler = builder.spawn(closure).unwrap();
    return handler.join().unwrap();
}
