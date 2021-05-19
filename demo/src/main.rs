use std::path::Path;

mod ast_manipulator;
use ast_manipulator::*;

fn main() {
    let file = &Path::new("./src/ast_manipulator.rs");
    let ast = match file_to_ast(file) {
        Ok(x) => x,
        Err(e) => panic!("{:?}", e)
    };
    println!("The ast of the file is:\n{:?}", ast);
    let extra_item = syn::parse_str("use foo::bar::baz;").expect("Failure in parsing extra item");
    let mut changed_ast = ast;
    changed_ast.items.push(extra_item);
    println!("");
    println!("The \"pretty\" printed version of the code is:\n{}",
             ast_to_string(changed_ast));
             
}
