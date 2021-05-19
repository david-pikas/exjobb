use std::fs;
use std::path::Path;
use std::str;
use syn;
use quote::quote;

#[derive(Debug)]
pub enum Error {
    IoError(std::io::Error),
    Utf8Error(str::Utf8Error),
    ParseError(syn::Error)
}

pub fn file_to_ast(path: &Path) -> Result<syn::File, Error> {
    let file_bytes = fs::read(path).map_err(Error::IoError)?;
    let file_str = str::from_utf8(&file_bytes).map_err(Error::Utf8Error)?;
    let ast = syn::parse_file(&file_str).map_err(Error::ParseError)?;
    return Ok(ast);
}

pub fn ast_to_string(ast: syn::File) -> String {
    return quote!(#ast).to_string();
}
