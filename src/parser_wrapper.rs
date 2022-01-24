use std::{backtrace::Backtrace, error::Error, fmt::Display, io, process::Command, str::{self, Utf8Error}};

#[derive(Debug)]
pub enum ParserError {
    Utf8(Utf8Error),
    Io(io::Error),
    // Parser(String, Backtrace)
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::Utf8(e) => e.fmt(f),
            ParserError::Io(e) => e.fmt(f),
            ParserError::Parser(s, bt) => write!(f, "ParserError: {}\n{}", s, bt)
        }
    }
}

impl Error for ParserError {}

impl From<Utf8Error> for ParserError {
    fn from(e: Utf8Error) -> Self {
        ParserError::Utf8(e)
    }
}

impl From<io::Error> for ParserError {
    fn from(e: io::Error) -> Self {
        ParserError::Io(e)
    }
}


pub fn check(filename: &str) -> Result<String, ParserError> {
    let output = Command::new("rustc")
        .arg("-A")
        .arg("warnings")
        .arg("-Z")
        .arg("no-codegen")
        .arg(filename)
        .output()
        .expect("Error executing commant");
    Ok(String::from_utf8(output.stderr).map_err(|e| e.utf8_error())?)
}

pub fn compile_with_opt(file_in: &str, file_out: &str, opt_level: usize) -> Result<String, ParserError> {
    let output = Command::new("rustc")
        .arg(file_in)
        .arg("-A").arg("warnings")
        .arg("-A").arg("arithmetic_overflow")
        .arg("-C").arg(format!("opt-level={}", opt_level))
        .arg("-o").arg(file_out)
        .output()
        .expect("Error executing command");
    Ok(String::from_utf8(output.stderr).map_err(|e| e.utf8_error())?)
}
