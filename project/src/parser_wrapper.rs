use rustc_ap_rustc_ast::{Item, ptr::P, token::TokenKind};
use rustc_ap_rustc_parse::parse_stream_from_source_str;
use rustc_ap_rustc_session::parse::ParseSess;
use rustc_ap_rustc_span::{edition::Edition, FileName, source_map::FilePathMapping};
use rustc_ap_rustc_errors::{Diagnostic, DiagnosticBuilder};
use rustc_ap_rustc_parse::parser::Parser;
use std::{fs, io, path::PathBuf, str::{self, Utf8Error}};

#[derive(Debug)]
pub enum ParserError {
    Utf8(Utf8Error),
    Io(io::Error),
    Parser(Diagnostic)
}

impl From<DiagnosticBuilder<'_>> for ParserError {
    fn from(e: DiagnosticBuilder) -> Self {
        Self::Parser(e.into_diagnostic().unwrap().0)
    }
}

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

pub fn parse(filename: &str) -> Result<Vec<P<Item>>, ParserError> {
    rustc_ap_rustc_span::with_session_globals(Edition::Edition2018, || {
        let wrapped_filename = make_filename(filename);
        let source = str::from_utf8(&fs::read(&filename)?)?.to_string();
        let parse_sess = ParseSess::new(FilePathMapping::new(vec![]));
        let tokens = parse_stream_from_source_str(wrapped_filename, source, &parse_sess, None);
        let mut parser = Parser::new(&parse_sess, tokens, false, None);
        let result = parser.parse_mod(&TokenKind::Eof)?.1;
        Ok(result)
    })
}

pub fn make_filename(s: &str) -> FileName {
    FileName::from(PathBuf::from(s))
}
