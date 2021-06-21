use rustc_ap_rustc_ast::{Item, ptr::P, token::TokenKind};
use rustc_ap_rustc_parse::parse_stream_from_source_str;
use rustc_ap_rustc_session::parse::ParseSess;
use rustc_ap_rustc_span::{edition::Edition, FileName, source_map::FilePathMapping};
use rustc_ap_rustc_errors::DiagnosticBuilder;
use rustc_ap_rustc_parse::parser::Parser;
extern crate rustc_interface;
extern crate rustc_errors;
extern crate rustc_hash;
extern crate rustc_session;
extern crate rustc_error_codes;
extern crate rustc_hir;
use std::{fs, io, path::PathBuf, str::{self, Utf8Error}, sync::{Arc, Mutex}};

#[derive(Debug)]
pub enum ParserError {
    Utf8(Utf8Error),
    Io(io::Error),
    Parser(String)
}

impl From<DiagnosticBuilder<'_>> for ParserError {
    fn from(e: DiagnosticBuilder) -> Self {
        Self::Parser(format!("{:?}", e.into_diagnostic().unwrap().0))
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
        // The conversion needs to happen inside the with_session_global call
        let result = parser.parse_mod(&TokenKind::Eof).map_err(ParserError::from)?.1;
        Ok(result)
    })
}

pub fn make_filename(s: &str) -> FileName {
    FileName::from(PathBuf::from(s))
}


// see: https://rustc-dev-guide.rust-lang.org/rustc-driver-getting-diagnostics.html
pub fn compile(filename: &str) -> Result<String, ParserError> {
   let (config, output_arc) = make_config(filename)?;
   rustc_interface::run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            queries.global_ctxt().unwrap().take().enter(|tcx| {
            //     tcx.analysis(()).unwrap();
                for (_, item) in &tcx.hir().krate().items {
                    match item.kind {
                        rustc_hir::ItemKind::Static(_, _, _) | rustc_hir::ItemKind::Fn(_, _, _) => {
                            let name = item.ident;
                            let ty = tcx.type_of(tcx.hir().local_def_id(item.hir_id()));
                            println!("{:?}:\t{:?}", name, ty)
                        }
                        _ => (),
                    }
                }
            });

        })
    });
    let mutex = Arc::try_unwrap(output_arc).unwrap();
    let char_vec = mutex.into_inner().unwrap();
   
    Ok(String::from_utf8(char_vec).map_err(|e| e.utf8_error())?)
}

// see: https://github.com/rust-lang/rustc-dev-guide/blob/master/examples/rustc-driver-example.rs
fn make_config(filename: &str) -> Result<(rustc_interface::Config, Arc<Mutex<Vec<u8>>>), ParserError> {
    let source = str::from_utf8(&fs::read(&filename)?)?.to_string();
    let output_arc = Arc::new(Mutex::new(vec![]));
    Ok((rustc_interface::Config {
        // Command line options
        opts: rustc_session::config::Options::default(),
        // cfg! configuration in addition to the default ones
        crate_cfg: rustc_hash::FxHashSet::default(), // FxHashSet<(String, Option<String>)>
        input: rustc_session::config::Input::Str {
            name: (PathBuf::from(filename).into()),
            input: source
        },
        input_path: None,
        output_dir: None,
        output_file: None,
        file_loader: None,
        diagnostic_output: rustc_session::DiagnosticOutput::Default,
        // Set to capture stderr output during compiler execution
        stderr: Some(output_arc.clone()),
        lint_caps: rustc_hash::FxHashMap::default(),
        parse_sess_created: None, 
        register_lints: None,
        override_queries: None,
        registry: rustc_errors::registry::Registry::new(&rustc_error_codes::DIAGNOSTICS),
        make_codegen_backend: None,
    }, output_arc))
}
