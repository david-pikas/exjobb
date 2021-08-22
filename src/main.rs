#![feature(once_cell)]
#![feature(cell_update)]
#![feature(backtrace)]
#![feature(never_type)]

use std::env::Args;
use std::error::Error;
use std::fmt::Display;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::{env, fs, io::Read};
use std::path::Path;

use rand::{distributions, prelude::Distribution, thread_rng, Rng};
use subprocess::{Popen, PopenConfig, Redirection};

mod parser_wrapper;
use parser_wrapper::parse;

#[macro_use]
mod context_arbitrary;

#[macro_use]
mod choose;

#[macro_use]
mod context;
use context::Options;

#[macro_use]
mod semantics;

mod scopes;

#[macro_use]
mod ty_macros;

mod string_wrapper;

mod generate_program;

mod syn_arbitrary;

#[macro_use]
mod branching;

fn main() -> Result<(), MainError> {

    let mut args = env::args();
    let program_name = args.next().unwrap();
    let program_path = Path::new(&program_name);
    let program_dir = program_path.parent().unwrap();
    let dir_str = program_dir.to_str().unwrap();

    let flags = parse_args(env::args());
    if flags.exit_imediately {
        return Ok(())
    }
    Options::from(flags.clone()).export_env();

    let ast_filename = "./output_files/ast.rs";
    let filename = "./output_files/generated_code.rs";

    
    let is_interrupeted: Arc<AtomicBool> = Arc::new(AtomicBool::new(false));
    let r = is_interrupeted.clone();
    ctrlc::set_handler(move || {
        r.store(true, Ordering::SeqCst)
    }).expect("Error setting the Ctl-C handler");

    let mut count = 0;
    let mut errors = 0;
    while !is_interrupeted.load(Ordering::SeqCst) && match flags.repeat {
        Repeat::N(n) => n > count,
        Repeat::Forever => true,
        Repeat::FirstError => errors < 1
    } {
        count += 1;
        let mut rng = thread_rng();
        let len = rng.gen_range(1000000..10000000);
        let data: Vec<u8> = distributions::Standard
            .sample_iter(&mut rng)
            .take(len)
            .collect();
        let mut program_generator = Popen::create(
            &[format!("{}/generate-program", dir_str), filename.to_string()], PopenConfig {
                stdin: Redirection::Pipe,
                stdout: Redirection::Pipe,
                ..Default::default()
            }
        )?;
        let (m_program_code, m_error) = program_generator.communicate_bytes(Some(&data))?;
        if let Some(err) = m_error {
            eprintln!("{}", String::from_utf8(err)?);
            errors += 1;
            continue;
        } else if let Some(print_stmnts) = m_program_code {
            print!("{}", String::from_utf8(print_stmnts)?);
        }
        let mut output;
        if flags.use_semantics {
            output = parser_wrapper::compile(filename)?;
        } else {
            let buf = gag::BufferRedirect::stderr().expect("Error redirecting stdout");
            output = String::new();
            if let Err(e) = parse(filename) {
                output = format!("{:?}", e).to_string();
            }
            buf.into_inner().read_to_string(&mut output)?;
        }

        if !output.is_empty() {
            errors += 1;
            let timestamp = chrono::Utc::now().format("%Y-%m-%d:%H:%M:%S%.f");
            fs::copy(filename, format!("possible_bugs/{}.rs", timestamp)).expect("Failed writing to file");
            fs::copy(ast_filename, format!("possible_bugs/{}_ast.rs", timestamp)).expect("Failed writing to file");
            fs::write(format!("possible_bugs/{}_err.txt", timestamp), output.clone()).expect("Failed writing to file");
            println!("({}):\n{}", timestamp, output);
        }
    }
    println!("Finished {} tests with {} errors", count, errors);
    return Ok(())
}

#[derive(Debug)]
enum MainError {
    GenError(context_arbitrary::GenerationError),
    ParserError(parser_wrapper::ParserError),
    SubProcError(subprocess::PopenError),
    IoError(std::io::Error),
    Utf8Error(std::string::FromUtf8Error)
}

impl Display for MainError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use MainError::*;
        match self {
            GenError(e) => e.fmt(f),
            ParserError(e) => e.fmt(f),
            SubProcError(e) => e.fmt(f),
            IoError(e) => e.fmt(f),
            Utf8Error(e) => e.fmt(f)
        }
    }
}

impl Error for MainError {}

impl From<context_arbitrary::GenerationError> for MainError {
    fn from(e: context_arbitrary::GenerationError) -> Self {
        MainError::GenError(e)
    }
}

impl From<parser_wrapper::ParserError> for MainError {
    fn from(e: parser_wrapper::ParserError) -> Self {
        MainError::ParserError(e)
    }
}

impl From<subprocess::PopenError> for MainError {
    fn from(e: subprocess::PopenError) -> Self {
        MainError::SubProcError(e)
    }
}

impl From<std::io::Error> for MainError {
    fn from(e: std::io::Error) -> Self {
        MainError::IoError(e)
    }
}

impl From<std::string::FromUtf8Error> for MainError {
    fn from(e: std::string::FromUtf8Error) -> Self {
        MainError::Utf8Error(e)
    }
}

#[derive(Clone, PartialEq, Debug)]
enum Repeat { N(usize), Forever, FirstError }
#[derive(Clone, PartialEq, Debug)]
struct Flags {
    repeat: Repeat,
    exit_imediately: bool,
    use_semantics: bool,
    use_panics: bool,
    print_vars: bool,
}
impl From<Flags> for Options {
    fn from(flags: Flags) -> Self {
        Options {
            use_semantics: flags.use_semantics,
            use_panics: flags.use_panics,
            print_vars: flags.print_vars,
        }
    }
}

fn parse_args(mut args: Args) -> Flags {
    type Options = Vec<(Option<&'static str>, &'static str, ArgAction, &'static str)>;
    enum ArgAction {
        Nullary(fn (&mut Flags)),
        Unnary(fn (&mut Flags, String)),
        Meta(fn (&mut Flags, &Options))
    }
    let mut flags = Flags {
        repeat: Repeat::N(1),
        use_semantics: true,
        exit_imediately: false,
        use_panics: true,
        print_vars: false
    };
    let options: Options = vec![
        (Some("-n"), "--num_of_iterations", ArgAction::Unnary(|flags, arg| {
            flags.repeat = Repeat::N(arg.parse().expect("Error while parsing argument"));
        }), "Generate x programs and test them"),
        (Some("-r"), "--repeat", ArgAction::Nullary(|flags| {
            flags.repeat = Repeat::Forever;
        }), "Generate programs and test them untill interrupted"),
        (Some("-f"), "--first-error", ArgAction::Nullary(|flags| {
            flags.repeat = Repeat::FirstError;
        }), "Generate programs and test them untill one program fails"),
        (None, "--syntax", ArgAction::Nullary(|flags| {
            flags.use_semantics = false;
        }), "Generate programs that are syntactically but not necessarily semantically valid"),
        (None, "--no-panic", ArgAction::Nullary(|flags| {
            flags.use_panics = false
        }), "Don't generate any panic statements"),
        (None, "--print-vars", ArgAction::Nullary(|flags| {
            flags.print_vars = true
        }), "Print the value of every variable (usefull for comparing e.g. different optimisation levels)"),
        (Some("-h"), "--help", ArgAction::Meta(|flags, options| {
            for (short_form, long_form, act, explanation) in options {
                let arg = match act {
                    ArgAction::Unnary(_) => "=x",
                    _ => "  "
                };
                println!("  {}  {}{}   {}", 
                    short_form.unwrap_or("  "),
                    long_form,
                    arg,
                    explanation
                );
            }
            flags.exit_imediately = true;
        }), "Print this help")
    ];
    let _program_name = args.next();
    'outer: while let Some(arg) = args.next() {
        for (short_form, long_form, arg_action, _) in &options {
            if &Some(arg.as_str()) == short_form || &arg == long_form {
                match arg_action {
                    ArgAction::Nullary(f) => f(&mut flags),
                    ArgAction::Unnary(f) => f(&mut flags, args.next().expect("Expected value for argument")),
                    ArgAction::Meta(f) => f(&mut flags, &options),
                }
                continue 'outer;
            }
        }
        panic!("Unknown argument: {}", arg)
        
    }
    return flags;
}
