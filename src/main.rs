use arbitrary::Unstructured;
use quote::quote;
use rand::{distributions, prelude::Distribution, thread_rng, Rng};
use std::env::Args;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::{env, fs, thread, io::Read};

mod parser_wrapper;
use parser_wrapper::parse;

#[macro_use]
mod context_arbitrary;
use context_arbitrary::GenerationError;

#[macro_use]
mod context;
use context::Context;

#[macro_use]
mod semantics;

mod syn_arbitrary;
use syn_arbitrary::make_wrapped_file;

use crate::syn_arbitrary::WrappedFile;

fn main() -> Result<(), MainError> {

    let flags = parse_args(env::args());
    if flags.exit_imediately {
        return Ok(())
    }

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
        Repeat::FirstError => 1 > errors,
    } {
        count += 1;
        let ast = gen_code(flags.use_semantics)?;
        let ast_string = format!("{:#?}", ast);
        fs::write(ast_filename, &ast_string).expect("Failed writing file");
        let code_str = quote!(#ast).to_string();
        fs::write(filename, &code_str).expect("Failed writing file");
        let mut output;
        if flags.use_semantics {
            output = parser_wrapper::compile(filename)?;
        } else {
            let buf = gag::BufferRedirect::stderr().unwrap();
            output = String::new();
            if let Err(e) = parse(filename) {
                output = format!("{:?}", e).to_string();
            }
            buf.into_inner().read_to_string(&mut output).unwrap();
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

fn with_stack_space_of<T, F>(stack_space: usize, closure: F) -> T
where
    F: FnOnce() -> T + 'static + Send,
    T: 'static + Send,
{
    let builder = thread::Builder::new().stack_size(stack_space);
    let handler = builder.spawn(closure).unwrap();
    return handler.join().unwrap();
}

#[derive(Debug)]
enum MainError {
    GenError(context_arbitrary::GenerationError),
    ParserError(parser_wrapper::ParserError)
}

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

enum Repeat { N(usize), Forever, FirstError }
struct Flags {
    repeat: Repeat,
    use_semantics: bool,
    exit_imediately: bool
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
        exit_imediately: false
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
    while let Some(arg) = args.next() {
        // if arg == "syntax" { 
        //     flags.use_semantics = false
        // } else if arg == "-n" || arg == "--number_of_iterations" {
        //     let n = args.next().expect("Expected value for argument")
        //                 .parse().expect("Error while parsing argument");
        //     flags.repeat = Repeat::N(n);
        // } else if arg == "-r" || arg == "--repeat" {
        //     flags.repeat = Repeat::Forever;
        // } else if arg == "-f" || arg == "--first-error" {
        //     flags.repeat = Repeat::FirstError;
        // }
        for (short_form, long_form, arg_action, _) in &options {
            if &Some(arg.as_str()) == short_form || &arg == long_form {
                match arg_action {
                    ArgAction::Nullary(f) => f(&mut flags),
                    ArgAction::Unnary(f) => f(&mut flags, args.next().expect("Expected value for argument")),
                    ArgAction::Meta(f) => f(&mut flags, &options),
                }
            }
        }
        
    }
    return flags;
}

fn gen_code(use_semantics: bool) -> Result<syn::File, GenerationError> {
    let WrappedFile(ast): syn_arbitrary::WrappedFile =
        with_stack_space_of(64 * 1024 * 1024, move || {
            let mut rng = thread_rng();
            let len = rng.gen_range(1000000..10000000);
            let data: Vec<u8> = distributions::Standard
                .sample_iter(&mut rng)
                .take(len)
                .collect();
            let mut u = Unstructured::new(&data);
            let mut ctx = Context::make_context(use_semantics);
            make_wrapped_file(&mut ctx, &mut u)
        })?;
    return Ok(ast);
}
