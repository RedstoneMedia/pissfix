use std::path::PathBuf;
use clap::Parser;
use clap_derive::Parser;
use pissfix::code_generator::CodeGenerator;
use pissfix::errors::ErrorTracker;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The file containing the code to compile
    #[arg(value_name = "FILE")]
    input: PathBuf,
    /// Optionally save the transpiled code to the specified location
    #[arg(short, long, value_name = "OUT-FILE")]
    output_to: Option<PathBuf>,
    /// Werther or not to print the ast of the program
    #[arg(long)]
    print_ast: bool
}

fn main() {
    let args = Args::parse();

    let extension = args.input.extension()
        .expect("Pissfix files have to have a extension")
        .to_str()
        .expect("Could not read file extension");
    if !["piss", "pissfix"].contains(&extension) {
        eprintln!("Error: Pissfix files have to end in .piss or .pissfix");
        return;
    }

    let code = std::fs::read_to_string(&args.input)
        .expect("Could not read input file!");

    let mut error_tracker = ErrorTracker::new();
    let mut lexer = pissfix::lexer::Lexer::new(&code);
    lexer.lex(&mut error_tracker);
    if error_tracker.has_errors() {
        eprintln!("{}", error_tracker.get_errors_text(&code));
        return;
    }
    let mut parser = pissfix::parser::Parser::new(lexer.tokens);
    let root = parser.parse_all(&mut error_tracker);
    if error_tracker.has_errors() {
        eprintln!("{}", error_tracker.get_errors_text(&code));
        return;
    }
    if args.print_ast {
        println!("{:#?}\n\n\n", root);
    }
    let mut code_generator = CodeGenerator::default();
    code_generator.generate_code(&root, 0);
    let output_code = code_generator.code.trim_start().trim_end();
    println!("{}", output_code);
    if let Some(to) = args.output_to {
        std::fs::write(&to, output_code).expect(&format!("Could not save transpiled code to: {}", to.display()))
    }
}