//! kz80_prolog CLI - Prolog compiler for Z80.

use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

use kz80_prolog::{parse, Program};

fn print_usage() {
    eprintln!("kz80_prolog - Prolog compiler for Z80");
    eprintln!();
    eprintln!("Usage: kz80_prolog [options] <input.pl>");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -o <file>     Output binary file (default: out.bin)");
    eprintln!("  --ast         Print AST and exit");
    eprintln!("  --tokens      Print tokens and exit");
    eprintln!("  -h, --help    Show this help");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  kz80_prolog program.pl           Compile to out.bin");
    eprintln!("  kz80_prolog -o test.bin test.pl  Compile to test.bin");
    eprintln!("  kz80_prolog --ast program.pl     Print AST");
}

fn print_ast(program: &Program) {
    println!("=== Clauses ===");
    for (i, clause) in program.clauses.iter().enumerate() {
        println!("Clause {}:", i + 1);
        println!("  Head: {:?}", clause.head);
        if !clause.body.is_empty() {
            println!("  Body:");
            for goal in &clause.body {
                println!("    {:?}", goal);
            }
        }
    }

    if let Some(query) = &program.query {
        println!("\n=== Query ===");
        for goal in query {
            println!("  {:?}", goal);
        }
    }
}

fn print_tokens(input: &str) {
    let mut lexer = kz80_prolog::Lexer::new(input);
    println!("=== Tokens ===");
    loop {
        match lexer.next_token() {
            Ok(token) => {
                println!("{:?}", token);
                if token == kz80_prolog::Token::Eof {
                    break;
                }
            }
            Err(e) => {
                eprintln!("Lexer error: {}", e);
                break;
            }
        }
    }
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        print_usage();
        return ExitCode::from(1);
    }

    let mut input_file: Option<PathBuf> = None;
    let mut output_file = PathBuf::from("out.bin");
    let mut print_ast_flag = false;
    let mut print_tokens_flag = false;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => {
                print_usage();
                return ExitCode::SUCCESS;
            }
            "-o" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: -o requires an argument");
                    return ExitCode::from(1);
                }
                output_file = PathBuf::from(&args[i]);
            }
            "--ast" => {
                print_ast_flag = true;
            }
            "--tokens" => {
                print_tokens_flag = true;
            }
            arg if arg.starts_with('-') => {
                eprintln!("Unknown option: {}", arg);
                return ExitCode::from(1);
            }
            _ => {
                input_file = Some(PathBuf::from(&args[i]));
            }
        }
        i += 1;
    }

    let input_file = match input_file {
        Some(f) => f,
        None => {
            eprintln!("Error: No input file specified");
            print_usage();
            return ExitCode::from(1);
        }
    };

    // Read input file
    let input = match fs::read_to_string(&input_file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", input_file.display(), e);
            return ExitCode::from(1);
        }
    };

    // Print tokens if requested
    if print_tokens_flag {
        print_tokens(&input);
        return ExitCode::SUCCESS;
    }

    // Parse the program
    let program = match parse(&input) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            return ExitCode::from(1);
        }
    };

    // Print AST if requested
    if print_ast_flag {
        print_ast(&program);
        return ExitCode::SUCCESS;
    }

    // Compile to bytecode
    let compiled = kz80_prolog::compile::compile_program(&program);

    println!("Compiled {} clauses", program.clauses.len());
    println!("  {} predicates", compiled.predicates.len());
    println!("  {} atoms interned", compiled.atoms.len());
    println!("  {} bytes of bytecode", compiled.code.len());

    if compiled.query_offset.is_some() {
        println!("  Query present at offset 0x{:04X}", compiled.query_offset.unwrap());
    }

    // Generate ROM image
    match kz80_prolog::codegen::generate_rom_file(&compiled, &output_file) {
        Ok(()) => {
            println!("\nWrote ROM to: {}", output_file.display());
        }
        Err(e) => {
            eprintln!("Error writing ROM: {}", e);
            return ExitCode::from(1);
        }
    }

    ExitCode::SUCCESS
}
