//! kz80_prolog: Prolog compiler for Z80.
//!
//! A Prolog implementation targeting the Z80 processor using a Simplified Prolog Machine (SPM)
//! approach with compact bytecode and a small runtime interpreter.
//!
//! # Features
//!
//! - Facts and rules (Horn clauses)
//! - Unification of first-order terms
//! - Backtracking via choice points
//! - Atoms, variables, 16-bit integers
//! - Compound terms and lists
//! - Arithmetic evaluation and comparisons
//! - Cut for control flow
//!
//! # Example
//!
//! ```prolog
//! parent(tom, bob).
//! parent(bob, pat).
//! ancestor(X, Y) :- parent(X, Y).
//! ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
//! ?- ancestor(tom, pat).
//! ```

pub mod analyze;
pub mod ast;
pub mod codegen;
pub mod compile;
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod symbol;
pub mod token;

pub use ast::{ArithExpr, ArithOp, Clause, CompareOp, Goal, Program, Term};
pub use lexer::{Lexer, LexerError};
pub use parser::{parse, ParseError, Parser};
pub use token::{Span, Token};
