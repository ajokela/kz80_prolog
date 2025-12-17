//! Token definitions for the Prolog lexer.
//!
//! Defines all token types used in Prolog syntax including atoms, variables,
//! integers, operators, and punctuation.

/// Source location for error reporting.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

/// Token types for Prolog.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    /// Atom: lowercase identifier or quoted string
    Atom(String),
    /// Variable: uppercase identifier or underscore
    Variable(String),
    /// Anonymous variable: _
    Anonymous,
    /// Integer literal
    Integer(i32),

    // Operators
    /// :- (clause neck / query)
    Neck,
    /// ?- (query)
    Query,
    /// , (conjunction)
    Comma,
    /// ; (disjunction)
    Semicolon,
    /// . (end of clause)
    Dot,
    /// | (list tail separator)
    Pipe,
    /// ! (cut)
    Cut,

    // Brackets
    /// (
    LParen,
    /// )
    RParen,
    /// [
    LBracket,
    /// ]
    RBracket,

    // Arithmetic operators
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Star,
    /// /
    Slash,
    /// mod
    Mod,
    /// is
    Is,

    // Comparison operators
    /// =
    Unify,
    /// \=
    NotUnify,
    /// ==
    Identical,
    /// \==
    NotIdentical,
    /// <
    Lt,
    /// >
    Gt,
    /// =<
    Le,
    /// >=
    Ge,
    /// =:=
    ArithEq,
    /// =\=
    ArithNe,

    // Special
    /// End of file
    Eof,
}

impl Token {
    /// Returns true if this token can start a term.
    pub fn can_start_term(&self) -> bool {
        matches!(
            self,
            Token::Atom(_)
                | Token::Variable(_)
                | Token::Anonymous
                | Token::Integer(_)
                | Token::LParen
                | Token::LBracket
                | Token::Minus
        )
    }
}
