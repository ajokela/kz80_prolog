//! Lexer for Prolog source code.
//!
//! Tokenizes Prolog programs into a stream of tokens, handling atoms, variables,
//! integers, operators, and comments.

use crate::token::{Span, Token};

/// Lexer error with location information.
#[derive(Debug, Clone)]
pub struct LexerError {
    pub message: String,
    pub span: Span,
}

impl LexerError {
    pub fn new(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self {
            message: message.into(),
            span: Span::new(line, column),
        }
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Lexer error at {}:{}: {}",
            self.span.line, self.span.column, self.message
        )
    }
}

impl std::error::Error for LexerError {}

/// Lexer for Prolog source code.
pub struct Lexer<'a> {
    input: &'a str,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    line: usize,
    column: usize,
    current_pos: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given input.
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.char_indices().peekable(),
            line: 1,
            column: 1,
            current_pos: 0,
        }
    }

    /// Get current span for error reporting.
    pub fn span(&self) -> Span {
        Span::new(self.line, self.column)
    }

    /// Peek at the next character without consuming it.
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, c)| c)
    }

    /// Peek at the character after the next one.
    fn peek_next(&self) -> Option<char> {
        let mut iter = self.input[self.current_pos..].char_indices().peekable();
        iter.next();
        iter.peek().map(|&(_, c)| c)
    }

    /// Consume and return the next character.
    fn advance(&mut self) -> Option<char> {
        if let Some((pos, c)) = self.chars.next() {
            self.current_pos = pos + c.len_utf8();
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            Some(c)
        } else {
            None
        }
    }

    /// Skip whitespace and comments.
    fn skip_whitespace_and_comments(&mut self) {
        loop {
            // Skip whitespace
            while let Some(c) = self.peek() {
                if c.is_whitespace() {
                    self.advance();
                } else {
                    break;
                }
            }

            // Check for comments
            if let Some('%') = self.peek() {
                // Line comment: skip to end of line
                while let Some(c) = self.advance() {
                    if c == '\n' {
                        break;
                    }
                }
            } else if let Some('/') = self.peek() {
                if self.peek_next() == Some('*') {
                    // Block comment: /* ... */
                    self.advance(); // consume '/'
                    self.advance(); // consume '*'
                    let mut depth = 1;
                    while depth > 0 {
                        match self.advance() {
                            Some('*') if self.peek() == Some('/') => {
                                self.advance();
                                depth -= 1;
                            }
                            Some('/') if self.peek() == Some('*') => {
                                self.advance();
                                depth += 1;
                            }
                            Some(_) => {}
                            None => break, // Unterminated comment
                        }
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    /// Read an identifier (atom or variable).
    fn read_identifier(&mut self, first: char) -> String {
        let mut ident = String::new();
        ident.push(first);
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }
        ident
    }

    /// Read a quoted atom.
    fn read_quoted_atom(&mut self) -> Result<String, LexerError> {
        let start_line = self.line;
        let start_col = self.column;
        let mut atom = String::new();

        loop {
            match self.advance() {
                Some('\'') => {
                    // Check for escaped quote
                    if self.peek() == Some('\'') {
                        self.advance();
                        atom.push('\'');
                    } else {
                        break;
                    }
                }
                Some('\\') => {
                    // Escape sequence
                    match self.advance() {
                        Some('n') => atom.push('\n'),
                        Some('t') => atom.push('\t'),
                        Some('\\') => atom.push('\\'),
                        Some('\'') => atom.push('\''),
                        Some(c) => atom.push(c),
                        None => {
                            return Err(LexerError::new(
                                "Unterminated quoted atom",
                                start_line,
                                start_col,
                            ))
                        }
                    }
                }
                Some(c) => atom.push(c),
                None => {
                    return Err(LexerError::new(
                        "Unterminated quoted atom",
                        start_line,
                        start_col,
                    ))
                }
            }
        }
        Ok(atom)
    }

    /// Read an integer.
    fn read_integer(&mut self, first: char) -> i32 {
        let mut num = String::new();
        num.push(first);
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                num.push(c);
                self.advance();
            } else {
                break;
            }
        }
        num.parse().unwrap_or(0)
    }

    /// Get the next token.
    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace_and_comments();

        let start_line = self.line;
        let start_col = self.column;

        let c = match self.advance() {
            Some(c) => c,
            None => return Ok(Token::Eof),
        };

        match c {
            // Single character tokens
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '[' => Ok(Token::LBracket),
            ']' => Ok(Token::RBracket),
            ',' => Ok(Token::Comma),
            ';' => Ok(Token::Semicolon),
            '|' => Ok(Token::Pipe),
            '!' => Ok(Token::Cut),
            '+' => Ok(Token::Plus),
            '*' => Ok(Token::Star),
            '/' => Ok(Token::Slash),

            // Dot - could be end of clause or decimal (we don't support floats)
            '.' => Ok(Token::Dot),

            // Minus or negative number
            '-' => {
                if let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        let first = self.advance().unwrap();
                        let n = self.read_integer(first);
                        Ok(Token::Integer(-n))
                    } else {
                        Ok(Token::Minus)
                    }
                } else {
                    Ok(Token::Minus)
                }
            }

            // Operators starting with :
            ':' => {
                if self.peek() == Some('-') {
                    self.advance();
                    Ok(Token::Neck)
                } else {
                    Err(LexerError::new(
                        "Expected '-' after ':'",
                        start_line,
                        start_col,
                    ))
                }
            }

            // Operators starting with ?
            '?' => {
                if self.peek() == Some('-') {
                    self.advance();
                    Ok(Token::Query)
                } else {
                    Err(LexerError::new(
                        "Expected '-' after '?'",
                        start_line,
                        start_col,
                    ))
                }
            }

            // Operators starting with =
            '=' => match self.peek() {
                Some(':') => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Ok(Token::ArithEq)
                    } else {
                        Err(LexerError::new("Expected '=' after '=:'", start_line, start_col))
                    }
                }
                Some('\\') => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Ok(Token::ArithNe)
                    } else {
                        Err(LexerError::new(
                            "Expected '=' after '=\\'",
                            start_line,
                            start_col,
                        ))
                    }
                }
                Some('=') => {
                    self.advance();
                    Ok(Token::Identical)
                }
                Some('<') => {
                    self.advance();
                    Ok(Token::Le)
                }
                _ => Ok(Token::Unify),
            },

            // Operators starting with \
            '\\' => match self.peek() {
                Some('=') => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Ok(Token::NotIdentical)
                    } else {
                        Ok(Token::NotUnify)
                    }
                }
                _ => Err(LexerError::new(
                    "Unexpected character '\\'",
                    start_line,
                    start_col,
                )),
            },

            // Less than or less-equal
            '<' => Ok(Token::Lt),

            // Greater than or greater-equal
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(Token::Ge)
                } else {
                    Ok(Token::Gt)
                }
            }

            // Quoted atom
            '\'' => {
                let atom = self.read_quoted_atom()?;
                Ok(Token::Atom(atom))
            }

            // Underscore - anonymous variable or named variable starting with _
            '_' => {
                if let Some(c) = self.peek() {
                    if c.is_alphanumeric() || c == '_' {
                        let ident = self.read_identifier('_');
                        Ok(Token::Variable(ident))
                    } else {
                        Ok(Token::Anonymous)
                    }
                } else {
                    Ok(Token::Anonymous)
                }
            }

            // Integer
            c if c.is_ascii_digit() => {
                let n = self.read_integer(c);
                Ok(Token::Integer(n))
            }

            // Lowercase letter - atom or keyword
            c if c.is_ascii_lowercase() => {
                let ident = self.read_identifier(c);
                match ident.as_str() {
                    "is" => Ok(Token::Is),
                    "mod" => Ok(Token::Mod),
                    _ => Ok(Token::Atom(ident)),
                }
            }

            // Uppercase letter - variable
            c if c.is_ascii_uppercase() => {
                let ident = self.read_identifier(c);
                Ok(Token::Variable(ident))
            }

            _ => Err(LexerError::new(
                format!("Unexpected character '{}'", c),
                start_line,
                start_col,
            )),
        }
    }

    /// Tokenize the entire input.
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            if token == Token::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_fact() {
        let mut lexer = Lexer::new("parent(tom, bob).");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Atom("parent".to_string()),
                Token::LParen,
                Token::Atom("tom".to_string()),
                Token::Comma,
                Token::Atom("bob".to_string()),
                Token::RParen,
                Token::Dot,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_rule() {
        let mut lexer = Lexer::new("grandparent(X, Z) :- parent(X, Y), parent(Y, Z).");
        let tokens = lexer.tokenize().unwrap();
        assert!(tokens.contains(&Token::Neck));
        assert!(tokens.contains(&Token::Variable("X".to_string())));
        assert!(tokens.contains(&Token::Variable("Y".to_string())));
        assert!(tokens.contains(&Token::Variable("Z".to_string())));
    }

    #[test]
    fn test_query() {
        let mut lexer = Lexer::new("?- parent(X, bob).");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0], Token::Query);
    }

    #[test]
    fn test_arithmetic() {
        let mut lexer = Lexer::new("X is 1 + 2 * 3.");
        let tokens = lexer.tokenize().unwrap();
        assert!(tokens.contains(&Token::Is));
        assert!(tokens.contains(&Token::Plus));
        assert!(tokens.contains(&Token::Star));
        assert!(tokens.contains(&Token::Integer(1)));
        assert!(tokens.contains(&Token::Integer(2)));
        assert!(tokens.contains(&Token::Integer(3)));
    }

    #[test]
    fn test_comparisons() {
        let mut lexer = Lexer::new("X < Y, X > Y, X =< Y, X >= Y, X =:= Y, X =\\= Y.");
        let tokens = lexer.tokenize().unwrap();
        assert!(tokens.contains(&Token::Lt));
        assert!(tokens.contains(&Token::Gt));
        assert!(tokens.contains(&Token::Le));
        assert!(tokens.contains(&Token::Ge));
        assert!(tokens.contains(&Token::ArithEq));
        assert!(tokens.contains(&Token::ArithNe));
    }

    #[test]
    fn test_list() {
        let mut lexer = Lexer::new("[H|T]");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LBracket,
                Token::Variable("H".to_string()),
                Token::Pipe,
                Token::Variable("T".to_string()),
                Token::RBracket,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_anonymous_variable() {
        let mut lexer = Lexer::new("foo(_, _X).");
        let tokens = lexer.tokenize().unwrap();
        assert!(tokens.contains(&Token::Anonymous));
        assert!(tokens.contains(&Token::Variable("_X".to_string())));
    }

    #[test]
    fn test_quoted_atom() {
        let mut lexer = Lexer::new("'Hello World'");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0], Token::Atom("Hello World".to_string()));
    }

    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("% comment\nfoo. /* block */ bar.");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Atom("foo".to_string()),
                Token::Dot,
                Token::Atom("bar".to_string()),
                Token::Dot,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_cut() {
        let mut lexer = Lexer::new("foo :- bar, !, baz.");
        let tokens = lexer.tokenize().unwrap();
        assert!(tokens.contains(&Token::Cut));
    }

    #[test]
    fn test_negative_integer() {
        let mut lexer = Lexer::new("-42");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0], Token::Integer(-42));
    }
}
