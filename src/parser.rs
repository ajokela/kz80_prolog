//! Parser for Prolog source code.
//!
//! Implements a recursive descent parser for Prolog programs,
//! handling facts, rules, queries, and all term types.

use crate::ast::{ArithExpr, ArithOp, Clause, CompareOp, Goal, Program, Term};
use crate::lexer::Lexer;
use crate::token::{Span, Token};

/// Parse error with location information.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    pub fn new(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self {
            message: message.into(),
            span: Span::new(line, column),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Parse error at {}:{}: {}",
            self.span.line, self.span.column, self.message
        )
    }
}

impl std::error::Error for ParseError {}

/// Parser for Prolog programs.
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
    span: Span,
}

impl<'a> Parser<'a> {
    /// Create a new parser for the given input.
    pub fn new(input: &'a str) -> Result<Self, ParseError> {
        let mut lexer = Lexer::new(input);
        let span = lexer.span();
        let current = lexer.next_token().map_err(|e| ParseError {
            message: e.message,
            span: e.span,
        })?;
        Ok(Self {
            lexer,
            current,
            span,
        })
    }

    /// Get current span for error reporting.
    fn span(&self) -> Span {
        self.span
    }

    /// Advance to the next token.
    fn advance(&mut self) -> Result<Token, ParseError> {
        let old = std::mem::replace(&mut self.current, Token::Eof);
        self.span = self.lexer.span();
        self.current = self.lexer.next_token().map_err(|e| ParseError {
            message: e.message,
            span: e.span,
        })?;
        Ok(old)
    }

    /// Check if current token matches.
    fn check(&self, token: &Token) -> bool {
        std::mem::discriminant(&self.current) == std::mem::discriminant(token)
    }

    /// Expect a specific token, error if not found.
    fn expect(&mut self, expected: &Token) -> Result<Token, ParseError> {
        if self.check(expected) {
            self.advance()
        } else {
            Err(ParseError::new(
                format!("Expected {:?}, found {:?}", expected, self.current),
                self.span.line,
                self.span.column,
            ))
        }
    }

    /// Parse a complete program.
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new();

        while self.current != Token::Eof {
            if self.current == Token::Query {
                // Query: ?- goals.
                self.advance()?;
                let goals = self.parse_goals()?;
                self.expect(&Token::Dot)?;
                program.query = Some(goals);
            } else {
                // Clause: fact or rule
                let clause = self.parse_clause()?;
                program.clauses.push(clause);
            }
        }

        Ok(program)
    }

    /// Parse a single clause.
    fn parse_clause(&mut self) -> Result<Clause, ParseError> {
        let head = self.parse_term()?;

        if self.current == Token::Neck {
            // Rule: head :- body.
            self.advance()?;
            let body = self.parse_goals()?;
            self.expect(&Token::Dot)?;
            Ok(Clause::rule(head, body))
        } else {
            // Fact: head.
            self.expect(&Token::Dot)?;
            Ok(Clause::fact(head))
        }
    }

    /// Parse a comma-separated list of goals.
    fn parse_goals(&mut self) -> Result<Vec<Goal>, ParseError> {
        let mut goals = Vec::new();
        goals.push(self.parse_goal()?);

        while self.current == Token::Comma {
            self.advance()?;
            goals.push(self.parse_goal()?);
        }

        Ok(goals)
    }

    /// Parse a single goal.
    fn parse_goal(&mut self) -> Result<Goal, ParseError> {
        // Check for cut
        if self.current == Token::Cut {
            self.advance()?;
            return Ok(Goal::Cut);
        }

        // Check for built-in predicates
        if let Token::Atom(name) = &self.current {
            match name.as_str() {
                "fail" => {
                    self.advance()?;
                    return Ok(Goal::Fail);
                }
                "true" => {
                    self.advance()?;
                    return Ok(Goal::True);
                }
                "nl" => {
                    self.advance()?;
                    return Ok(Goal::Nl);
                }
                "write" => {
                    self.advance()?;
                    self.expect(&Token::LParen)?;
                    let term = self.parse_term()?;
                    self.expect(&Token::RParen)?;
                    return Ok(Goal::Write(term));
                }
                _ => {}
            }
        }

        // Try to parse as arithmetic: Var is Expr
        if let Token::Variable(var_name) = &self.current {
            let var_name = var_name.clone();
            let _saved_span = self.span;

            self.advance()?;

            if self.current == Token::Is {
                self.advance()?;
                let expr = self.parse_arith_expr()?;
                return Ok(Goal::Is(var_name, expr));
            }

            // Check for comparison: Var op Expr
            if let Some(op) = self.try_compare_op() {
                self.advance()?;
                let right = self.parse_arith_expr()?;
                return Ok(Goal::Compare(
                    op,
                    ArithExpr::Variable(var_name),
                    right,
                ));
            }

            // Check for unification
            if self.current == Token::Unify {
                self.advance()?;
                let right = self.parse_term()?;
                return Ok(Goal::Unify(Term::Variable(var_name), right));
            }

            if self.current == Token::NotUnify {
                self.advance()?;
                let right = self.parse_term()?;
                return Ok(Goal::NotUnify(Term::Variable(var_name), right));
            }

            // It was just a variable call - restore and parse as term
            // Since we can't easily backtrack, we'll construct the term
            // Check if this is a compound term (variable can't be a functor)
            // Variables as goals are just calls
            return Ok(Goal::Call(Term::Variable(var_name)));
        }

        // Try to parse arithmetic comparison: Expr op Expr
        // First, check if it looks like an arithmetic expression
        if matches!(
            self.current,
            Token::Integer(_) | Token::LParen | Token::Minus
        ) {
            let left = self.parse_arith_expr()?;
            if let Some(op) = self.try_compare_op() {
                self.advance()?;
                let right = self.parse_arith_expr()?;
                return Ok(Goal::Compare(op, left, right));
            }
            // Not a comparison, this is an error
            return Err(ParseError::new(
                "Expected comparison operator",
                self.span.line,
                self.span.column,
            ));
        }

        // Parse as a term (predicate call)
        let term = self.parse_term()?;

        // Check for unification
        if self.current == Token::Unify {
            self.advance()?;
            let right = self.parse_term()?;
            return Ok(Goal::Unify(term, right));
        }

        if self.current == Token::NotUnify {
            self.advance()?;
            let right = self.parse_term()?;
            return Ok(Goal::NotUnify(term, right));
        }

        Ok(Goal::Call(term))
    }

    /// Try to match a comparison operator.
    fn try_compare_op(&self) -> Option<CompareOp> {
        match &self.current {
            Token::Lt => Some(CompareOp::Lt),
            Token::Gt => Some(CompareOp::Gt),
            Token::Le => Some(CompareOp::Le),
            Token::Ge => Some(CompareOp::Ge),
            Token::ArithEq => Some(CompareOp::ArithEq),
            Token::ArithNe => Some(CompareOp::ArithNe),
            _ => None,
        }
    }

    /// Parse a term.
    fn parse_term(&mut self) -> Result<Term, ParseError> {
        self.parse_term_1000()
    }

    /// Parse term at precedence 1000 (comma in some contexts, but we handle that separately)
    fn parse_term_1000(&mut self) -> Result<Term, ParseError> {
        self.parse_primary_term()
    }

    /// Parse a primary term: atom, variable, integer, compound, list.
    fn parse_primary_term(&mut self) -> Result<Term, ParseError> {
        match &self.current {
            Token::Integer(n) => {
                let n = *n;
                self.advance()?;
                Ok(Term::Integer(n))
            }

            Token::Atom(name) => {
                let name = name.clone();
                self.advance()?;

                // Check for compound term
                if self.current == Token::LParen {
                    self.advance()?;
                    let args = self.parse_term_list()?;
                    self.expect(&Token::RParen)?;
                    Ok(Term::Compound { functor: name, args })
                } else {
                    Ok(Term::Atom(name))
                }
            }

            Token::Variable(name) => {
                let name = name.clone();
                self.advance()?;
                Ok(Term::Variable(name))
            }

            Token::Anonymous => {
                self.advance()?;
                Ok(Term::Anonymous)
            }

            Token::LBracket => {
                self.advance()?;
                self.parse_list()
            }

            Token::LParen => {
                self.advance()?;
                let term = self.parse_term()?;
                self.expect(&Token::RParen)?;
                Ok(term)
            }

            Token::Minus => {
                self.advance()?;
                if let Token::Integer(n) = self.current {
                    self.advance()?;
                    Ok(Term::Integer(-n))
                } else {
                    Err(ParseError::new(
                        "Expected integer after '-'",
                        self.span.line,
                        self.span.column,
                    ))
                }
            }

            _ => Err(ParseError::new(
                format!("Expected term, found {:?}", self.current),
                self.span.line,
                self.span.column,
            )),
        }
    }

    /// Parse a comma-separated list of terms.
    fn parse_term_list(&mut self) -> Result<Vec<Term>, ParseError> {
        let mut terms = Vec::new();
        terms.push(self.parse_term()?);

        while self.current == Token::Comma {
            self.advance()?;
            terms.push(self.parse_term()?);
        }

        Ok(terms)
    }

    /// Parse a list: [], [a], [a,b], [H|T], [a,b|T]
    fn parse_list(&mut self) -> Result<Term, ParseError> {
        // Empty list
        if self.current == Token::RBracket {
            self.advance()?;
            return Ok(Term::Nil);
        }

        // Non-empty list
        let mut elements = Vec::new();
        elements.push(self.parse_term()?);

        while self.current == Token::Comma {
            self.advance()?;
            elements.push(self.parse_term()?);
        }

        // Check for tail
        let tail = if self.current == Token::Pipe {
            self.advance()?;
            self.parse_term()?
        } else {
            Term::Nil
        };

        self.expect(&Token::RBracket)?;

        // Build the list from right to left
        Ok(Term::list_with_tail(elements, tail))
    }

    /// Parse an arithmetic expression.
    fn parse_arith_expr(&mut self) -> Result<ArithExpr, ParseError> {
        self.parse_arith_additive()
    }

    /// Parse additive expression: + -
    fn parse_arith_additive(&mut self) -> Result<ArithExpr, ParseError> {
        let mut left = self.parse_arith_multiplicative()?;

        loop {
            let op = match &self.current {
                Token::Plus => ArithOp::Add,
                Token::Minus => ArithOp::Sub,
                _ => break,
            };
            self.advance()?;
            let right = self.parse_arith_multiplicative()?;
            left = ArithExpr::BinOp(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    /// Parse multiplicative expression: * / mod
    fn parse_arith_multiplicative(&mut self) -> Result<ArithExpr, ParseError> {
        let mut left = self.parse_arith_unary()?;

        loop {
            let op = match &self.current {
                Token::Star => ArithOp::Mul,
                Token::Slash => ArithOp::Div,
                Token::Mod => ArithOp::Mod,
                _ => break,
            };
            self.advance()?;
            let right = self.parse_arith_unary()?;
            left = ArithExpr::BinOp(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    /// Parse unary expression: - or primary
    fn parse_arith_unary(&mut self) -> Result<ArithExpr, ParseError> {
        if self.current == Token::Minus {
            self.advance()?;
            let expr = self.parse_arith_primary()?;
            Ok(ArithExpr::Neg(Box::new(expr)))
        } else {
            self.parse_arith_primary()
        }
    }

    /// Parse primary arithmetic expression: integer, variable, (expr)
    fn parse_arith_primary(&mut self) -> Result<ArithExpr, ParseError> {
        match &self.current {
            Token::Integer(n) => {
                let n = *n;
                self.advance()?;
                Ok(ArithExpr::Integer(n))
            }

            Token::Variable(name) => {
                let name = name.clone();
                self.advance()?;
                Ok(ArithExpr::Variable(name))
            }

            Token::LParen => {
                self.advance()?;
                let expr = self.parse_arith_expr()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }

            _ => Err(ParseError::new(
                format!("Expected arithmetic expression, found {:?}", self.current),
                self.span.line,
                self.span.column,
            )),
        }
    }
}

/// Parse a Prolog program from source.
pub fn parse(input: &str) -> Result<Program, ParseError> {
    let mut parser = Parser::new(input)?;
    parser.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_fact() {
        let program = parse("parent(tom, bob).").unwrap();
        assert_eq!(program.clauses.len(), 1);
        assert!(program.clauses[0].is_fact());
    }

    #[test]
    fn test_parse_rule() {
        let program = parse("grandparent(X, Z) :- parent(X, Y), parent(Y, Z).").unwrap();
        assert_eq!(program.clauses.len(), 1);
        assert!(!program.clauses[0].is_fact());
        assert_eq!(program.clauses[0].body.len(), 2);
    }

    #[test]
    fn test_parse_query() {
        let program = parse("?- parent(tom, X).").unwrap();
        assert!(program.query.is_some());
    }

    #[test]
    fn test_parse_arithmetic() {
        let program = parse("double(X, Y) :- Y is X * 2.").unwrap();
        assert_eq!(program.clauses.len(), 1);
        if let Goal::Is(var, _) = &program.clauses[0].body[0] {
            assert_eq!(var, "Y");
        } else {
            panic!("Expected Is goal");
        }
    }

    #[test]
    fn test_parse_comparison() {
        let program = parse("positive(X) :- X > 0.").unwrap();
        assert_eq!(program.clauses.len(), 1);
        if let Goal::Compare(op, _, _) = &program.clauses[0].body[0] {
            assert_eq!(*op, CompareOp::Gt);
        } else {
            panic!("Expected Compare goal");
        }
    }

    #[test]
    fn test_parse_list() {
        let program = parse("test([1, 2, 3]).").unwrap();
        if let Term::Compound { args, .. } = &program.clauses[0].head {
            assert!(matches!(args[0], Term::Cons(_, _)));
        } else {
            panic!("Expected compound term");
        }
    }

    #[test]
    fn test_parse_list_with_tail() {
        let program = parse("head([H|_], H).").unwrap();
        assert_eq!(program.clauses.len(), 1);
    }

    #[test]
    fn test_parse_cut() {
        let program = parse("first(X) :- find(X), !.").unwrap();
        assert_eq!(program.clauses[0].body.len(), 2);
        assert!(matches!(program.clauses[0].body[1], Goal::Cut));
    }

    #[test]
    fn test_parse_write_nl() {
        let program = parse("greet :- write(hello), nl.").unwrap();
        assert_eq!(program.clauses[0].body.len(), 2);
        assert!(matches!(program.clauses[0].body[0], Goal::Write(_)));
        assert!(matches!(program.clauses[0].body[1], Goal::Nl));
    }

    #[test]
    fn test_parse_unification() {
        let program = parse("same(X, Y) :- X = Y.").unwrap();
        assert!(matches!(program.clauses[0].body[0], Goal::Unify(_, _)));
    }

    #[test]
    fn test_parse_multiple_clauses() {
        let program = parse(
            "
            parent(tom, bob).
            parent(bob, pat).
            parent(bob, ann).
        ",
        )
        .unwrap();
        assert_eq!(program.clauses.len(), 3);
    }

    #[test]
    fn test_parse_recursive_rule() {
        let program = parse(
            "
            ancestor(X, Y) :- parent(X, Y).
            ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
        ",
        )
        .unwrap();
        assert_eq!(program.clauses.len(), 2);
    }

    #[test]
    fn test_parse_empty_list() {
        let program = parse("empty([]).").unwrap();
        if let Term::Compound { args, .. } = &program.clauses[0].head {
            assert!(matches!(args[0], Term::Nil));
        }
    }

    #[test]
    fn test_parse_nested_compound() {
        let program = parse("test(foo(bar(1))).").unwrap();
        assert_eq!(program.clauses.len(), 1);
    }
}
