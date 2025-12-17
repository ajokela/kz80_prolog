//! Abstract Syntax Tree definitions for Prolog.
//!
//! Defines the core data structures representing Prolog programs:
//! terms, clauses, goals, and queries.

/// A complete Prolog program.
#[derive(Debug, Clone)]
pub struct Program {
    /// List of clauses (facts and rules)
    pub clauses: Vec<Clause>,
    /// Optional query to execute
    pub query: Option<Vec<Goal>>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            clauses: Vec::new(),
            query: None,
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

/// A clause: fact or rule.
///
/// A fact has an empty body: `parent(tom, bob).`
/// A rule has a non-empty body: `grandparent(X, Z) :- parent(X, Y), parent(Y, Z).`
#[derive(Debug, Clone)]
pub struct Clause {
    /// The head of the clause
    pub head: Term,
    /// The body goals (empty for facts)
    pub body: Vec<Goal>,
}

impl Clause {
    /// Create a new fact (clause with empty body).
    pub fn fact(head: Term) -> Self {
        Self {
            head,
            body: Vec::new(),
        }
    }

    /// Create a new rule (clause with non-empty body).
    pub fn rule(head: Term, body: Vec<Goal>) -> Self {
        Self { head, body }
    }

    /// Returns true if this is a fact (no body).
    pub fn is_fact(&self) -> bool {
        self.body.is_empty()
    }

    /// Get the predicate name and arity of this clause.
    pub fn predicate_key(&self) -> (String, usize) {
        match &self.head {
            Term::Atom(name) => (name.clone(), 0),
            Term::Compound { functor, args } => (functor.clone(), args.len()),
            _ => ("_".to_string(), 0),
        }
    }
}

/// A goal in a clause body or query.
#[derive(Debug, Clone)]
pub enum Goal {
    /// Simple predicate call
    Call(Term),
    /// Arithmetic evaluation: Var is Expr
    Is(String, ArithExpr),
    /// Arithmetic comparison
    Compare(CompareOp, ArithExpr, ArithExpr),
    /// Unification: Term = Term
    Unify(Term, Term),
    /// Not unifiable: Term \= Term
    NotUnify(Term, Term),
    /// Cut
    Cut,
    /// Write a term
    Write(Term),
    /// Print newline
    Nl,
    /// Fail
    Fail,
    /// True (always succeeds)
    True,
}

/// A Prolog term.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// Integer literal
    Integer(i32),
    /// Atom (symbol)
    Atom(String),
    /// Variable
    Variable(String),
    /// Anonymous variable
    Anonymous,
    /// Compound term: functor(arg1, arg2, ...)
    Compound { functor: String, args: Vec<Term> },
    /// List cons cell: [H|T]
    Cons(Box<Term>, Box<Term>),
    /// Empty list: []
    Nil,
}

impl Term {
    /// Create a compound term.
    pub fn compound(functor: impl Into<String>, args: Vec<Term>) -> Self {
        Term::Compound {
            functor: functor.into(),
            args,
        }
    }

    /// Create an atom term.
    pub fn atom(name: impl Into<String>) -> Self {
        Term::Atom(name.into())
    }

    /// Create a variable term.
    pub fn var(name: impl Into<String>) -> Self {
        Term::Variable(name.into())
    }

    /// Create an integer term.
    pub fn int(n: i32) -> Self {
        Term::Integer(n)
    }

    /// Create a list from a vector of terms.
    pub fn list(terms: Vec<Term>) -> Self {
        let mut result = Term::Nil;
        for term in terms.into_iter().rev() {
            result = Term::Cons(Box::new(term), Box::new(result));
        }
        result
    }

    /// Create a list with a tail: [h1, h2 | tail]
    pub fn list_with_tail(heads: Vec<Term>, tail: Term) -> Self {
        let mut result = tail;
        for term in heads.into_iter().rev() {
            result = Term::Cons(Box::new(term), Box::new(result));
        }
        result
    }

    /// Returns true if this term is a variable.
    pub fn is_variable(&self) -> bool {
        matches!(self, Term::Variable(_) | Term::Anonymous)
    }

    /// Returns true if this term is ground (no variables).
    pub fn is_ground(&self) -> bool {
        match self {
            Term::Integer(_) | Term::Atom(_) | Term::Nil => true,
            Term::Variable(_) | Term::Anonymous => false,
            Term::Compound { args, .. } => args.iter().all(|a| a.is_ground()),
            Term::Cons(h, t) => h.is_ground() && t.is_ground(),
        }
    }

    /// Get the functor name and arity.
    pub fn functor_arity(&self) -> Option<(&str, usize)> {
        match self {
            Term::Atom(name) => Some((name.as_str(), 0)),
            Term::Compound { functor, args } => Some((functor.as_str(), args.len())),
            Term::Cons(_, _) => Some((".", 2)),
            Term::Nil => Some(("[]", 0)),
            _ => None,
        }
    }

    /// Collect all variable names in this term.
    pub fn variables(&self) -> Vec<String> {
        let mut vars = Vec::new();
        self.collect_variables(&mut vars);
        vars
    }

    fn collect_variables(&self, vars: &mut Vec<String>) {
        match self {
            Term::Variable(name) => {
                if !vars.contains(name) {
                    vars.push(name.clone());
                }
            }
            Term::Compound { args, .. } => {
                for arg in args {
                    arg.collect_variables(vars);
                }
            }
            Term::Cons(h, t) => {
                h.collect_variables(vars);
                t.collect_variables(vars);
            }
            _ => {}
        }
    }
}

/// Arithmetic expression.
#[derive(Debug, Clone, PartialEq)]
pub enum ArithExpr {
    /// Integer constant
    Integer(i32),
    /// Variable reference
    Variable(String),
    /// Binary operation
    BinOp(ArithOp, Box<ArithExpr>, Box<ArithExpr>),
    /// Unary negation
    Neg(Box<ArithExpr>),
}

impl ArithExpr {
    /// Collect all variable names in this expression.
    pub fn variables(&self) -> Vec<String> {
        let mut vars = Vec::new();
        self.collect_variables(&mut vars);
        vars
    }

    fn collect_variables(&self, vars: &mut Vec<String>) {
        match self {
            ArithExpr::Variable(name) => {
                if !vars.contains(name) {
                    vars.push(name.clone());
                }
            }
            ArithExpr::BinOp(_, left, right) => {
                left.collect_variables(vars);
                right.collect_variables(vars);
            }
            ArithExpr::Neg(inner) => inner.collect_variables(vars),
            ArithExpr::Integer(_) => {}
        }
    }
}

/// Arithmetic operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

/// Comparison operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompareOp {
    /// <
    Lt,
    /// >
    Gt,
    /// =<
    Le,
    /// >=
    Ge,
    /// =:= (arithmetic equality)
    ArithEq,
    /// =\= (arithmetic inequality)
    ArithNe,
}

/// Predicate identifier (name/arity).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PredicateKey {
    pub name: String,
    pub arity: usize,
}

impl PredicateKey {
    pub fn new(name: impl Into<String>, arity: usize) -> Self {
        Self {
            name: name.into(),
            arity,
        }
    }
}

impl std::fmt::Display for PredicateKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.name, self.arity)
    }
}
