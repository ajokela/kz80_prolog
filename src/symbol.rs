//! Symbol table for atom interning and predicate registry.
//!
//! Manages atom strings and maps them to compact indices for efficient
//! runtime representation. Also tracks predicates and their clauses.

use std::collections::HashMap;

use crate::ast::{Clause, PredicateKey, Program, Term};

/// Atom table for string interning.
///
/// Maps atom strings to small integer indices for compact representation.
#[derive(Debug, Default)]
pub struct AtomTable {
    /// Map from atom string to index
    atoms: HashMap<String, u16>,
    /// Reverse map from index to string
    strings: Vec<String>,
}

impl AtomTable {
    /// Create a new empty atom table.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create an atom table with built-in atoms pre-registered.
    pub fn with_builtins() -> Self {
        let mut table = Self::new();
        // Register common atoms
        table.intern("[]");  // Empty list
        table.intern(".");   // List cons
        table.intern("true");
        table.intern("false");
        table.intern("fail");
        table.intern("nl");
        table.intern("write");
        table.intern("is");
        table.intern("cut");
        table
    }

    /// Intern an atom string, returning its index.
    ///
    /// If the atom already exists, returns its existing index.
    /// Otherwise, assigns a new index.
    pub fn intern(&mut self, name: &str) -> u16 {
        if let Some(&idx) = self.atoms.get(name) {
            idx
        } else {
            let idx = self.strings.len() as u16;
            self.atoms.insert(name.to_string(), idx);
            self.strings.push(name.to_string());
            idx
        }
    }

    /// Look up an atom by name, returning its index if it exists.
    pub fn lookup(&self, name: &str) -> Option<u16> {
        self.atoms.get(name).copied()
    }

    /// Get the string for an atom index.
    pub fn get_string(&self, idx: u16) -> Option<&str> {
        self.strings.get(idx as usize).map(|s| s.as_str())
    }

    /// Get the number of interned atoms.
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    /// Check if the table is empty.
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }

    /// Iterate over all atoms with their indices.
    pub fn iter(&self) -> impl Iterator<Item = (u16, &str)> {
        self.strings
            .iter()
            .enumerate()
            .map(|(i, s)| (i as u16, s.as_str()))
    }
}

/// Predicate registry tracking all predicates and their clauses.
#[derive(Debug, Default)]
pub struct PredicateRegistry {
    /// Map from predicate key to list of clause indices
    predicates: HashMap<PredicateKey, Vec<usize>>,
    /// All clauses in order
    clauses: Vec<Clause>,
}

impl PredicateRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Build a registry from a program.
    pub fn from_program(program: &Program) -> Self {
        let mut registry = Self::new();
        for clause in &program.clauses {
            registry.add_clause(clause.clone());
        }
        registry
    }

    /// Add a clause to the registry.
    pub fn add_clause(&mut self, clause: Clause) {
        let key = PredicateKey::new(
            clause.predicate_key().0,
            clause.predicate_key().1,
        );
        let idx = self.clauses.len();
        self.clauses.push(clause);
        self.predicates.entry(key).or_default().push(idx);
    }

    /// Get all clause indices for a predicate.
    pub fn get_clauses(&self, key: &PredicateKey) -> Option<&[usize]> {
        self.predicates.get(key).map(|v| v.as_slice())
    }

    /// Get a clause by index.
    pub fn get_clause(&self, idx: usize) -> Option<&Clause> {
        self.clauses.get(idx)
    }

    /// Get all predicates.
    pub fn predicates(&self) -> impl Iterator<Item = &PredicateKey> {
        self.predicates.keys()
    }

    /// Get total number of clauses.
    pub fn clause_count(&self) -> usize {
        self.clauses.len()
    }

    /// Get total number of predicates.
    pub fn predicate_count(&self) -> usize {
        self.predicates.len()
    }
}

/// Symbol table combining atoms and predicates.
#[derive(Debug)]
pub struct SymbolTable {
    pub atoms: AtomTable,
    pub predicates: PredicateRegistry,
}

impl SymbolTable {
    /// Create a new symbol table from a program.
    pub fn from_program(program: &Program) -> Self {
        let mut atoms = AtomTable::with_builtins();
        let predicates = PredicateRegistry::from_program(program);

        // Intern all atoms from the program
        for clause in &program.clauses {
            Self::intern_term_atoms(&mut atoms, &clause.head);
            for goal in &clause.body {
                Self::intern_goal_atoms(&mut atoms, goal);
            }
        }

        if let Some(query) = &program.query {
            for goal in query {
                Self::intern_goal_atoms(&mut atoms, goal);
            }
        }

        Self { atoms, predicates }
    }

    fn intern_term_atoms(atoms: &mut AtomTable, term: &Term) {
        match term {
            Term::Atom(name) => {
                atoms.intern(name);
            }
            Term::Compound { functor, args } => {
                atoms.intern(functor);
                for arg in args {
                    Self::intern_term_atoms(atoms, arg);
                }
            }
            Term::Cons(head, tail) => {
                Self::intern_term_atoms(atoms, head);
                Self::intern_term_atoms(atoms, tail);
            }
            _ => {}
        }
    }

    fn intern_goal_atoms(atoms: &mut AtomTable, goal: &crate::ast::Goal) {
        use crate::ast::Goal;
        match goal {
            Goal::Call(term) => Self::intern_term_atoms(atoms, term),
            Goal::Write(term) => Self::intern_term_atoms(atoms, term),
            Goal::Unify(t1, t2) | Goal::NotUnify(t1, t2) => {
                Self::intern_term_atoms(atoms, t1);
                Self::intern_term_atoms(atoms, t2);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_atom_interning() {
        let mut table = AtomTable::new();
        let idx1 = table.intern("foo");
        let idx2 = table.intern("bar");
        let idx3 = table.intern("foo"); // Same as idx1

        assert_eq!(idx1, idx3);
        assert_ne!(idx1, idx2);
        assert_eq!(table.get_string(idx1), Some("foo"));
        assert_eq!(table.get_string(idx2), Some("bar"));
    }

    #[test]
    fn test_predicate_registry() {
        let mut registry = PredicateRegistry::new();

        let clause1 = Clause::fact(Term::compound("parent", vec![
            Term::atom("tom"),
            Term::atom("bob"),
        ]));
        let clause2 = Clause::fact(Term::compound("parent", vec![
            Term::atom("bob"),
            Term::atom("pat"),
        ]));

        registry.add_clause(clause1);
        registry.add_clause(clause2);

        let key = PredicateKey::new("parent", 2);
        let clauses = registry.get_clauses(&key).unwrap();
        assert_eq!(clauses.len(), 2);
    }

    #[test]
    fn test_symbol_table_from_program() {
        let program = crate::parse("parent(tom, bob). parent(bob, pat).").unwrap();
        let symbols = SymbolTable::from_program(&program);

        assert!(symbols.atoms.lookup("parent").is_some());
        assert!(symbols.atoms.lookup("tom").is_some());
        assert!(symbols.atoms.lookup("bob").is_some());
        assert!(symbols.atoms.lookup("pat").is_some());

        assert_eq!(symbols.predicates.clause_count(), 2);
        assert_eq!(symbols.predicates.predicate_count(), 1);
    }
}
