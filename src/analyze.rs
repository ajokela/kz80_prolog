//! Variable analysis for Prolog clauses.
//!
//! Classifies variables in clauses for efficient code generation:
//! - First occurrence (needs allocation)
//! - Subsequent occurrence (needs unification)
//! - Void (anonymous, can be ignored)
//! - Permanent (used across goals, stored in environment)
//! - Temporary (used within single goal, can use registers)

use std::collections::{HashMap, HashSet};

use crate::ast::{ArithExpr, Clause, Goal, Term};

/// Classification of a variable occurrence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarOccurrence {
    /// First occurrence - allocate new variable
    First,
    /// Subsequent occurrence - unify with existing
    Subsequent,
    /// Void variable (anonymous or unused)
    Void,
}

/// Variable classification: permanent or temporary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarClass {
    /// Permanent variable - lives in environment frame
    Permanent(u8),
    /// Temporary variable - lives in argument register
    Temporary(u8),
}

/// Analysis result for a clause.
#[derive(Debug)]
pub struct ClauseAnalysis {
    /// Variable classifications
    pub var_classes: HashMap<String, VarClass>,
    /// Number of permanent variables
    pub permanent_count: u8,
    /// Number of temporary variables needed
    pub temporary_count: u8,
    /// Variables in head
    pub head_vars: Vec<String>,
    /// Variables used in each goal
    pub goal_vars: Vec<HashSet<String>>,
}

impl ClauseAnalysis {
    /// Get the classification for a variable.
    pub fn get_class(&self, name: &str) -> Option<VarClass> {
        self.var_classes.get(name).copied()
    }

    /// Check if this is the first occurrence of a variable in a scope.
    pub fn is_first_occurrence(&self, name: &str, seen: &HashSet<String>) -> bool {
        !seen.contains(name)
    }
}

/// Analyze a clause for variable classification.
pub fn analyze_clause(clause: &Clause) -> ClauseAnalysis {
    let mut all_vars: HashMap<String, Vec<usize>> = HashMap::new();
    let mut head_vars = Vec::new();
    let mut goal_vars = Vec::new();

    // Collect variables from head (goal index 0)
    collect_term_vars(&clause.head, &mut head_vars);
    for var in &head_vars {
        all_vars.entry(var.clone()).or_default().push(0);
    }

    // Collect variables from each goal
    for (i, goal) in clause.body.iter().enumerate() {
        let mut vars = HashSet::new();
        collect_goal_vars(goal, &mut vars);
        for var in &vars {
            all_vars.entry(var.clone()).or_default().push(i + 1);
        }
        goal_vars.push(vars);
    }

    // Classify variables:
    // - Permanent if used in more than one goal (including head)
    // - Temporary otherwise
    let mut var_classes = HashMap::new();
    let mut permanent_count = 0u8;
    let mut temporary_count = 0u8;

    for (name, occurrences) in &all_vars {
        let unique_goals: HashSet<_> = occurrences.iter().collect();

        if unique_goals.len() > 1 {
            // Permanent - used across multiple goals
            var_classes.insert(name.clone(), VarClass::Permanent(permanent_count));
            permanent_count += 1;
        } else {
            // Temporary - used in single goal only
            var_classes.insert(name.clone(), VarClass::Temporary(temporary_count));
            temporary_count += 1;
        }
    }

    ClauseAnalysis {
        var_classes,
        permanent_count,
        temporary_count,
        head_vars,
        goal_vars,
    }
}

/// Collect variable names from a term.
fn collect_term_vars(term: &Term, vars: &mut Vec<String>) {
    match term {
        Term::Variable(name) => {
            if !vars.contains(name) {
                vars.push(name.clone());
            }
        }
        Term::Compound { args, .. } => {
            for arg in args {
                collect_term_vars(arg, vars);
            }
        }
        Term::Cons(head, tail) => {
            collect_term_vars(head, vars);
            collect_term_vars(tail, vars);
        }
        _ => {}
    }
}

/// Collect variable names from a goal.
fn collect_goal_vars(goal: &Goal, vars: &mut HashSet<String>) {
    match goal {
        Goal::Call(term) => {
            let mut v = Vec::new();
            collect_term_vars(term, &mut v);
            vars.extend(v);
        }
        Goal::Is(name, expr) => {
            vars.insert(name.clone());
            collect_arith_vars(expr, vars);
        }
        Goal::Compare(_, left, right) => {
            collect_arith_vars(left, vars);
            collect_arith_vars(right, vars);
        }
        Goal::Unify(t1, t2) | Goal::NotUnify(t1, t2) => {
            let mut v = Vec::new();
            collect_term_vars(t1, &mut v);
            collect_term_vars(t2, &mut v);
            vars.extend(v);
        }
        Goal::Write(term) => {
            let mut v = Vec::new();
            collect_term_vars(term, &mut v);
            vars.extend(v);
        }
        Goal::Cut | Goal::Nl | Goal::Fail | Goal::True => {}
    }
}

/// Collect variable names from an arithmetic expression.
fn collect_arith_vars(expr: &ArithExpr, vars: &mut HashSet<String>) {
    match expr {
        ArithExpr::Variable(name) => {
            vars.insert(name.clone());
        }
        ArithExpr::BinOp(_, left, right) => {
            collect_arith_vars(left, vars);
            collect_arith_vars(right, vars);
        }
        ArithExpr::Neg(inner) => {
            collect_arith_vars(inner, vars);
        }
        ArithExpr::Integer(_) => {}
    }
}

/// Determine if a variable is void (anonymous or never actually used).
pub fn is_void_var(name: &str) -> bool {
    name == "_" || name.starts_with('_')
}

/// Compute the set of variables that need to be saved across a goal.
pub fn vars_to_save(
    analysis: &ClauseAnalysis,
    goal_index: usize,
    seen: &HashSet<String>,
) -> HashSet<String> {
    // Variables that are permanent and have been seen (bound)
    // and will be used in a later goal
    let mut to_save = HashSet::new();

    for (name, class) in &analysis.var_classes {
        if let VarClass::Permanent(_) = class {
            if seen.contains(name) {
                // Check if used in any later goal
                for (i, goal_vars) in analysis.goal_vars.iter().enumerate() {
                    if i > goal_index && goal_vars.contains(name) {
                        to_save.insert(name.clone());
                        break;
                    }
                }
            }
        }
    }

    to_save
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    #[test]
    fn test_simple_fact_analysis() {
        let program = parse("parent(tom, bob).").unwrap();
        let analysis = analyze_clause(&program.clauses[0]);

        // No variables in a ground fact
        assert_eq!(analysis.permanent_count, 0);
        assert_eq!(analysis.temporary_count, 0);
    }

    #[test]
    fn test_rule_with_shared_vars() {
        let program = parse("grandparent(X, Z) :- parent(X, Y), parent(Y, Z).").unwrap();
        let analysis = analyze_clause(&program.clauses[0]);

        // X, Y, Z are all used in multiple goals -> permanent
        assert_eq!(analysis.permanent_count, 3);
        assert!(matches!(analysis.get_class("X"), Some(VarClass::Permanent(_))));
        assert!(matches!(analysis.get_class("Y"), Some(VarClass::Permanent(_))));
        assert!(matches!(analysis.get_class("Z"), Some(VarClass::Permanent(_))));
    }

    #[test]
    fn test_temporary_var() {
        let program = parse("foo(X) :- bar(X, Y), baz(Y).").unwrap();
        let analysis = analyze_clause(&program.clauses[0]);

        // X is used in head and first goal -> permanent
        // Y is used in both body goals -> permanent
        assert!(matches!(analysis.get_class("X"), Some(VarClass::Permanent(_))));
        assert!(matches!(analysis.get_class("Y"), Some(VarClass::Permanent(_))));
    }

    #[test]
    fn test_arithmetic_vars() {
        let program = parse("double(X, Y) :- Y is X * 2.").unwrap();
        let analysis = analyze_clause(&program.clauses[0]);

        // X is in head and body -> permanent
        // Y is in head and body -> permanent
        assert!(matches!(analysis.get_class("X"), Some(VarClass::Permanent(_))));
        assert!(matches!(analysis.get_class("Y"), Some(VarClass::Permanent(_))));
    }

    #[test]
    fn test_void_detection() {
        assert!(is_void_var("_"));
        assert!(is_void_var("_X"));
        assert!(is_void_var("_Unused"));
        assert!(!is_void_var("X"));
        assert!(!is_void_var("Var"));
    }
}
