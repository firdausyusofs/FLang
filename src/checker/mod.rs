#![allow(dead_code)]

use crate::parser::ast::Program;

type ScopeId = usize;

struct Scope {
    parent_id: Option<ScopeId>,
}

struct Checker {
    scopes: Vec<Scope>,
}

impl Checker {
    fn new() -> Self {
        Self {
            scopes: vec![Scope { parent_id: None }],
        }
    }

    fn create_scope(&mut self, parent_id: Option<ScopeId>) -> ScopeId {
        self.scopes.push(Scope { parent_id });

        self.scopes.len() - 1
    }

    fn check_program(&mut self, program: &Program) -> CheckedProgram {
        CheckedProgram { stmts: vec![] }
    }
}

struct CheckedProgram {
    stmts: Vec<CheckedExpressionStatement>,
}

struct CheckedExpressionStatement {
    expr: CheckedExpression,
}

struct CheckedExpression {}

#[cfg(test)]
mod tests {}
