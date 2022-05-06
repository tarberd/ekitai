use std::collections::HashMap;

use la_arena::{Arena, Idx};

use super::{term::{Body, Pattern, PatternId, Statement, Term, TermId}, name::Name};

pub type TermScopeId = Idx<TermScope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TermScope {
    pub parent: Option<TermScopeId>,
    pub entries: Vec<(Name, PatternId)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BodyContext {
    pub scopes: Arena<TermScope>,
    pub scope_map: HashMap<TermId, TermScopeId>,
}

impl BodyContext {
    pub fn new(body: &Body) -> Self {
        let mut scopes = Arena::default();

        let root = scopes.alloc(Self::build_root_scope(body));

        let ExpressionScopeFold {
            scopes, scope_map, ..
        } = ExpressionScopeFold {
            body,
            scopes,
            scope_map: HashMap::new(),
        }
        .fold_expression(body.root_expression, root);

        Self { scopes, scope_map }
    }

    pub fn expression_scope_ids(
        &self,
        expression_id: TermId,
    ) -> impl Iterator<Item = TermScopeId> + '_ {
        let scope_id = self.scope_map[&expression_id];
        std::iter::successors(Some(scope_id), move |&scope| self.scopes[scope].parent)
    }

    fn build_root_scope(body: &Body) -> TermScope {
        let entries = body
            .parameters
            .iter()
            .flat_map(|pattern_id| {
                let pattern = &body.patterns[*pattern_id];
                match pattern {
                    Pattern::Deconstructor(_, _) => todo!(),
                    Pattern::Bind(bind) => Some((bind.clone(), *pattern_id)),
                }
            })
            .collect();

        TermScope {
            parent: None,
            entries,
        }
    }
}

struct ExpressionScopeFold<'body> {
    pub body: &'body Body,
    pub scopes: Arena<TermScope>,
    pub scope_map: HashMap<TermId, TermScopeId>,
}

impl<'body> ExpressionScopeFold<'body> {
    fn fold_expression(mut self, expr_id: TermId, scope_id: TermScopeId) -> Self {
        self.scope_map.insert(expr_id, scope_id);
        match &self.body.expressions[expr_id] {
            Term::Block {
                statements,
                trailing_expression,
            } => {
                let (fold, scope_id) =
                    statements
                        .iter()
                        .fold(
                            (self, scope_id),
                            |(fold, scope_id), statement| match statement {
                                Statement::Let(pattern_id, expr_id) => {
                                    let mut fold = fold.fold_expression(*expr_id, scope_id);
                                    let entries = fold.fold_pattern_bindings(*pattern_id);
                                    let scope_id = fold.scopes.alloc(TermScope {
                                        parent: Some(scope_id),
                                        entries,
                                    });
                                    (fold, scope_id)
                                }
                                Statement::Expression(expr_id) => {
                                    (fold.fold_expression(*expr_id, scope_id), scope_id)
                                }
                            },
                        );
                fold.fold_expression(*trailing_expression, scope_id)
            }
            Term::If {
                condition,
                then_branch,
                else_branch,
            } => self
                .fold_expression(*condition, scope_id)
                .fold_expression(*then_branch, scope_id)
                .fold_expression(*else_branch, scope_id),
            Term::Match { matchee, case_list } => case_list.iter().fold(
                self.fold_expression(*matchee, scope_id),
                |mut fold, (pattern_id, expr_id)| {
                    let entries = fold.fold_pattern_bindings(*pattern_id);
                    let scope_id = fold.scopes.alloc(TermScope {
                        parent: Some(scope_id),
                        entries,
                    });
                    fold.fold_expression(*expr_id, scope_id)
                },
            ),
            Term::Call { callee, arguments } => arguments
                .iter()
                .fold(self.fold_expression(*callee, scope_id), |fold, argument| {
                    fold.fold_expression(*argument, scope_id)
                }),
            Term::Binary(_, lhs, rhs) => self
                .fold_expression(*lhs, scope_id)
                .fold_expression(*rhs, scope_id),
            Term::Unary(_, expr) => self.fold_expression(*expr, scope_id),
            Term::Path(_) => self,
            Term::Literal(_) => self,
            Term::New(inner) => self.fold_expression(*inner, scope_id),
        }
    }

    fn fold_pattern_bindings(&self, pattern_id: PatternId) -> Vec<(Name, PatternId)> {
        self.fold_pattern_bindings_0(pattern_id, Vec::new())
    }

    fn fold_pattern_bindings_0(
        &self,
        pattern_id: PatternId,
        mut entries: Vec<(Name, PatternId)>,
    ) -> Vec<(Name, PatternId)> {
        match &self.body.patterns[pattern_id] {
            Pattern::Deconstructor(_, subpatterns) => {
                subpatterns.iter().fold(entries, |entries, pattern| {
                    self.fold_pattern_bindings_0(*pattern, entries)
                })
            }
            Pattern::Bind(name) => {
                entries.push((name.clone(), pattern_id));
                entries
            }
        }
    }
}
