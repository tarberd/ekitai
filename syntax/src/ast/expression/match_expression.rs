use crate::ast::{AstNode, Expression, Pattern, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct MatchExpression(pub(super) SyntaxNode);

impl_trait_ast_node!(MatchExpression);

impl MatchExpression {
    pub fn matchee(&self) -> Option<Expression> {
        self.syntax().children().find_map(Expression::cast)
    }

    pub fn case_list(&self) -> Option<MatchCaseList> {
        self.syntax().children().find_map(MatchCaseList::cast)
    }
}

#[derive(Debug)]
pub struct MatchCaseList(SyntaxNode);

impl_trait_ast_node!(MatchCaseList);

impl MatchCaseList {
    pub fn cases(&self) -> impl Iterator<Item = MatchCase> {
        let mut children = self.syntax().children();
        std::iter::from_fn(move || children.by_ref().find_map(MatchCase::cast))
    }
}

#[derive(Debug)]
pub struct MatchCase(SyntaxNode);

impl_trait_ast_node!(MatchCase);

impl MatchCase {
    pub fn pattern(&self) -> Option<Pattern> {
        self.syntax().children().find_map(Pattern::cast)
    }

    pub fn expression(&self) -> Option<Expression> {
        self.syntax().children().find_map(Expression::cast)
    }
}
