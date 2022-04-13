use crate::ast::{AstNode, Expression, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct IfExpression(pub(super) SyntaxNode);

impl_trait_ast_node!(IfExpression);

impl IfExpression {
    pub fn condition(&self) -> Option<Expression> {
        self.syntax().children().find_map(Expression::cast)
    }

    pub fn then_branch(&self) -> Option<Expression> {
        self.syntax().children().filter_map(Expression::cast).nth(1)
    }

    pub fn else_branch(&self) -> Option<Expression> {
        self.syntax().children().filter_map(Expression::cast).nth(2)
    }
}
