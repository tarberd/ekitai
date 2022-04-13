use crate::ast::{AstNode, Expression, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct ParenthesisExpression(pub(super) SyntaxNode);

impl_trait_ast_node!(ParenthesisExpression);

impl ParenthesisExpression {
    pub fn inner_expression(&self) -> Option<Expression> {
        self.syntax().children().find_map(Expression::cast)
    }
}
