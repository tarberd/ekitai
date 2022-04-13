use crate::ast::{AstNode, AstToken, BinaryOperator, Expression, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct InfixExpression(pub(super) SyntaxNode);

impl_trait_ast_node!(InfixExpression);

impl InfixExpression {
    pub fn lhs(&self) -> Option<Expression> {
        self.syntax().children().find_map(Expression::cast)
    }

    pub fn rhs(&self) -> Option<Expression> {
        self.syntax().children().filter_map(Expression::cast).nth(1)
    }

    pub fn operator(&self) -> Option<BinaryOperator> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(BinaryOperator::cast)
    }
}
