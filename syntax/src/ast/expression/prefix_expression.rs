use crate::ast::{AstNode, AstToken, Expression, SyntaxNode, UnaryOperator};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct PrefixExpression(pub(super) SyntaxNode);

impl_trait_ast_node!(PrefixExpression);

impl PrefixExpression {
    pub fn inner(&self) -> Option<Expression> {
        self.syntax().children().find_map(Expression::cast)
    }

    pub fn operator(&self) -> Option<UnaryOperator> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(UnaryOperator::cast)
    }
}
