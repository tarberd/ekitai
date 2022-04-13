use crate::ast::{AstNode, CallExpression, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct NewExpression(pub(super) SyntaxNode);

impl_trait_ast_node!(NewExpression);

impl NewExpression {
    pub fn call_expression(&self) -> Option<CallExpression> {
        self.syntax().children().find_map(CallExpression::cast)
    }
}
