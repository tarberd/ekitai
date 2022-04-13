use crate::ast::{AstNode, AstToken, SyntaxNode, TokenLiteral};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct Literal(pub(super) SyntaxNode);

impl_trait_ast_node!(Literal);

impl Literal {
    pub fn literal_kind(&self) -> TokenLiteral {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(TokenLiteral::cast)
            .unwrap()
    }
}
