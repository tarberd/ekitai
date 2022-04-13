use crate::ast::{AstNode, AstToken, Identifier, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct NameReference(pub(crate) SyntaxNode);

impl_trait_ast_node!(NameReference);

impl NameReference {
    pub fn identifier(&self) -> Identifier {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(Identifier::cast)
            .unwrap()
    }
}
