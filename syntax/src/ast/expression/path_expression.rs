use crate::ast::{AstNode, Path, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct PathExpression(pub(super) SyntaxNode);

impl_trait_ast_node!(PathExpression);

impl PathExpression {
    pub fn path(&self) -> Option<Path> {
        self.syntax().children().find_map(Path::cast)
    }
}
