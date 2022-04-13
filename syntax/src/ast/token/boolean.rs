use crate::ast::{AstToken, SyntaxToken};
use crate::impl_trait_ast_token;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct True(pub(super) SyntaxToken);
impl_trait_ast_token!(True, TrueKw);

#[derive(Debug)]
pub struct False(pub(super) SyntaxToken);
impl_trait_ast_token!(False, FalseKw);

#[derive(Debug)]
pub enum Boolean {
    True(True),
    False(False),
}

impl AstToken for Boolean {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        match kind {
            SyntaxKind::TrueKw | SyntaxKind::FalseKw => true,
            _ => false,
        }
    }

    fn cast(node: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        Some(match node.kind() {
            SyntaxKind::TrueKw => Boolean::True(True(node)),
            SyntaxKind::FalseKw => Boolean::False(False(node)),
            _ => return None,
        })
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            Boolean::True(lit) => lit.syntax(),
            Boolean::False(lit) => lit.syntax(),
        }
    }
}

impl std::fmt::Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
