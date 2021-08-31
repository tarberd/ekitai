use crate::cst::{raw::SyntaxNode, CstNode, Path, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct PathExpression(pub(super) SyntaxNode);

impl CstNode for PathExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl PathExpression {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::PathExpression
    }

    pub fn path(&self) -> Option<Path> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|s| Path::try_from(s).ok())
    }
}

impl std::fmt::Display for PathExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for PathExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
