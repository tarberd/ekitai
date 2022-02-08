use super::CallExpression;
use crate::cst::{raw::SyntaxNode, CstNode, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct NewExpression(pub(super) SyntaxNode);

impl CstNode for NewExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl NewExpression {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::NewExpression
    }

    pub fn call_expression(&self) -> Option<CallExpression> {
        self.as_syntax_node()
            .children()
            .find_map(|s| CallExpression::try_from(s).ok())
    }
}

impl std::fmt::Display for NewExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for NewExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
