use crate::cst::{raw::SyntaxNode, CstNode, Expression, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct BlockExpression(pub(super) SyntaxNode);

impl CstNode for BlockExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl BlockExpression {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::BlockExpression
    }

    pub fn tail_expression(&self) -> Option<Expression> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|s| Expression::try_from(s).ok())
    }
}

impl std::fmt::Display for BlockExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for BlockExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
