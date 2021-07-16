use crate::cst::{raw::SyntaxNode, CstNode, Expression, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct ParenthesisExpression(pub(super) SyntaxNode);

impl CstNode for ParenthesisExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ParenthesisExpression {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::ParenthesisExpression
    }

    pub fn inner_expression(&self) -> Option<Expression> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|s| Expression::try_from(s).ok())
    }
}

impl std::fmt::Display for ParenthesisExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for ParenthesisExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
