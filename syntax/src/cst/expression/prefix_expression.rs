use crate::cst::{raw::SyntaxNode, CstNode, Expression, SyntaxToAstError, UnaryOperator};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct PrefixExpression(pub(super) SyntaxNode);

impl PrefixExpression {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::PrefixExpression
    }

    pub fn inner(&self) -> Option<Expression> {
        self.as_syntax_node()
            .children()
            .find_map(|n| Expression::try_from(n).ok())
    }

    pub fn operator(&self) -> Option<UnaryOperator> {
        self.as_syntax_node()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|tok| UnaryOperator::try_from(tok).ok())
    }
}

impl CstNode for PrefixExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl std::fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for PrefixExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
