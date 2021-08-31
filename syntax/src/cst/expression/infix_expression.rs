use crate::cst::{raw::SyntaxNode, BinaryOperator, CstNode, Expression, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct InfixExpression(pub(super) SyntaxNode);

impl InfixExpression {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::InfixExpression
    }

    pub fn lhs(&self) -> Option<Expression> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Expression::try_from(n).ok())
    }

    pub fn rhs(&self) -> Option<Expression> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .filter_map(|n| Expression::try_from(n).ok())
            .nth(1)
    }

    pub fn operator(&self) -> Option<BinaryOperator> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|tok| BinaryOperator::try_from(tok).ok())
    }
}

impl CstNode for InfixExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl std::fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for InfixExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
