use super::{
    super::{raw::SyntaxNode, CstNode, SyntaxToAstError},
    Expression,
};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct IfExpression(pub(super) SyntaxNode);

impl CstNode for IfExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl IfExpression {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::IfExpression
    }

    pub fn contidion(&self) -> Option<Expression> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|s| Expression::try_from(s).ok())
    }

    pub fn than_branch(&self) -> Option<Expression> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .filter_map(|s| Expression::try_from(s).ok())
            .nth(1)
    }

    pub fn else_branch(&self) -> Option<Expression> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .filter_map(|s| Expression::try_from(s).ok())
            .nth(2)
    }
}

impl std::fmt::Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for IfExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

