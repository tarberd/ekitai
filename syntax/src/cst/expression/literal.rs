use crate::cst::{raw::SyntaxNode, CstNode, SyntaxToAstError, TokenLiteral};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct Literal(pub(super) SyntaxNode);

impl Literal {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Literal
    }

    pub fn literal_kind(&self) -> TokenLiteral {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|tok| TokenLiteral::try_from(tok).ok())
            .unwrap()
    }
}

impl CstNode for Literal {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for Literal {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
