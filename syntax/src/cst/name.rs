use super::{raw::SyntaxNode, token::Identifier, CstNode, SyntaxToAstError};
use parser::SyntaxKind;
use std::{convert::TryFrom, fmt::Display};

#[derive(Debug)]
pub struct Name(SyntaxNode);

impl CstNode for Name {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl Name {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Name
    }

    pub fn identifier(&self) -> Option<Identifier> {
        self.as_syntax_node()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|it| Identifier::try_from(it).ok())
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for Name {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
