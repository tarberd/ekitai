use super::super::{raw::SyntaxToken, CstToken, SyntaxToAstError};
use parser::SyntaxKind;
use std::{convert::TryFrom, fmt::Display};

#[derive(Debug)]
pub struct Identifier(SyntaxToken);

impl CstToken for Identifier {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl Identifier {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Identifier
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_syntax_token(), f)
    }
}

impl TryFrom<SyntaxToken> for Identifier {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
