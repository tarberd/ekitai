use super::super::{raw::SyntaxToken, CstToken, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct Integer(pub(super) SyntaxToken);

impl CstToken for Integer {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl Integer {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Integer
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }

    pub fn radical_and_sufix(&self) -> (&str, Option<&str>) {
        if let Some((index, _)) = self.text().char_indices().find(|(_, c)| c.is_alphabetic()) {
            (&self.text()[0..index], Some(&self.text()[index..]))
        } else {
            (self.text(), None)
        }
    }
}

impl std::fmt::Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for Integer {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
