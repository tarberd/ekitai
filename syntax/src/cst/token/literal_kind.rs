use super::Integer;
use crate::cst::{raw::SyntaxToken, CstToken, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub enum LiteralKind {
    Integer(Integer),
}

impl LiteralKind {
    fn try_from_set() -> &'static [SyntaxKind] {
        static KINDS: &[SyntaxKind] = &[SyntaxKind::Integer];
        KINDS
    }

    fn from_raw_unchecked(raw: SyntaxToken) -> Self {
        match raw.kind() {
            SyntaxKind::Integer => Self::Integer(Integer(raw)),
            _ => panic!(),
        }
    }
}

impl CstToken for LiteralKind {
    fn as_syntax_token(&self) -> &SyntaxToken {
        match self {
            LiteralKind::Integer(lit) => lit.as_syntax_token(),
        }
    }
}

impl std::fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for LiteralKind {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::try_from_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::try_from_set()[0], other)),
        }
    }
}
