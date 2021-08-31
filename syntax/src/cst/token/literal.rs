use super::{Boolean, Integer};
use crate::cst::{raw::SyntaxToken, CstToken, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub enum TokenLiteral {
    Integer(Integer),
    Boolean(Boolean),
}

impl TokenLiteral {
    const fn syntax_kind_set() -> [SyntaxKind; 3] {
        [
            Integer::syntax_kind(),
            Boolean::syntax_kind_set()[0],
            Boolean::syntax_kind_set()[1],
        ]
    }

    fn from_raw_unchecked(raw: SyntaxToken) -> Self {
        match raw.kind() {
            kind if kind == Integer::syntax_kind() => Self::Integer(Integer(raw)),
            kind if Boolean::syntax_kind_set().contains(&kind) => {
                Self::Boolean(Boolean::from_raw_unchecked(raw))
            }
            _ => panic!(),
        }
    }
}

impl CstToken for TokenLiteral {
    fn as_syntax_token(&self) -> &SyntaxToken {
        match self {
            TokenLiteral::Integer(lit) => lit.as_syntax_token(),
            TokenLiteral::Boolean(_) => todo!(),
        }
    }
}

impl std::fmt::Display for TokenLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for TokenLiteral {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::syntax_kind_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind_set()[0], other)),
        }
    }
}
