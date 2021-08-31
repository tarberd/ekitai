use super::super::{raw::SyntaxToken, CstToken, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct True(pub(super) SyntaxToken);

impl CstToken for True {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl True {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::TrueKw
    }
}

impl std::fmt::Display for True {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for True {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct False(pub(super) SyntaxToken);

impl CstToken for False {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl False {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::FalseKw
    }
}

impl std::fmt::Display for False {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for False {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub enum Boolean {
    True(True),
    False(False),
}

impl Boolean {
    pub(crate) const fn syntax_kind_set() -> [SyntaxKind; 2] {
        [True::syntax_kind(), False::syntax_kind()]
    }

    pub(crate) fn from_raw_unchecked(raw: SyntaxToken) -> Self {
        match raw.kind() {
            kind if kind == True::syntax_kind() => Self::True(True(raw)),
            kind if kind == False::syntax_kind() => Self::False(False(raw)),
            _ => panic!(),
        }
    }
}

impl CstToken for Boolean {
    fn as_syntax_token(&self) -> &SyntaxToken {
        match self {
            Boolean::True(lit) => lit.as_syntax_token(),
            Boolean::False(lit) => lit.as_syntax_token(),
        }
    }
}

impl std::fmt::Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for Boolean {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::syntax_kind_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind_set()[0], other)),
        }
    }
}
