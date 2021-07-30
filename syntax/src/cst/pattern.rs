use super::{raw::SyntaxNode, CstNode, Name, Path, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub enum Pattern {
    PathPattern(PathPattern),
    IdentifierPattern(IdentifierPattern),
}

impl CstNode for Pattern {
    fn as_syntax_node(&self) -> &SyntaxNode {
        match self {
            Pattern::PathPattern(f) => f.as_syntax_node(),
            Pattern::IdentifierPattern(f) => f.as_syntax_node(),
        }
    }
}

impl Pattern {
    fn try_from_set() -> &'static [SyntaxKind] {
        static KINDS: &[SyntaxKind] = &[SyntaxKind::PathPattern, SyntaxKind::IdentifierPattern];
        KINDS
    }

    fn from_raw_unchecked(raw: SyntaxNode) -> Self {
        match raw.kind() {
            SyntaxKind::PathPattern => Self::PathPattern(PathPattern(raw)),
            SyntaxKind::IdentifierPattern => Self::IdentifierPattern(IdentifierPattern(raw)),
            _ => panic!(),
        }
    }
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for Pattern {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::try_from_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::try_from_set()[0], other)),
        }
    }
}

#[derive(Debug)]
pub struct IdentifierPattern(SyntaxNode);

impl CstNode for IdentifierPattern {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl IdentifierPattern {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::IdentifierPattern
    }

    pub fn name(&self) -> Option<Name> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Name::try_from(n).ok())
    }
}

impl std::fmt::Display for IdentifierPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for IdentifierPattern {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct PathPattern(SyntaxNode);

impl CstNode for PathPattern {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl PathPattern {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::PathPattern
    }

    pub fn path(&self) -> Option<Path> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Path::try_from(n).ok())
    }
}

impl std::fmt::Display for PathPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for PathPattern {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
