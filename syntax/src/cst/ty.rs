use super::{raw::SyntaxNode, CstNode, Path, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub enum Type {
    PathType(PathType),
}

impl CstNode for Type {
    fn as_syntax_node(&self) -> &SyntaxNode {
        match self {
            Type::PathType(f) => f.as_syntax_node(),
        }
    }
}

impl Type {
    const fn syntax_kind_set() -> [SyntaxKind; 1] {
        [PathType::syntax_kind()]
    }

    fn from_raw_unchecked(raw: SyntaxNode) -> Self {
        match raw.kind() {
            SyntaxKind::PathType => Self::PathType(PathType(raw)),
            _ => panic!(),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for Type {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::syntax_kind_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind_set()[0], other)),
        }
    }
}

#[derive(Debug)]
pub struct PathType(SyntaxNode);

impl CstNode for PathType {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl PathType {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::PathType
    }

    pub fn path(&self) -> Option<Path> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Path::try_from(n).ok())
    }
}

impl std::fmt::Display for PathType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for PathType {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
