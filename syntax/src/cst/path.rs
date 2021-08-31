use super::{raw::SyntaxNode, CstNode, NameReference, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct Path(SyntaxNode);

impl CstNode for Path {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl Path {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Path
    }

    pub fn path_segment(&self) -> Option<PathSegment> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| PathSegment::try_from(n).ok())
    }

    pub fn path(&self) -> Option<Path> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Path::try_from(n).ok())
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for Path {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct PathSegment(SyntaxNode);

impl CstNode for PathSegment {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl PathSegment {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::PathSegment
    }

    pub fn name_reference(&self) -> Option<NameReference> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| NameReference::try_from(n).ok())
    }
}

impl std::fmt::Display for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for PathSegment {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
