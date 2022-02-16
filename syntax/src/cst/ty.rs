use super::{raw::SyntaxNode, CstNode, Expression, Path, Pattern, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub enum Type {
    PathType(PathType),
    PointerType(PointerType),
    RefinementType(RefinementType),
}

impl CstNode for Type {
    fn as_syntax_node(&self) -> &SyntaxNode {
        match self {
            Type::PathType(f) => f.as_syntax_node(),
            Type::PointerType(p) => p.as_syntax_node(),
            Type::RefinementType(r) => r.as_syntax_node(),
        }
    }
}

impl Type {
    const fn syntax_kind_set() -> [SyntaxKind; 3] {
        [
            PathType::syntax_kind(),
            PointerType::syntax_kind(),
            RefinementType::syntax_kind(),
        ]
    }

    fn from_raw_unchecked(raw: SyntaxNode) -> Self {
        match raw.kind() {
            SyntaxKind::PathType => Self::PathType(PathType(raw)),
            SyntaxKind::PointerType => Self::PointerType(PointerType(raw)),
            SyntaxKind::RefinementType => Self::RefinementType(RefinementType(raw)),
            _ => panic!(),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for Type {
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

impl TryFrom<SyntaxNode> for PathType {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct PointerType(SyntaxNode);

impl CstNode for PointerType {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl PointerType {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::PointerType
    }

    pub fn inner_type(&self) -> Option<Type> {
        self.as_syntax_node()
            .children()
            .find_map(|n| Type::try_from(n).ok())
    }
}

impl std::fmt::Display for PointerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for PointerType {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct RefinementType(SyntaxNode);

impl CstNode for RefinementType {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl RefinementType {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::RefinementType
    }

    pub fn inner_pattern(&self) -> Option<Pattern> {
        self.as_syntax_node()
            .children()
            .find_map(|n| Pattern::try_from(n).ok())
    }

    pub fn inner_type(&self) -> Option<Type> {
        self.as_syntax_node()
            .children()
            .find_map(|n| Type::try_from(n).ok())
    }

    pub fn refinement_expression(&self) -> Option<Expression> {
        self.as_syntax_node()
            .children()
            .find_map(|n| Expression::try_from(n).ok())
    }
}

impl std::fmt::Display for RefinementType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for RefinementType {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
