use super::{raw::SyntaxNode, CstNode, Name, Path, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub enum Pattern {
    DeconstructorPattern(DeconstructorPattern),
    BindingPattern(BindingPattern),
}

impl CstNode for Pattern {
    fn as_syntax_node(&self) -> &SyntaxNode {
        match self {
            Pattern::DeconstructorPattern(f) => f.as_syntax_node(),
            Pattern::BindingPattern(f) => f.as_syntax_node(),
        }
    }
}

impl Pattern {
    const fn syntax_kind_set() -> &'static [SyntaxKind] {
        const KINDS: &[SyntaxKind] = &[
            DeconstructorPattern::syntax_kind(),
            BindingPattern::syntax_kind(),
        ];
        KINDS
    }

    fn from_raw_unchecked(raw: SyntaxNode) -> Self {
        match raw.kind() {
            kind if kind == DeconstructorPattern::syntax_kind() => {
                Self::DeconstructorPattern(DeconstructorPattern(raw))
            }
            kind if kind == BindingPattern::syntax_kind() => {
                Self::BindingPattern(BindingPattern(raw))
            }
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
            x if Self::syntax_kind_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind_set()[0], other)),
        }
    }
}

#[derive(Debug)]
pub struct BindingPattern(SyntaxNode);

impl CstNode for BindingPattern {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl BindingPattern {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::BindingPattern
    }

    pub fn name(&self) -> Option<Name> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Name::try_from(n).ok())
    }
}

impl std::fmt::Display for BindingPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for BindingPattern {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct DeconstructorPattern(SyntaxNode);

impl CstNode for DeconstructorPattern {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl DeconstructorPattern {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::DeconstructorPattern
    }

    pub fn path(&self) -> Option<Path> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Path::try_from(n).ok())
    }

    pub fn pattern_list(&self) -> Option<PatternList> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| PatternList::try_from(n).ok())
    }
}

impl std::fmt::Display for DeconstructorPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for DeconstructorPattern {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct PatternList(SyntaxNode);

impl CstNode for PatternList {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl PatternList {
    const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::BindingPatternList
    }

    pub fn patterns(&self) -> impl Iterator<Item = Pattern> {
        use std::convert::TryFrom;
        let mut children = self.as_syntax_node().children();
        std::iter::from_fn(move || {
            children
                .by_ref()
                .find_map(|n| Pattern::try_from(n).ok())
        })
    }
}

impl std::fmt::Display for PatternList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for PatternList {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
