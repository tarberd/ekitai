use crate::ast::{AstNode, EkitaiLanguage, Name, Path, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub enum Pattern {
    DeconstructorPattern(DeconstructorPattern),
    BindingPattern(BindingPattern),
}

impl AstNode for Pattern {
    type Language = EkitaiLanguage;

    fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
    where
        Self: Sized,
    {
        match kind {
            SyntaxKind::DeconstructorPattern | SyntaxKind::BindingPattern => true,
            _ => false,
        }
    }

    fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self>
    where
        Self: Sized,
    {
        let node = match node.kind() {
            SyntaxKind::DeconstructorPattern => {
                Self::DeconstructorPattern(DeconstructorPattern(node))
            }
            SyntaxKind::BindingPattern => Self::BindingPattern(BindingPattern(node)),
            _ => return None,
        };
        Some(node)
    }

    fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
        match self {
            Pattern::DeconstructorPattern(p) => p.syntax(),
            Pattern::BindingPattern(p) => p.syntax(),
        }
    }
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}

#[derive(Debug)]
pub struct BindingPattern(SyntaxNode);

impl_trait_ast_node!(BindingPattern);

impl BindingPattern {
    pub fn name(&self) -> Option<Name> {
        self.syntax().children().find_map(Name::cast)
    }
}

#[derive(Debug)]
pub struct DeconstructorPattern(SyntaxNode);

impl_trait_ast_node!(DeconstructorPattern);

impl DeconstructorPattern {
    pub fn path(&self) -> Option<Path> {
        self.syntax().children().find_map(Path::cast)
    }

    pub fn pattern_list(&self) -> Option<BindingPatternList> {
        self.syntax().children().find_map(BindingPatternList::cast)
    }
}

#[derive(Debug)]
pub struct BindingPatternList(SyntaxNode);

impl_trait_ast_node!(BindingPatternList);

impl BindingPatternList {
    pub fn patterns(&self) -> impl Iterator<Item = Pattern> {
        let mut children = self.syntax().children();
        std::iter::from_fn(move || children.by_ref().find_map(Pattern::cast))
    }
}
