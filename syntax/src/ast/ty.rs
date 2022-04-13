use crate::ast::{AstNode, EkitaiLanguage, Expression, Path, Pattern, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub enum Type {
    PathType(PathType),
    PointerType(PointerType),
    RefinementType(RefinementType),
}

impl AstNode for Type {
    type Language = EkitaiLanguage;

    fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
    where
        Self: Sized,
    {
        match kind {
            SyntaxKind::PathType | SyntaxKind::PointerType | SyntaxKind::RefinementType => true,
            _ => false,
        }
    }

    fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self>
    where
        Self: Sized,
    {
        let node = match node.kind() {
            SyntaxKind::PathType => Self::PathType(PathType(node)),
            SyntaxKind::PointerType => Self::PointerType(PointerType(node)),
            SyntaxKind::RefinementType => Self::RefinementType(RefinementType(node)),
            _ => return None,
        };
        Some(node)
    }

    fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
        match self {
            Type::PathType(t) => t.syntax(),
            Type::PointerType(t) => t.syntax(),
            Type::RefinementType(t) => t.syntax(),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}

#[derive(Debug)]
pub struct PathType(SyntaxNode);

impl_trait_ast_node!(PathType);

impl PathType {
    pub fn path(&self) -> Option<Path> {
        self.syntax().children().find_map(Path::cast)
    }
}

#[derive(Debug)]
pub struct PointerType(SyntaxNode);

impl_trait_ast_node!(PointerType);

impl PointerType {
    pub fn inner_type(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }
}

#[derive(Debug)]
pub struct RefinementType(SyntaxNode);

impl_trait_ast_node!(RefinementType);

impl RefinementType {
    pub fn inner_pattern(&self) -> Option<Pattern> {
        self.syntax().children().find_map(Pattern::cast)
    }

    pub fn inner_type(&self) -> Option<Type> {
        self.syntax().children().find_map(Type::cast)
    }

    pub fn predicate(&self) -> Option<Expression> {
        self.syntax().children().find_map(Expression::cast)
    }
}
