use crate::ast::{AstNode, EkitaiLanguage, FunctionDefinition, TypeDefinition};
use parser::SyntaxKind;

#[derive(Debug)]
pub enum ModuleItem {
    FunctionDefinition(FunctionDefinition),
    TypeDefinition(TypeDefinition),
}

impl AstNode for ModuleItem {
    type Language = EkitaiLanguage;

    fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
    where
        Self: Sized,
    {
        match kind {
            SyntaxKind::FunctionDefinition | SyntaxKind::TypeDefinition => true,
            _ => false,
        }
    }

    fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self>
    where
        Self: Sized,
    {
        let node = match node.kind() {
            SyntaxKind::FunctionDefinition => Self::FunctionDefinition(FunctionDefinition(node)),
            SyntaxKind::TypeDefinition => Self::TypeDefinition(TypeDefinition(node)),
            _ => return None,
        };
        Some(node)
    }

    fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
        match self {
            ModuleItem::FunctionDefinition(i) => i.syntax(),
            ModuleItem::TypeDefinition(i) => i.syntax(),
        }
    }
}

impl std::fmt::Display for ModuleItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
