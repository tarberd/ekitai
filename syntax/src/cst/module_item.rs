use super::{raw::SyntaxNode, CstNode, FunctionDefinition, SyntaxToAstError, TypeDefinition};
use parser::SyntaxKind;

#[derive(Debug)]
pub enum ModuleItem {
    FunctionDefinition(FunctionDefinition),
    TypeDefinition(TypeDefinition),
}

impl CstNode for ModuleItem {
    fn as_syntax_node(&self) -> &SyntaxNode {
        match self {
            ModuleItem::FunctionDefinition(f) => f.as_syntax_node(),
            ModuleItem::TypeDefinition(t) => t.as_syntax_node(),
        }
    }
}

impl ModuleItem {
    fn try_from_set() -> &'static [SyntaxKind] {
        static KINDS: &[SyntaxKind] = &[SyntaxKind::FunctionDefinition, SyntaxKind::TypeDefinition];
        &KINDS
    }

    fn from_raw_unchecked(raw: SyntaxNode) -> Self {
        match raw.kind() {
            SyntaxKind::FunctionDefinition => Self::FunctionDefinition(FunctionDefinition(raw)),
            SyntaxKind::TypeDefinition => Self::TypeDefinition(TypeDefinition(raw)),
            _ => panic!(),
        }
    }
}

impl std::fmt::Display for ModuleItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for ModuleItem {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::try_from_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::try_from_set()[0], other)),
        }
    }
}
