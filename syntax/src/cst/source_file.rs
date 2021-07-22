use super::{raw::SyntaxNode, CstNode, ModuleItem, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile(SyntaxNode);

impl CstNode for SourceFile {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl SourceFile {
    pub fn module_items(&self) -> impl Iterator<Item = ModuleItem> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .filter_map(|s| ModuleItem::try_from(s).ok())
    }

    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::SourceFile
    }
}

impl std::fmt::Display for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for SourceFile {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
