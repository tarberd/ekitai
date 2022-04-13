use crate::ast::{AstNode, ModuleItem, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile(SyntaxNode);

impl_trait_ast_node!(SourceFile);

impl SourceFile {
    pub fn module_items(&self) -> impl Iterator<Item = ModuleItem> {
        self.syntax().children().filter_map(ModuleItem::cast)
    }
}
