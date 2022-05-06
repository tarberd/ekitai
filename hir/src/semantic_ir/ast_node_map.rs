use std::marker::PhantomData;

use la_arena::{Arena, Idx};
use syntax::ast::{self, AstNode, AstPtr, EkitaiLanguage, SyntaxNodePtr};

/// Tries to index the syntax tree nodes in the same order every time
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNodeMap {
    syntax_node_pointers: Arena<SyntaxNodePtr>,
}

impl AstNodeMap {
    pub(crate) fn from_source_file(source_file: ast::SourceFile) -> Self {
        let syntax_node_pointers = source_file
            .module_items()
            .map(|item| SyntaxNodePtr::new(item.syntax()))
            .collect();

        Self {
            syntax_node_pointers,
        }
    }

    pub(crate) fn ast_id<N: ast::AstNode<Language = EkitaiLanguage>>(
        &self,
        ast_node: &N,
    ) -> AstNodeId<N> {
        let node_ptr = SyntaxNodePtr::new(ast_node.syntax());

        let cst_id = self
            .syntax_node_pointers
            .iter()
            .find(|(_, ptr)| **ptr == node_ptr)
            .map(|(id, _)| id)
            .expect("cant find node");

        AstNodeId {
            ast_node_id: cst_id,
            ast_node_type: PhantomData,
        }
    }

    pub(crate) fn get<N>(&self, id: &AstNodeId<N>) -> AstPtr<N>
    where
        N: ast::AstNode<Language = EkitaiLanguage>,
    {
        self.syntax_node_pointers[id.ast_node_id]
            .clone()
            .cast()
            .unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNodeId<N: ast::AstNode> {
    ast_node_id: Idx<SyntaxNodePtr>,
    ast_node_type: PhantomData<fn() -> N>,
}
