use la_arena::{Arena, Idx};
use syntax::ast::{self, SourceFile};

use super::{
    ast_node_map::AstNodeMap,
    item::{FunctionDefinition, Item, TypeDefinition},
};

/// A tree of items
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    root_items: Vec<Item>,
    functions: Arena<FunctionDefinition>,
    types: Arena<TypeDefinition>,
}

impl ItemTree {
    pub(crate) fn new(source: ast::SourceFile, ast_node_map: AstNodeMap) -> Self {
        ItemTreeFold::new(source, ast_node_map)
            .collect_root_items()
            .into()
    }

    pub(crate) fn root_items(&self) -> &[Item] {
        &self.root_items
    }

    pub(crate) fn get<I: ItemTreeNode>(&self, item_id: ItemTreeNodeId<I>) -> &I {
        I::lookup(self, item_id)
    }
}

impl From<ItemTreeFold> for ItemTree {
    fn from(fold: ItemTreeFold) -> Self {
        let ItemTreeFold {
            root_items,
            functions,
            types,
            ..
        } = fold;
        Self {
            root_items,
            functions,
            types,
        }
    }
}

pub trait ItemTreeNode: Sized {
    fn lookup(item_tree: &ItemTree, item_id: ItemTreeNodeId<Self>) -> &Self;
}

impl ItemTreeNode for FunctionDefinition {
    fn lookup(item_tree: &ItemTree, item_id: ItemTreeNodeId<Self>) -> &Self {
        &item_tree.functions[item_id.id]
    }
}

impl ItemTreeNode for TypeDefinition {
    fn lookup(item_tree: &ItemTree, item_id: ItemTreeNodeId<Self>) -> &Self {
        &item_tree.types[item_id.id]
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ItemTreeNodeId<I: ItemTreeNode> {
    id: Idx<I>,
}

impl<I: ItemTreeNode> Clone for ItemTreeNodeId<I> {
    fn clone(&self) -> Self {
        Self {
            id: self.id.clone(),
        }
    }
}

impl<I: ItemTreeNode> Copy for ItemTreeNodeId<I> {}

impl<I: ItemTreeNode> std::hash::Hash for ItemTreeNodeId<I> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

struct ItemTreeFold {
    source: SourceFile,
    ast_node_map: AstNodeMap,
    root_items: Vec<Item>,
    functions: Arena<FunctionDefinition>,
    types: Arena<TypeDefinition>,
}

impl ItemTreeFold {
    fn new(source: SourceFile, ast_node_map: AstNodeMap) -> Self {
        Self {
            source,
            ast_node_map,
            root_items: Vec::default(),
            functions: Arena::default(),
            types: Arena::default(),
        }
    }

    fn collect_root_items(self) -> Self {
        self.source
            .module_items()
            .fold(self, |fold, item| match item {
                ast::ModuleItem::FunctionDefinition(function) => fold.collect_function(function),
                ast::ModuleItem::TypeDefinition(ty) => fold.collect_type(ty),
            })
    }

    fn collect_function(mut self, function: ast::FunctionDefinition) -> Self {
        let function = FunctionDefinition::from_ast(&self.ast_node_map, function);
        let id = self.functions.alloc(function);
        self.root_items.push(ItemTreeNodeId { id }.into());
        self
    }

    fn collect_type(mut self, ty: ast::TypeDefinition) -> Self {
        let ty = TypeDefinition::from_ast(&self.ast_node_map, ty);
        let id = self.types.alloc(ty);
        self.root_items.push(ItemTreeNodeId { id }.into());
        self
    }
}
