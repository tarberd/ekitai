use la_arena::{Arena, Idx};
use syntax::ast::{self, SourceFile};

use crate::{
    ast_node_map::{AstNodeId, AstNodeMap},
    definitions_map::ValueConstructor,
    Name, TypeReference,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    root_items: Vec<Item>,
    functions: Arena<FunctionDefinition>,
    types: Arena<TypeDefinition>,
}

impl ItemTree {
    pub(crate) fn new(source: ast::SourceFile, ast_node_map: AstNodeMap) -> Self {
        let ItemTreeFold {
            root_items,
            functions,
            types,
            ..
        } = ItemTreeFold::new(source, ast_node_map).collect_root_items();
        Self {
            root_items,
            functions,
            types,
        }
    }

    pub(crate) fn root_items(&self) -> &[Item] {
        &self.root_items
    }

    pub(crate) fn get<I: ItemTreeNode>(&self, item_id: ItemId<I>) -> &I {
        I::lookup(self, item_id.id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Item {
    Function(ItemId<FunctionDefinition>),
    Type(ItemId<TypeDefinition>),
}

impl From<Idx<FunctionDefinition>> for Item {
    fn from(id: Idx<FunctionDefinition>) -> Self {
        Self::Function(ItemId { id })
    }
}

impl From<Idx<TypeDefinition>> for Item {
    fn from(id: Idx<TypeDefinition>) -> Self {
        Self::Type(ItemId { id })
    }
}

pub trait ItemTreeNode: Sized {
    fn lookup(item_tree: &ItemTree, id: Idx<Self>) -> &Self;
}

#[derive(Debug, PartialEq, Eq)]
pub struct ItemId<I: ItemTreeNode> {
    id: Idx<I>,
}

impl<I: ItemTreeNode> Clone for ItemId<I> {
    fn clone(&self) -> Self {
        Self {
            id: self.id.clone(),
        }
    }
}
impl<I: ItemTreeNode> Copy for ItemId<I> {}
impl<I: ItemTreeNode> std::hash::Hash for ItemId<I> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinition {
    pub name: Name,
    pub parameter_types: Vec<TypeReference>,
    pub return_type: TypeReference,
    pub ast_node_id: AstNodeId<ast::FunctionDefinition>,
}

impl ItemTreeNode for FunctionDefinition {
    fn lookup(item_tree: &ItemTree, id: Idx<Self>) -> &Self {
        &item_tree.functions[id]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition {
    pub name: Name,
    pub value_constructors: Arena<ValueConstructor>,
    pub ast_node_id: AstNodeId<ast::TypeDefinition>,
}

impl ItemTreeNode for TypeDefinition {
    fn lookup(item_tree: &ItemTree, id: Idx<Self>) -> &Self {
        &item_tree.types[id]
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
        let function = FunctionDefinition::lower(&self.ast_node_map, function);
        let id = self.functions.alloc(function);
        self.root_items.push(id.into());
        self
    }

    fn collect_type(mut self, ty: ast::TypeDefinition) -> Self {
        let ty = TypeDefinition::lower(&self.ast_node_map, ty);
        let id = self.types.alloc(ty);
        self.root_items.push(id.into());
        self
    }
}
