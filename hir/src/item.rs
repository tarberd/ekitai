use la_arena::Arena;
use syntax::ast;

use crate::{
    ast_node_map::{AstNodeId, AstNodeMap},
    name::Name,
    type_reference::TypeReference,
};

use crate::item_tree::ItemTreeNodeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Item {
    Function(ItemTreeNodeId<FunctionDefinition>),
    Type(ItemTreeNodeId<TypeDefinition>),
}

impl From<ItemTreeNodeId<FunctionDefinition>> for Item {
    fn from(id: ItemTreeNodeId<FunctionDefinition>) -> Self {
        Self::Function(id)
    }
}

impl From<ItemTreeNodeId<TypeDefinition>> for Item {
    fn from(id: ItemTreeNodeId<TypeDefinition>) -> Self {
        Self::Type(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition {
    pub name: Name,
    pub value_constructors: Arena<ValueConstructor>,
    pub ast_node_id: AstNodeId<ast::TypeDefinition>,
}

impl TypeDefinition {
    pub(crate) fn from_ast(ast_node_map: &AstNodeMap, ty: ast::TypeDefinition) -> Self {
        TypeDefinition {
            name: Name::from_ast_name(ty.name().unwrap()),
            value_constructors: ty
                .value_constructor_list()
                .unwrap()
                .constructors()
                .map(ValueConstructor::from_ast)
                .collect(),
            ast_node_id: ast_node_map.ast_id(&ty),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueConstructor {
    pub name: Name,
    pub parameters: Vec<TypeReference>,
}

impl ValueConstructor {
    pub(crate) fn from_ast(ctor: ast::ValueConstructor) -> Self {
        ValueConstructor {
            name: Name::from_ast_name(ctor.name().unwrap()),
            parameters: ctor
                .constructor_parameter_list()
                .unwrap()
                .types()
                .map(TypeReference::from_ast)
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinition {
    pub name: Name,
    pub parameter_types: Vec<TypeReference>,
    pub return_type: TypeReference,
    pub ast_node_id: AstNodeId<ast::FunctionDefinition>,
}

impl FunctionDefinition {
    pub(crate) fn from_ast(ast_node_map: &AstNodeMap, f: ast::FunctionDefinition) -> Self {
        let parameter_types = f.parameter_list().map_or(Vec::new(), |param_list| {
            let params = param_list.parameters();
            params
                .map(|param| {
                    TypeReference::from_ast(
                        param.ty().expect("missing type for function parameter"),
                    )
                })
                .collect()
        });
        let name = Name::from_ast_name(f.name().expect("missing name from function declaration."));
        let return_type =
            TypeReference::from_ast(f.return_type().expect("missin return type from function"));
        let ast_node_id = ast_node_map.ast_id(&f);
        Self {
            name,
            parameter_types,
            return_type,
            ast_node_id,
        }
    }
}
