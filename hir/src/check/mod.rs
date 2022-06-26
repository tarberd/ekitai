pub mod type_inference;

use crate::semantic_ir::{
    definition_map::{CallableDefinitionId, TypeDefinitionId},
    intrinsic::{BuiltinInteger, BuiltinType},
    name::Name,
    path_resolver::TypeNamespaceItem,
    refinement::Predicate,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeableDefinition {
    Type(TypeDefinitionId),
    Builtin(BuiltinType),
}

impl From<TypeNamespaceItem> for TypeableDefinition {
    fn from(typeable_item: TypeNamespaceItem) -> Self {
        match typeable_item {
            TypeNamespaceItem::TypeDefinition(ty_def) => Self::Type(ty_def),
            TypeNamespaceItem::Builtin(builtin) => Self::Builtin(builtin),
        }
    }
}

impl From<TypeDefinitionId> for TypeableDefinition {
    fn from(from: TypeDefinitionId) -> Self {
        Self::Type(from)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    AbstractDataType(TypeDefinitionId),
    FunctionDefinition(CallableDefinitionId),
    Pointer(Box<Type>),
    Scalar(ScalarType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarType {
    Integer(IntegerKind),
    Boolean,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerKind {
    I32,
    I64,
}

impl From<BuiltinInteger> for IntegerKind {
    fn from(int: BuiltinInteger) -> Self {
        match int {
            BuiltinInteger::I32 => Self::I32,
            BuiltinInteger::I64 => Self::I64,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
}
