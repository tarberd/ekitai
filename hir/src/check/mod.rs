pub mod type_inference;
pub mod solver;

use crate::semantic_ir::{
    definition_map::{CallableDefinitionId, TypeDefinitionId},
    intrinsic::{BuiltinInteger, BuiltinType},
    path_resolver::TypeNamespaceItem,
    term::{BodyPatternId, Pattern}, refinement::Predicate, name::Name,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiquidType {
    Base(Name, Type, Predicate),
    DependentFunction(CallableDefinitionId),
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
pub struct DependentFunctionSignature {
    pub parameters: Vec<(BodyPatternId, LiquidType)>,
    pub return_type: LiquidType,
}
