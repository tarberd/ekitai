use crate::{
    check::{
        type_inference::{InferenceResult, TypeReferenceResolver},
        FunctionSignature, IntegerKind, ScalarType, TypeableDefinition, Type,
    },
    semantic_ir::{intrinsic::{BuiltinInteger, BuiltinType}, definition_map::{TypeableValueDefinitionId, CallableDefinitionId, FunctionDefinitionId, ValueConstructorId}, path_resolver::Resolver},
    DefinitionsDatabase, Upcast,
};

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: DefinitionsDatabase + Upcast<dyn DefinitionsDatabase> {
    fn type_of_definition(&self, definition: TypeableDefinition) -> Type;

    fn type_of_value(&self, id: TypeableValueDefinitionId) -> Type;

    fn callable_definition_signature(&self, callable: CallableDefinitionId) -> FunctionSignature;

    fn infer_body_expression_types(&self, function: FunctionDefinitionId) -> InferenceResult;
}

fn type_of_definition(_db: &dyn HirDatabase, definition: TypeableDefinition) -> Type {
    match definition {
        TypeableDefinition::Type(type_def_location) => Type::AbstractDataType(type_def_location),
        TypeableDefinition::Builtin(builtin) => match builtin {
            BuiltinType::Integer(int) => match int {
                BuiltinInteger::I32 => Type::Scalar(ScalarType::Integer(IntegerKind::I32)),
                BuiltinInteger::I64 => Type::Scalar(ScalarType::Integer(IntegerKind::I64)),
            },
            BuiltinType::Boolean => Type::Scalar(ScalarType::Boolean),
        },
    }
}

fn type_of_value(_db: &dyn HirDatabase, value: TypeableValueDefinitionId) -> Type {
    match value {
        TypeableValueDefinitionId::Function(function_location_id) => Type::FunctionDefinition(
            CallableDefinitionId::FunctionDefinition(function_location_id),
        ),
        TypeableValueDefinitionId::ValueConstructor(value_constructor_id) => {
            Type::FunctionDefinition(CallableDefinitionId::ValueConstructor(value_constructor_id))
        }
    }
}

fn callable_definition_signature(
    db: &dyn HirDatabase,
    callable: CallableDefinitionId,
) -> FunctionSignature {
    match callable {
        CallableDefinitionId::FunctionDefinition(function_id) => {
            function_definition_signature(db, function_id)
        }
        CallableDefinitionId::ValueConstructor(value_constructor_id) => {
            value_constructor_signature(db, value_constructor_id)
        }
    }
}

fn function_definition_signature(
    db: &dyn HirDatabase,
    function_id: FunctionDefinitionId,
) -> FunctionSignature {
    let resolver = Resolver::new_for_function(db.upcast(), function_id);
    let function = db.function_definition_data(function_id);

    let typeref_resolver = TypeReferenceResolver::new(db, &resolver);

    let parameter_types = function
        .parameter_types
        .iter()
        .map(|type_reference| {
            typeref_resolver
                .resolve_type_reference(type_reference)
                .expect("missing function definition argument type")
        })
        .collect();

    let return_type = typeref_resolver
        .resolve_type_reference(&function.return_type)
        .expect("missing function definition return type");

    FunctionSignature {
        parameter_types,
        return_type,
    }
}

fn value_constructor_signature(
    db: &dyn HirDatabase,
    value_constructor_id: ValueConstructorId,
) -> FunctionSignature {
    let resolver = Resolver::new_for_type(db.upcast(), value_constructor_id.type_definition_id);
    let typeref_resolver = TypeReferenceResolver::new(db, &resolver);
    let type_data = db.type_definition_data(value_constructor_id.type_definition_id);
    let constructor = type_data.value_constructor(value_constructor_id.id);

    let parameter_types = constructor
        .parameters
        .iter()
        .map(|type_reference| {
            typeref_resolver
                .resolve_type_reference(type_reference)
                .expect("missing value constructor definition data type")
        })
        .collect();

    let return_type = Type::AbstractDataType(value_constructor_id.type_definition_id);

    FunctionSignature {
        parameter_types,
        return_type,
    }
}

fn infer_body_expression_types(
    db: &dyn HirDatabase,
    function_id: FunctionDefinitionId,
) -> InferenceResult {
    InferenceResult::new(db, function_id)
}
