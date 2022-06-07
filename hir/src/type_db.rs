use crate::{
    check::{
        type_inference::{InferenceResult, TypeReferenceResolver},
        DependentFunctionSignature, IntegerKind, LiquidType, ScalarType, Type, TypeableDefinition,
    },
    semantic_ir::{
        definition_map::{
            CallableDefinitionId, FunctionDefinitionId, TypeableValueDefinitionId,
            ValueConstructorId,
        },
        intrinsic::{BuiltinInteger, BuiltinType},
        name::Name,
        path_resolver::Resolver,
        term::Pattern,
    },
    DefinitionsDatabase, Upcast,
};

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: DefinitionsDatabase + Upcast<dyn DefinitionsDatabase> {
    fn type_of_definition(&self, definition: TypeableDefinition) -> Type;

    fn type_of_value(&self, id: TypeableValueDefinitionId) -> LiquidType;

    fn callable_definition_signature(
        &self,
        callable: CallableDefinitionId,
    ) -> DependentFunctionSignature;

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

fn type_of_value(_db: &dyn HirDatabase, value: TypeableValueDefinitionId) -> LiquidType {
    match value {
        TypeableValueDefinitionId::Function(function_location_id) => LiquidType::DependentFunction(
            CallableDefinitionId::FunctionDefinition(function_location_id),
        ),
        TypeableValueDefinitionId::ValueConstructor(value_constructor_id) => {
            LiquidType::DependentFunction(CallableDefinitionId::ValueConstructor(
                value_constructor_id,
            ))
        }
    }
}

fn callable_definition_signature(
    db: &dyn HirDatabase,
    callable: CallableDefinitionId,
) -> DependentFunctionSignature {
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
) -> DependentFunctionSignature {
    let resolver = Resolver::new_for_function(db.upcast(), function_id);
    let function = db.function_definition_data(function_id);

    let typeref_resolver = TypeReferenceResolver::new(db, &resolver);

    let parameters = function
        .parameters
        .iter()
        .map(|(pattern, type_reference)| {
            (
                pattern.clone(),
                typeref_resolver
                    .resolve_type_reference(type_reference)
                    .expect("missing function definition argument type"),
            )
        })
        .collect();

    let return_type = typeref_resolver
        .resolve_type_reference(&function.return_type)
        .expect("missing function definition return type");

    DependentFunctionSignature {
        parameters,
        return_type,
    }
}

fn value_constructor_signature(
    db: &dyn HirDatabase,
    value_constructor_id: ValueConstructorId,
) -> DependentFunctionSignature {
    let resolver = Resolver::new_for_type(db.upcast(), value_constructor_id.type_definition_id);
    let typeref_resolver = TypeReferenceResolver::new(db, &resolver);
    let type_data = db.type_definition_data(value_constructor_id.type_definition_id);
    let constructor = type_data.value_constructor(value_constructor_id.id);

    let parameters = constructor
        .parameters
        .iter()
        .enumerate()
        .map(|(index, type_reference)| {
            (
                Pattern::Bind(Name::new_inline(format!("arg_{}", index).as_str())),
                typeref_resolver
                    .resolve_type_reference(type_reference)
                    .expect("missing value constructor definition data type"),
            )
        })
        .collect();

    let return_type = LiquidType::Base(Type::AbstractDataType(
        value_constructor_id.type_definition_id,
    ));

    DependentFunctionSignature {
        parameters,
        return_type,
    }
}

fn infer_body_expression_types(
    db: &dyn HirDatabase,
    function_id: FunctionDefinitionId,
) -> InferenceResult {
    InferenceResult::new(db, function_id)
}
