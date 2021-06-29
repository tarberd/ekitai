use crate::{Body, Expression, Function, Module, Name, TypeReference};

use la_arena::{ArenaMap, Idx};

#[derive(Debug, PartialEq)]
pub enum TypeError {
    Error,
    TypeMismatch { expected: Type, actual: Type },
}

#[derive(Debug, PartialEq)]
pub enum Type {
    I32,
    I64,
    Function(Vec<Type>, Box<Type>),
    Unknown,
}

impl Type {
    fn from(type_reference: &TypeReference) -> Self {
        let TypeReference { id } = type_reference;
        match id.as_str() {
            "i32" => Type::I32,
            "i64" => Type::I64,
            _ => Type::Unknown,
        }
    }
}

pub struct ModuleTypeMap {
    pub function_to_type: ArenaMap<Idx<Function>, Type>,
}

impl ModuleTypeMap {
    pub fn new(module: &Module) -> Self {
        let function_to_type = module.functions.iter().fold(
            ArenaMap::default(),
            |mut function_to_type, (fid, function)| {
                let parameter_types = function.parameter_types.iter().fold(
                    Vec::new(),
                    |mut parameter_types, type_reference| {
                        parameter_types.push(Type::from(type_reference));
                        parameter_types
                    },
                );
                let return_type = Type::from(&function.return_type);
                function_to_type
                    .insert(fid, Type::Function(parameter_types, Box::new(return_type)));
                function_to_type
            },
        );
        Self { function_to_type }
    }
}

pub struct BodyTypeMap {
    type_of_expression: ArenaMap<Idx<Expression>, Type>,
    type_of_name: ArenaMap<Idx<Name>, Type>,
}

impl BodyTypeMap {
    fn new(body: &Body) -> Self {
        let type_of_name =
            body.bindings
                .iter()
                .fold(ArenaMap::default(), |mut type_of_name, (name, typeref)| {
                    let ref typeref = body.types[*typeref];
                    let ty = Type::from(typeref);
                    type_of_name.insert(name, ty);
                    type_of_name
                });
        Self {
            type_of_expression: ArenaMap::default(),
            type_of_name,
        }
    }
}
