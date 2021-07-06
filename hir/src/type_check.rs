use crate::{
    Body, Expression, ExpressionId, FunctionId, IntegerKind, Literal, Module, NameId, TypeReference,
};

use la_arena::ArenaMap;

#[derive(Debug, PartialEq)]
pub enum TypeError {
    Error,
    TypeMismatch { expected: Type, actual: Type },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Integer(IntegerType),
    Function(Vec<Type>, Box<Type>),
    Unknown,
}

impl Type {
    fn from(type_reference: &TypeReference) -> Self {
        let TypeReference { id } = type_reference;
        match id.as_str() {
            "i32" => Type::Integer(IntegerType::I32),
            "i64" => Type::Integer(IntegerType::I64),
            _ => Type::Unknown,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum IntegerType {
    I32,
    I64,
}

pub struct ModuleTypeMap {
    pub function_to_type: ArenaMap<FunctionId, Type>,
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

pub struct ExpressionScopes {}

pub struct BodyTypeMap {
    pub type_of_expression: ArenaMap<ExpressionId, Type>,
    pub type_of_name: ArenaMap<NameId, Type>,
}

impl BodyTypeMap {
    pub fn new(module: &Module, module_map: &ModuleTypeMap, body: &Body) -> Self {
        let type_of_name = body.parameters.iter().fold(
            ArenaMap::default(),
            |mut type_of_name, (name, typeref)| {
                let ref typeref = body.types[*typeref];
                let ty = Type::from(typeref);
                type_of_name.insert(name, ty);
                type_of_name
            },
        );

        let body_type_map = BodyTypeMap {
            type_of_expression: ArenaMap::default(),
            type_of_name,
        };
        let (body_type_map, _) =
            Self::collect_expression_type(body_type_map, module, module_map, body, body.block);
        body_type_map
    }

    fn collect_expression_type(
        mut body_type_map: BodyTypeMap,
        module: &Module,
        module_map: &ModuleTypeMap,
        body: &Body,
        expr_id: ExpressionId,
    ) -> (BodyTypeMap, Type) {
        let expr = &body.expressions[expr_id];
        match expr {
            Expression::BlockExpression(block) => {
                let (mut body_type_map, ty) = Self::collect_expression_type(
                    body_type_map,
                    module,
                    module_map,
                    body,
                    block.tail_expression,
                );
                body_type_map.type_of_expression.insert(expr_id, ty.clone());
                (body_type_map, ty)
            }
            Expression::BinaryExpression(_op, lhs, rhs) => {
                let (body_type_map, lhs_ty) =
                    Self::collect_expression_type(body_type_map, module, module_map, body, *lhs);
                let (mut body_type_map, rhs_ty) =
                    Self::collect_expression_type(body_type_map, module, module_map, body, *rhs);

                let ty = match (lhs_ty, rhs_ty) {
                    (a, b) if a == b => a,
                    (Type::Unknown, b) => b,
                    (a, Type::Unknown) => a,
                    _ => panic!("type error"),
                };

                body_type_map.type_of_expression.insert(expr_id, ty.clone());
                (body_type_map, ty)
            }
            Expression::UnaryExpression(_op, inner_expr) => {
                let (mut body_type_map, ty) = Self::collect_expression_type(
                    body_type_map,
                    module,
                    module_map,
                    body,
                    *inner_expr,
                );
                body_type_map.type_of_expression.insert(expr_id, ty.clone());
                (body_type_map, ty)
            }
            Expression::Literal(lit) => match lit {
                Literal::Integer(_, int_kind) => {
                    let ty = match int_kind {
                        IntegerKind::Unsuffixed => Type::Unknown,
                        IntegerKind::I32 => Type::Integer(IntegerType::I32),
                        IntegerKind::I64 => Type::Integer(IntegerType::I64),
                    };
                    body_type_map.type_of_expression.insert(expr_id, ty.clone());
                    (body_type_map, ty)
                }
            },
            Expression::NameReference(name) => {
                match body
                    .names
                    .iter()
                    .find(|(_, name_to_find)| name_to_find.id == name.id)
                {
                    Some((id, _)) => {
                        let ty = body_type_map.type_of_name[id].clone();
                        body_type_map.type_of_expression.insert(expr_id, ty.clone());
                        (body_type_map, ty)
                    }
                    None => match module
                        .functions
                        .iter()
                        .find(|(_, function)| function.name.id == name.id)
                    {
                        Some((id, _)) => {
                            let ty = module_map.function_to_type[id].clone();
                            body_type_map.type_of_expression.insert(expr_id, ty.clone());
                            (body_type_map, ty)
                        }
                        None => panic!(),
                    },
                }
            }
            Expression::Call(call) => {
                let (body_type_map, callee_ty) = Self::collect_expression_type(
                    body_type_map,
                    module,
                    module_map,
                    body,
                    call.callee,
                );
                let (arguments_ty, ty) = match callee_ty {
                    Type::Function(args, ret) => (args, *ret),
                    x => panic!("cally type is not a function. {:?}", x),
                };
                let body_type_map =
                    call.arguments
                        .iter()
                        .zip(arguments_ty)
                        .fold(body_type_map, |body_type_map, (arg_id, arg_ty)| {
                            let (mut body_type_map, ty) = Self::collect_expression_type(
                                body_type_map,
                                module,
                                module_map,
                                body,
                                *arg_id,
                            );

                            let ty = if ty == Type::Unknown {
                                arg_ty.clone()
                            } else {
                                ty
                            };

                            if ty != arg_ty {
                                panic!("mismatch type of function call argument. Exprected {:?} but found {:?}.", arg_ty, ty);
                            }

                            body_type_map.type_of_expression.insert(*arg_id, ty);
                            body_type_map
                        });
                (body_type_map, ty)
            }
        }
    }
}
