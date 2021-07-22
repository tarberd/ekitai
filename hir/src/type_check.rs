use crate::{
    BinaryOperator, Body, Expression, ExpressionId, FunctionId, IntegerKind, Literal, FileDefinitions, Name,
    NameId, TypeId, TypeReference, ValueConstructor, ValueConstructorId,
};

use la_arena::ArenaMap;

#[derive(Debug, PartialEq)]
pub enum TypeError {
    Error,
    TypeMismatch { expected: Type, actual: Type },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Boolean,
    Integer(IntegerType),
    Function(Vec<Type>, Box<Type>),
    Custom(Name),
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub enum IntegerType {
    I32,
    I64,
}

pub struct ModuleTypeMap {
    pub function_to_type: ArenaMap<FunctionId, Type>,
    pub type_to_type: ArenaMap<TypeId, Type>,
    pub ctors_to_type: ArenaMap<ValueConstructorId, Type>,
    pub type_to_ctors: ArenaMap<TypeId, Vec<ValueConstructor>>,
}

impl ModuleTypeMap {
    pub fn new(module: &FileDefinitions) -> Self {
        let mut type_to_type = ArenaMap::default();
        let mut ctors_to_type = ArenaMap::default();
        let mut type_to_ctors = ArenaMap::default();
        for (ctor_id, type_id) in module.ctors_to_type.iter() {
            let hir_type = &module.types[*type_id];
            let ty = Type::Custom(hir_type.name.clone());
            type_to_type.insert(*type_id, ty.clone());
            ctors_to_type.insert(ctor_id, ty);
            type_to_ctors.insert(*type_id, hir_type.value_constructors.clone());
        }

        let function_to_type = module.functions.iter().fold(
            ArenaMap::default(),
            |mut function_to_type, (fid, function)| {
                let parameter_types = function.parameter_types.iter().fold(
                    Vec::new(),
                    |mut parameter_types, type_reference| {
                        let (ty_id, _) = module
                            .types
                            .iter()
                            .find(|(_, ctor)| ctor.name.id == type_reference.id)
                            .expect(&format!("cant find type with name {:?}", type_reference.id));
                        let ty = type_to_type[ty_id].clone();
                        parameter_types.push(ty);
                        parameter_types
                    },
                );
                let (ty_id, _) = module
                    .types
                    .iter()
                    .find(|(_, ctor)| ctor.name.id == function.return_type.id)
                    .unwrap();
                let ty = type_to_type[ty_id].clone();
                function_to_type.insert(fid, Type::Function(parameter_types, Box::new(ty)));
                function_to_type
            },
        );
        Self {
            function_to_type,
            type_to_type,
            ctors_to_type,
            type_to_ctors,
        }
    }
}

pub struct ExpressionScopes {}

#[derive(Debug)]
pub struct BodyTypeMap {
    pub type_of_expression: ArenaMap<ExpressionId, Type>,
    pub type_of_name: ArenaMap<NameId, Type>,
}

impl BodyTypeMap {
    pub fn new(module: &FileDefinitions, module_map: &ModuleTypeMap, body: &Body) -> Self {
        let type_of_name = body.parameters.iter().fold(
            ArenaMap::default(),
            |mut type_of_name, (name, typeref)| {
                let typeref = &body.types[*typeref];
                let (ty_id, _) = module
                    .types
                    .iter()
                    .find(|(_, ctor)| ctor.name.id == typeref.id)
                    .unwrap();
                let ty = module_map.type_to_type[ty_id].clone();
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
        module: &FileDefinitions,
        module_map: &ModuleTypeMap,
        body: &Body,
        expr_id: ExpressionId,
    ) -> (BodyTypeMap, Type) {
        let expr = dbg!(&body.expressions[expr_id]);
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
            Expression::BinaryExpression(op, lhs, rhs) => {
                let (body_type_map, lhs_ty) =
                    Self::collect_expression_type(body_type_map, module, module_map, body, *lhs);
                let (mut body_type_map, rhs_ty) =
                    Self::collect_expression_type(body_type_map, module, module_map, body, *rhs);

                let ret_ty = match op {
                    BinaryOperator::Arithmetic(_) => match rhs_ty {
                        Type::Integer(_) => rhs_ty.clone(),
                        _ => panic!(),
                    },
                    BinaryOperator::Compare(_) => Type::Boolean,
                };

                match (lhs_ty, rhs_ty) {
                    (a, b) if a == b => a,
                    (a, b) => panic!(
                        "Mismatch types on binary expression. lhs {:?} rhs {:?}",
                        a, b
                    ),
                };

                body_type_map
                    .type_of_expression
                    .insert(expr_id, ret_ty.clone());
                (body_type_map, ret_ty)
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
                        None => match module
                            .ctors
                            .iter()
                            .find(|(_, ctor)| ctor.name.id == name.id)
                        {
                            Some((id, _)) => {
                                let ty = module_map.ctors_to_type[id].clone();
                                body_type_map.type_of_expression.insert(expr_id, ty.clone());
                                println!("yay {:?} {:?}", name, ty);
                                (body_type_map, ty)
                            }
                            None => panic!("cant find name {}", name.id),
                        },
                    },
                }
            }
            Expression::Call(call) => {
                print!("asdhflakshdf");
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
                println!("arguments_ty {:?}", arguments_ty);
                let mut body_type_map =
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

                            println!("ty {:?} argty {:?}", ty, arg_ty);
                            if ty != arg_ty {
                                panic!("mismatch type of function call argument. Exprected {:?} but found {:?}.", arg_ty, ty);
                            }

                            body_type_map.type_of_expression.insert(*arg_id, ty);
                            body_type_map
                        });
                body_type_map.type_of_expression.insert(expr_id, ty.clone());
                (body_type_map, ty)
            }
            Expression::IfExpression(if_expr) => {
                let (body_type_map, condition_ty) = Self::collect_expression_type(
                    body_type_map,
                    module,
                    module_map,
                    body,
                    if_expr.condition,
                );

                if condition_ty != Type::Boolean {
                    panic!(
                        "if condition type. Expected boolean found {:?}",
                        condition_ty
                    );
                }

                let (body_type_map, than_ty) = Self::collect_expression_type(
                    body_type_map,
                    module,
                    module_map,
                    body,
                    if_expr.then_branch,
                );

                let (mut body_type_map, else_ty) = Self::collect_expression_type(
                    body_type_map,
                    module,
                    module_map,
                    body,
                    if_expr.else_branch,
                );

                if than_ty != else_ty {
                    panic!(
                        "If branches difers in type. Than type {:?} Else type {:?}",
                        than_ty, else_ty
                    );
                }

                body_type_map
                    .type_of_expression
                    .insert(expr_id, than_ty.clone());
                (body_type_map, than_ty)
            }
            Expression::MatchExpression(match_expr) => {
                // todo matchee
                let (body_type_map, matchee_type) = Self::collect_expression_type(
                    body_type_map,
                    module,
                    module_map,
                    body,
                    match_expr.matchee,
                );

                // let match_ctors = module_map.type_to_ctors
                match_expr.cases.iter().map(|case| {
                    // let r = case.pattern;

                    // module_map.ctors_to_type[]
                });

                // todo cases
                let (mut body_type_map, types) = match_expr.cases.iter().fold(
                    (body_type_map, vec![]),
                    |(body_type_map, mut types), case| {
                        let (body_type_map, ty) = Self::collect_expression_type(
                            body_type_map,
                            module,
                            module_map,
                            body,
                            case.expression,
                        );
                        types.push(ty);
                        (body_type_map, types)
                    },
                );

                let match_type = types
                    .get(0)
                    .unwrap_or_else(|| panic!("match expression has no type"));
                let all_cases_same_type = types.iter().all(|ty| ty == match_type);

                if !all_cases_same_type {
                    panic!()
                };

                body_type_map
                    .type_of_expression
                    .insert(expr_id, match_type.clone());
                (body_type_map, match_type.clone())
            }
        }
    }
}
