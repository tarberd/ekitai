use la_arena::ArenaMap;

use crate::{
    semantic_ir::{
        definition_map::{CallableDefinitionId, FunctionDefinitionId, TypeableValueDefinitionId},
        name::Name,
        path::Path,
        path_resolver::{Resolver, ValueNamespaceItem},
        refinement::Predicate,
        term::{
            BinaryOperator, Body, CompareOperator, Literal, Pattern, PatternId, Statement, Term,
            TermId, UnaryOperator,
        },
        type_reference::TypeReference,
    },
    HirDatabase,
};

use super::{IntegerKind, ScalarType, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferenceResult {
    pub type_of_expression: ArenaMap<TermId, Type>,
    pub type_of_pattern: ArenaMap<PatternId, Type>,
}

impl InferenceResult {
    pub fn new(db: &dyn HirDatabase, function_id: FunctionDefinitionId) -> Self {
        let body = &db.body_of_definition(function_id);
        InferenceResultFold::fold_function(db, function_id, body).inference_result
    }
}

struct InferenceResultFold<'s> {
    pub db: &'s dyn HirDatabase,
    pub function_id: FunctionDefinitionId,
    pub body: &'s Body,
    pub resolver: Resolver,
    pub inference_result: InferenceResult,
}

impl<'s> InferenceResultFold<'s> {
    fn new(db: &'s dyn HirDatabase, function_id: FunctionDefinitionId, body: &'s Body) -> Self {
        Self {
            db,
            function_id,
            body,
            resolver: Resolver::new_root_resolver(db.upcast()),
            inference_result: InferenceResult {
                type_of_expression: ArenaMap::default(),
                type_of_pattern: ArenaMap::default(),
            },
        }
    }

    pub fn fold_function(
        db: &'s dyn HirDatabase,
        function_id: FunctionDefinitionId,
        body: &'s Body,
    ) -> Self {
        Self::new(db, function_id, body)
            .fold_function_parameters()
            .fold_body_root_expression()
    }

    fn fold_function_parameters(mut self) -> Self {
        let function = self.db.function_definition_data(self.function_id);
        self.resolver = Resolver::new_for_function(self.db.upcast(), self.function_id);
        let ty_resolver = TypeReferenceResolver::new(self.db, &self.resolver);
        let parameter_types = function
            .parameter_types
            .iter()
            .map(|type_reference| {
                ty_resolver
                    .resolve_type_reference(type_reference)
                    .expect("missing parameter type")
            })
            .collect::<Vec<_>>();

        self = self
            .body
            .parameters
            .iter()
            .zip(parameter_types)
            .fold(self, |fold, (pattern_id, ty)| {
                fold.fold_pattern(*pattern_id, ty)
            });

        self
    }

    fn fold_pattern(self, pattern_id: PatternId, expected_type: Type) -> Self {
        let (mut fold, ty) = match &self.body.patterns[pattern_id] {
            Pattern::Deconstructor(path, subpatterns) => {
                let path_resolver =
                    ValuePathResolver::new(self.db, &self.inference_result, &self.resolver);
                let expected_type = path_resolver.resolve_type_for_value_path(path);

                let subpattern_types = match expected_type {
                    Type::FunctionDefinition(CallableDefinitionId::ValueConstructor(
                        constructor_id,
                    )) => {
                        let signature =
                            self.db.callable_definition_signature(constructor_id.into());
                        signature.parameter_types
                    }
                    _ => panic!(),
                };

                let fold = subpatterns
                    .iter()
                    .zip(subpattern_types.into_iter())
                    .fold(self, |fold, (pattern_id, pattern_type)| {
                        fold.fold_pattern(*pattern_id, pattern_type)
                    });

                (fold, expected_type)
            }
            Pattern::Bind(_) => (self, expected_type),
        };
        fold.inference_result.type_of_pattern.insert(pattern_id, ty);
        fold
    }

    fn fold_body_root_expression(self) -> Self {
        let root_expression = self.body.root_expression;
        let (fold, ty) = self.fold_expression_type(root_expression);
        let ret_ty = fold
            .db
            .callable_definition_signature(fold.function_id.into())
            .return_type;
        if ty != ret_ty {
            panic!("expected {:?} found {:?}", ret_ty, ty);
        }
        fold
    }

    fn fold_expression_type(self, expr_id: TermId) -> (Self, Type) {
        let expr = &self.body.expressions[expr_id];
        let (mut fold, ty) = match expr {
            Term::Block {
                statements,
                trailing_expression,
            } => {
                let fold = statements
                    .iter()
                    .fold(self, |fold, statement| fold.fold_statement(statement));
                fold.fold_expression_type(*trailing_expression)
            }
            Term::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let (fold, condition_ty) = self.fold_expression_type(*condition);
                let (fold, then_ty) = fold.fold_expression_type(*then_branch);
                let (fold, else_ty) = fold.fold_expression_type(*else_branch);

                if condition_ty != Type::Scalar(ScalarType::Boolean) {
                    panic!("condition is not boolean");
                }

                if then_ty != else_ty {
                    panic!("mismatching types of then and else branches");
                }

                (fold, then_ty)
            }
            Term::Binary(op, lhs, rhs) => {
                let (fold, lhs_ty) = self.fold_expression_type(*lhs);
                let (fold, rhs_ty) = fold.fold_expression_type(*rhs);

                if lhs_ty != rhs_ty {
                    panic!()
                }

                let ret_ty = match op {
                    BinaryOperator::Arithmetic(_) => match rhs_ty {
                        Type::Scalar(ScalarType::Integer(_)) => rhs_ty,
                        _ => panic!(),
                    },
                    BinaryOperator::Compare(_) => Type::Scalar(ScalarType::Boolean),
                    BinaryOperator::Logic(_) => Type::Scalar(ScalarType::Boolean),
                };

                (fold, ret_ty)
            }
            Term::Unary(op, expr) => match op {
                UnaryOperator::Minus => self.fold_expression_type(*expr),
                UnaryOperator::Negation => self.fold_expression_type(*expr),
                UnaryOperator::Reference => {
                    let (fold, inner) = self.fold_expression_type(*expr);
                    (fold, Type::Pointer(Box::new(inner)))
                }
                UnaryOperator::Dereference => {
                    let (fold, inner) = self.fold_expression_type(*expr);
                    match inner {
                        Type::Pointer(inner) => (fold, *inner),
                        ty => panic!("Can not dereference type: {:?}", ty),
                    }
                }
            },
            Term::Match { matchee, case_list } => {
                let (fold, matchee_type) = self.fold_expression_type(*matchee);

                let (fold, case_types) = case_list.iter().fold(
                    (fold, Vec::new()),
                    |(mut fold, mut case_types), (pattern_id, case)| {
                        fold.resolver = Resolver::new_for_expression(
                            fold.db.upcast(),
                            fold.function_id,
                            expr_id,
                        );
                        let fold = fold.fold_pattern(*pattern_id, matchee_type.clone());
                        let (fold, case_type) = fold.fold_expression_type(*case);
                        case_types.push(case_type);
                        (fold, case_types)
                    },
                );

                let mut case_types = case_types.iter();

                if let Some(first_case_type) = case_types.next() {
                    for case_type in case_types {
                        if case_type != first_case_type {
                            panic!(
                                "on match case: Expected {:?} found {:?}",
                                first_case_type, case_type
                            );
                        }
                    }

                    (fold, first_case_type.clone())
                } else {
                    panic!("empty match case list")
                }
            }
            Term::Literal(lit) => {
                let ty = match lit {
                    Literal::Integer(value, some_kind) => match some_kind {
                        Some(int_kind) => Type::Scalar(ScalarType::Integer((*int_kind).into())),
                        None => Type::Scalar(ScalarType::Integer(IntegerKind::I64)),
                    },
                    Literal::Bool(value) => Type::Scalar(ScalarType::Boolean),
                };
                (self, ty)
            }
            Term::Path(path) => {
                let resolver =
                    Resolver::new_for_expression(self.db.upcast(), self.function_id, expr_id);
                let path_resolver =
                    ValuePathResolver::new(self.db, &self.inference_result, &resolver);
                let ty = path_resolver.resolve_type_for_value_path(path);
                (self, ty)
            }
            Term::Call { callee, arguments } => {
                let (fold, callee_type) = self.fold_expression_type(*callee);
                match callee_type {
                    Type::FunctionDefinition(f_id) => {
                        let sig = fold.db.callable_definition_signature(f_id);
                        let (fold, arg_tys) = arguments.iter().fold(
                            (fold, Vec::new()),
                            |(fold, mut arguments), arg| {
                                let (fold, arg_ty) = fold.fold_expression_type(*arg);
                                arguments.push(arg_ty);
                                (fold, arguments)
                            },
                        );
                        if sig.parameter_types != arg_tys {
                            panic!("type of parameters to function call do not match the parameters in the function definition")
                        };
                        (fold, sig.return_type)
                    }
                    x => panic!("function call not implemented for {:?} type", x),
                }
            }
            Term::New(inner) => {
                let (fold, inner) = self.fold_expression_type(*inner);
                (fold, Type::Pointer(Box::new(inner)))
            }
        };
        fold.inference_result
            .type_of_expression
            .insert(expr_id, ty.clone());
        (fold, ty)
    }

    fn fold_statement(self, statement: &'s Statement) -> InferenceResultFold {
        match statement {
            Statement::Let(pattern_id, expr_id) => {
                let (fold, expr_type) = self.fold_expression_type(*expr_id);
                let fold = fold.fold_pattern(*pattern_id, expr_type);
                fold
            }
            Statement::Expression(expr_id) => {
                let (mut fold, ty) = self.fold_expression_type(*expr_id);
                fold.inference_result
                    .type_of_expression
                    .insert(*expr_id, ty);
                fold
            }
        }
    }
}

pub(crate) struct TypeReferenceResolver<'d> {
    db: &'d dyn HirDatabase,
    resolver: &'d Resolver,
}

impl<'d> TypeReferenceResolver<'d> {
    pub fn new(db: &'d dyn HirDatabase, resolver: &'d Resolver) -> Self {
        Self { db, resolver }
    }

    pub fn resolve_type_reference(&self, type_reference: &TypeReference) -> Option<Type> {
        let ty = match type_reference {
            TypeReference::Path(path) => {
                let typed_item = self
                    .resolver
                    .resolve_path_in_type_namespace(self.db.upcast(), path)?;

                self.db.type_of_definition(typed_item.into())
            }
            TypeReference::Pointer(inner) => {
                let inner = self.resolve_type_reference(inner)?;
                Type::Pointer(Box::new(inner))
            }
            TypeReference::Refinement(inner, _name, _predicate) => {
                self.resolve_type_reference(inner)?
            }
        };
        Some(ty)
    }
}

struct ValuePathResolver<'d> {
    db: &'d dyn HirDatabase,
    resolver: &'d Resolver,
    inference_result: &'d InferenceResult,
}

impl<'d> ValuePathResolver<'d> {
    pub fn new(
        db: &'d dyn HirDatabase,
        inference_result: &'d InferenceResult,
        resolver: &'d Resolver,
    ) -> Self {
        Self {
            db,
            resolver,
            inference_result,
        }
    }

    pub fn resolve_type_for_value_path(&self, path: &Path) -> Type {
        let value_item = self
            .resolver
            .resolve_path_in_value_namespace(self.db.upcast(), path)
            .unwrap();

        let ty = match value_item {
            ValueNamespaceItem::LocalBinding(pattern_id) => self
                .inference_result
                .type_of_pattern
                .get(pattern_id)
                .expect("pattern has no type")
                .clone(),
            non_local_item => {
                let typeable_value = match non_local_item {
                    ValueNamespaceItem::Function(id) => TypeableValueDefinitionId::Function(id),
                    ValueNamespaceItem::ValueConstructor(id) => {
                        TypeableValueDefinitionId::ValueConstructor(id)
                    }
                    _ => panic!(),
                };

                self.db.type_of_value(typeable_value)
            }
        };

        ty
    }
}
