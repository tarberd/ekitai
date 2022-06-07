use la_arena::{ArenaMap, Idx};

use crate::{
    semantic_ir::{
        definition_map::{CallableDefinitionId, FunctionDefinitionId, TypeableValueDefinitionId},
        name::Name,
        path::Path,
        path_resolver::{Resolver, ValueNamespaceItem},
        refinement::{self, Predicate},
        term::{
            ArithmeticOperator, BinaryOperator, Body, CompareOperator, Literal, Pattern, PatternId,
            Statement, Term, TermId, UnaryOperator,
        },
        type_reference::TypeReference,
    },
    HirDatabase,
};

use super::{IntegerKind, LiquidType, ScalarType, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint {
    True,
    False,
}

pub type ConstraintId = Idx<Constraint>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferenceResult {
    pub type_of_expression: ArenaMap<TermId, LiquidType>,
    pub type_of_pattern: ArenaMap<PatternId, LiquidType>,
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
            .synthetize_body_root_term()
    }

    fn fold_function_parameters(mut self) -> Self {
        let function = self.db.function_definition_data(self.function_id);
        self.resolver = Resolver::new_for_function(self.db.upcast(), self.function_id);
        let ty_resolver = TypeReferenceResolver::new(self.db, &self.resolver);
        let parameter_types = function
            .parameters
            .iter()
            .map(|(pattern, type_reference)| {
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

    fn fold_pattern(self, pattern_id: PatternId, expected_type: LiquidType) -> Self {
        let (mut fold, ty) = match &self.body.patterns[pattern_id] {
            Pattern::Deconstructor(path, subpatterns) => {
                let path_resolver =
                    ValuePathResolver::new(self.db, &self.inference_result, &self.resolver);
                let expected_type = path_resolver.resolve_type_for_value_path(path);

                let subpattern_types = match expected_type {
                    LiquidType::DependentFunction(callable_def) => {
                        let signature = self.db.callable_definition_signature(callable_def);
                        signature.parameters
                    }
                    _ => panic!(),
                };

                let fold = subpatterns.iter().zip(subpattern_types.into_iter()).fold(
                    self,
                    |fold, (pattern_id, (pattern, pattern_type))| {
                        fold.fold_pattern(*pattern_id, pattern_type)
                    },
                );

                (fold, expected_type)
            }
            Pattern::Bind(_) => (self, expected_type),
        };
        fold.inference_result.type_of_pattern.insert(pattern_id, ty);
        fold
    }

    fn synthetize_body_root_term(self) -> Self {
        let root_expression = self.body.root_expression;
        let (fold, _c, ty) = self.synthetize_term(root_expression);
        let ret_ty = fold
            .db
            .callable_definition_signature(fold.function_id.into())
            .return_type;
        if ty != ret_ty {
            panic!("expected {:?} found {:?}", ret_ty, ty);
        }
        fold
    }

    fn synthetize_term(self, expr_id: TermId) -> (Self, Constraint, LiquidType) {
        let expr = &self.body.expressions[expr_id];
        let (mut fold, constraint, ty) = match expr {
            Term::Block {
                statements,
                trailing_expression,
            } => {
                let fold = statements
                    .iter()
                    .fold(self, |fold, statement| fold.fold_statement(statement));
                fold.synthetize_term(*trailing_expression)
            }
            Term::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let (fold, _c1, condition_ty) = self.synthetize_term(*condition);
                let (fold, _c2, then_ty) = fold.synthetize_term(*then_branch);
                let (fold, _c3, else_ty) = fold.synthetize_term(*else_branch);

                // if condition_ty != Type::Scalar(ScalarType::Boolean) {
                //     panic!("condition is not boolean");
                // }

                // if then_ty != else_ty {
                //     panic!("mismatching types of then and else branches");
                // }

                // (fold, Constraint::True, then_ty)
                todo!()
            }
            Term::Binary(op, lhs, rhs) => {
                let (fold, _c1, lhs_ty) = self.synthetize_term(*lhs);
                let (fold, _c2, rhs_ty) = fold.synthetize_term(*rhs);

                if lhs_ty != rhs_ty {
                    panic!()
                }

                // let application_type = match op {
                //     BinaryOperator::Arithmetic(arith) => match arith {
                //         ArithmeticOperator::Add => [
                //             Type::Scalar(ScalarType::Integer(IntegerKind::I64)),
                //             Type::Scalar(ScalarType::Integer(IntegerKind::I64)),
                //             Type::Refinement(
                //                 Box::new(Type::Scalar(ScalarType::Integer(IntegerKind::I64))),
                //                 Name::new_inline("rhs"),
                //                 Predicate::Binary(
                //                     BinaryOperator::Arithmetic(ArithmeticOperator::Add),
                //                     Box::new(Predicate::Variable(Name::new_inline("lhs"))),
                //                     Box::new(Predicate::Variable(Name::new_inline("rhs"))),
                //                 ),
                //             ),
                //         ],
                //         ArithmeticOperator::Sub => todo!(),
                //         ArithmeticOperator::Div => todo!(),
                //         ArithmeticOperator::Mul => todo!(),
                //         ArithmeticOperator::Rem => todo!(),
                //     },
                //     // BinaryOperator::Compare(_) => Type::Scalar(ScalarType::Boolean),
                //     // BinaryOperator::Logic(_) => Type::Scalar(ScalarType::Boolean),
                // };

                // let ret_ty = Type::Refinement(
                //     Box::new(Type::Scalar(ScalarType::Integer(IntegerKind::I64))),
                //     Name::new_inline("rhs"),
                //     Predicate::Binary(
                //         BinaryOperator::Arithmetic(ArithmeticOperator::Add),
                //         Box::new(Predicate::Variable(Name::new_inline("lhs"))),
                //         Box::new(Predicate::Variable(Name::new_inline("rhs"))),
                //     ),
                // );

                // (fold, Constraint::True, ret_ty)
                todo!()
            }
            Term::Unary(op, expr) => {
                // match op {
                //     UnaryOperator::Minus => self.synthetize_term(*expr),
                //     UnaryOperator::Negation => self.synthetize_term(*expr),
                //     UnaryOperator::Reference => {
                //         let (fold, _c, inner) = self.synthetize_term(*expr);
                //         (fold, Constraint::True, Type::Pointer(Box::new(inner)))
                //     }
                //     UnaryOperator::Dereference => {
                //         let (fold, _c, inner) = self.synthetize_term(*expr);
                //         match inner {
                //             Type::Pointer(inner) => (fold, Constraint::True, *inner),
                //             ty => panic!("Can not dereference type: {:?}", ty),
                //         }
                //     }
                // }
                todo!()
            }
            Term::Match { matchee, case_list } => {
                let (fold, _c, matchee_type) = self.synthetize_term(*matchee);

                let (fold, case_types) = case_list.iter().fold(
                    (fold, Vec::new()),
                    |(mut fold, mut case_types), (pattern_id, case)| {
                        fold.resolver = Resolver::new_for_expression(
                            fold.db.upcast(),
                            fold.function_id,
                            expr_id,
                        );
                        let fold = fold.fold_pattern(*pattern_id, matchee_type.clone());
                        let (fold, _c, case_type) = fold.synthetize_term(*case);
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

                    (fold, Constraint::True, first_case_type.clone())
                } else {
                    panic!("empty match case list")
                }
            }
            Term::Literal(lit) => {
                // self.synthetize_literal(lit)
                todo!()
            }
            Term::Path(path) => {
                let resolver =
                    Resolver::new_for_expression(self.db.upcast(), self.function_id, expr_id);
                let path_resolver =
                    ValuePathResolver::new(self.db, &self.inference_result, &resolver);
                let ty = path_resolver.resolve_type_for_value_path(path);
                (self, Constraint::True, ty)
            }
            Term::Call { callee, arguments } => {
                // let (fold, _c, callee_type) = self.synthetize_term(*callee);
                // match callee_type {
                //     Type::FunctionDefinition(f_id) => {
                //         let sig = fold.db.callable_definition_signature(f_id);
                //         let (fold, arg_tys) = arguments.iter().fold(
                //             (fold, Vec::new()),
                //             |(fold, mut arguments), arg| {
                //                 let (fold, _c, arg_ty) = fold.synthetize_term(*arg);
                //                 arguments.push(arg_ty);
                //                 (fold, arguments)
                //             },
                //         );
                //         if sig.parameter_types != arg_tys {
                //             panic!("type of parameters to function call do not match the parameters in the function definition")
                //         };
                //         (fold, Constraint::True, sig.return_type)
                //     }
                //     x => panic!("function call not implemented for {:?} type", x),
                // }
                todo!()
            }
            Term::New(inner) => {
                // let (fold, _c, inner) = self.synthetize_term(*inner);
                // (fold, Constraint::True, Type::Pointer(Box::new(inner)))
                todo!()
            }
        };
        fold.inference_result
            .type_of_expression
            .insert(expr_id, ty.clone());
        (fold, constraint, ty)
    }

    fn fold_statement(self, statement: &'s Statement) -> InferenceResultFold {
        match statement {
            Statement::Let(pattern_id, expr_id) => {
                let (fold, _c, expr_type) = self.synthetize_term(*expr_id);
                let fold = fold.fold_pattern(*pattern_id, expr_type);
                fold
            }
            Statement::Expression(expr_id) => {
                let (mut fold, _c, ty) = self.synthetize_term(*expr_id);
                fold.inference_result
                    .type_of_expression
                    .insert(*expr_id, ty);
                fold
            }
        }
    }

    fn synthetize_literal(self, lit: &Literal) -> (Self, Constraint, Type) {
        let ty = match lit {
            Literal::Integer(value, some_kind) => {
                let base = match some_kind {
                    Some(int_kind) => Type::Scalar(ScalarType::Integer((*int_kind).into())),
                    None => Type::Scalar(ScalarType::Integer(IntegerKind::I64)),
                };

                let name = Name { id: "lit".into() };
                let predicate = Predicate::Binary(
                    BinaryOperator::Compare(CompareOperator::Equality { negated: false }),
                    Box::new(Predicate::Variable(name.clone())),
                    Box::new(Predicate::Integer(*value)),
                );

                // Type::Refinement(Box::new(base), name, predicate)
                todo!()
            }
            Literal::Bool(value) => {
                let base = Type::Scalar(ScalarType::Boolean);
                let name = Name { id: "lit".into() };
                let predicate = match value {
                    true => Predicate::Variable(name.clone()),
                    false => Predicate::Unary(
                        refinement::UnaryOperator::Negation,
                        Box::new(Predicate::Variable(name.clone())),
                    ),
                };
                // Type::Refinement(Box::new(base), name, predicate)
                todo!()
            }
        };
        (self, Constraint::True, ty)
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

    pub fn resolve_type_reference(&self, type_reference: &TypeReference) -> Option<LiquidType> {
        let base = match type_reference {
            TypeReference::Path(path) => {
                let typed_item = self
                    .resolver
                    .resolve_path_in_type_namespace(self.db.upcast(), path)?;

                LiquidType::Base(self.db.type_of_definition(typed_item.into()))
            }
            TypeReference::Pointer(inner) => match self.resolve_type_reference(inner)? {
                LiquidType::Base(base) => LiquidType::Base(Type::Pointer(Box::new(base))),
                LiquidType::DependentFunction(_) => todo!(),
            },
            TypeReference::Refinement(inner, name, predicate) => {
                let inner = self.resolve_type_reference(inner)?;
                inner
            }
        };
        Some(base)
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

    pub fn resolve_type_for_value_path(&self, path: &Path) -> LiquidType {
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
