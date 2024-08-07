use core::panic;
use std::vec;

use la_arena::Idx;
use z3::ast::{Ast, Bool, Int};

use crate::{
    check::{
        type_inference::{InferenceResult, TypeReferenceResolver},
        IntegerKind, ScalarType, Type,
    },
    semantic_ir::{
        definition_map::{FunctionDefinitionData, FunctionDefinitionId},
        intrinsic::BuiltinInteger,
        name::Name,
        path::Path,
        path_resolver::Resolver,
        refinement::{Predicate, UnaryOperator},
        term::{
            ArithmeticOperator, BinaryOperator, Body, CompareOperator, Literal, LogicOperator,
            Pattern, PatternId, Statement, Term, TermId, UnaryOperator as TermUnaryOp,
        },
        type_reference::TypeReference,
    },
    HirDatabase,
};

#[derive(Debug, Clone)]
struct RefinedBase {
    pub base: Type,
    pub binder: Name,
    pub predicate: Predicate,
}

#[derive(Debug, Clone)]
struct DependentFunction {
    pub parameter: (Name, RefinedBase),
    pub tail_type: Box<RefinedType>,
}

#[derive(Debug, Clone)]
enum RefinedType {
    Base(RefinedBase),
    Fn(DependentFunction),
}

impl RefinedType {
    fn as_refined_base(self) -> RefinedBase {
        match self {
            RefinedType::Base(base) => base,
            _ => panic!("Not a RefinedBase."),
        }
    }

    fn as_dependent_function(self) -> DependentFunction {
        match self {
            RefinedType::Fn(func) => func,
            _ => panic!("Not a DependentFunction."),
        }
    }
}

impl From<RefinedBase> for RefinedType {
    fn from(base: RefinedBase) -> Self {
        Self::Base(base)
    }
}

impl From<DependentFunction> for RefinedType {
    fn from(fun: DependentFunction) -> Self {
        Self::Fn(fun)
    }
}

#[derive(Debug, Clone)]
struct Context {
    bindings: Vec<(Path, RefinedType)>,
}

impl Context {
    fn get(&self, path: &Path) -> Option<RefinedType> {
        self.bindings
            .iter()
            .find(|(to_find, _)| path == to_find)
            .map(|(_, ty)| ty.clone())
    }

    fn pop(mut self) -> (Self, Option<(Path, RefinedType)>) {
        let opt = self.bindings.pop();
        (self, opt)
    }
}

impl FromIterator<(Path, RefinedType)> for Context {
    fn from_iter<T: IntoIterator<Item = (Path, RefinedType)>>(iter: T) -> Self {
        let bindings = iter.into_iter().collect();
        Self { bindings }
    }
}

#[derive(Debug)]
enum Constraint {
    Predicate(Predicate),
    Implication {
        binder: Name,
        base: Type,
        antecedent: Predicate,
        consequent: Box<Self>,
    },
    Conjunction(Box<Self>, Box<Self>),
}

impl Constraint {
    fn make_conjunction(left: Option<Self>, right: Self) -> Self {
        match left {
            Some(left) => Self::Conjunction(left.into(), right.into()),
            None => right,
        }
    }
}

fn something(
    db: &dyn HirDatabase,
    resolver: &Resolver,
    type_reference: &TypeReference,
) -> RefinedBase {
    match type_reference {
        TypeReference::Path(_) => todo!(),
        TypeReference::Refinement(inner, binder, predicate) => {
            let type_ref_resolver = TypeReferenceResolver::new(db, resolver);
            let inner_type = type_ref_resolver.resolve_type_reference(&inner).unwrap();
            RefinedBase {
                base: inner_type,
                binder: binder.clone(),
                predicate: predicate.clone(),
            }
        }
        TypeReference::Pointer(_) => todo!(),
    }
}

pub fn check_abstraction(db: &dyn HirDatabase, function_id: FunctionDefinitionId) -> bool {
    let context = {
        db.source_file_definitions_map()
            .root_module_item_scope()
            .iter_function_locations()
            .map(|fid| {
                let FunctionDefinitionData { name, .. } = db.function_definition_data(*fid);
                (
                    Path {
                        segments: vec![name],
                    },
                    make_function_type(db, *fid),
                )
            })
            .collect()
    };

    let function_type = make_function_type(db, function_id);
    let body = db.body_of_definition(function_id);

    let (context, constraint) = match function_type {
        RefinedType::Base(base) => {
            let (Fold { context, .. }, constraint) = Fold {
                body: &body,
                context,
                inference: db.infer_body_expression_types(function_id),
                fresh_var_counter: 0,
            }
            .check_type(body.root_expression, base);
            (context, constraint)
        }
        RefinedType::Fn(depfn) => {
            let (Fold { context, .. }, constraint) = Fold {
                body: &body,
                context,
                inference: db.infer_body_expression_types(function_id),
                fresh_var_counter: 0,
            }
            .check_abstraction_type(depfn, body.root_expression);
            (context, constraint)
        }
    };

    println!("Context: {:?}", context);
    println!("Constraint: {:?}", constraint);
    entailment(context, constraint)
}

fn make_function_type(db: &dyn HirDatabase, function_id: FunctionDefinitionId) -> RefinedType {
    let resolver = Resolver::new_for_function(db.upcast(), function_id);
    let function = db.function_definition_data(function_id);
    let output_type = something(db, &resolver, &function.return_type);

    let body = db.body_of_definition(function_id.into());
    let param_types = function
        .parameter_types
        .iter()
        .map(|reference| something(db, &resolver, reference));

    let function_type = body
        .parameters
        .iter()
        .map(|id| {
            let pat = &body.patterns[*id];
            match pat {
                Pattern::Deconstructor(_, _) => panic!("no deconstructor on parameter"),
                Pattern::Bind(name) => name,
            }
        })
        .cloned()
        .zip(param_types.clone())
        .rfold(
            RefinedType::Base(output_type),
            |tail_type, (param_name, param_type)| {
                DependentFunction {
                    parameter: (param_name, param_type),
                    tail_type: tail_type.into(),
                }
                .into()
            },
        );

    function_type
}

fn entailment(context: Context, constraint: Constraint) -> bool {
    match context.pop() {
        (_, None) => solve(constraint),
        (tail, Some((_path, RefinedType::Fn(_)))) => entailment(tail, constraint),
        (tail, Some((path, RefinedType::Base(refinement)))) => {
            let RefinedBase {
                binder,
                predicate,
                base,
            } = refinement;

            let context_binder = path.as_name();
            let predicate = substitution(binder, context_binder.clone(), predicate);

            entailment(
                tail,
                Constraint::Implication {
                    binder: context_binder,
                    base,
                    antecedent: predicate,
                    consequent: constraint.into(),
                },
            )
        }
    }
}

#[derive(Debug, Clone)]
enum Z3Predicate<'ctx> {
    Bool(Bool<'ctx>),
    Int(Int<'ctx>),
}

impl<'ctx> From<Bool<'ctx>> for Z3Predicate<'ctx> {
    fn from(from: Bool<'ctx>) -> Self {
        Z3Predicate::Bool(from)
    }
}

impl<'ctx> From<Int<'ctx>> for Z3Predicate<'ctx> {
    fn from(from: Int<'ctx>) -> Self {
        Z3Predicate::Int(from)
    }
}

fn solve(constraint: Constraint) -> bool {
    let flattened_constraint = flatten(constraint);

    let solver_config = z3::Config::new();
    let solver_context = z3::Context::new(&solver_config);
    let solver = z3::Solver::new(&solver_context);

    for constraint in flattened_constraint {
        let constraint = lower(&solver_context, constraint);

        solver.push();
        solver.assert(&constraint);
        println!("Solver:\n{solver}");
        let result = solver.check();
        println!("Result: {result:?}");
        solver.pop(1);

        match result {
            z3::SatResult::Unsat => continue,
            z3::SatResult::Unknown => return false,
            z3::SatResult::Sat => return false,
        };
    }

    true
}

fn lower(context: &z3::Context, constraint: FlatImplicationConstraint) -> Bool {
    let FlatImplicationConstraint {
        binders,
        antecedent,
        consequent,
    } = constraint;

    let variables: Vec<(Name, Z3Predicate)> = binders
        .into_iter()
        .map(|(name, base)| match base {
            Type::AbstractDataType(_) => todo!("no abstract data types supported in liquid terms"),
            Type::FunctionDefinition(_) => todo!("no function data types in liquid terms"),
            Type::Pointer(_) => todo!("no pointer type in liquid terms"),
            Type::Scalar(scalar) => match scalar {
                crate::check::ScalarType::Integer(_) => (
                    name.clone(),
                    Int::new_const(context, name.id.as_str()).into(),
                ),
                crate::check::ScalarType::Boolean => (
                    name.clone(),
                    Bool::new_const(context, name.id.as_str()).into(),
                ),
            },
        })
        .collect();

    let antecedent = lower_predicate(context, &variables, antecedent);
    let consequent = lower_predicate(context, &variables, consequent);

    match (antecedent, consequent) {
        (Z3Predicate::Bool(prepo), Z3Predicate::Bool(conse)) => {
            Bool::and(context, &[&prepo, &conse.not()])
        }
        _ => todo!(),
    }
}

fn lower_predicate<'ctx>(
    context: &'ctx z3::Context,
    variables: &Vec<(Name, Z3Predicate<'ctx>)>,
    predicate: Predicate,
) -> Z3Predicate<'ctx> {
    match predicate {
        Predicate::Variable(name) => {
            let x = variables
                .iter()
                .find(|(to_find, _)| name == *to_find)
                .map(|(_, ty)| ty)
                .expect("liquid variable not in context");
            x.clone()
        }
        Predicate::Boolean(value) => Bool::from_bool(context, value).into(),
        Predicate::Integer(value) => Int::from_u64(context, value as u64).into(),
        Predicate::Binary(op, lhs, rhs) => {
            let lhs = lower_predicate(context, variables, *lhs);
            let rhs = lower_predicate(context, variables, *rhs);
            match op {
                BinaryOperator::Arithmetic(arith_op) => {
                    let (lhs, rhs) = match (lhs, rhs) {
                        (Z3Predicate::Int(lhs), Z3Predicate::Int(rhs)) => (lhs, rhs),
                        _ => panic!(),
                    };
                    match arith_op {
                        ArithmeticOperator::Add => Int::add(context, &[&lhs, &rhs]),
                        ArithmeticOperator::Sub => Int::sub(context, &[&lhs, &rhs]),
                        ArithmeticOperator::Div => lhs.div(&rhs),
                        ArithmeticOperator::Mul => Int::mul(context, &[&lhs, &rhs]),
                        ArithmeticOperator::Rem => lhs.rem(&rhs),
                    }
                    .into()
                }
                BinaryOperator::Logic(op) => {
                    let (lhs, rhs) = match (lhs, rhs) {
                        (Z3Predicate::Bool(lhs), Z3Predicate::Bool(rhs)) => (lhs, rhs),
                        _ => todo!(),
                    };
                    match op {
                        LogicOperator::And => Bool::and(context, &[&lhs, &rhs]).into(),
                        LogicOperator::Or => Bool::or(context, &[&lhs, &rhs]).into(),
                    }
                }
                BinaryOperator::Compare(compare) => match compare {
                    CompareOperator::Equality { negated } => match (lhs, rhs) {
                        (Z3Predicate::Bool(lhs), Z3Predicate::Bool(rhs)) => match negated {
                            false => lhs._eq(&rhs).into(),
                            true => Bool::distinct(context, &[&lhs, &rhs]).into(),
                        },
                        (Z3Predicate::Int(lhs), Z3Predicate::Int(rhs)) => match negated {
                            false => lhs._eq(&rhs).into(),
                            true => Int::distinct(context, &[&lhs, &rhs]).into(),
                        },
                        _ => panic!("mismatch types in z3"),
                    },
                    CompareOperator::Order { ordering, strict } => match ordering {
                        crate::semantic_ir::term::Ordering::Less => match (lhs, rhs) {
                            (Z3Predicate::Int(lhs), Z3Predicate::Int(rhs)) => match strict {
                                true => lhs.lt(&rhs).into(),
                                false => lhs.le(&rhs).into(),
                            },
                            _ => todo!(),
                        },
                        crate::semantic_ir::term::Ordering::Greater => match (lhs, rhs) {
                            (Z3Predicate::Int(lhs), Z3Predicate::Int(rhs)) => match strict {
                                true => lhs.gt(&rhs).into(),
                                false => lhs.ge(&rhs).into(),
                            },
                            _ => todo!(),
                        },
                    },
                },
            }
        }
        Predicate::Unary(op, predicate) => {
            let predicate = lower_predicate(context, variables, *predicate);
            match (op, predicate) {
                (UnaryOperator::Minus, Z3Predicate::Int(int)) => int.unary_minus().into(),
                (UnaryOperator::Negation, Z3Predicate::Bool(boolean)) => boolean.not().into(),
                (UnaryOperator::Minus, Z3Predicate::Bool(_)) => todo!(),
                (UnaryOperator::Negation, Z3Predicate::Int(_)) => todo!(),
            }
        }
    }
}

struct FlatImplicationConstraint {
    binders: Vec<(Name, Type)>,
    antecedent: Predicate,
    consequent: Predicate,
}

fn flatten(constraint: Constraint) -> Vec<FlatImplicationConstraint> {
    flatten_fold(Vec::new(), constraint)
}

fn flatten_fold(
    mut flattened: Vec<FlatImplicationConstraint>,
    constraint: Constraint,
) -> Vec<FlatImplicationConstraint> {
    match constraint {
        Constraint::Predicate(predicate) => {
            flattened.push(FlatImplicationConstraint {
                binders: vec![],
                antecedent: Predicate::Boolean(true),
                consequent: predicate,
            });
            flattened
        }
        Constraint::Implication {
            binder,
            base,
            antecedent,
            consequent,
        } => {
            flattened.extend(flatten(*consequent).into_iter().map(
                |FlatImplicationConstraint {
                     mut binders,
                     antecedent: sub_antecedent,
                     consequent,
                 }| {
                    let binders = {
                        binders.push((binder.clone(), base.clone()));
                        binders
                    };
                    let antecedent = Predicate::Binary(
                        BinaryOperator::Logic(LogicOperator::And),
                        sub_antecedent.into(),
                        antecedent.clone().into(),
                    );
                    FlatImplicationConstraint {
                        binders,
                        antecedent,
                        consequent,
                    }
                },
            ));
            flattened
        }
        Constraint::Conjunction(first, second) => {
            let flattened = flatten_fold(flattened, *first);
            flatten_fold(flattened, *second)
        }
    }
}

struct Fold<'a> {
    body: &'a Body,
    context: Context,
    inference: InferenceResult,
    fresh_var_counter: usize,
}

impl<'a> Fold<'a> {
    fn subtype(self, lesser: RefinedBase, greater: RefinedBase) -> (Self, Constraint) {
        let RefinedBase {
            base: lesser_base,
            binder: lesser_binder,
            predicate: lesser_predicate,
        } = lesser;
        let RefinedBase {
            base: greater_base,
            binder: greater_binder,
            predicate: greater_predicate,
        } = greater;

        if lesser_base != greater_base {
            panic!("missmatch base types: {lesser_base:?} <: {greater_base:?}")
        }

        let constraint = Constraint::Implication {
            binder: lesser_binder.clone(),
            base: lesser_base,
            antecedent: lesser_predicate,
            consequent: Constraint::Predicate(substitution(
                greater_binder,
                lesser_binder,
                greater_predicate,
            ))
            .into(),
        };

        (self, constraint)
    }

    fn check_abstraction_type(
        mut self,
        constraint_type: DependentFunction,
        body_term: TermId,
    ) -> (Self, Constraint) {
        let DependentFunction {
            parameter: (arg_name, arg_ty),
            tail_type,
        } = constraint_type;
        //add to context
        let arg_path = Path {
            segments: vec![arg_name.clone()],
        };
        self.context
            .bindings
            .push((arg_path, arg_ty.clone().into()));
        //check
        let (fold, constraint) = match *tail_type {
            RefinedType::Base(base) => self.check_type(body_term, base),
            RefinedType::Fn(depfn) => self.check_abstraction_type(depfn, body_term),
        };
        //implication constraint
        let constraint = implication_constraint(arg_name, arg_ty, Some(constraint));
        (fold, constraint)
    }

    fn check_type(mut self, term_id: TermId, constraint_type: RefinedBase) -> (Self, Constraint) {
        let term = &self.body.expressions[term_id];
        match term {
            Term::Block {
                statements,
                trailing_expression,
            } => {
                let (fold, constraints) =
                    statements
                        .iter()
                        .fold(
                            (self, vec![]),
                            |(fold, mut constraints), statement| match statement {
                                Statement::Let(pattern, init_term) => {
                                    let (mut fold, ty, c) = fold.synth_type(*init_term);
                                    let pattern = &fold.body.patterns[*pattern];
                                    let (path, name) = match pattern {
                                        Pattern::Bind(name) => (
                                            Path {
                                                segments: vec![name.clone()],
                                            },
                                            name.clone(),
                                        ),
                                        _ => todo!(),
                                    };
                                    fold.context.bindings.push((path, ty.clone()));
                                    constraints.push((name, ty, c));
                                    (fold, constraints)
                                }
                                Statement::Expression(_) => todo!(),
                            },
                        );
                let (fold, c) = fold.check_type(*trailing_expression, constraint_type);
                let constraint = constraints.into_iter().rfold(
                    Some(c),
                    |inner_constraint, (name, ty, outer_constraint)| {
                        let implication_constraint =
                            implication_constraint(name, ty.as_refined_base(), inner_constraint);
                        Some(Constraint::make_conjunction(
                            outer_constraint,
                            implication_constraint,
                        ))
                    },
                );
                (fold, constraint.unwrap())
            }
            Term::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let fresh_var = self.make_fresh_var();
                let (fold, constraint) = self.check_type(
                    *condition,
                    RefinedBase {
                        base: Type::Scalar(ScalarType::Boolean),
                        binder: Name::new_inline("b"),
                        predicate: Predicate::Boolean(true),
                    },
                );
                if !entailment(fold.context.clone(), constraint) {
                    panic!("if condition not a valid boolean")
                }

                let (fold, constraint) = fold.check_type(*then_branch, constraint_type.clone());
                let c1 = implication_constraint(
                    fresh_var.clone(),
                    RefinedBase {
                        base: Type::Scalar(ScalarType::Boolean),
                        binder: Name::new_inline("branch_"),
                        predicate: Predicate::Variable(fold.term_as_name(*condition)),
                    },
                    Some(constraint),
                );
                let (fold, constraint) = fold.check_type(*else_branch, constraint_type);
                let c2 = implication_constraint(
                    fresh_var,
                    RefinedBase {
                        base: Type::Scalar(ScalarType::Boolean),
                        binder: Name::new_inline("branch_"),
                        predicate: Predicate::Unary(
                            UnaryOperator::Negation,
                            Predicate::Variable(fold.term_as_name(*condition)).into(),
                        ),
                    },
                    Some(constraint),
                );
                (fold, Constraint::Conjunction(c1.into(), c2.into()))
            }
            _ => {
                let (fold, t, c) = self.synth_type(term_id);
                let (fold, c2) = fold.subtype(t.as_refined_base(), constraint_type);
                (fold, Constraint::make_conjunction(c, c2))
            }
        }
    }

    fn synth_type(self, term_id: TermId) -> (Self, RefinedType, Option<Constraint>) {
        let term = &self.body.expressions[term_id];
        match term {
            Term::Block {
                statements,
                trailing_expression,
            } => {
                let (fold, constraints) =
                    statements
                        .iter()
                        .fold(
                            (self, vec![]),
                            |(fold, mut constraints), statement| match statement {
                                Statement::Let(pattern, init_term) => {
                                    let (mut fold, ty, c) = fold.synth_type(*init_term);
                                    let pattern = &fold.body.patterns[*pattern];
                                    let (path, name) = match pattern {
                                        Pattern::Bind(name) => (
                                            Path {
                                                segments: vec![name.clone()],
                                            },
                                            name.clone(),
                                        ),
                                        _ => todo!(),
                                    };
                                    fold.context.bindings.push((path, ty.clone()));
                                    constraints.push((name, ty, c));
                                    (fold, constraints)
                                }
                                Statement::Expression(_) => todo!(),
                            },
                        );
                let (fold, base, c) = fold.synth_type(*trailing_expression);
                let constraint = constraints.into_iter().rfold(
                    c,
                    |inner_constraint, (name, ty, outer_constraint)| {
                        let implication_constraint =
                            implication_constraint(name, ty.as_refined_base(), inner_constraint);
                        Some(Constraint::make_conjunction(
                            outer_constraint,
                            implication_constraint,
                        ))
                    },
                );
                (fold, base, constraint)
            }
            Term::Path(path) => match self
                .context
                .get(path)
                .expect(format!("{path:?} not in context.").as_str())
                .clone()
            {
                RefinedType::Base(RefinedBase {
                    base,
                    binder,
                    predicate,
                }) => {
                    assert!(binder != path.as_name());

                    let path_type = RefinedBase {
                        base,
                        binder: binder.clone(),
                        predicate: Predicate::Binary(
                            BinaryOperator::Logic(LogicOperator::And),
                            predicate.into(),
                            Predicate::Binary(
                                BinaryOperator::Compare(CompareOperator::Equality {
                                    negated: false,
                                }),
                                Predicate::Variable(binder).into(),
                                Predicate::Variable(path.as_name()).into(),
                            )
                            .into(),
                        ),
                    };
                    (self, path_type.into(), None)
                }
                dependent_fn => (self, dependent_fn, None),
            },
            Term::Literal(lit) => match lit {
                Literal::Integer(value, sufix) => {
                    (self, primitive_integer(*value, *sufix).into(), None)
                }
                Literal::Bool(value) => (self, primitive_bool(*value).into(), None),
            },
            Term::Unary(op, term) => self.synth_unary_term(op, *term),
            Term::Binary(op, lhs, rhs) => self.synth_binary_term(op, *lhs, *rhs).into(),
            Term::Call { callee, arguments } => {
                self.synth_call_term(*callee, arguments.clone()).into()
            }
            term => panic!("Unhandled term {:?}", term),
        }
    }

    pub(crate) fn synth_unary_term(
        self,
        op: &'a TermUnaryOp,
        term: Idx<Term>,
    ) -> (Fold, RefinedType, Option<Constraint>) {
        match op {
            TermUnaryOp::Minus => {
                let minus_signature = DependentFunction {
                    parameter: (
                        Name::new_inline("param0"),
                        RefinedBase {
                            base: Type::Scalar(ScalarType::Integer(IntegerKind::I64)),
                            binder: Name::new_inline("param0"),
                            predicate: Predicate::Boolean(true),
                        },
                    ),
                    tail_type: RefinedType::Base(RefinedBase {
                        base: Type::Scalar(ScalarType::Integer(IntegerKind::I64)),
                        binder: Name::new_inline("ret"),
                        predicate: Predicate::Binary(
                            BinaryOperator::Compare(CompareOperator::Equality { negated: false }),
                            Predicate::Variable(Name::new_inline("ret")).into(),
                            Predicate::Unary(
                                UnaryOperator::Minus,
                                Predicate::Variable(Name::new_inline("param0")).into(),
                            )
                            .into(),
                        ),
                    })
                    .into(),
                };

                let (fold, ty, constraint) =
                    self.synth_function_call(minus_signature, vec![term], None);
                (fold, ty, constraint)
            }
            TermUnaryOp::Negation => {
                let minus_signature = DependentFunction {
                    parameter: (
                        Name::new_inline("param0"),
                        RefinedBase {
                            base: Type::Scalar(ScalarType::Boolean),
                            binder: Name::new_inline("param0"),
                            predicate: Predicate::Boolean(true),
                        },
                    ),
                    tail_type: RefinedType::Base(RefinedBase {
                        base: Type::Scalar(ScalarType::Boolean),
                        binder: Name::new_inline("ret"),
                        predicate: Predicate::Binary(
                            BinaryOperator::Compare(CompareOperator::Equality { negated: false }),
                            Predicate::Variable(Name::new_inline("ret")).into(),
                            Predicate::Unary(
                                UnaryOperator::Negation,
                                Predicate::Variable(Name::new_inline("param0")).into(),
                            )
                            .into(),
                        ),
                    })
                    .into(),
                };

                let (fold, ty, constraint) =
                    self.synth_function_call(minus_signature, vec![term], None);
                (fold, ty, constraint)
            }
            TermUnaryOp::Reference => todo!(),
            TermUnaryOp::Dereference => todo!(),
        }
    }

    fn synth_binary_term(
        self,
        op: &'a BinaryOperator,
        lhs: Idx<Term>,
        rhs: Idx<Term>,
    ) -> (Fold, RefinedType, Option<Constraint>) {
        let lhs_ty = self.inference.type_of_expression[lhs].clone();
        let rhs_ty = self.inference.type_of_expression[rhs].clone();
        let sum_signature = DependentFunction {
            parameter: (
                Name::new_inline("param0"),
                RefinedBase {
                    base: lhs_ty.clone(),
                    binder: Name::new_inline("param0"),
                    predicate: Predicate::Boolean(true),
                },
            ),
            tail_type: RefinedType::Fn(DependentFunction {
                parameter: (
                    Name::new_inline("param1"),
                    RefinedBase {
                        base: rhs_ty,
                        binder: Name::new_inline("param1"),
                        predicate: Predicate::Boolean(true),
                    },
                ),
                tail_type: RefinedType::Base(RefinedBase {
                    base: match op {
                        BinaryOperator::Arithmetic(_) => lhs_ty,
                        BinaryOperator::Logic(_) | BinaryOperator::Compare(_) => {
                            Type::Scalar(ScalarType::Boolean)
                        }
                    },
                    binder: Name::new_inline("ret"),
                    predicate: Predicate::Binary(
                        BinaryOperator::Compare(CompareOperator::Equality { negated: false }),
                        Predicate::Variable(Name::new_inline("ret")).into(),
                        Predicate::Binary(
                            *op,
                            Predicate::Variable(Name::new_inline("param0")).into(),
                            Predicate::Variable(Name::new_inline("param1")).into(),
                        )
                        .into(),
                    ),
                })
                .into(),
            })
            .into(),
        };

        let (fold, ty, constraint) = self.synth_function_call(sum_signature, vec![lhs, rhs], None);
        (fold, ty, constraint)
    }

    fn synth_call_term(
        self,
        callee: Idx<Term>,
        arguments: Vec<Idx<Term>>,
    ) -> (Self, RefinedType, Option<Constraint>) {
        let (fold, function_ty, constraint) = self.synth_type(callee);

        let (fold, ret_ty, constraint) =
            fold.synth_function_call(function_ty.as_dependent_function(), arguments, constraint);
        (fold, ret_ty, constraint)
    }

    fn synth_function_call(
        self,
        function_ty: DependentFunction,
        mut argument_terms: Vec<TermId>,
        constraint: Option<Constraint>,
    ) -> (Self, RefinedType, Option<Constraint>) {
        if argument_terms.is_empty() {
            (self, function_ty.into(), constraint)
        } else {
            let argument = argument_terms.pop().unwrap();
            // synth
            let (fold, ty, constraint) =
                self.synth_function_call(function_ty, argument_terms, constraint);

            let function_ty = ty.as_dependent_function();
            let (parameter_name, argument_type) = function_ty.parameter;

            // check
            let (fold, c) = fold.check_type(argument, argument_type.clone());

            let argument_name = fold.term_as_name(argument);

            // substitue
            let return_type =
                substitution_in_refined_type(*function_ty.tail_type, parameter_name, argument_name);
            (
                fold,
                return_type.into(),
                Some(Constraint::make_conjunction(constraint, c)),
            )
        }
    }

    fn make_fresh_var(&mut self) -> Name {
        let fresh = self.fresh_var_counter;
        self.fresh_var_counter += 1;
        Name {
            id: format!("__fresh{}", fresh).into(),
        }
    }

    fn term_as_name(&self, term_id: TermId) -> Name {
        match &self.body.expressions[term_id] {
            Term::Path(path) => path.as_name(),
            _ => panic!("not a path term"),
        }
    }
}

fn substitution_in_refined_base(ty: RefinedBase, old_name: Name, new_name: Name) -> RefinedBase {
    let RefinedBase {
        base,
        binder,
        predicate,
    } = ty;
    if new_name == binder {
        let new_binder = Name::new_inline(format!("{}{}", binder.id.as_str(), 1).as_str());
        substitution_in_refined_base(
            RefinedBase {
                base,
                binder: new_binder.clone(),
                predicate: substitution(binder, new_binder, predicate),
            },
            old_name,
            new_name,
        )
    } else if old_name == binder {
        RefinedBase {
            base,
            binder,
            predicate,
        }
    } else {
        RefinedBase {
            base,
            binder,
            predicate: substitution(old_name, new_name, predicate),
        }
    }
}

fn substitution_in_function_type(
    function_ty: DependentFunction,
    old_name: Name,
    new_name: Name,
) -> DependentFunction {
    let DependentFunction {
        parameter: (arg_name, arg_type),
        tail_type,
    } = function_ty;
    if new_name == arg_name {
        let new_arg_name = Name::new_inline(format!("{}{}", arg_name.id.as_str(), 1).as_str());
        substitution_in_function_type(
            DependentFunction {
                parameter: (
                    new_arg_name.clone(),
                    substitution_in_refined_base(arg_type, arg_name, new_arg_name),
                ),
                tail_type,
            },
            old_name,
            new_name,
        )
    } else if old_name == arg_name {
        DependentFunction {
            parameter: (
                arg_name,
                substitution_in_refined_base(arg_type, old_name, new_name),
            ),
            tail_type,
        }
    } else {
        DependentFunction {
            parameter: (
                arg_name,
                substitution_in_refined_base(arg_type, old_name.clone(), new_name.clone()),
            ),
            tail_type: substitution_in_refined_type(*tail_type, old_name, new_name).into(),
        }
    }
}

fn substitution_in_refined_type(ty: RefinedType, old_name: Name, new_name: Name) -> RefinedType {
    match ty {
        RefinedType::Base(base) => substitution_in_refined_base(base, old_name, new_name).into(),
        RefinedType::Fn(func) => substitution_in_function_type(func, old_name, new_name).into(),
    }
}

fn primitive_bool(value: bool) -> RefinedBase {
    let binder = Name::new_inline("lit");
    RefinedBase {
        base: Type::Scalar(ScalarType::Boolean),
        binder: binder.clone(),
        predicate: match value {
            true => Predicate::Variable(binder),
            false => Predicate::Unary(UnaryOperator::Negation, Predicate::Variable(binder).into()),
        },
    }
}

fn primitive_integer(value: u128, sufix: Option<BuiltinInteger>) -> RefinedBase {
    let binder = Name::new_inline("lit");
    RefinedBase {
        base: Type::Scalar(ScalarType::Integer(
            sufix.map_or(IntegerKind::I64, Into::into),
        )),
        binder: binder.clone(),
        predicate: Predicate::Binary(
            BinaryOperator::Compare(CompareOperator::Equality { negated: false }),
            Predicate::Variable(binder).into(),
            Predicate::Integer(value).into(),
        ),
    }
}

fn substitution(old: Name, new: Name, predicate: Predicate) -> Predicate {
    match predicate.clone() {
        Predicate::Variable(name) => {
            if name == old {
                Predicate::Variable(new)
            } else {
                predicate
            }
        }
        Predicate::Binary(op, lhs, rhs) => Predicate::Binary(
            op,
            substitution(old.clone(), new.clone(), *lhs).into(),
            substitution(old, new, *rhs).into(),
        ),
        Predicate::Unary(op, predicate) => {
            Predicate::Unary(op, substitution(old, new, *predicate).into())
        }
        Predicate::Boolean(_) | Predicate::Integer(_) => predicate,
    }
}

fn implication_constraint(
    name: Name,
    ty: RefinedBase,
    constraint: Option<Constraint>,
) -> Constraint {
    let RefinedBase {
        base,
        binder,
        predicate,
    } = ty;

    let constraint = Constraint::Implication {
        binder: name.clone(),
        base,
        antecedent: substitution(binder, name, predicate),
        consequent: constraint
            .unwrap_or(Constraint::Predicate(Predicate::Boolean(true)))
            .into(),
    };

    constraint
}
