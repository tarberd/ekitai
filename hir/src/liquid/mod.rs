use z3::ast::{Ast, Bool, Int};

use crate::{
    check::{type_inference::TypeReferenceResolver, Type},
    semantic_ir::{
        definition_map::{FunctionDefinitionData, FunctionDefinitionId},
        name::Name,
        path::Path,
        path_resolver::Resolver,
        refinement::Predicate,
        term::{BinaryOperator, Body, CompareOperator, LogicOperator, Pattern, Term, TermId},
        type_reference::{self, TypeReference},
    },
    HirDatabase,
};

#[derive(Clone)]
struct RefinedBase {
    pub base: Type,
    pub binder: Name,
    pub predicate: Predicate,
}

struct Context {
    bindings: Vec<(Path, RefinedBase)>,
}

impl Context {
    fn get(&self, path: &Path) -> Option<RefinedBase> {
        self.bindings
            .iter()
            .find(|(to_find, _)| path == to_find)
            .map(|(_, ty)| ty.clone())
    }

    fn pop(mut self) -> (Self, Option<(Path, RefinedBase)>) {
        let opt = self.bindings.pop();
        (self, opt)
    }
}

impl FromIterator<(Path, RefinedBase)> for Context {
    fn from_iter<T: IntoIterator<Item = (Path, RefinedBase)>>(iter: T) -> Self {
        let bindings = iter.into_iter().collect();
        Self { bindings }
    }
}

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
    let resolver = Resolver::new_for_function(db.upcast(), function_id);
    let function = db.function_definition_data(function_id);
    let output_type = something(db, &resolver, &function.return_type);

    let body = db.body_of_definition(function_id.into());
    let param_types = function
        .parameter_types
        .iter()
        .map(|reference| something(db, &resolver, reference));

    let context = body
        .parameters
        .iter()
        .map(|id| {
            let pat = &body.patterns[*id];
            match pat {
                Pattern::Deconstructor(_, _) => panic!("no deconstructor on parameter"),
                Pattern::Bind(name) => Path {
                    segments: vec![name.clone()],
                },
            }
        })
        .zip(param_types.clone())
        .collect();

    let (Fold { context, .. }, constraint) = Fold {
        body: &body,
        context,
    }
    .check_type(body.root_expression, output_type);

    let constraint = body
        .parameters
        .iter()
        .map(|id| {
            let pat = &body.patterns[*id];
            match pat {
                Pattern::Deconstructor(_, _) => panic!("no deconstructor on parameter"),
                Pattern::Bind(name) => name.clone(),
            }
        })
        .zip(param_types)
        .fold(constraint, |c, (name, ty)| {
            let RefinedBase {
                base,
                binder,
                predicate,
            } = ty;
            let predicate = substitution(binder, name.clone(), predicate);
            Constraint::Implication {
                binder: name,
                base,
                antecedent: predicate,
                consequent: Box::new(c),
            }
        });

    entailment(context, constraint)
}

fn entailment(context: Context, constraint: Constraint) -> bool {
    match context.pop() {
        (_, None) => solve(constraint),
        (tail, Some((path, refinement))) => {
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
                    consequent: Box::new(constraint),
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
                BinaryOperator::Arithmetic(_) => todo!(),
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
                    CompareOperator::Equality { negated: false } => match (lhs, rhs) {
                        (Z3Predicate::Bool(lhs), Z3Predicate::Bool(rhs)) => lhs._eq(&rhs).into(),
                        (Z3Predicate::Int(lhs), Z3Predicate::Int(rhs)) => lhs._eq(&rhs).into(),
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
                    _ => todo!(),
                },
                _ => todo!(),
            }
        }
        Predicate::Unary(_, _) => todo!(),
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
                        Box::new(sub_antecedent),
                        Box::new(antecedent.clone()),
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
}

impl<'a> Fold<'a> {
    fn check_type(self, term_id: TermId, output_type: RefinedBase) -> (Self, Constraint) {
        let term = &self.body.expressions[term_id];
        match term {
            _ => {
                let (fold, t, c) = self.synth_type(term_id);
                let (fold, c2) = fold.subtype(t, output_type);

                (
                    fold,
                    match c {
                        Some(c) => Constraint::Conjunction(Box::new(c), Box::new(c2)),
                        None => c2,
                    },
                )
            }
        }
    }

    fn synth_type(self, term_id: TermId) -> (Self, RefinedBase, Option<Constraint>) {
        let term = &self.body.expressions[term_id];
        match term {
            Term::Block {
                statements: _,
                trailing_expression,
            } => self.synth_type(*trailing_expression),

            Term::Path(path) => {
                let RefinedBase {
                    base,
                    binder,
                    predicate,
                } = self.context.get(path).unwrap().clone();

                let path_type = RefinedBase {
                    base,
                    binder: binder.clone(),
                    predicate: Predicate::Binary(
                        BinaryOperator::Logic(LogicOperator::And),
                        Box::new(predicate),
                        Box::new(Predicate::Binary(
                            BinaryOperator::Compare(CompareOperator::Equality { negated: false }),
                            Box::new(Predicate::Variable(binder)),
                            Box::new(Predicate::Variable(path.as_name())),
                        )),
                    ),
                };
                (self, path_type, None)
            }
            _ => todo!(),
        }
    }

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

        let constraint = Constraint::Implication {
            binder: lesser_binder.clone(),
            base: lesser_base,
            antecedent: lesser_predicate,
            consequent: Box::new(Constraint::Predicate(substitution(
                greater_binder,
                lesser_binder,
                greater_predicate,
            ))),
        };

        (self, constraint)
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
            Box::new(substitution(old.clone(), new.clone(), *lhs)),
            Box::new(substitution(old, new, *rhs)),
        ),
        Predicate::Unary(op, predicate) => {
            Predicate::Unary(op, Box::new(substitution(old, new, *predicate)))
        }
        Predicate::Boolean(_) | Predicate::Integer(_) => predicate,
    }
}