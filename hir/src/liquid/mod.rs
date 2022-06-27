use std::collections::HashMap;

use crate::{
    check::{type_inference::TypeReferenceResolver, Type},
    semantic_ir::{
        definition_map::{FunctionDefinitionData, FunctionDefinitionId},
        path::Path,
        path_resolver::Resolver,
        refinement::Predicate,
        term::{Body, Pattern, Term, TermId},
        type_reference::{self, TypeReference}, name::Name,
    },
    HirDatabase,
};

#[derive(Clone)]
struct RefinedBase {
    pub base: Type,
    pub binder: Name,
    pub predicate: Predicate,
}

struct Scope {
    bindings: Vec<(Path, RefinedBase)>,
}

impl Scope {
    fn get(&self, path: &Path) -> Option<RefinedBase> {
        self.bindings
            .iter()
            .find(|(to_find, _)| path == to_find)
            .map(|(_, ty)| ty.clone())
    }
}

impl FromIterator<(Path, RefinedBase)> for Scope {
    fn from_iter<T: IntoIterator<Item = (Path, RefinedBase)>>(iter: T) -> Self {
        let bindings = iter.into_iter().collect();
        Self { bindings }
    }
}

struct Context {
    scopes: Vec<Scope>,
}

impl Context {
    fn get(&self, path: &Path) -> Option<RefinedBase> {
        self.scopes.iter().find_map(|scope| scope.get(path))
    }
}

struct Constraint {
    binder: Name,
    base: Type,
    premisse: Predicate,
    consequence: Predicate,
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

pub fn check_refinements(db: &dyn HirDatabase, function_id: FunctionDefinitionId) -> bool {
    let resolver = Resolver::new_for_function(db.upcast(), function_id);
    let function = db.function_definition_data(function_id);
    let output_type = something(db, &resolver, &function.return_type);

    let body = db.body_of_definition(function_id.into());

    let scope = body
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
        .zip(
            function
                .parameter_types
                .iter()
                .map(|reference| something(db, &resolver, reference)),
        )
        .collect();

    let context = Context {
        scopes: vec![scope],
    };

    let constraint = Fold {
        body: &body,
        context,
    }
    .check_type(body.root_expression, output_type);

    false
}

struct Fold<'a> {
    body: &'a Body,
    context: Context,
}

impl<'a> Fold<'a> {
    fn check_type(self, term_id: TermId, output_type: RefinedBase) -> Self {
        let term = &self.body.expressions[term_id];
        match term {
            Term::Block {
                statements: _,
                trailing_expression,
            } => self.check_type(*trailing_expression, output_type),
            Term::Path(path) => {
                let path_type = self.context.get(path).unwrap().clone();
                self.subtype(path_type, output_type)
            }
            // Term::If {
            //     condition,
            //     then_branch,
            //     else_branch,
            // } => todo!(),
            // Term::Match { matchee, case_list } => todo!(),
            // Term::Call { callee, arguments } => todo!(),
            // Term::New(_) => todo!(),
            // Term::Binary(_, _, _) => todo!(),
            // Term::Unary(_, _) => todo!(),
            // Term::Literal(_) => todo!(),
            _ => todo!(),
        }
    }

    fn subtype(self, lesser: RefinedBase, greater: RefinedBase) -> Self {
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

        let constraint = Constraint {
            binder: lesser_binder.clone(),
            base: lesser_base,
            premisse: lesser_predicate,
            consequence: substitution(greater_binder, lesser_binder, greater_predicate),
        };

        self
    }
}

fn substitution(greater_binder: Name, lesser_binder: Name, greater_predicate: Predicate) -> Predicate {
    match greater_predicate {
        Predicate::Variable(_) => todo!(),
        Predicate::Boolean(_) => todo!(),
        Predicate::Integer(_) => todo!(),
        Predicate::Binary(_, _, _) => todo!(),
        Predicate::Unary(_, _) => todo!(),
    }
}
