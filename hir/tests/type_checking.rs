use hir::{
    check::type_inference::Constraint, DefinitionsDatabase, DefinitionsDatabaseStorage,
    HirDatabase, HirDatabaseStorage, InternerStorage, SourceDatabase, SourceDatabaseStorage,
    Upcast, semantic_ir::refinement::Predicate,
};
use z3::ast::Ast;

#[salsa::database(
    SourceDatabaseStorage,
    InternerStorage,
    DefinitionsDatabaseStorage,
    HirDatabaseStorage
)]
#[derive(Default)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl Upcast<dyn hir::SourceDatabase> for Database {
    fn upcast(&self) -> &(dyn hir::SourceDatabase + 'static) {
        &*self
    }
}

impl Upcast<dyn hir::Interner> for Database {
    fn upcast(&self) -> &(dyn hir::Interner + 'static) {
        &*self
    }
}

impl Upcast<dyn hir::DefinitionsDatabase> for Database {
    fn upcast(&self) -> &(dyn hir::DefinitionsDatabase + 'static) {
        &*self
    }
}

impl Upcast<dyn hir::HirDatabase> for Database {
    fn upcast(&self) -> &(dyn hir::HirDatabase + 'static) {
        &*self
    }
}

impl salsa::Database for Database {}

#[test]
fn test_id_no_refinements() {
    let source = "fn id(x: {y:i64 | y == 2}) -> {z:i64 | z ==4 } { x }";
    // id: x:{y:i64 | y == 2} -> {z:i64 | z == 4} = (\x. x)

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        let inference_result = db.infer_body_expression_types(*fid);

        let constraint = inference_result
            .constraints
            .iter()
            .map(|(_, constraint)| constraint)
            .cloned()
            .last()
            .unwrap();

        let solver_config = z3::Config::new();
        let solver_context = z3::Context::new(&solver_config);

        match constraint {
            Constraint::Implication(binder, ty, predicate, Constraint) => match predicate {
                Predicate::Variable(_) => todo!(),
                Predicate::Boolean(_) => todo!(),
                Predicate::Integer(_) => todo!(),
                Predicate::Binary(_, _, _) => todo!(),
                Predicate::Unary(_, _) => todo!(),
            },
            Constraint::Predicate(_) => todo!(),
            Constraint::Conjunction(_, _) => todo!(),
            Constraint::True => todo!(),
        };

        let y = z3::ast::Int::new_const(&solver_context, "y");
        let true_value = z3::ast::Bool::from_bool(&solver_context, true);

        // V (x1:B) p1 -> p2[x2 := x1]
        // G |- B{x1: p1} <: B{x2: p2}

        // note the substitution
        let z = z3::ast::Int::new_const(&solver_context, "y");
        let one = z3::ast::Int::from_i64(&solver_context, 1);
        let z_equals_one = z._eq(&one);

        let p1_impl_p2 = true_value.implies(&z_equals_one);

        let for_all = z3::ast::forall_const(&solver_context, &[&y], &[], &p1_impl_p2);

        let solver = z3::Solver::new(&solver_context);
        solver.assert(&for_all);

        println!("{solver}");

        let entailment = match dbg!(solver.check()) {
            z3::SatResult::Unsat => false,
            z3::SatResult::Unknown => false,
            z3::SatResult::Sat => true,
        };
        assert_eq!(entailment, false);
    }
}

// E |-
// E |- (\x. x): x:{y:i64 | y == 2} -> {z:i64 | z == 4} <=
// E |- (\x. x): x:{y:i64 | y == 2} -> {z:i64 | z == 4} <=

// E |- (\x. x): x:{y:i64 | y == 2} -> {z:i64 | z == 4} <=
// E |- let id= (\x. x): x:{y:i64 | y == 2} -> {z:i64 | z == 4} in 1
// ---------------------
// E |- let id= (\x. x): x:{y:i64 | y == 2} -> {z:i64 | z == 4} in 1

//
//  E; x:Int{x:x=5}; x:Int{x:x=5} |- x => Int{x:x=5}
//  E; x:Int{x:x=5} |- x => Int{x:x=5}
//  -------------------
//  E; x:Int{x:x=5} |- let x = x in x => Int{x:x=5}
//
//  E; x:Int{x:x=5} |- let x = x in x => Int{x:x=5}
//  E |- 5 => Int{x:x=5}
//  ----------------
//  E |- let x = 5 in let x = x in x => Int{x:x=5}
