use hir::{
    DefinitionsDatabase, DefinitionsDatabaseStorage, HirDatabaseStorage, InternerStorage,
    SourceDatabase, SourceDatabaseStorage, Upcast,
};

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
fn test_function_body_entailment() {
    let source = "fn id(x: {y: i64 | y == 0}) -> {z: i64| z == x} { x }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_impossible_premisse() {
    let source = "fn id(x: {y:i64| y > 10 && y < 10 }) -> {z:i64| z == x} { x }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_or_bigger_small() {
    let source = "fn id(x: {y:i64| y > 10 || y < 10 }) -> {z:i64| z == x} { x }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_or_bigger_small_err() {
    let source = "fn id(x: {y:i64| y > 10 || y < 10 }) -> {z:i64| z == 10} { x }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(false, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_single_literal() {
    let source = "fn id() -> {z:i64| z == 0} { 0 }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_single_literal_err() {
    let source = "fn id() -> {z:i64| z == 10} { 0 }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(false, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_single_bool() {
    let source = "fn id() -> {z:bool| z} { true }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_single_bool_err() {
    let source = "fn id() -> {z:bool| !z} { true }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(false, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_minus_call() {
    let source = "fn id(x: {y:i64| true}) -> {z:i64| z == -x} { ---x }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_minus_call_literal() {
    let source = "fn id() -> {z:i64| z == -5} { -5 }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_sum_call_argument() {
    let source = "fn sum(x: {x2:i64| true}, y: {y2:i64| true}) -> {z:i64| z == x + y } { x + y }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_sum_call_argument_err() {
    let source =
        "fn sum(x: {x2:i64| true}, y: {y2:i64| true}) -> {z:i64| z == x + y + 1} { x + y }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(false, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_sum_call_literal() {
    let source = "fn sum() -> {z:i64| z == 42} { --10 + 15 + 15 + 2 }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_sum_call_argument_literal() {
    let source = "fn sum(x: {x2:i64| true}, y: {y2:i64| true}) -> {z:i64| z == 42 + x + y} { 10 + 30 + x + 2 + y }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_arith_operations() {
    let source = "fn sum(x: {x2:i64| true}, y: {y2:i64| true}) -> {z:i64| z == x * 10 + y * 35} { x * 10 + y * 35 }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_arith_operations_err() {
    let source = "fn sum(x: {x2:i64| true}, y: {y2:i64| true}) -> {z:i64| z == x * 10 + y * 35 + 1} { x * 10 + y * 35 }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();

    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(false, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_compare_operations() {
    let source = "fn sum(x: {x2:i64| true}, y: {y2:i64| y2 > x}) -> {z:bool| z == true} { x < y }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();
    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }

    let source =
        "fn sum(x: {x2:i64| true}, y: {y2:i64| y2 > x}) -> {z:bool| z == false} { x == y }";
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();
    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }

    let source = "fn sum(x: {x2:i64| true}, y: {y2:i64| y2 > x}) -> {z:i64| z >= y} { y }";
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();
    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }

    let source =
        "fn sum(x: {x2:i64| true}, y: {y2:i64| y2 > x}) -> {z:bool| true } { y + x > x + x }";
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();
    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_i32() {
    let source = "fn sum(x: {x2:i32| true}, y: {y2:i32| true}) -> {z:i32| z == x - y} { x - y }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();
    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_div() {
    let source = "fn div(x: {x2:i32| true}, y: {y2:i32| true}) -> {z:i32| z == x / y} { x / y }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();
    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }

    let source = "fn div(x: {x2:i32| true}, y: {y2:i32| true}) -> {z:i32| z == y / x} { x / y }";
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();
    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(false, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_function_abs() {
    let source = "fn abs(x: {x2:i64| true}) -> {z:i64| z >= x} { if x >= 0 { x } else { -x } }";

    let mut db = Database::default();
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();
    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_recursive_call() {
    let mut db = Database::default();

    let source = "fn rec(x: {x2:i64| true}) -> {z:i64| true} { rec(x) }";
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();
    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }

    let source = "fn sum(n: {x:i64| true}) -> {z:i64| z >= 0 && n <= z } {
        if n <= 0 {
            0
        } else {
            let n1 = n - 1;
            let t1 = sum(n1);
            n + t1
        }
    }";
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();
    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}

#[test]
fn test_two_functions_recursive_call() {
    let mut db = Database::default();

    let source = "fn sum(n: {x:i64| true}) -> {z:i64| z >= 0 && n <= z } {
        if n <= 0 {
            let zero = 0;
            id(zero)
        } else {
            let n1 = n - 1;
            let t1 = sum(n1);
            n + t1
        }
    }
    fn id(x: {x2:i64| true}) -> {z:i64| z == x} { x }
    ";
    db.set_source_file_text(source.into());
    let definitions = db.source_file_definitions_map();
    for fid in definitions
        .root_module_item_scope()
        .iter_function_locations()
    {
        assert_eq!(true, hir::liquid::check_abstraction(&db, *fid));
    }
}
