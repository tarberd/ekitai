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
