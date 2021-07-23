use hir::{HirDatabase, Upcast};
use std::fs;
use std::path::PathBuf;

#[salsa::database(
    hir::SourceDatabaseStorage,
    hir::InternDatabaseStorage,
    hir::DefinitionsDatabaseStorage,
    hir::HirDatabaseStorage
)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl Upcast<dyn hir::SourceDatabase> for Database {
    fn upcast(&self) -> &(dyn hir::SourceDatabase + 'static) {
        &*self
    }
}

impl Upcast<dyn hir::DefinitionsDatabase> for Database {
    fn upcast(&self) -> &(dyn hir::DefinitionsDatabase + 'static) {
        &*self
    }
}

impl salsa::Database for Database {}

impl Database {
    pub fn new() -> Self {
        Self {
            storage: salsa::Storage::default(),
        }
    }
}

fn main() {
    let args: Vec<_> = std::env::args().into_iter().map(PathBuf::from).collect();

    let mut db = Database::new();
    if let Some(file_path) = args.get(1) {
        println!("Compiling file: {}", file_path.to_str().unwrap());
        match fs::read_to_string(file_path) {
            Ok(source) => drive(&mut db, source),
            Err(err) => println!("{}", err),
        }
    }
}

fn drive(db: &mut dyn HirDatabase, source: String) {
    db.set_source_file_text(source);

    let item_tree = db.source_file_item_tree();

    for (id, f) in item_tree.functions.iter() {
        let fn_sig =
            db.function_definition_signature(db.intern_function(hir::FunctionLocation { id }));

        println!("{:?} \n sig: {:?}", f, fn_sig)
    }
}
