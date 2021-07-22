use std::fs;
use std::path::PathBuf;

#[salsa::database(
    hir::SourceDatabaseStorage,
    hir::DefinitionsDatabaseStorage,
    hir::InternDatabaseStorage
)]
pub struct Database {
    storage: salsa::Storage<Self>,
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

use hir::DefinitionsDatabase;
fn drive(db: &mut dyn DefinitionsDatabase, source: String) {
    db.set_source_file_text(source);
    let str = db.source_file_text();
    println!("{}", str);

    let parse = db.source_file_parse();
    println!("{}", parse.debug_dump());
    println!("Syntax Errors: {:#?}", parse.errors());

    let file_defs = db.source_file_item_tree();
    println!("{:#?}", file_defs);

    let _cst_node_map = db.source_file_cst_id_map();
}
