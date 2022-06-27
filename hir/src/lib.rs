pub mod check;
mod definitions_db;
pub mod liquid;
pub mod semantic_ir;
mod source_db;
mod type_db;

pub use definitions_db::{DefinitionsDatabase, DefinitionsDatabaseStorage};
pub use semantic_ir::definition_map::{Interner, InternerStorage};
pub use source_db::{SourceDatabase, SourceDatabaseStorage};
pub use type_db::{HirDatabase, HirDatabaseStorage};

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}
