pub mod check;
mod definitions_db;
pub mod semantic_ir;
mod source_db;
mod type_db;

use std::fmt::Debug;

use check::{IntegerKind, ScalarType, Type};
use la_arena::ArenaMap;

pub use definitions_db::{DefinitionsDatabase, DefinitionsDatabaseStorage};
use semantic_ir::definition_map::{
    CallableDefinitionId, FunctionDefinitionId, TypeableValueDefinitionId, ValueConstructorId,
};
pub use semantic_ir::definition_map::{Interner, InternerStorage};
use semantic_ir::name::Name;
use semantic_ir::path::Path;
use semantic_ir::path_resolver::{Resolver, ValueNamespaceItem};
use semantic_ir::refinement::Predicate;
use semantic_ir::term::{
    BinaryOperator, Body, CompareOperator, Literal, Pattern, PatternId, Statement, Term, TermId,
    UnaryOperator,
};
use semantic_ir::type_reference::TypeReference;
pub use source_db::{SourceDatabase, SourceDatabaseStorage};
pub use type_db::{HirDatabase, HirDatabaseStorage};

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}
