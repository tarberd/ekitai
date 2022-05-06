use crate::DefinitionsDatabase;

use super::{
    definition_map::{DefinitionMap, FunctionDefinitionId, TypeDefinitionId, ValueConstructorId},
    intrinsic::BuiltinType,
    path::Path,
    term::{PatternId, TermId},
    term_context_map::{BodyContext, TermScopeId},
};

pub enum Scope {
    Module {
        definitions_map: DefinitionMap,
    },
    Expression {
        scope_map: BodyContext,
        scope_id: TermScopeId,
    },
}
pub struct Resolver {
    scopes: Vec<Scope>,
}

impl Resolver {
    pub fn new_empty() -> Self {
        Resolver { scopes: Vec::new() }
    }

    pub fn new_root_resolver(db: &dyn DefinitionsDatabase) -> Self {
        let scopes = vec![Scope::Module {
            definitions_map: db.source_file_definitions_map(),
        }];

        Self { scopes }
    }

    pub fn new_for_function(db: &dyn DefinitionsDatabase, _fid: FunctionDefinitionId) -> Self {
        Self::new_root_resolver(db)
    }

    pub fn new_for_type(db: &dyn DefinitionsDatabase, _id: TypeDefinitionId) -> Self {
        Self::new_root_resolver(db)
    }

    pub fn new_for_expression(
        db: &dyn DefinitionsDatabase,
        function_id: FunctionDefinitionId,
        expression_id: TermId,
    ) -> Self {
        let resolver = Self::new_root_resolver(db);
        let expr_scope_map = db.expression_scope_map(function_id);

        let resolver = expr_scope_map
            .expression_scope_ids(expression_id)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .fold(resolver, |mut resolver, scope_id| {
                resolver.scopes.push(Scope::Expression {
                    scope_map: expr_scope_map.clone(),
                    scope_id,
                });
                resolver
            });

        resolver
    }

    pub fn resolve_path_in_type_namespace(
        &self,
        db: &dyn DefinitionsDatabase,
        path: &Path,
    ) -> Option<TypeNamespaceItem> {
        path.segments
            .first()
            .map(|_| {
                self.scopes.iter().rev().find_map(|scope| match scope {
                    Scope::Module { definitions_map } => {
                        definitions_map.resolve_path(db, path).in_type_namespace()
                    }
                    Scope::Expression { .. } => None,
                })
            })
            .expect("empty path")
    }

    pub fn resolve_path_in_value_namespace(
        &self,
        db: &dyn DefinitionsDatabase,
        path: &Path,
    ) -> Option<ValueNamespaceItem> {
        path.segments
            .first()
            .map(|first_name| {
                self.scopes.iter().rev().find_map(|scope| match scope {
                    Scope::Module { definitions_map } => {
                        definitions_map.resolve_path(db, path).in_value_namespace()
                    }
                    Scope::Expression {
                        scope_map,
                        scope_id,
                    } if path.segments.len() == 1 => {
                        let scope = &scope_map.scopes[*scope_id];

                        scope.entries.iter().find_map(|(name, pattern_id)| {
                            match name == first_name {
                                true => Some(ValueNamespaceItem::LocalBinding(*pattern_id)),
                                false => None,
                            }
                        })
                    }
                    _ => None,
                })
            })
            .expect("empty path")
    }
}

pub enum TypeNamespaceItem {
    TypeDefinition(TypeDefinitionId),
    Builtin(BuiltinType),
}

impl From<TypeDefinitionId> for TypeNamespaceItem {
    fn from(type_location: TypeDefinitionId) -> Self {
        Self::TypeDefinition(type_location)
    }
}

impl From<BuiltinType> for TypeNamespaceItem {
    fn from(builtin: BuiltinType) -> Self {
        Self::Builtin(builtin)
    }
}

#[derive(Debug)]
pub enum ValueNamespaceItem {
    Function(FunctionDefinitionId),
    ValueConstructor(ValueConstructorId),
    /// local binding in expression body
    LocalBinding(PatternId),
}
