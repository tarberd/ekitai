use std::collections::HashMap;

use crate::{
    definitions_map::{
        DefinitionsMap, FunctionLocation, FunctionLocationId, InternDatabase, ItemScope,
        LocationId, TypeLocation, TypeLocationId,
    },
    item_tree::{FunctionDefinition, Item, ItemTree, TypeDefinition},
    Body, ExpressionScopeMap, FunctionDefinitionData, SourceDatabase, TypeDefinitionData, Upcast,
};

#[salsa::query_group(DefinitionsDatabaseStorage)]
pub trait DefinitionsDatabase:
    SourceDatabase + InternDatabase + Upcast<dyn SourceDatabase>
{
    fn source_file_item_tree(&self) -> ItemTree;

    fn source_file_definitions_map(&self) -> DefinitionsMap;

    fn body_of_definition(&self, def: FunctionLocationId) -> Body;

    fn expression_scope_map(&self, def: FunctionLocationId) -> ExpressionScopeMap;

    fn function_definition_data(&self, id: FunctionLocationId) -> FunctionDefinitionData;

    fn type_definition_data(&self, id: TypeLocationId) -> TypeDefinitionData;
}

fn source_file_item_tree(def_db: &dyn DefinitionsDatabase) -> ItemTree {
    let parse = def_db.source_file_parse();
    let ast_node_map = def_db.source_file_ast_node_map();

    let source = parse.ast_node();

    ItemTree::new(source, ast_node_map)
}

fn source_file_definitions_map(def_db: &dyn DefinitionsDatabase) -> DefinitionsMap {
    let item_tree = def_db.source_file_item_tree();

    let mut types = HashMap::new();
    let mut values = HashMap::new();
    let mut definitions = Vec::new();

    for item in item_tree.root_items() {
        match item {
            Item::Function(id) => {
                let function = item_tree.get(*id);
                let function_location = FunctionLocation { id: *id };
                let function_location_id = def_db.intern_function(function_location);
                let location_id = LocationId::FunctionLocationId(function_location_id);

                definitions.push(location_id.clone());
                values.insert(function.name.clone(), location_id);
            }
            Item::Type(id) => {
                let ty = item_tree.get(*id);
                let type_location = TypeLocation { id: *id };
                let type_location_id = def_db.intern_type(type_location);
                let location_id = LocationId::TypeLocationId(type_location_id);

                definitions.push(location_id.clone());
                types.insert(ty.name.clone(), location_id);
            }
        }
    }

    let item_scope = ItemScope {
        types,
        values,
        definitions,
    };

    DefinitionsMap { item_scope }
}

fn body_of_definition(db: &dyn DefinitionsDatabase, id: FunctionLocationId) -> Body {
    let source_file = db.source_file_parse();
    let ast_node_map = db.source_file_ast_node_map();
    let item_tree = db.source_file_item_tree();
    let location = db.lookup_intern_function(id);

    let fun_def = item_tree.get(location.id);
    let source = ast_node_map.get(&fun_def.ast_node_id);
    let function_cst_node = source.to_node(&source_file.syntax_node());

    Body::lower(function_cst_node)
}

fn expression_scope_map(
    db: &dyn DefinitionsDatabase,
    def: FunctionLocationId,
) -> ExpressionScopeMap {
    let body = db.body_of_definition(def);
    ExpressionScopeMap::new(&body)
}

fn function_definition_data(
    db: &dyn DefinitionsDatabase,
    id: FunctionLocationId,
) -> FunctionDefinitionData {
    let loc = db.lookup_intern_function(id);
    let item_tree = db.source_file_item_tree();
    let FunctionDefinition {
        name,
        parameter_types,
        return_type,
        ..
    } = item_tree.get(loc.id).clone();

    FunctionDefinitionData {
        name,
        parameter_types,
        return_type,
    }
}

fn type_definition_data(db: &dyn DefinitionsDatabase, id: TypeLocationId) -> TypeDefinitionData {
    let loc = db.lookup_intern_type(id);
    let item_tree = db.source_file_item_tree();
    let TypeDefinition {
        name,
        value_constructors,
        ..
    } = item_tree.get(loc.id).clone();

    TypeDefinitionData {
        name,
        value_constructors,
    }
}
