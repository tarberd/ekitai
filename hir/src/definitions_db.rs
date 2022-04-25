use crate::{
    definition_map::{DefinitionMap, FunctionDefinitionId, Interner, TypeDefinitionId},
    item::{FunctionDefinition, TypeDefinition},
    item_tree::ItemTree,
    term::Body,
    ExpressionScopeMap, FunctionDefinitionData, SourceDatabase, TypeDefinitionData, Upcast,
};

#[salsa::query_group(DefinitionsDatabaseStorage)]
pub trait DefinitionsDatabase:
    SourceDatabase + Interner + Upcast<dyn SourceDatabase> + Upcast<dyn Interner>
{
    fn source_file_item_tree(&self) -> ItemTree;

    fn source_file_definitions_map(&self) -> DefinitionMap;

    fn body_of_definition(&self, def: FunctionDefinitionId) -> Body;

    fn expression_scope_map(&self, def: FunctionDefinitionId) -> ExpressionScopeMap;

    fn function_definition_data(&self, id: FunctionDefinitionId) -> FunctionDefinitionData;

    fn type_definition_data(&self, id: TypeDefinitionId) -> TypeDefinitionData;
}

fn source_file_item_tree(def_db: &dyn DefinitionsDatabase) -> ItemTree {
    let parse = def_db.source_file_parse();
    let ast_node_map = def_db.source_file_ast_node_map();
    ItemTree::new(parse.ast_node(), ast_node_map)
}

fn source_file_definitions_map(def_db: &dyn DefinitionsDatabase) -> DefinitionMap {
    let item_tree = def_db.source_file_item_tree();
    let interner = def_db.upcast();
    DefinitionMap::new(item_tree, interner)
}

fn body_of_definition(db: &dyn DefinitionsDatabase, id: FunctionDefinitionId) -> Body {
    let source_file = db.source_file_parse();
    let ast_node_map = db.source_file_ast_node_map();
    let item_tree = db.source_file_item_tree();
    let location = db.lookup_intern_function(id);

    let fun_def = item_tree.get(location.item_id);
    let source = ast_node_map.get(&fun_def.ast_node_id);
    let function_cst_node = source.to_node(&source_file.syntax_node());

    Body::lower(function_cst_node)
}

fn expression_scope_map(
    db: &dyn DefinitionsDatabase,
    def: FunctionDefinitionId,
) -> ExpressionScopeMap {
    let body = db.body_of_definition(def);
    ExpressionScopeMap::new(&body)
}

fn function_definition_data(
    db: &dyn DefinitionsDatabase,
    id: FunctionDefinitionId,
) -> FunctionDefinitionData {
    let loc = db.lookup_intern_function(id);
    let item_tree = db.source_file_item_tree();
    let FunctionDefinition {
        name,
        parameter_types,
        return_type,
        ..
    } = item_tree.get(loc.item_id).clone();

    FunctionDefinitionData {
        name,
        parameter_types,
        return_type,
    }
}

fn type_definition_data(db: &dyn DefinitionsDatabase, id: TypeDefinitionId) -> TypeDefinitionData {
    let loc = db.lookup_intern_type(id);
    let item_tree = db.source_file_item_tree();
    let TypeDefinition {
        name,
        value_constructors,
        ..
    } = item_tree.get(loc.item_id).clone();

    TypeDefinitionData {
        name,
        value_constructors,
    }
}
