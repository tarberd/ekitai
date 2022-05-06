use crate::{
    semantic_ir::{
        definition_map::{
            DefinitionMap, FunctionDefinitionData, FunctionDefinitionId, Interner,
            TypeDefinitionData, TypeDefinitionId,
        },
        item_tree::ItemTree,
        term::Body,
        term_context_map::BodyContext,
    },
    SourceDatabase, Upcast,
};

#[salsa::query_group(DefinitionsDatabaseStorage)]
pub trait DefinitionsDatabase:
    SourceDatabase + Interner + Upcast<dyn SourceDatabase> + Upcast<dyn Interner>
{
    fn source_file_item_tree(&self) -> ItemTree;

    fn source_file_definitions_map(&self) -> DefinitionMap;

    fn function_definition_data(&self, id: FunctionDefinitionId) -> FunctionDefinitionData;

    fn type_definition_data(&self, id: TypeDefinitionId) -> TypeDefinitionData;

    fn body_of_definition(&self, def: FunctionDefinitionId) -> Body;

    fn expression_scope_map(&self, def: FunctionDefinitionId) -> BodyContext;
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

    let fun_def = location.in_item_tree(&item_tree);
    let source = ast_node_map.get(&fun_def.ast_node_id);
    let function_cst_node = source.to_node(&source_file.syntax_node());

    Body::lower(function_cst_node)
}

fn expression_scope_map(db: &dyn DefinitionsDatabase, def: FunctionDefinitionId) -> BodyContext {
    let body = db.body_of_definition(def);
    BodyContext::new(&body)
}

fn function_definition_data(
    db: &dyn DefinitionsDatabase,
    id: FunctionDefinitionId,
) -> FunctionDefinitionData {
    let loc = db.lookup_intern_function(id);
    let item_tree = db.source_file_item_tree();
    loc.in_item_tree(&item_tree).clone().into()
}

fn type_definition_data(db: &dyn DefinitionsDatabase, id: TypeDefinitionId) -> TypeDefinitionData {
    let loc = db.lookup_intern_type(id);
    let item_tree = db.source_file_item_tree();
    loc.in_item_tree(&item_tree).clone().into()
}
