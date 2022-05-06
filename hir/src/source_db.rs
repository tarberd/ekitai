use syntax::{ast, Parse};

use crate::semantic_ir::ast_node_map::AstNodeMap;

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase {
    #[salsa::input]
    fn source_file_text(&self) -> String;

    fn source_file_parse(&self) -> Parse<ast::SourceFile>;

    fn source_file_ast_node_map(&self) -> AstNodeMap;
}

fn source_file_parse(source_db: &dyn SourceDatabase) -> Parse<ast::SourceFile> {
    let source = source_db.source_file_text();
    ast::SourceFile::parse(source.as_str())
}

fn source_file_ast_node_map(source_db: &dyn SourceDatabase) -> AstNodeMap {
    let parse = source_db.source_file_parse();
    AstNodeMap::from_source_file(parse.ast_node())
}
