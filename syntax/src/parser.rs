pub(crate) mod syntax_token_source;
pub(crate) mod syntax_tree_sink;

pub use syntax_tree_sink::SyntaxError;
use crate::lexer::Lexer;
use rowan::GreenNode;
use syntax_token_source::SyntaxTokenSource;
use syntax_tree_sink::SyntaxTreeSink;

pub fn parse(input: &str) -> (GreenNode, Vec<SyntaxError>) {
    let tokens: Vec<_> = Lexer::new(input).collect();
    let token_source = SyntaxTokenSource::new(&tokens);
    let tree_sink = SyntaxTreeSink::new(&tokens);
    let tree_sink = parser::parse(token_source, tree_sink);
    tree_sink.finish()
}