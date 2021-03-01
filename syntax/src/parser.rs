pub(crate) mod token_source;
pub(crate) mod tree_sink;

use super::lexer::Lexer;
use super::Parse;
use parser::parse;
use token_source::TextTokenSource;
use tree_sink::TextTreeSink;

pub fn parse_text(input: &str) -> Parse {
    let tokens: Vec<_> = Lexer::new(input).collect();
    let token_source = TextTokenSource::new(&tokens);
    let tree_sink = TextTreeSink::new(&tokens);
    let sink = parse(token_source, tree_sink);
    let (tree, errors) = sink.finish();
    Parse::new(tree, errors)
}
