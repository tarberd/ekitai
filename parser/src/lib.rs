mod grammar;
mod parser;
mod syntax_kind;

pub use crate::syntax_kind::SyntaxKind;
pub use crate::parser::{event, Parser};

pub trait TokenSource {
    fn current(&self) -> SyntaxKind;

    /// Lookahead n token
    fn lookahead(&self, n: usize) -> Option<SyntaxKind>;
}

pub trait TreeSink {
    /// Adds new token to the current branch.
    fn add_token(&mut self);

    /// Start new branch and make it current.
    fn start_node(&mut self, kind: SyntaxKind);

    /// Finish current branch and restore previous
    /// branch as current.
    fn finish_node(&mut self);
}

pub fn parse<Source, Sink>(token_source: Source, tree_sink: Sink) -> Sink
where
    Source: TokenSource,
    Sink: TreeSink,
{
    parse_grammar(token_source, tree_sink, grammar::parse_root)
}

fn parse_grammar<Source, Sink, F>(token_source: Source, tree_sink: Sink, f: F) -> Sink
where
    Source: TokenSource,
    Sink: TreeSink,
    F: FnOnce(&mut Parser<Source>),
{
    let mut parser = Parser::new(token_source);
    f(&mut parser);
    let events = parser.finish();
    event::sink_events(events, tree_sink)
}
