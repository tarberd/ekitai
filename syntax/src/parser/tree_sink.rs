use super::super::cst::EkitaiLanguage;
use super::super::lexer::Token;
use parser::{SyntaxKind, TreeSink};
use rowan::{GreenNode, GreenNodeBuilder, Language};
use std::slice::Iter;

pub(crate) struct TextTreeSink<'t, 'i> {
    tokens: Iter<'t, Token<'i>>,
    builder: GreenNodeBuilder<'static>,
}

impl<'t, 'i> TextTreeSink<'t, 'i> {
    pub(crate) fn new(tokens: &'t [Token<'i>]) -> Self {
        Self {
            tokens: tokens.iter(),
            builder: GreenNodeBuilder::new(),
        }
    }

    pub(crate) fn finish(self) -> (GreenNode, ()) {
        (self.builder.finish(), ())
    }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.do_add_token();
        }
    }

    fn at_trivia(&self) -> bool {
        self.tokens
            .clone()
            .next()
            .map_or(false, |tok| tok.kind.is_trivia())
    }

    fn do_add_token(&mut self) {
        let Token { kind, lexeme, .. } = self.tokens.next().unwrap();
        self.builder
            .token(EkitaiLanguage::kind_to_raw(kind.to_owned().into()), lexeme);
    }
}

impl<'t, 'i> TreeSink for TextTreeSink<'t, 'i> {
    fn add_token(&mut self) {
        self.eat_trivia();
        self.do_add_token();
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(EkitaiLanguage::kind_to_raw(kind))
    }

    fn finish_node(&mut self) {
        self.builder.finish_node()
    }
}
