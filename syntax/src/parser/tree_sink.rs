use super::super::cst::EkitaiLanguage;
use super::super::lexer::Token;
use parser::{TreeSink, SyntaxKind};
use rowan::{GreenNode, GreenNodeBuilder, Language};

pub(crate) struct TextTreeSink<'t, 'i> {
    tokens: &'t [Token<'i>],
    builder: GreenNodeBuilder<'static>,
    cursor: usize,
}

impl<'t, 'i> TextTreeSink<'t, 'i> {
    pub(crate) fn new(tokens: &'t [Token<'i>]) -> Self {
        Self {
            tokens,
            builder: GreenNodeBuilder::new(),
            cursor: 0,
        }
    }

    pub(crate) fn finish(self) -> (GreenNode, ()) {
        (self.builder.finish(), ())
    }
}

impl<'t, 'i> TreeSink for TextTreeSink<'t, 'i> {
    fn add_token(&mut self) {
        let Token { kind, lexeme, .. } = self.tokens[self.cursor];
        self.builder
            .token(EkitaiLanguage::kind_to_raw(kind.into()), lexeme);
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(EkitaiLanguage::kind_to_raw(kind))
    }

    fn finish_node(&mut self) {
        self.builder.finish_node()
    }
}
