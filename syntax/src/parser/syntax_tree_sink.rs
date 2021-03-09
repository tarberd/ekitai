use crate::lexer::Token;
use crate::syntax_tree::EkitaiLanguage;
use parser::{ParseError, SyntaxKind, TreeSink};
use rowan::{GreenNode, GreenNodeBuilder, Language};
use std::slice::Iter;
use text_size::TextRange;

#[derive(Debug, PartialEq)]
pub struct SyntaxError {
    pub error: ParseError,
    pub range: TextRange,
}

impl SyntaxError {
    pub fn new(error: ParseError, range: TextRange) -> Self {
        Self { error, range }
    }
}

pub(crate) struct SyntaxTreeSink<'t, 'i> {
    tokens: Iter<'t, Token<'i>>,
    previous_token: Option<&'t Token<'i>>,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<SyntaxError>,
}

impl<'t, 'i> SyntaxTreeSink<'t, 'i> {
    pub(crate) fn new(tokens: &'t [Token<'i>]) -> Self {
        Self {
            tokens: tokens.iter(),
            previous_token: None,
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
        }
    }

    pub(crate) fn finish(self) -> (GreenNode, Vec<SyntaxError>) {
        (self.builder.finish(), self.errors)
    }

    fn current(&self) -> Option<Token> {
        self.tokens.clone().next().cloned()
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
        let current = self.tokens.next().unwrap();
        let Token { kind, lexeme, .. } = current;
        let _ = std::mem::replace(&mut self.previous_token, Some(current));
        self.builder
            .token(EkitaiLanguage::kind_to_raw(kind.to_owned().into()), lexeme);
    }
}

impl<'t, 'i> TreeSink for SyntaxTreeSink<'t, 'i> {
    fn add_token(&mut self) {
        self.eat_trivia();
        self.do_add_token();
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.eat_trivia();
        self.builder.start_node(EkitaiLanguage::kind_to_raw(kind));
        self.eat_trivia();
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn add_error(&mut self, error: ParseError) {
        let range = if let Some(Token { range, .. }) = self.current() {
            range
        } else {
            match self.previous_token {
                Some(tok) => tok.range,
                None => unreachable!(),
            }
        };
        self.errors.push(SyntaxError::new(error, range));
        self.eat_trivia();
    }
}
