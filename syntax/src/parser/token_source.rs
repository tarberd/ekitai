use super::super::lexer::Token;
use parser::{SyntaxKind, TokenSource};
use std::slice::Iter;

pub(crate) struct TextTokenSource<'i> {
    tokens: Iter<'i, Token<'i>>,
}

impl<'i> TextTokenSource<'i> {
    pub fn new(tokens: &'i [Token<'i>]) -> Self {
        Self {
            tokens: tokens.iter(),
        }
    }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.do_bump();
        }
    }

    fn at_trivia(&self) -> bool {
        self.tokens.clone().next().cloned().map_or(false, |token| token.kind.is_trivia())
    }

    fn do_bump(&mut self) {
        self.tokens.next();
    }
}

impl<'i> Iterator for TextTokenSource<'i> {
    type Item = SyntaxKind;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.tokens.next();
        self.eat_trivia();
        item.map(|item| item.kind)
    }
}

impl<'i> TokenSource for TextTokenSource<'i> {
}
