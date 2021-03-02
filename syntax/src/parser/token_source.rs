use super::super::lexer::Token;
use parser::{SyntaxKind, TokenSource};
use std::slice::Iter;

pub(crate) struct TextTokenSource<'t, 'i> {
    tokens: Iter<'t, Token<'i>>,
}

impl<'t, 'i> TextTokenSource<'t, 'i> {
    pub fn new(tokens: &'t [Token<'i>]) -> Self {
        Self { tokens: tokens.iter() }
    }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.do_bump();
        }
    }

    fn at_trivia(&self) -> bool {
        self.current().map_or(false, |kind| kind.is_trivia())
    }

    fn do_bump(&mut self) {
        self.tokens.next();
    }
}

impl<'t, 'i> TokenSource for TextTokenSource<'t, 'i> {
    fn current(&self) -> Option<SyntaxKind> {
        Some(self.tokens.clone().next()?.kind)
    }

    fn lookahead(&self, n: usize) -> Option<SyntaxKind> {
        Some(self.tokens.clone().nth(n)?.kind)
    }

    fn bump(&mut self) {
        self.do_bump();
        self.eat_trivia();
    }
}
