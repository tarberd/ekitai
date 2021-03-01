use super::super::lexer::Token;
use parser::TokenSource;

pub(crate) struct TextTokenSource<'t, 'i> {
    tokens: &'t [Token<'i>],
    cursor: usize,
}

impl<'t, 'i> TextTokenSource<'t, 'i> {
    pub fn new(tokens: &'t [Token<'i>]) -> Self {
        Self { tokens, cursor: 0 }
    }
}

impl<'t, 'i> TokenSource for TextTokenSource<'t, 'i> {
    fn current(&self) -> parser::SyntaxKind {
        self.tokens[self.cursor].kind
    }

    fn lookahead(&self, n: usize) -> Option<parser::SyntaxKind> {
        Some(self.tokens.iter().cloned().nth(self.cursor + n)?.kind)
    }
}
