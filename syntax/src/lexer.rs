use lexer::TokenKind;
use parser::SyntaxKind;
use text_size::TextRange;

pub(crate) struct Lexer<'i> {
    inner: lexer::Lexer<'i>,
}

impl<'i> Lexer<'i> {
    pub(crate) fn new(input: &'i str) -> Self {
        Self {
            inner: lexer::Lexer::new(input),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Token<'i> {
    pub kind: SyntaxKind,
    pub lexeme: &'i str,
    pub range: TextRange,
}

impl<'i> Token<'i> {
    fn new(kind: SyntaxKind, lexeme: &'i str, range: TextRange) -> Self {
        Self {
            kind,
            lexeme,
            range,
        }
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        let lexer::Token {
            kind,
            lexeme,
            range,
        } = self.inner.next()?;

        Some(Token::new(into_syntax_kind(kind), lexeme, range))
    }
}

fn into_syntax_kind(token: TokenKind) -> SyntaxKind {
    match token {
        TokenKind::Whitespace => SyntaxKind::Whitespace,
        TokenKind::Comma => SyntaxKind::Comma,
        TokenKind::Comment => SyntaxKind::Comment,
        TokenKind::Colon => SyntaxKind::Colon,
        TokenKind::DoubleColon => SyntaxKind::DoubleColon,
        TokenKind::SemiColon => SyntaxKind::SemiColon,
        TokenKind::OpenParenthesis => SyntaxKind::OpenParenthesis,
        TokenKind::CloseParenthesis => SyntaxKind::CloseParenthesis,
        TokenKind::OpenBraces => SyntaxKind::OpenBraces,
        TokenKind::CloseBraces => SyntaxKind::CloseBraces,
        TokenKind::Equals => SyntaxKind::Equals,
        TokenKind::Plus => SyntaxKind::Plus,
        TokenKind::Minus => SyntaxKind::Minus,
        TokenKind::Asterisk => SyntaxKind::Asterisk,
        TokenKind::Slash => SyntaxKind::Slash,
        TokenKind::Percent => SyntaxKind::Percent,
        TokenKind::Exclamation => SyntaxKind::Exclamation,
        TokenKind::DoubleEquals => SyntaxKind::DoubleEquals,
        TokenKind::ExclamationEquals => SyntaxKind::ExclamationEquals,
        TokenKind::Greater => SyntaxKind::Greater,
        TokenKind::GreaterEquals => SyntaxKind::GreaterEquals,
        TokenKind::Less => SyntaxKind::Less,
        TokenKind::LessEquals => SyntaxKind::LessEquals,
        TokenKind::DoublePipe => SyntaxKind::DoublePipe,
        TokenKind::DoubleAmpersand => SyntaxKind::DoubleAmpersand,
        TokenKind::Ampersand => SyntaxKind::Ampersand,
        TokenKind::ThinArrow => SyntaxKind::ThinArrow,
        TokenKind::FnKw => SyntaxKind::FnKw,
        TokenKind::LetKw => SyntaxKind::LetKw,
        TokenKind::IfKw => SyntaxKind::IfKw,
        TokenKind::ElseKw => SyntaxKind::ElseKw,
        TokenKind::TrueKw => SyntaxKind::TrueKw,
        TokenKind::FalseKw => SyntaxKind::FalseKw,
        TokenKind::TypeKw => SyntaxKind::TypeKw,
        TokenKind::MatchKw => SyntaxKind::MatchKw,
        TokenKind::FatArrow => SyntaxKind::FatArrow,
        TokenKind::Identifier => SyntaxKind::Identifier,
        TokenKind::Integer => SyntaxKind::Integer,
        TokenKind::Error => SyntaxKind::Error,
    }
}
