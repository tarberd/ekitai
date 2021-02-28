use logos::Logos;
use std::convert::TryFrom;
use std::ops::Range as StdRange;
use text_size::{TextRange, TextSize};

#[derive(Debug, PartialEq, Logos)]
pub enum TokenKind {
    #[regex("[\n\t ]+")]
    Whitespace,
    #[regex("//.*")]
    Comment,
    #[token(":")]
    Collon,
    #[token(";")]
    SemiCollon,
    #[token("(")]
    OpenParentesis,
    #[token(")")]
    CloseParentesis,
    #[token("{")]
    OpenBraces,
    #[token("}")]
    CloseBraces,
    #[token("=")]
    Equals,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("->")]
    Arrow,
    #[token("fn")]
    FnKw,
    #[regex("[[:alpha:]_][[:word:]]*")]
    Identifier,
    #[regex("[[:digit:]][[:digit:]_]*([[:alpha:]][[:word:]]*)?")]
    Integer,
    #[error]
    Error,
}

#[derive(Debug, PartialEq)]
pub struct Token<'i> {
    pub kind: TokenKind,
    pub lexeme: &'i str,
    pub range: TextRange,
}

pub struct Lexer<'i> {
    inner: logos::Lexer<'i, TokenKind>,
}

impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            inner: TokenKind::lexer(input),
        }
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let lexeme = self.inner.slice();

        let range = {
            let StdRange { start, end } = self.inner.span();

            let start = TextSize::try_from(start).unwrap();
            let end = TextSize::try_from(end).unwrap();

            TextRange::new(start, end)
        };

        Some(Self::Item {
            kind,
            lexeme,
            range,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, kind: TokenKind) {
        let mut lexer = Lexer::new(input);

        let token = lexer.next().unwrap();
        assert_eq!(token.kind, kind);
        assert_eq!(token.lexeme, input);
    }

    #[test]
    fn lex_empty() {
        let mut lexer = Lexer::new("");
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn lex_whitespace() {
        check(" ", TokenKind::Whitespace);
    }

    #[test]
    fn lex_collon() {
        check(":", TokenKind::Collon);
    }

    #[test]
    fn lex_semicollon() {
        check(";", TokenKind::SemiCollon);
    }

    #[test]
    fn lex_open_braces() {
        check("{", TokenKind::OpenBraces);
    }

    #[test]
    fn lex_close_braces() {
        check("}", TokenKind::CloseBraces);
    }

    #[test]
    fn lex_open_parentesis() {
        check("(", TokenKind::OpenParentesis);
    }

    #[test]
    fn lex_close_parentesis() {
        check(")", TokenKind::CloseParentesis);
    }

    #[test]
    fn lex_plus() {
        check("+", TokenKind::Plus);
    }

    #[test]
    fn lex_minus() {
        check("-", TokenKind::Minus);
    }

    #[test]
    fn lex_asterisk() {
        check("*", TokenKind::Asterisk);
    }

    #[test]
    fn lex_slash() {
        check("/", TokenKind::Slash);
    }

    #[test]
    fn lex_percent() {
        check("%", TokenKind::Percent);
    }

    #[test]
    fn lex_equals() {
        check("=", TokenKind::Equals);
    }

    #[test]
    fn lex_arrow() {
        check("->", TokenKind::Arrow);
    }

    #[test]
    fn lex_fn() {
        check("fn", TokenKind::FnKw);
    }

    #[test]
    fn lex_identifier() {
        check("x", TokenKind::Identifier);
        check("_x", TokenKind::Identifier);
        check("abc1234", TokenKind::Identifier);
        check("abc1ABCD4", TokenKind::Identifier);
        check("_a___B_C_1234___", TokenKind::Identifier);
    }

    #[test]
    fn lex_singe_char_integer() {
        check("5", TokenKind::Integer);
    }

    #[test]
    fn lex_integer() {
        check("42123444341", TokenKind::Integer);
    }

    #[test]
    fn lex_underscore_integer() {
        check("4__21_23__444341___", TokenKind::Integer);
    }

    #[test]
    fn lex_underscore_integer_with_sufix() {
        check("1234__0987__Abacate_45", TokenKind::Integer);
    }


    #[test]
    fn lex_big_integer() {
        check("12__3412344431__424123442123444341", TokenKind::Integer);
    }

    #[test]
    fn lex_long_integer_with_sufix() {
        check("1234123__4132409__780987099876589756__45_B4l3_14_", TokenKind::Integer);
    }
}
