use logos::Logos;
use std::convert::TryFrom;
use std::ops::Range as StdRange;
use text_size::{TextRange, TextSize};

#[derive(Clone, Copy, Debug, PartialEq, Logos)]
pub enum TokenKind {
    #[regex("[\n\t ]+")]
    Whitespace,
    #[regex("//.*")]
    Comment,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(";")]
    SemiColon,
    #[token("(")]
    OpenParenthesis,
    #[token(")")]
    CloseParenthesis,
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
    #[token(">")]
    Greater,
    #[token("<")]
    Less,
    #[token("!")]
    Exclamation,
    #[token("==")]
    DoubleEquals,
    #[token("!=")]
    ExclamationEquals,
    #[token(">=")]
    GreaterEquals,
    #[token("<=")]
    LessEquals,
    #[token("->")]
    ThinArrow,
    #[token("=>")]
    FatArrow,
    #[token("fn")]
    FnKw,
    #[token("if")]
    IfKw,
    #[token("else")]
    ElseKw,
    #[token("true")]
    TrueKw,
    #[token("false")]
    FalseKw,
    #[token("type")]
    TypeKw,
    #[token("match")]
    MatchKw,
    #[regex("[[:alpha:]_][[:word:]]*")]
    Identifier,
    #[regex("[[:digit:]][[:digit:]_]*([[:alpha:]][[:word:]]*)?")]
    Integer,
    #[error]
    Error,
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
    fn lex_comma() {
        check(",", TokenKind::Comma);
    }

    #[test]
    fn lex_collon() {
        check(":", TokenKind::Colon);
    }

    #[test]
    fn lex_doublecollon() {
        check("::", TokenKind::DoubleColon);
    }

    #[test]
    fn lex_semicollon() {
        check(";", TokenKind::SemiColon);
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
    fn lex_open_parenthesis() {
        check("(", TokenKind::OpenParenthesis);
    }

    #[test]
    fn lex_close_parenthesis() {
        check(")", TokenKind::CloseParenthesis);
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
    fn lex_exclamation() {
        check("!", TokenKind::Exclamation);
    }

    #[test]
    fn lex_double_equals() {
        check("==", TokenKind::DoubleEquals);
    }

    #[test]
    fn lex_not_equals() {
        check("!=", TokenKind::ExclamationEquals);
    }

    #[test]
    fn lex_greater() {
        check(">", TokenKind::Greater);
    }

    #[test]
    fn lex_greater_equals() {
        check(">=", TokenKind::GreaterEquals);
    }

    #[test]
    fn lex_less() {
        check("<", TokenKind::Less);
    }

    #[test]
    fn lex_less_equals() {
        check("<=", TokenKind::LessEquals);
    }

    #[test]
    fn lex_arrow() {
        check("->", TokenKind::ThinArrow);
    }

    #[test]
    fn lex_fat_arrow() {
        check("=>", TokenKind::FatArrow);
    }

    #[test]
    fn lex_fn() {
        check("fn", TokenKind::FnKw);
    }

    #[test]
    fn lex_if() {
        check("if", TokenKind::IfKw);
    }

    #[test]
    fn lex_else() {
        check("else", TokenKind::ElseKw);
    }

    #[test]
    fn lex_true() {
        check("true", TokenKind::TrueKw);
    }

    #[test]
    fn lex_false() {
        check("false", TokenKind::FalseKw);
    }

    #[test]
    fn lex_type() {
        check("type", TokenKind::TypeKw);
    }

    #[test]
    fn lex_match() {
        check("match", TokenKind::MatchKw);
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
    fn lex_underscore_integer_with_suffix() {
        check("1234__0987__Abacate_45", TokenKind::Integer);
    }

    #[test]
    fn lex_big_integer() {
        check("12__3412344431__424123442123444341", TokenKind::Integer);
    }

    #[test]
    fn lex_long_integer_with_suffix() {
        check(
            "1234123__4132409__780987099876589756__45_B4l3_14_",
            TokenKind::Integer,
        );
    }
}
