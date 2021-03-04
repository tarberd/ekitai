use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Clone, Copy, Debug, PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    EkitaiSource,

    Whitespace,
    Comment,
    Collon,
    SemiCollon,
    OpenParenthesis,
    CloseParenthesis,
    OpenBraces,
    CloseBraces,
    Equals,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Arrow,
    FnKw,
    Identifier,
    Integer,

    FunctionDefinition,
    Name,
    NameReference,
    BlockExpression,
    Literal,
    PrefixExpression,
    InfixExpression,
    ParenthesisExpression,

    Error,
    Placeholder,
}

impl SyntaxKind {
    pub fn is_trivia(&self) -> bool {
        matches!(self, Self::Whitespace | Self::Comment)
    }
}
