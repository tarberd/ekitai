use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Clone, Copy, Debug, PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    SourceFile,

    Whitespace,
    Comment,
    Colon,
    SemiColon,
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
    ParameterList,
    Parameter,
    Name,

    NameReference,
    Literal,
    BlockExpression,
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
