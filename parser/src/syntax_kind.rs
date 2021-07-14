use num_derive::{FromPrimitive, ToPrimitive};

/// SyntaxKind means a token or node kind of the ekitai grammar, without structure.
/// This is a set of all syntatic category and is equivalent to the nodes and leaves in a syntax tree but only holds its kind.
#[derive(Clone, Copy, Debug, PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    SourceFile,

    Whitespace,
    Comment,
    Comma,
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
    Exclamation,
    DoubleEquals,
    ExclamationEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,
    Arrow,
    FnKw,
    IfKw,
    ElseKw,
    TrueKw,
    FalseKw,
    DataKw,
    MatchKw,
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
    CallExpression,
    IfExpression,
    ArgumentList,

    Error,
    Placeholder,
}

impl SyntaxKind {
    pub fn is_trivia(&self) -> bool {
        matches!(self, Self::Whitespace | Self::Comment)
    }
}
