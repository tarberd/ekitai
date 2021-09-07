use num_derive::{FromPrimitive, ToPrimitive};

/// SyntaxKind means a token or node kind of the ekitai grammar, without structure.
/// This is a set of all syntatic categories and is equivalent to the nodes and leaves in a syntax tree but only holds its kind.
#[derive(Clone, Copy, Debug, PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    /// root node
    SourceFile,

    // leaf nodes(lexical tokens)
    Whitespace,
    Comment,
    Comma,
    Colon,
    DoubleColon,
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
    DoublePipe,
    DoubleAmpersand,
    ThinArrow,
    FatArrow,
    FnKw,
    IfKw,
    ElseKw,
    TrueKw,
    FalseKw,
    TypeKw,
    MatchKw,
    Identifier,
    Integer,

    // Types
    PathType,

    // Patterns
    DeconstructorPattern,
    BindingPatternList,
    BindingPattern,

    // Module Items
    TypeDefinition,
    FunctionDefinition,

    // Expressions
    Literal,
    PathExpression,
    BlockExpression,
    PrefixExpression,
    InfixExpression,
    ParenthesisExpression,
    CallExpression,
    IfExpression,
    MatchExpression,

    // intermidiate nodes
    Name,
    NameReference,
    Path,
    PathSegment,
    ValueConstructor,
    ValueConstructorList,
    Parameter,
    ParameterList,
    ConstructorParameterList,
    MatchCase,
    MatchCaseList,
    ArgumentList,

    /// parser error node
    Error,
    /// placeholder
    Placeholder,
}

impl SyntaxKind {
    pub fn is_trivia(&self) -> bool {
        matches!(self, Self::Whitespace | Self::Comment)
    }
}
