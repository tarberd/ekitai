use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Clone, Copy, Debug, PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    EkitaiSource,
    Whitespace,
    Comment,
    Collon,
    SemiCollon,
    OpenParentesis,
    CloseParentesis,
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
    Error,
    Placeholder,
}
