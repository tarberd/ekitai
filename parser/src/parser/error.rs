use crate::syntax_kind::SyntaxKind;

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    pub expected: SyntaxKind,
    pub found: Option<SyntaxKind>,
}

impl ParseError {
    pub fn new(expected: SyntaxKind, found: Option<SyntaxKind>) -> Self {
        Self { expected, found }
    }
}
