use crate::syntax_kind::SyntaxKind;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParseError {
    pub expected: Vec<SyntaxKind>,
    pub found: Option<SyntaxKind>,
}

impl ParseError {
    pub fn new(expected: Vec<SyntaxKind>, found: Option<SyntaxKind>) -> Self {
        Self { expected, found }
    }
}
