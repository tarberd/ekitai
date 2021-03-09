pub mod function;

use crate::syntax_tree::SyntaxNode;
use parser::SyntaxKind;
use std::convert::TryFrom;
use std::fmt::Display;

pub struct SyntaxToAstError {
    pub expected: SyntaxKind,
    pub found: SyntaxKind,
}

impl SyntaxToAstError {
    pub fn new(expected: SyntaxKind, found: SyntaxKind) -> Self {
        Self { expected, found }
    }
}

pub struct SourceFile {
    syntax: SyntaxNode,
}

impl SourceFile {
    fn new(syntax: SyntaxNode) -> Self {
        Self { syntax }
    }

    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::SourceFile
    }

    fn syntax_node(&self) -> &SyntaxNode {
        &self.syntax
    }
}

impl Display for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.syntax)
    }
}

impl TryFrom<SyntaxNode> for SourceFile {
    type Error = SyntaxToAstError;

    fn try_from(syntax: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax.kind() {
            x if x == Self::syntax_kind() => Ok(Self::new(syntax)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
