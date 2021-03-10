pub mod expression;
pub mod function;
pub mod raw;
pub mod source_file;

use parser::SyntaxKind;
use raw::SyntaxNode;
use std::convert::TryFrom;
use std::fmt::Display;

pub trait CstNode: TryFrom<SyntaxNode, Error = SyntaxToAstError> + Display {
    fn as_syntax_node(&self) -> &SyntaxNode;
}

pub struct SyntaxToAstError {
    pub expected: SyntaxKind,
    pub found: SyntaxKind,
}

impl SyntaxToAstError {
    pub fn new(expected: SyntaxKind, found: SyntaxKind) -> Self {
        Self { expected, found }
    }
}
