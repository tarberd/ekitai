pub mod expression;
pub mod function;
pub mod raw;
pub mod source_file;

use parser::SyntaxKind;
use raw::SyntaxNode;
use std::convert::TryFrom;
use std::fmt::Display;

use self::{expression::Expression, function::Function, source_file::SourceFile};

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

// TODO: please let's change this, the only reason this exists
// is how are we going to match the types(implementors) of a trait in the HIR lower method
// https://stackoverflow.com/questions/26126683/how-to-match-trait-implementors
#[derive(Debug)]
pub enum CstNodeEnum {
    Function(Function),
    Expression(Expression),
    SourceFile(SourceFile),
}