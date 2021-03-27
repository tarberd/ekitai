mod expression;
mod function;
mod name;
pub mod raw;
mod source_file;
mod token;

pub use expression::{BlockExpression, Expression, InfixExpression, Literal};
pub use function::Function;
pub use name::Name;
pub use source_file::SourceFile;
pub use token::{
    Asterisk, BinaryOperator, Identifier, Integer, LiteralKind, Minus, Percent, Plus, Slash,
    UnaryOperator,
};

use parser::SyntaxKind;
use raw::{SyntaxNode, SyntaxToken};
use std::convert::TryFrom;
use std::fmt::Display;

pub trait CstNode: TryFrom<SyntaxNode, Error = SyntaxToAstError> + Display {
    fn as_syntax_node(&self) -> &SyntaxNode;
}

pub trait CstToken: TryFrom<SyntaxToken, Error = SyntaxToAstError> + Display {
    fn as_syntax_token(&self) -> &SyntaxToken;
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
