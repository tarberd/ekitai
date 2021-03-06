mod expression;
mod function;
mod name;
mod name_ref;
mod parameter;
mod parameter_list;
pub mod raw;
mod source_file;
mod token;

pub use expression::{
    BlockExpression, CallExpression, Expression, IfExpression, InfixExpression, Literal,
    PrefixExpression,
};
pub use function::Function;
pub use name::Name;
pub use name_ref::NameReference;
pub use parameter::Parameter;
pub use parameter_list::ParameterList;
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
