mod expression;
mod function_definition;
mod module_item;
mod name;
mod name_ref;
mod path;
mod pattern;
pub mod raw;
mod source_file;
mod token;
mod ty;
mod type_definition;

pub use expression::{
    BlockExpression, CallExpression, Expression, IfExpression, InfixExpression, Literal, MatchCase,
    MatchCaseList, MatchExpression, PathExpression, PrefixExpression,
};
pub use function_definition::{FunctionDefinition, Parameter, ParameterList};
pub use module_item::ModuleItem;
pub use name::Name;
pub use name_ref::NameReference;
pub use path::{Path, PathSegment};
pub use pattern::{BindingPattern, DeconstructorPattern, PatternList, Pattern};
pub use source_file::SourceFile;
pub use token::{
    Asterisk, BinaryOperator, Boolean, Identifier, Integer, Minus, Percent, Plus, Slash,
    TokenLiteral, UnaryOperator,
};
pub use ty::{PathType, Type};
pub use type_definition::{TypeDefinition, ValueConstructor, ValueConstructorList};

use parser::SyntaxKind;
use raw::{SyntaxNode, SyntaxToken};
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
