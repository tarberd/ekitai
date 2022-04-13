mod boolean;
mod literal;
mod operators;
pub use boolean::{Boolean, False, True};
pub use literal::TokenLiteral;
pub use operators::{Asterisk, BinaryOperator, Minus, Percent, Plus, Slash, UnaryOperator};

use crate::ast::{AstToken, SyntaxToken};
use crate::impl_trait_ast_token;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct Integer(pub(super) SyntaxToken);
impl_trait_ast_token!(Integer, Integer);

impl Integer {
    pub fn radical_and_suffix(&self) -> (&str, Option<&str>) {
        if let Some((index, _)) = self.text().char_indices().find(|(_, c)| c.is_alphabetic()) {
            (&self.text()[0..index], Some(&self.text()[index..]))
        } else {
            (self.text(), None)
        }
    }
}

#[derive(Debug)]
pub struct Identifier(SyntaxToken);
impl_trait_ast_token!(Identifier, Identifier);
