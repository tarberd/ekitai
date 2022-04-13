use crate::ast::token::{Boolean, Integer};
use crate::ast::{AstToken, SyntaxToken};
use parser::SyntaxKind;

#[derive(Debug)]
pub enum TokenLiteral {
    Integer(Integer),
    Boolean(Boolean),
}

impl AstToken for TokenLiteral {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        match kind {
            SyntaxKind::Integer => true,
            _ => false,
        }
    }

    fn cast(node: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        Some(match node.kind() {
            SyntaxKind::Integer => TokenLiteral::Integer(Integer(node)),
            kind if Boolean::can_cast(kind) => TokenLiteral::Boolean(Boolean::cast(node).unwrap()),
            _ => return None,
        })
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            TokenLiteral::Integer(lit) => lit.syntax(),
            TokenLiteral::Boolean(bool) => bool.syntax(),
        }
    }
}

impl std::fmt::Display for TokenLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
