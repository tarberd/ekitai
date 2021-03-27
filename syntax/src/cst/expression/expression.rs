use super::{
    super::{raw::SyntaxNode, CstNode, SyntaxToAstError},
    BlockExpression, InfixExpression, Literal, ParenthesisExpression, PrefixExpression,
};
use parser::SyntaxKind;

#[derive(Debug)]
pub enum Expression {
    BlockExpression(BlockExpression),
    Literal(Literal),
    InfixExpression(InfixExpression),
    PrefixExpression(PrefixExpression),
    ParenthesisExpression(ParenthesisExpression),
}

impl Expression {
    fn try_from_set() -> &'static [SyntaxKind] {
        static KINDS: &[SyntaxKind] = &[
            SyntaxKind::BlockExpression,
            SyntaxKind::Literal,
            SyntaxKind::InfixExpression,
            SyntaxKind::PrefixExpression,
            SyntaxKind::ParenthesisExpression,
        ];
        &KINDS
    }

    fn from_raw_unchecked(raw: SyntaxNode) -> Self {
        match raw.kind() {
            SyntaxKind::BlockExpression => Self::BlockExpression(BlockExpression(raw)),
            SyntaxKind::Literal => Self::Literal(Literal(raw)),
            SyntaxKind::InfixExpression => Self::InfixExpression(InfixExpression(raw)),
            SyntaxKind::PrefixExpression => Self::PrefixExpression(PrefixExpression(raw)),
            SyntaxKind::ParenthesisExpression => {
                Self::ParenthesisExpression(ParenthesisExpression(raw))
            }
            _ => panic!(),
        }
    }
}

impl CstNode for Expression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        match self {
            Expression::BlockExpression(e) => e.as_syntax_node(),
            Expression::Literal(e) => e.as_syntax_node(),
            Expression::InfixExpression(e) => e.as_syntax_node(),
            Expression::PrefixExpression(e) => e.as_syntax_node(),
            Expression::ParenthesisExpression(e) => e.as_syntax_node(),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for Expression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::try_from_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::try_from_set()[0], other)),
        }
    }
}
