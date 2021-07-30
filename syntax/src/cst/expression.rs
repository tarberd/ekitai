mod block_expression;
mod call_expression;
mod if_expression;
mod infix_expression;
mod literal;
mod match_expression;
mod parenthesis_expression;
mod path_expression;
mod prefix_expression;

pub use block_expression::BlockExpression;
pub use call_expression::{ArgumentList, CallExpression};
pub use if_expression::IfExpression;
pub use infix_expression::InfixExpression;
pub use literal::Literal;
pub use match_expression::{MatchCase, MatchCaseList, MatchExpression};
pub use parenthesis_expression::ParenthesisExpression;
pub use path_expression::PathExpression;
pub use prefix_expression::PrefixExpression;

use super::{raw::SyntaxNode, CstNode, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    PathExpression(PathExpression),
    BlockExpression(BlockExpression),
    InfixExpression(InfixExpression),
    PrefixExpression(PrefixExpression),
    ParenthesisExpression(ParenthesisExpression),
    CallExpression(CallExpression),
    IfExpression(IfExpression),
    MatchExpression(MatchExpression),
}

impl Expression {
    fn try_from_set() -> &'static [SyntaxKind] {
        static KINDS: &[SyntaxKind] = &[
            SyntaxKind::Literal,
            SyntaxKind::PathExpression,
            SyntaxKind::BlockExpression,
            SyntaxKind::InfixExpression,
            SyntaxKind::PrefixExpression,
            SyntaxKind::ParenthesisExpression,
            SyntaxKind::CallExpression,
            SyntaxKind::IfExpression,
            SyntaxKind::MatchExpression,
        ];
        KINDS
    }

    fn from_raw_unchecked(raw: SyntaxNode) -> Self {
        match raw.kind() {
            SyntaxKind::Literal => Self::Literal(Literal(raw)),
            SyntaxKind::PathExpression => Self::PathExpression(PathExpression(raw)),
            SyntaxKind::BlockExpression => Self::BlockExpression(BlockExpression(raw)),
            SyntaxKind::InfixExpression => Self::InfixExpression(InfixExpression(raw)),
            SyntaxKind::PrefixExpression => Self::PrefixExpression(PrefixExpression(raw)),
            SyntaxKind::ParenthesisExpression => {
                Self::ParenthesisExpression(ParenthesisExpression(raw))
            }
            SyntaxKind::CallExpression => Self::CallExpression(CallExpression(raw)),
            SyntaxKind::IfExpression => Self::IfExpression(IfExpression(raw)),
            SyntaxKind::MatchExpression => Self::MatchExpression(MatchExpression(raw)),
            _ => panic!(),
        }
    }
}

impl CstNode for Expression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        match self {
            Expression::BlockExpression(e) => e.as_syntax_node(),
            Expression::Literal(e) => e.as_syntax_node(),
            Expression::PathExpression(e) => e.as_syntax_node(),
            Expression::InfixExpression(e) => e.as_syntax_node(),
            Expression::PrefixExpression(e) => e.as_syntax_node(),
            Expression::ParenthesisExpression(e) => e.as_syntax_node(),
            Expression::CallExpression(e) => e.as_syntax_node(),
            Expression::IfExpression(e) => e.as_syntax_node(),
            Expression::MatchExpression(e) => e.as_syntax_node(),
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
