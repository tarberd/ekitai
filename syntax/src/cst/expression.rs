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
    const fn syntax_kind_set() -> &'static [SyntaxKind] {
        const KINDS: &[SyntaxKind] = &[
            Literal::syntax_kind(),
            PathExpression::syntax_kind(),
            BlockExpression::syntax_kind(),
            InfixExpression::syntax_kind(),
            PrefixExpression::syntax_kind(),
            ParenthesisExpression::syntax_kind(),
            CallExpression::syntax_kind(),
            IfExpression::syntax_kind(),
            MatchExpression::syntax_kind(),
        ];
        KINDS
    }

    fn from_raw_unchecked(raw: SyntaxNode) -> Self {
        match raw.kind() {
            kind if kind == Literal::syntax_kind() => Self::Literal(Literal(raw)),
            kind if kind == PathExpression::syntax_kind() => {
                Self::PathExpression(PathExpression(raw))
            }
            kind if kind == BlockExpression::syntax_kind() => {
                Self::BlockExpression(BlockExpression(raw))
            }
            kind if kind == InfixExpression::syntax_kind() => {
                Self::InfixExpression(InfixExpression(raw))
            }
            kind if kind == PrefixExpression::syntax_kind() => {
                Self::PrefixExpression(PrefixExpression(raw))
            }
            kind if kind == ParenthesisExpression::syntax_kind() => {
                Self::ParenthesisExpression(ParenthesisExpression(raw))
            }
            kind if kind == CallExpression::syntax_kind() => {
                Self::CallExpression(CallExpression(raw))
            }
            kind if kind == IfExpression::syntax_kind() => Self::IfExpression(IfExpression(raw)),
            kind if kind == MatchExpression::syntax_kind() => {
                Self::MatchExpression(MatchExpression(raw))
            }
            _ => panic!("raw: {:?}", raw),
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

impl TryFrom<SyntaxNode> for Expression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::syntax_kind_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind_set()[0], other)),
        }
    }
}
