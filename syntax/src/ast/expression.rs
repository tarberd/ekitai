mod block_expression;
mod call_expression;
mod if_expression;
mod infix_expression;
mod literal;
mod match_expression;
mod new_expression;
mod parenthesis_expression;
mod path_expression;
mod prefix_expression;

pub use block_expression::{
    BlockExpression, ExpressionStatement, LetStatement, Statement, StatementList,
};
pub use call_expression::{ArgumentList, CallExpression};
pub use if_expression::IfExpression;
pub use infix_expression::InfixExpression;
pub use literal::Literal;
pub use match_expression::{MatchCase, MatchCaseList, MatchExpression};
pub use new_expression::NewExpression;
pub use parenthesis_expression::ParenthesisExpression;
pub use path_expression::PathExpression;
pub use prefix_expression::PrefixExpression;

use crate::ast::{AstNode, EkitaiLanguage};
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
    NewExpression(NewExpression),
}

impl AstNode for Expression {
    type Language = EkitaiLanguage;

    fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
    where
        Self: Sized,
    {
        match kind {
            SyntaxKind::Literal
            | SyntaxKind::PathExpression
            | SyntaxKind::BlockExpression
            | SyntaxKind::InfixExpression
            | SyntaxKind::PrefixExpression
            | SyntaxKind::ParenthesisExpression
            | SyntaxKind::CallExpression
            | SyntaxKind::IfExpression
            | SyntaxKind::MatchExpression
            | SyntaxKind::NewExpression => true,
            _ => false,
        }
    }

    fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self>
    where
        Self: Sized,
    {
        let node = match node.kind() {
            SyntaxKind::Literal => Self::Literal(Literal(node)),
            SyntaxKind::PathExpression => Self::PathExpression(PathExpression(node)),
            SyntaxKind::BlockExpression => Self::BlockExpression(BlockExpression(node)),
            SyntaxKind::InfixExpression => Self::InfixExpression(InfixExpression(node)),
            SyntaxKind::PrefixExpression => Self::PrefixExpression(PrefixExpression(node)),
            SyntaxKind::ParenthesisExpression => {
                Self::ParenthesisExpression(ParenthesisExpression(node))
            }
            SyntaxKind::CallExpression => Self::CallExpression(CallExpression(node)),
            SyntaxKind::IfExpression => Self::IfExpression(IfExpression(node)),
            SyntaxKind::MatchExpression => Self::MatchExpression(MatchExpression(node)),
            SyntaxKind::NewExpression => Self::NewExpression(NewExpression(node)),
            _ => return None,
        };
        Some(node)
    }

    fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
        match self {
            Expression::BlockExpression(e) => e.syntax(),
            Expression::Literal(e) => e.syntax(),
            Expression::PathExpression(e) => e.syntax(),
            Expression::InfixExpression(e) => e.syntax(),
            Expression::PrefixExpression(e) => e.syntax(),
            Expression::ParenthesisExpression(e) => e.syntax(),
            Expression::CallExpression(e) => e.syntax(),
            Expression::IfExpression(e) => e.syntax(),
            Expression::MatchExpression(e) => e.syntax(),
            Expression::NewExpression(e) => e.syntax(),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
