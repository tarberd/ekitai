use super::raw::SyntaxNode;
use super::SyntaxToAstError;
use parser::SyntaxKind;
use std::convert::TryFrom;
use std::fmt::Display;

pub struct BlockExpression {
    raw: SyntaxNode,
}

impl BlockExpression {
    fn from_raw(raw: SyntaxNode) -> Self {
        Self { raw }
    }

    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::BlockExpression
    }
}

impl Display for BlockExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl TryFrom<SyntaxNode> for BlockExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self::from_raw(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
