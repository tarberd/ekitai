use super::expression::BlockExpression;
use super::raw::SyntaxNode;
use super::SyntaxToAstError;
use parser::SyntaxKind;
use std::convert::TryFrom;
use std::fmt::Display;

pub struct Function {
    raw: SyntaxNode,
}

impl Function {
    fn from_raw(raw: SyntaxNode) -> Self {
        Self { raw }
    }

    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::FunctionDefinition
    }

    pub fn body(&self) -> Option<BlockExpression> {
        self.raw
            .children()
            .find_map(|r| BlockExpression::try_from(r).ok())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl TryFrom<SyntaxNode> for Function {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self::from_raw(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
