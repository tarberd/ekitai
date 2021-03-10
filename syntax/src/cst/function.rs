use super::expression::BlockExpression;
use super::raw::SyntaxNode;
use super::CstNode;
use super::SyntaxToAstError;
use parser::SyntaxKind;
use std::convert::TryFrom;
use std::fmt::Display;

pub struct Function(SyntaxNode);

impl CstNode for Function {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl Function {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::FunctionDefinition
    }

    pub fn body(&self) -> Option<BlockExpression> {
        self.as_syntax_node()
            .children()
            .find_map(|r| BlockExpression::try_from(r).ok())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for Function {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
