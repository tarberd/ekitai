use crate::cst::NameReference;
use super::{
    super::{raw::SyntaxNode, CstNode, SyntaxToAstError},
    ArgumentList,
};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct CallExpression(pub(super) SyntaxNode);

impl CstNode for CallExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl CallExpression {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::CallExpression
    }

    pub fn name_reference(&self) -> Option<NameReference> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|s| NameReference::try_from(s).ok())
    }

    pub fn argument_list(&self) -> Option<ArgumentList> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|s| ArgumentList::try_from(s).ok())
    }
}

impl std::fmt::Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for CallExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
