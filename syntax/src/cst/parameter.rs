use super::{raw::SyntaxNode, CstNode, Name, NameReference, SyntaxToAstError};
use parser::SyntaxKind;
use std::{convert::TryFrom, fmt::Display};

#[derive(Debug)]
pub struct Parameter(SyntaxNode);

impl CstNode for Parameter {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl Parameter {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Parameter
    }

    pub fn name(&self) -> Option<Name> {
        self.as_syntax_node()
            .children()
            .find_map(|n| Name::try_from(n).ok())
    }

    pub fn type_name(&self) -> Option<NameReference> {
        self.as_syntax_node()
            .children()
            .find_map(|n| NameReference::try_from(n).ok())
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for Parameter {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
