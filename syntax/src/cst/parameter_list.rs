use super::{raw::SyntaxNode, Parameter, CstNode, SyntaxToAstError};
use parser::SyntaxKind;
use std::{convert::TryFrom, fmt::Display};

#[derive(Debug)]
pub struct ParameterList(SyntaxNode);

impl CstNode for ParameterList {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ParameterList {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::ParameterList
    }

    pub fn parameters(&self) -> impl Iterator<Item = Parameter> {
        let mut children = self.as_syntax_node().children();
        std::iter::from_fn(move || {
            children.by_ref().find_map(|n| Parameter::try_from(n).ok())
        })
    }
}

impl Display for ParameterList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for ParameterList {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

