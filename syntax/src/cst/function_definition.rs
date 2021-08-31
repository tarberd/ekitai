use super::{BlockExpression, CstNode, Name, Pattern, SyntaxToAstError, Type, raw::SyntaxNode};
use parser::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinition(pub(super) SyntaxNode);

impl CstNode for FunctionDefinition {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl FunctionDefinition {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::FunctionDefinition
    }

    pub fn name(&self) -> Option<Name> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Name::try_from(n).ok())
    }

    pub fn parameter_list(&self) -> Option<ParameterList> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| ParameterList::try_from(n).ok())
    }

    pub fn return_type(&self) -> Option<Type> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Type::try_from(n).ok())
    }

    pub fn body(&self) -> Option<BlockExpression> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|r| BlockExpression::try_from(r).ok())
    }
}

impl std::fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for FunctionDefinition {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

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
        use std::convert::TryFrom;
        let mut children = self.as_syntax_node().children();
        std::iter::from_fn(move || children.by_ref().find_map(|n| Parameter::try_from(n).ok()))
    }
}

impl std::fmt::Display for ParameterList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for ParameterList {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

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

    pub fn pattern(&self) -> Option<Pattern> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Pattern::try_from(n).ok())
    }

    pub fn ty(&self) -> Option<Type> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Type::try_from(n).ok())
    }
}

impl std::fmt::Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for Parameter {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
