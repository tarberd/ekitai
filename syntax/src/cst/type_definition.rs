use super::{raw::SyntaxNode, CstNode, Name, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct TypeDefinition(pub(super) SyntaxNode);

impl CstNode for TypeDefinition {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl TypeDefinition {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::TypeDefinition
    }

    pub fn name(&self) -> Option<Name> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|it| Name::try_from(it).ok())
    }

    pub fn value_constructor_list(&self) -> Option<ValueConstructorList> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|it| ValueConstructorList::try_from(it).ok())
    }
}

impl std::fmt::Display for TypeDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for TypeDefinition {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct ValueConstructor(SyntaxNode);

impl CstNode for ValueConstructor {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ValueConstructor {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::ValueConstructor
    }

    pub fn name(&self) -> Option<Name> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|n| Name::try_from(n).ok())
    }
}

impl std::fmt::Display for ValueConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for ValueConstructor {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct ValueConstructorList(SyntaxNode);

impl CstNode for ValueConstructorList {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ValueConstructorList {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::ValueConstructorList
    }

    pub fn constructors(&self) -> impl Iterator<Item = ValueConstructor> {
        use std::convert::TryFrom;
        let mut children = self.as_syntax_node().children();
        std::iter::from_fn(move || {
            children
                .by_ref()
                .find_map(|n| ValueConstructor::try_from(n).ok())
        })
    }
}

impl std::fmt::Display for ValueConstructorList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for ValueConstructorList {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
