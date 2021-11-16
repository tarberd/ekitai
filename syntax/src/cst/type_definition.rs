use super::{CstNode, Name, SyntaxToAstError, Type, raw::SyntaxNode};
use parser::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition(pub(super) SyntaxNode);

impl CstNode for TypeDefinition {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl TypeDefinition {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::TypeDefinition
    }

    pub fn name(&self) -> Option<Name> {
        self.as_syntax_node()
            .children()
            .find_map(|it| Name::try_from(it).ok())
    }

    pub fn value_constructor_list(&self) -> Option<ValueConstructorList> {
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

impl TryFrom<SyntaxNode> for TypeDefinition {
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
        self.as_syntax_node()
            .children()
            .find_map(|n| Name::try_from(n).ok())
    }

    pub fn constructor_parameter_list(&self) -> Option<ConstructorParameterList> {
        self.as_syntax_node()
            .children()
            .find_map(|n| ConstructorParameterList::try_from(n).ok())
    }
}

impl std::fmt::Display for ValueConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for ValueConstructor {
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

impl TryFrom<SyntaxNode> for ValueConstructorList {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct ConstructorParameterList(SyntaxNode);

impl CstNode for ConstructorParameterList {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ConstructorParameterList {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::ConstructorParameterList
    }

    pub fn types(&self) -> impl Iterator<Item = Type> {
        let mut children = self.as_syntax_node().children();
        std::iter::from_fn(move || {
            children
                .by_ref()
                .find_map(|n| Type::try_from(n).ok())
        })
    }
}

impl std::fmt::Display for ConstructorParameterList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for ConstructorParameterList {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
