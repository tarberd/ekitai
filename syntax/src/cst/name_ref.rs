use super::{raw::SyntaxNode, CstNode, Identifier, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct NameReference(pub(crate) SyntaxNode);

impl CstNode for NameReference {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl NameReference {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::NameReference
    }

    pub fn identifier(&self) -> Identifier {
        self.as_syntax_node()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|it| Identifier::try_from(it).ok())
            .unwrap()
    }
}

impl std::fmt::Display for NameReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for NameReference {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
