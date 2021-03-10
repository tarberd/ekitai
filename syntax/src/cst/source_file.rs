use super::function::Function;
use super::raw::SyntaxNode;
use super::CstNode;
use super::SyntaxToAstError;
use parser::SyntaxKind;
use std::convert::TryFrom;
use std::fmt::Display;

pub struct SourceFile(SyntaxNode);

impl CstNode for SourceFile {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl SourceFile {
    pub fn functions(&self) -> impl Iterator<Item = Function> {
        self.as_syntax_node()
            .children()
            .filter_map(|s| Function::try_from(s).ok())
    }

    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::SourceFile
    }
}

impl Display for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for SourceFile {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
