use super::raw::SyntaxNode;
use super::SyntaxToAstError;
use parser::SyntaxKind;
use std::convert::TryFrom;
use std::fmt::Display;
use super::function::Function;

pub struct SourceFile {
    raw: SyntaxNode,
}

impl SourceFile {
    fn from_raw(raw: SyntaxNode) -> Self {
        Self { raw }
    }

    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::SourceFile
    }

    pub fn functions(&self) -> impl Iterator<Item = Function> {
        self.raw.children().filter_map(|s| Function::try_from(s).ok())
    }
}

impl Display for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl TryFrom<SyntaxNode> for SourceFile {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self::from_raw(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
