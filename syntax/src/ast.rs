use crate::syntax_tree::{SyntaxNode, SyntaxToken};
use parser::SyntaxKind;
use std::convert::TryFrom;

pub trait AstNode: TryFrom<SyntaxNode> {
    fn syntax(&self) -> &SyntaxNode;
}

pub trait AstToken: TryFrom<SyntaxToken> {
    fn syntax(&self) -> &SyntaxToken;
}

pub struct SourceFile {
    pub(crate) syntax: SyntaxNode,
}

impl TryFrom<SyntaxNode> for SourceFile {
    type Error = ();

    fn try_from(syntax: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax.kind() == SyntaxKind::SourceFile {
            true => Ok(Self { syntax }),
            false => Err(()),
        }
    }
}

impl AstNode for SourceFile {
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}

impl SourceFile {
    pub fn functions(&self) {}
}
