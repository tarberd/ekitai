use crate::ast::{AstNode, NameReference, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct Path(SyntaxNode);

impl_trait_ast_node!(Path);

impl Path {
    pub fn path_segment(&self) -> Option<PathSegment> {
        self.syntax().children().find_map(PathSegment::cast)
    }

    pub fn path(&self) -> Option<Path> {
        self.syntax().children().find_map(Path::cast)
    }
}

#[derive(Debug)]
pub struct PathSegment(SyntaxNode);

impl_trait_ast_node!(PathSegment);

impl PathSegment {
    pub fn name_reference(&self) -> Option<NameReference> {
        self.syntax().children().find_map(NameReference::cast)
    }
}
