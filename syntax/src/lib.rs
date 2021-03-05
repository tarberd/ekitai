pub(crate) mod lexer;
pub mod parser;
pub(crate) mod syntax_tree;
pub mod ast;

#[cfg(test)]
mod tests;

use crate::parser::SyntaxError;
use rowan::GreenNode;
use std::marker::PhantomData;
use syntax_tree::SyntaxNode;
use ast::SourceFile;

#[derive(Debug)]
pub struct Parse<T> {
    green_node: GreenNode,
    errors: Vec<SyntaxError>,
    _ty: PhantomData<T>,
}

impl<T> Parse<T> {
    fn new(green_node: GreenNode, errors: Vec<SyntaxError>) -> Self {
        Self {
            green_node,
            errors,
            _ty: PhantomData,
        }
    }

    fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn debug_dump(&self) -> String {
        let tree = format!("{:#?}", self.syntax());
        tree[..tree.len() - 1].to_string()
    }
}

impl SourceFile {
    pub fn parse(input: &str) -> Parse<SourceFile> {
        let (node, errors) = parser::parse_text(input);
        Parse::new(node, errors)
    }
}
