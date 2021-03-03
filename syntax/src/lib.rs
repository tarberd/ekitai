pub(crate) mod cst;
pub(crate) mod lexer;
pub mod parser;

#[cfg(test)]
mod tests;

use cst::SyntaxNode;
use rowan::GreenNode;
use ::parser::ParseError;
use text_size::TextRange;

#[derive(Debug, PartialEq)]
pub struct SyntaxError {
    pub error: ParseError,
    pub range: TextRange,
}

impl SyntaxError {
    pub fn new(error: ParseError, range: TextRange) -> Self {
        Self { error, range }
    }
}
#[derive(Debug)]
pub struct Parse {
    green_node: GreenNode,
    errors: Vec<SyntaxError>,
}

impl Parse {
    fn new(green_node: GreenNode, errors: Vec<SyntaxError>) -> Self {
        Self { green_node, errors }
    }

    fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn debug_dump(&self) -> String {
        let tree = format!("{:#?}", self.syntax());
        tree[..tree.len() - 1].to_string()
    }
}
