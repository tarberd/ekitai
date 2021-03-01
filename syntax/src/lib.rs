pub(crate) mod cst;
pub(crate) mod lexer;
pub mod parser;

#[cfg(test)]
mod tests;

use cst::SyntaxNode;
use rowan::GreenNode;

#[derive(Debug)]
pub struct Parse {
    green_node: GreenNode,
    errors: (),
}

impl Parse {
    fn new(green_node: GreenNode, errors: ()) -> Self {
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
