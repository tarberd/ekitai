pub mod cst;
pub(crate) mod lexer;
pub mod parser;
pub(crate) mod syntax_tree;

#[cfg(test)]
mod tests;

use crate::parser::SyntaxError;
use cst::SourceFile;
use rowan::GreenNode;
use std::convert::TryFrom;
use syntax_tree::SyntaxNode;

#[derive(Debug)]
pub struct Parse {
    green_node: GreenNode,
    errors: Vec<SyntaxError>,
}

impl Parse {
    fn new(green_node: GreenNode, errors: Vec<SyntaxError>) -> Self {
        Self {
            green_node,
            errors,
        }
    }

    fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn ast_node(&self) -> SourceFile {
        SourceFile::try_from(self.syntax_node()).ok().unwrap()
    }

    pub fn debug_dump(&self) -> String {
        let tree = format!("{:#?}", self.syntax_node());
        tree[..tree.len() - 1].to_string()
    }
}

impl SourceFile {
    pub fn parse(input: &str) -> Parse {
        let (node, errors) = parser::parse_text(input);
        Parse::new(node, errors)
    }
}
