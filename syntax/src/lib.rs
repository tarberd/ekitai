pub mod cst;
pub(crate) mod lexer;
pub mod parser;

#[cfg(test)]
mod tests;

use crate::parser::SyntaxError;
use cst::raw::SyntaxNode;
use cst::{Expression, SourceFile};
use rowan::GreenNode;
use std::marker::PhantomData;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parse<T: cst::CstNode> {
    green_node: GreenNode,
    errors: Vec<SyntaxError>,
    _cst_node_type: PhantomData<T>,
}

impl<T: cst::CstNode> Parse<T> {
    fn new(green_node: GreenNode, errors: Vec<SyntaxError>) -> Self {
        Self {
            green_node,
            errors,
            _cst_node_type: PhantomData,
        }
    }

    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn ast_node(&self) -> T {
        T::try_from(self.syntax_node()).ok().unwrap()
    }

    pub fn errors(&self) -> &Vec<SyntaxError> {
        &self.errors
    }

    pub fn debug_dump(&self) -> String {
        let dump = vec![format!("{:#?}", self.syntax_node()).trim_end().to_owned()];
        let dump = self.errors.iter().fold(dump, |mut dump, error| {
            dump.push(format!("{}", error));
            dump
        });
        dump.join("\n")
    }
}

impl SourceFile {
    pub fn parse(input: &str) -> Parse<Self> {
        let (node, errors) = parser::parse(input);
        Parse::new(node, errors)
    }
}

impl Expression {
    pub fn parse(input: &str) -> Parse<Self> {
        let (node, errors) = parser::parse_expression(input);
        Parse::new(node, errors)
    }
}
