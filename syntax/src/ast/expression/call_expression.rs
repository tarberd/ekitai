use crate::ast::{AstNode, Expression, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct CallExpression(pub(super) SyntaxNode);

impl_trait_ast_node!(CallExpression);

impl CallExpression {
    pub fn callee(&self) -> Option<Expression> {
        self.syntax().children().find_map(Expression::cast)
    }

    pub fn argument_list(&self) -> Option<ArgumentList> {
        self.syntax().children().find_map(ArgumentList::cast)
    }
}

#[derive(Debug)]
pub struct ArgumentList(SyntaxNode);

impl_trait_ast_node!(ArgumentList);

impl ArgumentList {
    pub fn arguments(&self) -> impl Iterator<Item = Expression> {
        let mut children = self.syntax().children();
        std::iter::from_fn(move || children.by_ref().find_map(Expression::cast))
    }
}
