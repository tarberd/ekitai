use crate::ast::{BlockExpression, AstNode, Name, Pattern, Type, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinition(pub(super) SyntaxNode);

impl_trait_ast_node!(FunctionDefinition);

impl FunctionDefinition {
    pub fn name(&self) -> Option<Name> {
        self.syntax()
            .children()
            .find_map(Name::cast)
    }

    pub fn parameter_list(&self) -> Option<ParameterList> {
        self.syntax()
            .children()
            .find_map(ParameterList::cast)
    }

    pub fn return_type(&self) -> Option<Type> {
        self.syntax()
            .children()
            .find_map(Type::cast)
    }

    pub fn body(&self) -> Option<BlockExpression> {
        self.syntax()
            .children()
            .find_map(BlockExpression::cast)
    }
}

#[derive(Debug)]
pub struct ParameterList(SyntaxNode);

impl_trait_ast_node!(ParameterList);

impl ParameterList {
    pub fn parameters(&self) -> impl Iterator<Item = Parameter> {
        let mut children = self.syntax().children();
        std::iter::from_fn(move || children.by_ref().find_map(Parameter::cast))
    }
}

#[derive(Debug)]
pub struct Parameter(SyntaxNode);

impl_trait_ast_node!(Parameter);

impl Parameter {
    pub fn pattern(&self) -> Option<Pattern> {
        self.syntax()
            .children()
            .find_map(Pattern::cast)
    }

    pub fn ty(&self) -> Option<Type> {
        self.syntax()
            .children()
            .find_map(Type::cast)
    }
}
