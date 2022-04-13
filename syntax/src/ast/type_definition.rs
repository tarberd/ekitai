use crate::ast::{AstNode, Name, SyntaxNode, Type};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition(pub(super) SyntaxNode);

impl_trait_ast_node!(TypeDefinition);

impl TypeDefinition {
    pub fn name(&self) -> Option<Name> {
        self.syntax().children().find_map(Name::cast)
    }

    pub fn value_constructor_list(&self) -> Option<ValueConstructorList> {
        self.syntax()
            .children()
            .find_map(ValueConstructorList::cast)
    }
}

#[derive(Debug)]
pub struct ValueConstructor(SyntaxNode);

impl_trait_ast_node!(ValueConstructor);

impl ValueConstructor {
    pub fn name(&self) -> Option<Name> {
        self.syntax().children().find_map(Name::cast)
    }

    pub fn constructor_parameter_list(&self) -> Option<ConstructorParameterList> {
        self.syntax()
            .children()
            .find_map(ConstructorParameterList::cast)
    }
}

#[derive(Debug)]
pub struct ValueConstructorList(SyntaxNode);

impl_trait_ast_node!(ValueConstructorList);

impl ValueConstructorList {
    pub fn constructors(&self) -> impl Iterator<Item = ValueConstructor> {
        let mut children = self.syntax().children();
        std::iter::from_fn(move || children.by_ref().find_map(ValueConstructor::cast))
    }
}

#[derive(Debug)]
pub struct ConstructorParameterList(SyntaxNode);

impl_trait_ast_node!(ConstructorParameterList);

impl ConstructorParameterList {
    pub fn types(&self) -> impl Iterator<Item = Type> {
        let mut children = self.syntax().children();
        std::iter::from_fn(move || children.by_ref().find_map(Type::cast))
    }
}
