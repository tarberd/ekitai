mod expression;
mod function_definition;
mod module_item;
mod name;
mod name_ref;
mod path;
mod pattern;
mod source_file;
mod token;
mod ty;
mod type_definition;

pub use expression::{
    BlockExpression, CallExpression, Expression, ExpressionStatement, IfExpression,
    InfixExpression, LetStatement, Literal, MatchCase, MatchCaseList, MatchExpression,
    NewExpression, PathExpression, PrefixExpression, Statement, StatementList,
};
pub use function_definition::{FunctionDefinition, Parameter, ParameterList};
pub use module_item::ModuleItem;
pub use name::Name;
pub use name_ref::NameReference;
pub use path::{Path, PathSegment};
pub use pattern::{BindingPattern, BindingPatternList, DeconstructorPattern, Pattern};
pub use source_file::SourceFile;
pub use token::{
    Asterisk, BinaryOperator, Boolean, Identifier, Integer, Minus, Percent, Plus, Slash,
    TokenLiteral, UnaryOperator,
};
pub use ty::{PathType, PointerType, RefinementType, Type};
pub use type_definition::{TypeDefinition, ValueConstructor, ValueConstructorList};

use parser::SyntaxKind;
use rowan::Language;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct EkitaiLanguage {}

impl Language for EkitaiLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        use num_traits::FromPrimitive;
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        use num_traits::ToPrimitive;
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}

pub type SyntaxToken = rowan::SyntaxToken<EkitaiLanguage>;
pub type SyntaxNode = rowan::SyntaxNode<EkitaiLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<EkitaiLanguage>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<EkitaiLanguage>;
pub use rowan::ast::AstPtr;
pub use rowan::ast::AstNode;

pub trait AstToken {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(node: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }
}

#[macro_export]
macro_rules! impl_trait_ast_node {
    ($ast:ident) => {
        impl crate::ast::AstNode for $ast {
            type Language = crate::ast::EkitaiLanguage;

            fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
            where
                Self: Sized,
            {
                kind == SyntaxKind::$ast
            }

            fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self>
            where
                Self: Sized,
            {
                Self::can_cast(node.kind()).then(|| Self(node))
            }

            fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
                &self.0
            }
        }

        impl std::fmt::Display for $ast {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(self.syntax(), f)
            }
        }
    };
}

#[macro_export]
macro_rules! impl_trait_ast_token {
    ($ast:ident, $syntax:ident) => {
        impl crate::ast::AstToken for $ast {
            fn can_cast(kind: SyntaxKind) -> bool
            where
                Self: Sized,
            {
                kind == SyntaxKind::$syntax
            }

            fn cast(node: SyntaxToken) -> Option<Self>
            where
                Self: Sized,
            {
                Self::can_cast(node.kind()).then(|| Self(node))
            }

            fn syntax(&self) -> &SyntaxToken {
                &self.0
            }
        }

        impl std::fmt::Display for $ast {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(self.syntax(), f)
            }
        }
    };
}
