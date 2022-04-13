use crate::ast::{AstNode, EkitaiLanguage, Expression, Pattern, SyntaxNode};
use crate::impl_trait_ast_node;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct BlockExpression(pub(super) SyntaxNode);

impl_trait_ast_node!(BlockExpression);

impl BlockExpression {
    pub fn statement_list(&self) -> Option<StatementList> {
        self.syntax().children().find_map(StatementList::cast)
    }
}

#[derive(Debug)]
pub struct StatementList(pub(super) SyntaxNode);

impl_trait_ast_node!(StatementList);

impl StatementList {
    pub fn statements(&self) -> impl Iterator<Item = Statement> {
        let mut children = self.syntax().children();
        std::iter::from_fn(move || children.by_ref().find_map(Statement::cast))
    }

    pub fn tail_expression(&self) -> Option<Expression> {
        self.syntax().children().find_map(Expression::cast)
    }
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Expression(ExpressionStatement),
}

impl AstNode for Statement {
    type Language = EkitaiLanguage;

    fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
    where
        Self: Sized,
    {
        match kind {
            SyntaxKind::LetStatement | SyntaxKind::ExpressionStatement => true,
            _ => false,
        }
    }

    fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self>
    where
        Self: Sized,
    {
        let node = match node.kind() {
            kind if LetStatement::can_cast(kind) => Self::Let(LetStatement(node)),
            kind if ExpressionStatement::can_cast(kind) => {
                Self::Expression(ExpressionStatement(node))
            }
            _ => return None,
        };
        Some(node)
    }

    fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
        match self {
            Statement::Let(f) => f.syntax(),
            Statement::Expression(t) => t.syntax(),
        }
    }
}

#[derive(Debug)]
pub struct LetStatement(pub(super) SyntaxNode);

impl_trait_ast_node!(LetStatement);

impl LetStatement {
    pub fn pattern(&self) -> Option<Pattern> {
        self.syntax().children().find_map(Pattern::cast)
    }

    pub fn expression(&self) -> Option<Expression> {
        self.syntax().children().find_map(Expression::cast)
    }
}

#[derive(Debug)]
pub struct ExpressionStatement(pub(super) SyntaxNode);

impl_trait_ast_node!(ExpressionStatement);

impl ExpressionStatement {
    pub fn expression(&self) -> Option<Expression> {
        self.syntax().children().find_map(Expression::cast)
    }
}
