use crate::cst::{raw::SyntaxNode, CstNode, Expression, SyntaxToAstError, Pattern};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct BlockExpression(pub(super) SyntaxNode);

impl CstNode for BlockExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl BlockExpression {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::BlockExpression
    }

    pub fn statement_list(&self) -> Option<StatementList> {
        self.as_syntax_node()
            .children()
            .find_map(|s| StatementList::try_from(s).ok())
    }
}

impl std::fmt::Display for BlockExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for BlockExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct StatementList(pub(super) SyntaxNode);

impl CstNode for StatementList {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl StatementList {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::StatementList
    }

    pub fn statements(&self) -> impl Iterator<Item = Statement> {
        let mut children = self.as_syntax_node().children();
        std::iter::from_fn(move || children.by_ref().find_map(|n| Statement::try_from(n).ok()))
    }

    pub fn tail_expression(&self) -> Option<Expression> {
        self.as_syntax_node()
            .children()
            .find_map(|s| Expression::try_from(s).ok())
    }
}

impl std::fmt::Display for StatementList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for StatementList {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Expression(ExpressionStatement),
}

impl CstNode for Statement {
    fn as_syntax_node(&self) -> &SyntaxNode {
        match self {
            Statement::Let(f) => f.as_syntax_node(),
            Statement::Expression(t) => t.as_syntax_node(),
        }
    }
}

impl Statement {
    fn syntax_kind_set() -> &'static [SyntaxKind] {
        static KINDS: &[SyntaxKind] = &[
            LetStatement::syntax_kind(),
            ExpressionStatement::syntax_kind(),
        ];
        KINDS
    }

    fn from_raw_unchecked(raw: SyntaxNode) -> Self {
        match raw.kind() {
            kind if kind == LetStatement::syntax_kind() => {
                Self::Let(LetStatement(raw))
            }
            kind if kind == ExpressionStatement::syntax_kind() => {
                Self::Expression(ExpressionStatement(raw))
            }
            _ => panic!(),
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for Statement {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::syntax_kind_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind_set()[0], other)),
        }
    }
}

#[derive(Debug)]
pub struct LetStatement(pub(super) SyntaxNode);

impl CstNode for LetStatement {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl LetStatement {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::LetStatement
    }

    pub fn pattern(&self) -> Option<Pattern> {
        self.as_syntax_node()
            .children()
            .find_map(|s| Pattern::try_from(s).ok())
    }

    pub fn expression(&self) -> Option<Expression> {
        self.as_syntax_node()
            .children()
            .find_map(|s| Expression::try_from(s).ok())
    }
}

impl std::fmt::Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for LetStatement {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct ExpressionStatement(pub(super) SyntaxNode);

impl CstNode for ExpressionStatement {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl ExpressionStatement {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::ExpressionStatement
    }

    pub fn expression(&self) -> Option<Expression> {
        self.as_syntax_node()
            .children()
            .find_map(|s| Expression::try_from(s).ok())
    }
}

impl std::fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for ExpressionStatement {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
