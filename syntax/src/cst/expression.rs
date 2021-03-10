use super::raw::SyntaxNode;
use super::CstNode;
use super::SyntaxToAstError;
use parser::SyntaxKind;
use std::convert::TryFrom;
use std::fmt::Display;

#[derive(Debug)]
pub enum Expression {
    BlockExpression(BlockExpression),
    Literal(Literal),
    InfixExpression(InfixExpression),
}

impl Expression {
    fn try_from_set() -> &'static [SyntaxKind] {
        static KINDS: &[SyntaxKind] = &[
            SyntaxKind::BlockExpression,
            SyntaxKind::Literal,
            SyntaxKind::InfixExpression,
        ];
        &KINDS
    }

    fn from_raw_unchecked(raw: SyntaxNode) -> Self {
        match raw.kind() {
            SyntaxKind::BlockExpression => Self::BlockExpression(BlockExpression(raw)),
            SyntaxKind::Literal => Self::Literal(Literal(raw)),
            SyntaxKind::InfixExpression => Self::InfixExpression(InfixExpression(raw)),
            _ => panic!(),
        }
    }

    fn as_syntax_node(&self) -> &SyntaxNode {
        match self {
            Expression::BlockExpression(e) => e.as_syntax_node(),
            Expression::Literal(e) => e.as_syntax_node(),
            Expression::InfixExpression(e) => e.as_syntax_node(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for Expression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::try_from_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::try_from_set()[0], other)),
        }
    }
}

#[derive(Debug)]
pub struct Literal(SyntaxNode);

impl Literal {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Literal
    }
}

impl CstNode for Literal {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for Literal {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct InfixExpression(SyntaxNode);

impl InfixExpression {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::InfixExpression
    }

    pub fn lhs(&self) -> Option<Expression> {
        self.as_syntax_node()
            .children()
            .find_map(|n| Expression::try_from(n).ok())
    }

    pub fn rhs(&self) -> Option<Expression> {
        self.as_syntax_node()
            .children()
            .filter_map(|n| Expression::try_from(n).ok())
            .nth(1)
    }
}

impl CstNode for InfixExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_syntax_node(), f)
    }
}

impl TryFrom<SyntaxNode> for InfixExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct BlockExpression(SyntaxNode);

impl CstNode for BlockExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl BlockExpression {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::BlockExpression
    }

    pub fn tail_expression(&self) -> Option<Expression> {
        self.as_syntax_node()
            .children()
            .find_map(|s| Expression::try_from(s).ok())
    }
}

impl Display for BlockExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_syntax_node(), f)
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
