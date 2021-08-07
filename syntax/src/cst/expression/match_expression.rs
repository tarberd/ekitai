use crate::cst::{raw::SyntaxNode, CstNode, Expression, Pattern, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct MatchExpression(pub(super) SyntaxNode);

impl CstNode for MatchExpression {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl MatchExpression {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::MatchExpression
    }

    pub fn matchee(&self) -> Option<Expression> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|s| Expression::try_from(s).ok())
    }

    pub fn case_list(&self) -> Option<MatchCaseList> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|s| MatchCaseList::try_from(s).ok())
    }
}

impl std::fmt::Display for MatchExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for MatchExpression {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct MatchCaseList(SyntaxNode);

impl CstNode for MatchCaseList {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl MatchCaseList {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::MatchCaseList
    }

    pub fn cases(&self) -> impl Iterator<Item = MatchCase> {
        use std::convert::TryFrom;
        let mut children = self.as_syntax_node().children();
        std::iter::from_fn(move || children.by_ref().find_map(|n| MatchCase::try_from(n).ok()))
    }
}

impl std::fmt::Display for MatchCaseList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for MatchCaseList {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct MatchCase(SyntaxNode);

impl CstNode for MatchCase {
    fn as_syntax_node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl MatchCase {
    fn syntax_kind() -> SyntaxKind {
        SyntaxKind::MatchCase
    }

    pub fn pattern(&self) -> Option<Pattern> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|s| Pattern::try_from(s).ok())
    }

    pub fn expression(&self) -> Option<Expression> {
        use std::convert::TryFrom;
        self.as_syntax_node()
            .children()
            .find_map(|s| Expression::try_from(s).ok())
    }
}

impl std::fmt::Display for MatchCase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_node(), f)
    }
}

impl std::convert::TryFrom<SyntaxNode> for MatchCase {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxNode) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}
