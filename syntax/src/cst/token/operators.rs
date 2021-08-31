use super::super::{raw::SyntaxToken, CstToken, SyntaxToAstError};
use parser::SyntaxKind;

#[derive(Debug)]
pub struct Plus(SyntaxToken);

impl CstToken for Plus {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl Plus {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Plus
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for Plus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for Plus {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct Minus(SyntaxToken);

impl CstToken for Minus {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl Minus {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Minus
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for Minus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for Minus {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct Asterisk(SyntaxToken);

impl CstToken for Asterisk {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl Asterisk {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Asterisk
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for Asterisk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for Asterisk {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct Slash(SyntaxToken);

impl CstToken for Slash {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl Slash {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Slash
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for Slash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for Slash {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct Percent(SyntaxToken);

impl CstToken for Percent {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl Percent {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Percent
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for Percent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for Percent {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct DoubleEquals(SyntaxToken);

impl CstToken for DoubleEquals {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl DoubleEquals {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::DoubleEquals
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for DoubleEquals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for DoubleEquals {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct ExclamationEquals(SyntaxToken);

impl CstToken for ExclamationEquals {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl ExclamationEquals {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::ExclamationEquals
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for ExclamationEquals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for ExclamationEquals {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct Greater(SyntaxToken);

impl CstToken for Greater {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl Greater {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Greater
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for Greater {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for Greater {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct GreaterEquals(SyntaxToken);

impl CstToken for GreaterEquals {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl GreaterEquals {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::GreaterEquals
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for GreaterEquals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for GreaterEquals {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct Less(SyntaxToken);

impl CstToken for Less {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl Less {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Less
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for Less {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for Less {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct LessEquals(SyntaxToken);

impl CstToken for LessEquals {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl LessEquals {
    pub(crate) fn syntax_kind() -> SyntaxKind {
        SyntaxKind::LessEquals
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for LessEquals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for LessEquals {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
    Plus(Plus),
    Minus(Minus),
    Asterisk(Asterisk),
    Slash(Slash),
    Percent(Percent),
    DoubleEquals(DoubleEquals),
    ExclamationEquals(ExclamationEquals),
    Greater(Greater),
    GreaterEquals(GreaterEquals),
    Less(Less),
    LessEquals(LessEquals),
}

impl CstToken for BinaryOperator {
    fn as_syntax_token(&self) -> &SyntaxToken {
        match self {
            Self::Plus(tok) => tok.as_syntax_token(),
            Self::Minus(tok) => tok.as_syntax_token(),
            Self::Asterisk(tok) => tok.as_syntax_token(),
            Self::Slash(tok) => tok.as_syntax_token(),
            Self::Percent(tok) => tok.as_syntax_token(),
            Self::DoubleEquals(tok) => tok.as_syntax_token(),
            Self::ExclamationEquals(tok) => tok.as_syntax_token(),
            Self::Greater(tok) => tok.as_syntax_token(),
            Self::GreaterEquals(tok) => tok.as_syntax_token(),
            Self::Less(tok) => tok.as_syntax_token(),
            Self::LessEquals(tok) => tok.as_syntax_token(),
        }
    }
}

impl BinaryOperator {
    fn syntax_kind_set() -> &'static [SyntaxKind] {
        static KINDS: &[SyntaxKind] = &[
            SyntaxKind::Plus,
            SyntaxKind::Minus,
            SyntaxKind::Asterisk,
            SyntaxKind::Slash,
            SyntaxKind::Percent,
            SyntaxKind::DoubleEquals,
            SyntaxKind::ExclamationEquals,
            SyntaxKind::Less,
            SyntaxKind::LessEquals,
            SyntaxKind::Greater,
            SyntaxKind::GreaterEquals,
        ];
        KINDS
    }

    fn from_raw_unchecked(raw: SyntaxToken) -> Self {
        match raw.kind() {
            SyntaxKind::Plus => Self::Plus(Plus(raw)),
            SyntaxKind::Minus => Self::Minus(Minus(raw)),
            SyntaxKind::Asterisk => Self::Asterisk(Asterisk(raw)),
            SyntaxKind::Slash => Self::Slash(Slash(raw)),
            SyntaxKind::Percent => Self::Percent(Percent(raw)),
            SyntaxKind::DoubleEquals => Self::DoubleEquals(DoubleEquals(raw)),
            SyntaxKind::ExclamationEquals => Self::ExclamationEquals(ExclamationEquals(raw)),
            SyntaxKind::Less => Self::Less(Less(raw)),
            SyntaxKind::LessEquals => Self::LessEquals(LessEquals(raw)),
            SyntaxKind::Greater => Self::Greater(Greater(raw)),
            SyntaxKind::GreaterEquals => Self::GreaterEquals(GreaterEquals(raw)),
            _ => panic!(),
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for BinaryOperator {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::syntax_kind_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind_set()[0], other)),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOperator {
    Minus(Minus),
}

impl CstToken for UnaryOperator {
    fn as_syntax_token(&self) -> &SyntaxToken {
        match self {
            Self::Minus(tok) => tok.as_syntax_token(),
        }
    }
}

impl UnaryOperator {
    fn syntax_kind_set() -> &'static [SyntaxKind] {
        static KINDS: &[SyntaxKind] = &[SyntaxKind::Minus];
        KINDS
    }

    fn from_raw_unchecked(raw: SyntaxToken) -> Self {
        match raw.kind() {
            SyntaxKind::Minus => Self::Minus(Minus(raw)),
            _ => panic!(),
        }
    }
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for UnaryOperator {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if Self::syntax_kind_set().contains(&x) => Ok(Self::from_raw_unchecked(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind_set()[0], other)),
        }
    }
}
