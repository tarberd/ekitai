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
    pub(crate) const fn syntax_kind() -> SyntaxKind {
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
    pub(crate) const fn syntax_kind() -> SyntaxKind {
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
    pub(crate) const fn syntax_kind() -> SyntaxKind {
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
    pub(crate) const fn syntax_kind() -> SyntaxKind {
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
    pub(crate) const fn syntax_kind() -> SyntaxKind {
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
    pub(crate) const fn syntax_kind() -> SyntaxKind {
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
pub struct Exclamation(SyntaxToken);

impl CstToken for Exclamation {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl Exclamation {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::Exclamation
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for Exclamation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for Exclamation {
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
    pub(crate) const fn syntax_kind() -> SyntaxKind {
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
    pub(crate) const fn syntax_kind() -> SyntaxKind {
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
    pub(crate) const fn syntax_kind() -> SyntaxKind {
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
    pub(crate) const fn syntax_kind() -> SyntaxKind {
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
    pub(crate) const fn syntax_kind() -> SyntaxKind {
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
pub struct DoubleAmpersand(SyntaxToken);

impl CstToken for DoubleAmpersand {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl DoubleAmpersand {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::DoubleAmpersand
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for DoubleAmpersand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for DoubleAmpersand {
    type Error = SyntaxToAstError;

    fn try_from(syntax_node: SyntaxToken) -> Result<Self, Self::Error> {
        match syntax_node.kind() {
            x if x == Self::syntax_kind() => Ok(Self(syntax_node)),
            other => Err(Self::Error::new(Self::syntax_kind(), other)),
        }
    }
}

#[derive(Debug)]
pub struct DoublePipe(SyntaxToken);

impl CstToken for DoublePipe {
    fn as_syntax_token(&self) -> &SyntaxToken {
        &self.0
    }
}

impl DoublePipe {
    pub(crate) const fn syntax_kind() -> SyntaxKind {
        SyntaxKind::DoublePipe
    }

    pub fn text(&self) -> &str {
        self.as_syntax_token().text()
    }
}

impl std::fmt::Display for DoublePipe {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_syntax_token(), f)
    }
}

impl std::convert::TryFrom<SyntaxToken> for DoublePipe {
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
    DoubleAmpersand(DoubleAmpersand),
    DoublePipe(DoublePipe),
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
            Self::DoubleAmpersand(tok) => tok.as_syntax_token(),
            Self::DoublePipe(tok) => tok.as_syntax_token(),
        }
    }
}

impl BinaryOperator {
    const fn syntax_kind_set() -> &'static [SyntaxKind] {
        const KINDS: &[SyntaxKind] = &[
            Plus::syntax_kind(),
            Minus::syntax_kind(),
            Asterisk::syntax_kind(),
            Slash::syntax_kind(),
            Percent::syntax_kind(),
            DoubleEquals::syntax_kind(),
            ExclamationEquals::syntax_kind(),
            Less::syntax_kind(),
            LessEquals::syntax_kind(),
            Greater::syntax_kind(),
            GreaterEquals::syntax_kind(),
            DoubleAmpersand::syntax_kind(),
            DoublePipe::syntax_kind(),
        ];
        KINDS
    }

    fn from_raw_unchecked(raw: SyntaxToken) -> Self {
        match raw.kind() {
            kind if kind == Plus::syntax_kind() => Self::Plus(Plus(raw)),
            kind if kind == Minus::syntax_kind() => Self::Minus(Minus(raw)),
            kind if kind == Asterisk::syntax_kind() => Self::Asterisk(Asterisk(raw)),
            kind if kind == Slash::syntax_kind() => Self::Slash(Slash(raw)),
            kind if kind == Percent::syntax_kind() => Self::Percent(Percent(raw)),
            kind if kind == DoubleEquals::syntax_kind() => Self::DoubleEquals(DoubleEquals(raw)),
            kind if kind == ExclamationEquals::syntax_kind() => {
                Self::ExclamationEquals(ExclamationEquals(raw))
            }
            kind if kind == Less::syntax_kind() => Self::Less(Less(raw)),
            kind if kind == LessEquals::syntax_kind() => Self::LessEquals(LessEquals(raw)),
            kind if kind == Greater::syntax_kind() => Self::Greater(Greater(raw)),
            kind if kind == GreaterEquals::syntax_kind() => Self::GreaterEquals(GreaterEquals(raw)),
            kind if kind == DoubleAmpersand::syntax_kind() => {
                Self::DoubleAmpersand(DoubleAmpersand(raw))
            }
            kind if kind == DoublePipe::syntax_kind() => Self::DoublePipe(DoublePipe(raw)),
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
    Exclamation(Exclamation),
}

impl CstToken for UnaryOperator {
    fn as_syntax_token(&self) -> &SyntaxToken {
        match self {
            Self::Minus(tok) => tok.as_syntax_token(),
            Self::Exclamation(tok) => tok.as_syntax_token(),
        }
    }
}

impl UnaryOperator {
    const fn syntax_kind_set() -> &'static [SyntaxKind] {
        const KINDS: &[SyntaxKind] = &[Minus::syntax_kind(), Exclamation::syntax_kind()];
        KINDS
    }

    fn from_raw_unchecked(raw: SyntaxToken) -> Self {
        match raw.kind() {
            kind if kind == Minus::syntax_kind() => Self::Minus(Minus(raw)),
            kind if kind == Exclamation::syntax_kind() => Self::Exclamation(Exclamation(raw)),
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
