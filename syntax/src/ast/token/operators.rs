use crate::ast::{AstToken, SyntaxToken};
use crate::impl_trait_ast_token;
use parser::SyntaxKind;

#[derive(Debug)]
pub struct Plus(SyntaxToken);
impl_trait_ast_token!(Plus, Plus);

#[derive(Debug)]
pub struct Minus(SyntaxToken);
impl_trait_ast_token!(Minus, Minus);

#[derive(Debug)]
pub struct Asterisk(SyntaxToken);
impl_trait_ast_token!(Asterisk, Asterisk);

#[derive(Debug)]
pub struct Slash(SyntaxToken);
impl_trait_ast_token!(Slash, Slash);

#[derive(Debug)]
pub struct Percent(SyntaxToken);
impl_trait_ast_token!(Percent, Percent);

#[derive(Debug)]
pub struct DoubleEquals(SyntaxToken);
impl_trait_ast_token!(DoubleEquals, DoubleEquals);

#[derive(Debug)]
pub struct Exclamation(SyntaxToken);
impl_trait_ast_token!(Exclamation, Exclamation);

#[derive(Debug)]
pub struct ExclamationEquals(SyntaxToken);
impl_trait_ast_token!(ExclamationEquals, ExclamationEquals);

#[derive(Debug)]
pub struct Greater(SyntaxToken);
impl_trait_ast_token!(Greater, Greater);

#[derive(Debug)]
pub struct GreaterEquals(SyntaxToken);
impl_trait_ast_token!(GreaterEquals, GreaterEquals);

#[derive(Debug)]
pub struct Less(SyntaxToken);
impl_trait_ast_token!(Less, Less);

#[derive(Debug)]
pub struct LessEquals(SyntaxToken);
impl_trait_ast_token!(LessEquals, LessEquals);

#[derive(Debug)]
pub struct Ampersand(SyntaxToken);
impl_trait_ast_token!(Ampersand, Ampersand);

#[derive(Debug)]
pub struct DoubleAmpersand(SyntaxToken);
impl_trait_ast_token!(DoubleAmpersand, DoubleAmpersand);

#[derive(Debug)]
pub struct DoublePipe(SyntaxToken);
impl_trait_ast_token!(DoublePipe, DoublePipe);

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

impl AstToken for BinaryOperator {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        match kind {
            SyntaxKind::Plus
            | SyntaxKind::Minus
            | SyntaxKind::Asterisk
            | SyntaxKind::Slash
            | SyntaxKind::Percent
            | SyntaxKind::DoubleEquals
            | SyntaxKind::ExclamationEquals
            | SyntaxKind::Greater
            | SyntaxKind::GreaterEquals
            | SyntaxKind::Less
            | SyntaxKind::LessEquals
            | SyntaxKind::DoubleAmpersand
            | SyntaxKind::DoublePipe => true,
            _ => false,
        }
    }

    fn cast(node: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        Some(match node.kind() {
            SyntaxKind::Plus => Self::Plus(Plus(node)),
            SyntaxKind::Minus => Self::Minus(Minus(node)),
            SyntaxKind::Asterisk => Self::Asterisk(Asterisk(node)),
            SyntaxKind::Slash => Self::Slash(Slash(node)),
            SyntaxKind::Percent => Self::Percent(Percent(node)),
            SyntaxKind::DoubleEquals => Self::DoubleEquals(DoubleEquals(node)),
            SyntaxKind::ExclamationEquals => Self::ExclamationEquals(ExclamationEquals(node)),
            SyntaxKind::Greater => Self::Greater(Greater(node)),
            SyntaxKind::GreaterEquals => Self::GreaterEquals(GreaterEquals(node)),
            SyntaxKind::Less => Self::Less(Less(node)),
            SyntaxKind::LessEquals => Self::LessEquals(LessEquals(node)),
            SyntaxKind::DoubleAmpersand => Self::DoubleAmpersand(DoubleAmpersand(node)),
            SyntaxKind::DoublePipe => Self::DoublePipe(DoublePipe(node)),
            _ => return None,
        })
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            Self::Plus(tok) => tok.syntax(),
            Self::Minus(tok) => tok.syntax(),
            Self::Asterisk(tok) => tok.syntax(),
            Self::Slash(tok) => tok.syntax(),
            Self::Percent(tok) => tok.syntax(),
            Self::DoubleEquals(tok) => tok.syntax(),
            Self::ExclamationEquals(tok) => tok.syntax(),
            Self::Greater(tok) => tok.syntax(),
            Self::GreaterEquals(tok) => tok.syntax(),
            Self::Less(tok) => tok.syntax(),
            Self::LessEquals(tok) => tok.syntax(),
            Self::DoubleAmpersand(tok) => tok.syntax(),
            Self::DoublePipe(tok) => tok.syntax(),
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}

#[derive(Debug)]
pub enum UnaryOperator {
    Minus(Minus),
    Exclamation(Exclamation),
    Asterisk(Asterisk),
    Ampersand(Ampersand),
}

impl AstToken for UnaryOperator {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        match kind {
            SyntaxKind::Minus
            | SyntaxKind::Exclamation
            | SyntaxKind::Asterisk
            | SyntaxKind::Ampersand => true,
            _ => false,
        }
    }

    fn cast(node: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        Some(match node.kind() {
            SyntaxKind::Minus => Self::Minus(Minus(node)),
            SyntaxKind::Exclamation => Self::Exclamation(Exclamation(node)),
            SyntaxKind::Asterisk => Self::Asterisk(Asterisk(node)),
            SyntaxKind::Ampersand => Self::Ampersand(Ampersand(node)),
            _ => return None,
        })
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            Self::Minus(tok) => tok.syntax(),
            Self::Exclamation(tok) => tok.syntax(),
            Self::Asterisk(tok) => tok.syntax(),
            Self::Ampersand(tok) => tok.syntax(),
        }
    }
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
