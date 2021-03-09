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
