use parser::SyntaxKind;
use rowan::Language;
use rowan::TextRange;

pub use rowan::WalkEvent;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxNodePointer {
    kind: SyntaxKind,
    range: TextRange,
}

impl SyntaxNodePointer {
    pub fn new(node: SyntaxNode) -> Self {
        let range = node.text_range();
        let kind = node.kind();
        Self { kind, range }
    }

    pub fn get_syntax_node(&self, root: &SyntaxNode) -> SyntaxNode {
        assert!(root.parent().is_none());
        std::iter::successors(Some(root.clone()), |node| {
            node.child_or_token_at_range(self.range)
                .and_then(|node_or_token| node_or_token.into_node())
        })
        .find(|node| node.text_range() == self.range && node.kind() == self.kind)
        .unwrap_or_else(|| {
            panic!(
                "Cant convert SyntaxNodePointer {:?} to SyntaxNode from root {:?}",
                self, root
            )
        })
    }
}
