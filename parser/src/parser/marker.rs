use crate::syntax_kind::SyntaxKind;
use crate::TokenSource;
use super::Parser;
use super::Event;
use drop_bomb::DropBomb;

#[derive(Debug)]
pub(crate) struct Marker {
    position: usize,
    bomb: DropBomb,
}

impl Marker {
    pub(super) fn new(position: usize) -> Self {
        Self { position, bomb: DropBomb::new("Marker must be completed!") }
    }
}

impl Marker {
    pub(crate) fn complete<S: TokenSource>(mut self, p: &mut Parser<S>, kind: SyntaxKind) {
        self.bomb.defuse();

        let event_at_pos = &mut p.events[self.position];
        assert_eq!(*event_at_pos, Event::Placeholder);

        *event_at_pos = Event::StartNode {
            kind,
            forward_parent: None,
        };
        
        p.events.push(Event::FinishNode);
    }
}
