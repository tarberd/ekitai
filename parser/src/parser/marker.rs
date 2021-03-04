use super::Event;
use super::Parser;
use crate::syntax_kind::SyntaxKind;
use crate::TokenSource;
use drop_bomb::DropBomb;

#[derive(Debug)]
pub(crate) struct Marker {
    position: usize,
    bomb: DropBomb,
}

impl Marker {
    pub(super) fn new(position: usize) -> Self {
        Self {
            position,
            bomb: DropBomb::new("Marker must be completed!"),
        }
    }
}

impl Marker {
    pub(crate) fn complete<S: TokenSource>(
        mut self,
        p: &mut Parser<S>,
        kind: SyntaxKind,
    ) -> CompletedMarker {
        self.bomb.defuse();

        let event_at_pos = &mut p.events[self.position];
        assert_eq!(*event_at_pos, Event::Placeholder);

        *event_at_pos = Event::StartNode {
            kind,
            forward_parent: None,
        };

        p.events.push(Event::FinishNode);

        CompletedMarker {
            position: self.position,
        }
    }
}

pub(crate) struct CompletedMarker {
    position: usize,
}

impl CompletedMarker {
    pub(crate) fn precede<S: TokenSource>(self, p: &mut Parser<S>) -> Marker {
        let new_m = p.start();

        if let Event::StartNode {
            ref mut forward_parent,
            ..
        } = p.events[self.position]
        {
            *forward_parent = Some(new_m.position - self.position);
        } else {
            unreachable!();
        }

        new_m
    }
}
