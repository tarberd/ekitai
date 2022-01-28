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

        let end = p.events.len();
        p.events.push(Event::FinishNode);

        CompletedMarker {
            start: self.position,
            end,
            kind,
        }
    }
    pub(crate) fn abandon<S: TokenSource>(mut self, p: &mut Parser<S>) {
        self.bomb.defuse();

        if self.position == p.events.len() - 1 {
            p.events.pop().unwrap();
        }
    }
}

pub(crate) struct CompletedMarker {
    start: usize,
    end: usize,
    kind: SyntaxKind,
}

impl CompletedMarker {
    pub(crate) fn precede<S: TokenSource>(self, p: &mut Parser<S>) -> Marker {
        let new_m = p.start();

        if let Event::StartNode {
            ref mut forward_parent,
            ..
        } = p.events[self.start]
        {
            *forward_parent = Some(new_m.position - self.start);
        } else {
            unreachable!();
        }

        new_m
    }

    pub(crate) fn undo_completion<S: TokenSource>(self, p: &mut Parser<S>) -> Marker {
        match p.events[self.start] {
            ref mut event @ Event::StartNode {
                forward_parent: None,
                ..
            } => {
                *event = Event::Abandoned;
            }
            _ => unreachable!(),
        };
        match p.events[self.end] {
            ref mut slot @ Event::FinishNode => *slot = Event::Abandoned,
            _ => unreachable!(),
        }
        Marker::new(self.start)
    }

    pub(crate) fn kind(&self) -> SyntaxKind {
        self.kind
    }
}
