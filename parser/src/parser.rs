pub mod event;
mod marker;

use crate::syntax_kind::SyntaxKind;
use super::TokenSource;
use event::Event;
use marker::Marker;

pub struct Parser<Source: TokenSource> 
where Source: TokenSource {
    token_source: Source,
    events: Vec<Event>,
}

impl<Source: TokenSource> Parser<Source> {
    pub(crate) fn new(token_source: Source) -> Self {
        Self {
            token_source,
            events: Vec::new(),
        }
    }

    pub(crate) fn finish(self) -> Vec<Event> {
        self.events
    }

    pub(crate) fn current(&self) -> Option<SyntaxKind> {
        self.token_source.current()
    }

    pub(crate) fn bump(&mut self) {
        self.token_source.bump();
        self.events.push(Event::AddToken);
    }

    pub(crate) fn expect(&mut self, kind: SyntaxKind) {
        if self.at(kind) {
            self.bump();
        } else {
            panic!("unexpected token {:?}, found {:?}.", kind, self.current());
        }
    }

    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    pub(crate) fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        self.token_source
            .lookahead(n)
            .map_or(false, |token| token == kind)
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }
}
