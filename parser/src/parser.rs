pub mod error;
pub mod event;
mod marker;

use super::TokenSource;
use crate::syntax_kind::SyntaxKind;
use error::ParseError;
use event::Event;
use marker::Marker;

pub struct Parser<Source: TokenSource>
where
    Source: TokenSource,
{
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

    pub(crate) fn bump(&mut self) {
        self.token_source.bump();
        self.events.push(Event::AddToken);
    }

    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            self.error(ParseError::new(vec![kind], self.current()));
            false
        }
    }

    pub(crate) fn error(&mut self, error: ParseError) {
        self.events.push(Event::Error(error));
    }

    pub(crate) fn error_and_recover(&mut self, error: ParseError, recovery_set: &[SyntaxKind]) {
        if self
            .current()
            .map_or(false, |kind| recovery_set.contains(&kind))
        {
            self.error(error);
            return;
        }

        let m = self.start();
        self.error(error);
        self.bump();
        m.complete(self, SyntaxKind::Error);
    }
}
