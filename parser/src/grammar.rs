use std::{collections::HashMap, result::Result, usize};

use crate::{SyntaxKind, parser::{event::Event, error::ParseError}};
use crate::syntax_kind::SyntaxKind::*;
use crate::TokenSource;

// type Parser<Source: TokenSource> = Fn(Vec<Event>, Source) -> ParseResult<Source>;

#[derive(Debug, Clone, Copy)]
struct Marker {
    from: usize,
    // to: usize,
}

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
struct MarkerId(usize);

#[derive(Debug, Clone)]
struct State<Source: TokenSource> {
    events: Vec<Event>,
    source: Source,
    markers: HashMap<MarkerId, Marker>,
    current_index: usize,
}

impl<Source: TokenSource> State<Source> {
    pub fn new(events: Vec<Event>, source: Source, markers: HashMap<MarkerId, Marker>) -> Self {
        let current_index = events.len();
        Self { events, markers, source, current_index }
    }
    fn add_events(self, events: Vec<Event>) -> Self {
        State::new([self.events.clone(), events].concat(), self.source, self.markers)
    }
    fn add_marker(self) -> (MarkerId, Self) {
        let marker_id = MarkerId(self.markers.len() + 1);
        let mut markers = self.markers;
        markers.insert(marker_id, Marker{ from: self.events.len() });

        (marker_id, State::new(self.events, self.source, markers))
    }
    pub fn marker_start(&self, marker_id: MarkerId) -> usize {
        self.markers.get(&marker_id).unwrap().from
    }
}

type ParseResult<Source> = Result<State<Source>, State<Source>>;

fn wrap_events(parent_kind: SyntaxKind, events: Vec<Event>, at: usize) -> Vec<Event> {
    let mut wrapped = events;
    wrapped.insert(at, Event::StartNode{ forward_parent: None, kind: parent_kind });
    wrapped.append(&mut vec![Event::FinishNode]);
    wrapped
}

fn finish_marker<Source: TokenSource>(parent_kind: SyntaxKind, state: State<Source>, marker_id: MarkerId) -> ParseResult<Source> {
    let marker_start = state.marker_start(marker_id);
    let events = wrap_events(parent_kind, state.events, marker_start);
    let state = State::new(events, state.source, state.markers);
    Ok(state)
}

fn match_tokens<Source: TokenSource>(expected: Vec<SyntaxKind>, original_state: State<Source>) -> ParseResult<Source> {
    let mut state = original_state.clone();
    let events: Vec<Event> = (0..expected.len())
        .map(|i| {
            let expected_token = expected[i];
            let actual_token = state.source.next();
            if Some(expected_token) == actual_token {
                Event::AddToken
            } else {
                Event::Error(ParseError::new(vec![expected_token], actual_token))
            }
        })
        .collect();
    dbg!(events.clone());
    let match_keyword = match events.first() {
        Some(result) => match result {
            Event::AddToken => true,
            _ => false
        },
        _ => false
    };

    if match_keyword {
        Ok(state.add_events(events))
    } else {
        Err(original_state)
    }
}

fn parse_function<Source: TokenSource>(state: State<Source>) -> ParseResult<Source> {
    let (marker_id, state) = state.add_marker();
    let state = match_tokens(
        vec![FnKw, Identifier, OpenParenthesis, CloseParenthesis, Arrow, Identifier],
        state
    )?;
    let state = parse_block_expression(state)
        .and_then(|state| finish_marker(FunctionDefinition, state, marker_id))
        .or_else(|state| finish_marker(FunctionDefinition, state, marker_id))?;
    dbg!(Ok(state))
}

fn parse_block_expression<Source: TokenSource>(state: State<Source>) -> ParseResult<Source> {
    let (marker_id, state) = state.add_marker();
    let state = match_tokens(vec![OpenBraces], state)?;
    let state = parse_literal(state)
        .or_else(|state| finish_marker(BlockExpressionDefinition, state, marker_id))?;
    match_tokens(vec![CloseBraces], state)
        .and_then(|state| finish_marker(BlockExpressionDefinition, state, marker_id))
        .or_else(|state| finish_marker(BlockExpressionDefinition, state, marker_id))
}

fn parse_literal<Source: TokenSource>(state: State<Source>) -> ParseResult<Source> {
    let (marker_id, state) = state.add_marker();
    let state = match_tokens(vec![Integer], state)?;
    finish_marker(LiteralDefinition, state, marker_id)
}


pub(crate) fn parse_root<Source: TokenSource>(source: Source) -> Vec<Event> {
    let state = State::new(vec![], source, HashMap::new());
    let state = match parse_function(state) {
        Ok(state) => state,
        Err(state) => state,
    };

    wrap_events(EkitaiSource, state.events, 0)
}
