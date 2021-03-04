use std::result::Result;

use crate::{SyntaxKind, parser::{event::Event, error::ParseError}};
use crate::syntax_kind::SyntaxKind::*;
use crate::TokenSource;

type ParseResult<Source> = Result<(Vec<Event>, Source), (Vec<Event>, Source)>;
// type Parser<Source: TokenSource> = Fn(Vec<Event>, Source) -> ParseResult<Source>;

fn wrap_parent(parent_kind: SyntaxKind, events: Vec<Event>) -> Vec<Event> {
    let mut wrapped = events;
    wrapped.insert(0, Event::StartNode{ forward_parent: None, kind: parent_kind });
    wrapped.append(&mut vec![Event::FinishNode]);
    wrapped
}

fn match_tokens<Source: TokenSource>(parent_kind: SyntaxKind, expected: Vec<SyntaxKind>, token_source: Source) -> Option<(Vec<Event>, Source)> {
    let mut source = token_source;
    let events: Vec<Event> = (0..expected.len())
        .map(|i| {
            let expected_token = expected[i];
            let actual_token = source.next();
            if Some(expected_token) == actual_token {
                Event::AddToken
            } else {
                Event::Error(ParseError::new(vec![expected_token], actual_token))
            }
        })
        .collect();

    let match_keyword = match events.first() {
        Some(result) => match result {
            Event::AddToken => true,
            _ => false
        },
        _ => false
    };
    let matched_all = events.len() == expected.len();
    let match_error = events.iter().any(|ev| match ev {
        Event::Error {..} => true,
        _ => false
    });

    dbg!(match_keyword);
    dbg!(match_error);
    dbg!(matched_all);
    dbg!(&events);

    if match_keyword {
        Some((wrap_parent(parent_kind, events), source))
    } else {
        None
    }
}

fn parse_function<Source: TokenSource>(events: Vec<Event>, token_source: Source) -> ParseResult<Source> {
    let used_source = token_source.clone();
    match match_tokens(
        FunctionDefinition,
        vec![FnKw, Identifier, OpenParenthesis, CloseParenthesis, Arrow, Identifier],
        used_source,
    ) {
        Some((events, used_source)) => Ok((events, used_source)),
        None => Err((events, token_source))
    }
}


pub(crate) fn parse_root<Source: TokenSource>(token_source: Source) -> Vec<Event> {
    let initial_events: Vec<Event> = vec![];
    let events = match parse_function(initial_events, token_source)
        // .or_else(|(events, token_source)| parse_function(events, token_source))
        {
            Ok((events, _)) => events,
            Err((events, _)) => events,
        };

    wrap_parent(EkitaiSource, events)
}
