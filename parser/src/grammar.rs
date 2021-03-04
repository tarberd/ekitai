use std::result::Result;

use crate::{SyntaxKind, parser::{event::Event, error::ParseError}};
use crate::syntax_kind::SyntaxKind::*;
use crate::TokenSource;

type ParseResult<Source> = Result<(Vec<Event>, Source), (Vec<Event>, Source)>;
// type Parser<Source: TokenSource> = Fn(Vec<Event>, Source) -> ParseResult<Source>;


fn wrap_events(parent_kind: SyntaxKind, events: Vec<Event>) -> Vec<Event> {
    let mut wrapped = events;
    wrapped.insert(0, Event::StartNode{ forward_parent: None, kind: parent_kind });
    wrapped.append(&mut vec![Event::FinishNode]);
    wrapped
}

fn wrap_result<Source: TokenSource>(parent_kind: SyntaxKind, (events, source): (Vec<Event>, Source)) -> ParseResult<Source> {
    Ok((wrap_events(parent_kind, events), source))
}

fn match_tokens<Source: TokenSource>(expected: Vec<SyntaxKind>, original_events: Vec<Event>, original_source: Source) -> ParseResult<Source> {
    let mut used_source = original_source.clone();
    let events: Vec<Event> = (0..expected.len())
        .map(|i| {
            let expected_token = expected[i];
            let actual_token = used_source.next();
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
        Ok(([original_events, events].concat(), used_source))
    } else {
        Err((original_events, original_source))
    }
}

fn parse_function<Source: TokenSource>(events: Vec<Event>, source: Source) -> ParseResult<Source> {
    let (events, source) = match_tokens(
        vec![FnKw, Identifier, OpenParenthesis, CloseParenthesis, Arrow, Identifier],
        events, source
    )?;
    let (events, source) = parse_block_expression(events, source)
        .and_then(|result| wrap_result(FunctionDefinition, result))
        .or_else(|result| wrap_result(FunctionDefinition, result))?;
    Ok((events, source))
}

fn parse_block_expression<Source: TokenSource>(events: Vec<Event>, source: Source) -> ParseResult<Source> {
    match_tokens(vec![OpenBraces, CloseBraces], events, source)
}


pub(crate) fn parse_root<Source: TokenSource>(source: Source) -> Vec<Event> {
    let events: Vec<Event> = vec![];
    let (events, _source) = match parse_function(events, source) {
        Ok(x) => x,
        Err(x) => x,
    };

    wrap_events(EkitaiSource, events)
}
