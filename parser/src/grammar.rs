use crate::{SyntaxKind, parser::{event::Event, error::ParseError}};
use crate::syntax_kind::SyntaxKind::*;
use crate::TokenSource;

fn wrap_parent(parent_kind: SyntaxKind, events: Vec<Event>) -> Vec<Event> {
    let mut wrapped = events;
    wrapped.insert(0, Event::StartNode{ forward_parent: None, kind: parent_kind });
    wrapped.append(&mut vec![Event::FinishNode]);
    wrapped
}

fn match_tokens(parent_kind: SyntaxKind, expected: Vec<SyntaxKind>, actual: Vec<SyntaxKind>) -> Vec<Event> {
    let events: Vec<Event> = (0..expected.len())
        .map(|i| {
            let expected_token = expected[i];
            let actual_token = actual.get(i).copied();
            if Some(expected_token) == actual_token {
                Event::AddToken
            } else {
                Event::Error(ParseError::new(vec![expected_token], actual_token))
            }
        })
        .collect();

    let match_keyword = actual.first().copied() == expected.first().copied();
    let matched_all = events.len() == expected.len();
    let match_error = events.iter().any(|ev| match ev {
        Event::Error {..} => true,
        _ => false
    });
    dbg!(actual.first().copied());
    dbg!(match_keyword);
    dbg!(match_error);
    dbg!(matched_all);
    dbg!(&events);

    if match_keyword {
        wrap_parent(parent_kind, events)
    } else {
        vec![]
    }
}

pub(crate) fn parse_root<S: TokenSource>(token_source: S) -> Vec<Event> {
    let events = match_tokens(
        FunctionDefinition,
        vec![FnKw, Identifier, OpenParenthesis, CloseParenthesis, Arrow, Identifier],
        token_source.collect()
    );

    wrap_parent(EkitaiSource, events)
}
