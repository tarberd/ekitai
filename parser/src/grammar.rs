use super::{ParseError, Parser};
use crate::syntax_kind::SyntaxKind::{self, *};
use crate::TokenSource;

const ITEM_RECOVERY_SET: &[SyntaxKind] = &[FnKw];

pub(crate) fn parse_root<S: TokenSource>(p: &mut Parser<S>) {
    let m = p.start();

    while let Some(_) = p.current() {
        if p.at(FnKw) {
            parse_function(p)
        }
    }
    m.complete(p, EkitaiSource);
}

fn parse_function<S: TokenSource>(p: &mut Parser<S>) {
    assert!(p.at(FnKw));
    let m = p.start();
    p.bump();

    parse_name(p);

    p.expect(OpenParenthesis);
    p.expect(CloseParenthesis);
    p.expect(Arrow);
    p.expect(Identifier);

    m.complete(p, FunctionDefinition);
}

fn parse_name<S: TokenSource>(p: &mut Parser<S>) {
    if p.at(Identifier) {
        let m = p.start();
        p.bump();
        m.complete(p, Name);
    } else {
        p.error_and_recover(
            ParseError::new(vec![Identifier], p.current()),
            ITEM_RECOVERY_SET,
        );
    }
}
