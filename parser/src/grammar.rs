use crate::parser::Parser;
use crate::syntax_kind::SyntaxKind::*;
use crate::TokenSource;

pub(crate) fn parse_root<S: TokenSource>(p: &mut Parser<S>) {
    let m = p.start();
    if p.at(FnKw) {
        parse_function(p)
    }
    m.complete(p, EkitaiSource);
}

fn parse_function<S: TokenSource>(p: &mut Parser<S>) {
    assert!(p.at(FnKw));
    let m = p.start();
    p.bump();

    p.expect(Identifier);
    p.expect(OpenParentesis);
    p.expect(CloseParentesis);
    p.expect(Arrow);
    p.expect(Identifier);

    m.complete(p, FunctionDefinition);
}
