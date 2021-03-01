use crate::parser::Parser;
use crate::syntax_kind::SyntaxKind;
use crate::TokenSource;

pub(crate) fn parse_root<S: TokenSource>(p: &mut Parser<S>) {
    let m = p.start();
    if p.at(SyntaxKind::FnKw) {
        parse_function(p)
    }
    m.complete(p, SyntaxKind::EkitaiSource);
}

fn parse_function<S: TokenSource>(_p: &mut Parser<S>) {}
