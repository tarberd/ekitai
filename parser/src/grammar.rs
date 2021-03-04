use super::{ParseError, Parser};
use crate::parser::marker::CompletedMarker;
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

    parse_name_reference(p);

    parse_block_expression(p);

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

fn parse_name_reference<S: TokenSource>(p: &mut Parser<S>) {
    if p.at(Identifier) {
        let m = p.start();
        p.bump();
        m.complete(p, NameReference);
    } else {
        p.error(
            ParseError::new(vec![Identifier], p.current()),
        );
    }
}

fn parse_block_expression<S: TokenSource>(p: &mut Parser<S>) {
    if p.at(OpenBraces) {
        let m = p.start();
        p.bump();
        expression(p);
        p.expect(CloseBraces);
        m.complete(p, BlockExpression);
    } else {
        p.error(
            ParseError::new(vec![OpenBraces], p.current()),
        );
    }
}

enum PrefixOp {
    Neg,
}

impl PrefixOp {
    fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Neg => ((), 5),
        }
    }
}

enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl InfixOp {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
        }
    }
}

fn expression<S: TokenSource>(p: &mut Parser<S>) {
    expression_binding_power(p, 0);
}

fn expression_binding_power<S: TokenSource>(p: &mut Parser<S>, minimum_binding_power: u8) -> Option<CompletedMarker> {
    let mut lhs = lhs(p)?;

    loop {
        let op = if p.at(Plus) {
            InfixOp::Add
        } else if p.at(Minus) {
            InfixOp::Sub
        } else if p.at(Asterisk) {
            InfixOp::Mul
        } else if p.at(Slash) {
            InfixOp::Div
        } else {
            // We’re not at an operator; we don’t know what to do next, so we return and let the
            // caller decide.
            break;
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            break;
        }

        // Eat the operator’s token.
        p.bump();

        let m = lhs.precede(p);
        let parsed_rhs = expression_binding_power(p, right_binding_power).is_some();
        lhs = m.complete(p, InfixExpression);

        if !parsed_rhs {
            break;
        }
    }

    Some(lhs)
}

fn lhs<S: TokenSource>(p: &mut Parser<S>) -> Option<CompletedMarker> {
    let cm = if p.at(Integer) {
        literal(p)
    } else if p.at(Identifier) {
        name_reference(p)
    } else if p.at(Minus) {
        prefix_expression(p)
    } else if p.at(OpenParenthesis) {
        parenthesis_expression(p)
    } else {
        p.error(ParseError::new(vec![Integer, Identifier, Minus, OpenParenthesis], p.current()));
        return None;
    };

    Some(cm)
}

fn literal<S: TokenSource>(p: &mut Parser<S>) -> CompletedMarker {
    assert!(p.at(Integer));
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::Literal)
}

fn name_reference<S: TokenSource>(p: &mut Parser<S>) -> CompletedMarker {
    assert!(p.at(Identifier));

    let m = p.start();
    p.bump();
    m.complete(p, NameReference)
}

fn prefix_expression<S: TokenSource>(p: &mut Parser<S>) -> CompletedMarker {
    assert!(p.at(Minus));

    let m = p.start();

    let op = PrefixOp::Neg;
    let ((), right_binding_power) = op.binding_power();

    // Eat the operator’s token.
    p.bump();

    expression_binding_power(p, right_binding_power);

    m.complete(p, PrefixExpression)
}

fn parenthesis_expression<S: TokenSource>(p: &mut Parser<S>) -> CompletedMarker {
    assert!(p.at(OpenParenthesis));

    let m = p.start();
    p.bump();
    expression_binding_power(p, 0);
    p.expect(CloseParenthesis);

    m.complete(p, ParenthesisExpression)
}
