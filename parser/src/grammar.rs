use super::{ParseError, Parser};
use crate::parser::marker::{CompletedMarker, Marker};
use crate::syntax_kind::SyntaxKind::{self, *};
use crate::TokenSource;

const ITEM_RECOVERY_SET: &[SyntaxKind] = &[FnKw];

pub(crate) fn parse_root<S: TokenSource>(p: &mut Parser<S>) {
    let m = p.start();

    while p.current().is_some() {
        if p.at(FnKw) {
            parse_function_definition(p)
        } else if p.at(TypeKw) {
            parse_type_definition(p)
        }
    }
    m.complete(p, SourceFile);
}

fn parse_type_definition<S: TokenSource>(p: &mut Parser<S>) {
    assert!(p.at(TypeKw));
    let m = p.start();
    p.bump();
    parse_name(p);
    parse_value_constructors(p);
    m.complete(p, TypeDefinition);
}

fn parse_value_constructors<S: TokenSource>(p: &mut Parser<S>) {
    assert!(p.at(OpenBraces));

    let m = p.start();
    p.bump();

    while !p.at(CloseBraces) && p.current() != None {
        let m = p.start();
        parse_name(p);
        parse_value_constructor_parameters(p);
        m.complete(p, ValueConstructor);
        if p.at(Comma) {
            p.bump();
        }
    }

    p.expect(CloseBraces);
    m.complete(p, ValueConstructorList);
}

fn parse_value_constructor_parameters<S: TokenSource>(p: &mut Parser<S>) {
    if p.at(OpenParenthesis) {
        let m = p.start();
        p.bump();

        while !p.at(CloseParenthesis) && p.current() != None {
            parse_type(p);
            if p.at(Comma) {
                p.bump();
            }
        }

        p.expect(CloseParenthesis);
        m.complete(p, ConstructorParameterList);
    } else {
        p.error_and_recover(
            ParseError::new(OpenParenthesis, p.current()),
            ITEM_RECOVERY_SET,
        );
    }
}

fn parse_function_definition<S: TokenSource>(p: &mut Parser<S>) {
    assert!(p.at(FnKw));
    let m = p.start();
    p.bump();
    parse_name(p);
    parse_function_parameters(p);
    p.expect(ThinArrow);
    parse_type(p);
    parse_block_expression(p);
    m.complete(p, FunctionDefinition);
}

fn parse_function_parameters<S: TokenSource>(p: &mut Parser<S>) {
    if p.at(OpenParenthesis) {
        let m = p.start();
        p.bump();

        while !p.at(CloseParenthesis) && p.current() != None {
            parse_parameter(p);
            if p.at(Comma) {
                p.bump();
            }
        }

        p.expect(CloseParenthesis);
        m.complete(p, ParameterList);
    } else {
        p.error_and_recover(
            ParseError::new(OpenParenthesis, p.current()),
            ITEM_RECOVERY_SET,
        );
    }
}

fn parse_pattern<S: TokenSource>(p: &mut Parser<S>) {
    match p.current() {
        Some(Identifier) => match p.nth(1) {
            Some(DoubleColon) => parse_deconstructor_pattern(p),
            _ => parse_binding_pattern(p),
        },
        x => panic!("try to parse pattern but found {:?}", x),
    }
}

fn parse_deconstructor_pattern<S: TokenSource>(p: &mut Parser<S>) {
    let m = p.start();
    parse_path(p);
    parse_binding_pattern_list(p);
    m.complete(p, DeconstructorPattern);
}

fn parse_binding_pattern_list<S: TokenSource>(p: &mut Parser<S>) {
    if p.at(OpenParenthesis) {
        let m = p.start();
        p.bump();

        while !p.at(CloseParenthesis) && p.current() != None {
            parse_binding_pattern(p);
            if p.at(Comma) {
                p.bump();
            }
        }

        p.expect(CloseParenthesis);
        m.complete(p, BindingPatternList);
    } else {
        p.error_and_recover(
            ParseError::new(OpenParenthesis, p.current()),
            ITEM_RECOVERY_SET,
        );
    }
}

fn parse_binding_pattern<S: TokenSource>(p: &mut Parser<S>) {
    let m = p.start();
    parse_name(p);
    m.complete(p, BindingPattern);
}

fn parse_parameter<S: TokenSource>(p: &mut Parser<S>) {
    let m = p.start();
    parse_pattern(p);
    p.expect(Colon);
    parse_type(p);
    m.complete(p, Parameter);
}

fn parse_name<S: TokenSource>(p: &mut Parser<S>) {
    if p.at(Identifier) {
        let m = p.start();
        p.bump();
        m.complete(p, Name);
    } else {
        p.error_and_recover(ParseError::new(Identifier, p.current()), ITEM_RECOVERY_SET);
    }
}

fn parse_name_reference<S: TokenSource>(p: &mut Parser<S>) {
    if p.at(Identifier) {
        let m = p.start();
        p.bump();
        m.complete(p, NameReference);
    } else {
        p.error(ParseError::new(Identifier, p.current()));
    }
}

fn parse_path_segment<S: TokenSource>(p: &mut Parser<S>) {
    let m = p.start();
    match p.current() {
        Some(Identifier) => parse_name_reference(p),
        x => p.error(ParseError::new(Identifier, x)),
    }
    m.complete(p, PathSegment);
}

fn parse_path<S: TokenSource>(p: &mut Parser<S>) {
    let m = p.start();
    parse_path_segment(p);
    let mut qualifier = m.complete(p, Path);
    while p.at(DoubleColon) {
        let path = qualifier.precede(p);
        p.bump();
        parse_path_segment(p);
        let path = path.complete(p, Path);
        qualifier = path;
    }
}

fn parse_type<S: TokenSource>(p: &mut Parser<S>) {
    if p.at(Asterisk) {
        let m = p.start();
        p.bump();
        parse_type(p);
        m.complete(p, PointerType);
    } else if p.at(OpenBraces) {
        parse_refinement_type(p);
    } else if p.at(Identifier) {
        parse_path_type(p);
    }
}

fn parse_path_type<S: TokenSource>(p: &mut Parser<S>) {
    assert!(p.at(Identifier));
    let m = p.start();
    parse_path(p);
    m.complete(p, PathType);
}

fn parse_refinement_type<S: TokenSource>(p: &mut Parser<S>) {
    assert!(p.at(OpenBraces));
    let m = p.start();
    p.bump();
    parse_pattern(p);
    assert!(p.at(Colon));
    p.bump();
    parse_type(p);
    assert!(p.at(Pipe));
    p.bump();
    expression(p);
    assert!(p.at(CloseBraces));
    p.bump();
    m.complete(p, RefinementType);
}

fn parse_block_expression<S: TokenSource>(p: &mut Parser<S>) -> Option<CompletedMarker> {
    if p.at(OpenBraces) {
        let m = p.start();
        parse_statement_list(p);
        let mark = m.complete(p, BlockExpression);
        Some(mark)
    } else {
        p.error(ParseError::new(OpenBraces, p.current()));
        None
    }
}

fn parse_statement_list<S: TokenSource>(p: &mut Parser<S>) {
    assert!(p.at(OpenBraces));
    let mark = p.start();
    p.bump();
    while !p.at(CloseBraces) && p.current() != None {
        if p.at(SemiColon) {
            p.bump();
        }

        parse_statement(p);
    }
    p.expect(CloseBraces);
    mark.complete(p, StatementList);
}

fn parse_statement<S: TokenSource>(p: &mut Parser<S>) {
    let mark = p.start();

    if p.at(LetKw) {
        parse_let_statement(p, mark);
    } else {
        let completed_mark = expression_completed_marker(p);
        let kind = completed_mark.as_ref().map(|cm| cm.kind()).unwrap_or(Error);

        if p.at(CloseBraces) {
            if let Some(cm) = completed_mark {
                cm.undo_completion(p).abandon(p);
                mark.complete(p, kind);
            } else {
                mark.abandon(p);
            }
        } else {
            p.expect(SemiColon);
            mark.complete(p, ExpressionStatement);
        }
    }
}

fn parse_let_statement<S: TokenSource>(p: &mut Parser<S>, mark: Marker) {
    assert!(p.at(LetKw));
    p.bump();
    parse_pattern(p);
    p.expect(Equals);
    expression(p);
    p.expect(SemiColon);
    mark.complete(p, LetStatement);
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
    Rest,
    Equals,
    NotEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,
    And,
    Or,
}

impl InfixOp {
    fn binding_power(&self) -> (u8, u8) {
        // 2 + 2 == 5 ||
        match self {
            Self::And | Self::Or => (1, 2),
            Self::Equals
            | Self::NotEquals
            | Self::Less
            | Self::LessEquals
            | Self::Greater
            | Self::GreaterEquals => (3, 4),
            Self::Add | Self::Sub => (5, 6),
            Self::Mul | Self::Div | Self::Rest => (7, 8),
        }
    }
}

fn expression_completed_marker<S: TokenSource>(p: &mut Parser<S>) -> Option<CompletedMarker> {
    expression_binding_power(p, 0)
}

pub fn expression<S: TokenSource>(p: &mut Parser<S>) {
    expression_binding_power(p, 0);
}

fn expression_binding_power<S: TokenSource>(
    p: &mut Parser<S>,
    minimum_binding_power: u8,
) -> Option<CompletedMarker> {
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
        } else if p.at(Percent) {
            InfixOp::Rest
        } else if p.at(ExclamationEquals) {
            InfixOp::NotEquals
        } else if p.at(DoubleEquals) {
            InfixOp::Equals
        } else if p.at(Less) {
            InfixOp::Less
        } else if p.at(LessEquals) {
            InfixOp::LessEquals
        } else if p.at(Greater) {
            InfixOp::Greater
        } else if p.at(GreaterEquals) {
            InfixOp::GreaterEquals
        } else if p.at(DoublePipe) {
            InfixOp::Or
        } else if p.at(DoubleAmpersand) {
            InfixOp::And
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
    let lhs = if p.at(Integer) || p.at(TrueKw) || p.at(FalseKw) {
        literal(p)
    } else if p.at(Identifier) {
        path_expression(p)
    } else if p.at(Minus) || p.at(Exclamation) || p.at(Asterisk) || p.at(Ampersand) {
        prefix_expression(p)
    } else if p.at(OpenParenthesis) {
        parenthesis_expression(p)
    } else if p.at(OpenBraces) {
        parse_block_expression(p).unwrap()
    } else if p.at(IfKw) {
        if_expression(p)
    } else if p.at(MatchKw) {
        match_expression(p)
    } else if p.at(NewKw) {
        new_expression(p)
    } else {
        p.error(ParseError::new(Integer, p.current()));
        p.error(ParseError::new(Identifier, p.current()));
        p.error(ParseError::new(Minus, p.current()));
        p.error(ParseError::new(OpenParenthesis, p.current()));
        return None;
    };

    Some(postfix_expression(p, lhs))
}

fn new_expression<S: TokenSource>(p: &mut Parser<S>) -> CompletedMarker {
    assert!(p.at(NewKw));
    let m = p.start();
    p.bump();
    let path_marker = path_expression(p);
    call_expr(p, path_marker);
    m.complete(p, NewExpression)
}

fn postfix_expression<S: TokenSource>(p: &mut Parser<S>, lhs: CompletedMarker) -> CompletedMarker {
    match p.current() {
        Some(OpenParenthesis) => call_expr(p, lhs),
        _ => lhs,
    }
}

fn call_expr<S: TokenSource>(p: &mut Parser<S>, lhs: CompletedMarker) -> CompletedMarker {
    assert!(p.at(OpenParenthesis));
    let m = lhs.precede(p);

    argument_list(p);

    m.complete(p, CallExpression)
}

fn argument_list<S: TokenSource>(p: &mut Parser<S>) {
    assert!(p.at(OpenParenthesis));
    let m = p.start();
    p.bump();

    while !p.at(CloseParenthesis) && p.current() != None {
        expression(p);
        if p.at(Comma) {
            p.bump();
        }
    }

    if p.at(CloseParenthesis) {
        p.bump();
    } else {
        p.error(ParseError::new(CloseParenthesis, None))
    }
    m.complete(p, ArgumentList);
}

fn literal<S: TokenSource>(p: &mut Parser<S>) -> CompletedMarker {
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::Literal)
}

fn path_expression<S: TokenSource>(p: &mut Parser<S>) -> CompletedMarker {
    let m = p.start();
    parse_path(p);
    m.complete(p, PathExpression)
}

fn prefix_expression<S: TokenSource>(p: &mut Parser<S>) -> CompletedMarker {
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
    expression(p);
    p.expect(CloseParenthesis);

    m.complete(p, ParenthesisExpression)
}

fn if_expression<S: TokenSource>(p: &mut Parser<S>) -> CompletedMarker {
    assert!(p.at(IfKw));
    let m = p.start();
    p.bump();
    expression(p);
    parse_block_expression(p);
    p.bump();
    parse_block_expression(p);
    m.complete(p, IfExpression)
}

fn match_expression<S: TokenSource>(p: &mut Parser<S>) -> CompletedMarker {
    assert!(p.at(MatchKw));
    let m = p.start();
    p.bump();
    expression(p);
    parse_match_case_list(p);
    m.complete(p, MatchExpression)
}

fn parse_match_case_list<S: TokenSource>(p: &mut Parser<S>) -> CompletedMarker {
    let m = p.start();
    p.expect(OpenBraces);

    while !p.at(CloseBraces) && p.current() != None {
        let m = p.start();
        parse_pattern(p);
        p.expect(FatArrow);
        expression(p);
        m.complete(p, MatchCase);
        if p.at(Comma) {
            p.bump();
        }
    }
    p.expect(CloseBraces);

    m.complete(p, MatchCaseList)
}
