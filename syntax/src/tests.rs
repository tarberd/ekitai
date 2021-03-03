use super::*;
use ::parser::{ParseError, SyntaxKind};
use expect_test::expect;

fn check(input: &str, expected_tree: expect_test::Expect) {
    let parse = parser::parse_text(input);
    expected_tree.assert_eq(&parse.debug_dump());
}

#[test]
fn parse_empty() {
    check("", expect![["EkitaiSource@0..0"]]);
}

#[test]
fn parse_function_definition() {
    check(
        "fn id() -> i32",
        expect![[r#"
            EkitaiSource@0..14
              FunctionDefinition@0..14
                FnKw@0..2 "fn"
                Whitespace@2..3 " "
                Name@3..5
                  Identifier@3..5 "id"
                OpenParenthesis@5..6 "("
                CloseParenthesis@6..7 ")"
                Whitespace@7..8 " "
                Arrow@8..10 "->"
                Whitespace@10..11 " "
                Identifier@11..14 "i32""#]],
    );
}

#[test]
fn parse_function_definition_missing_id() {
    let parse = parser::parse_text("fn foo() ->");
    let expected_tree = expect![[r#"
        EkitaiSource@0..11
          FunctionDefinition@0..11
            FnKw@0..2 "fn"
            Whitespace@2..3 " "
            Name@3..6
              Identifier@3..6 "foo"
            OpenParenthesis@6..7 "("
            CloseParenthesis@7..8 ")"
            Whitespace@8..9 " "
            Arrow@9..11 "->""#]];
    expected_tree.assert_eq(&parse.debug_dump());
    let errors = vec![SyntaxError::new(
        ParseError::new(vec![SyntaxKind::Identifier], None),
        TextRange::new(9.into(), 11.into()),
    )];
    assert_eq!(errors, parse.errors);
}

#[test]
fn parse_function_half() {
    check(
        "fn foo fn id() -> i32",
        expect![[r#"
            EkitaiSource@0..21
              FunctionDefinition@0..7
                FnKw@0..2 "fn"
                Whitespace@2..3 " "
                Name@3..7
                  Identifier@3..6 "foo"
                  Whitespace@6..7 " "
              FunctionDefinition@7..21
                FnKw@7..9 "fn"
                Whitespace@9..10 " "
                Name@10..12
                  Identifier@10..12 "id"
                OpenParenthesis@12..13 "("
                CloseParenthesis@13..14 ")"
                Whitespace@14..15 " "
                Arrow@15..17 "->"
                Whitespace@17..18 " "
                Identifier@18..21 "i32""#]],
    );
}
