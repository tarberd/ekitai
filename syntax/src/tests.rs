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
        "fn id() -> i32 { 5 }",
        expect![[r#"
            EkitaiSource@0..20
              FunctionDefinition@0..20
                FnKw@0..2 "fn"
                Whitespace@2..3 " "
                Name@3..5
                  Identifier@3..5 "id"
                OpenParenthesis@5..6 "("
                CloseParenthesis@6..7 ")"
                Whitespace@7..8 " "
                Arrow@8..10 "->"
                Whitespace@10..11 " "
                NameReference@11..14
                  Identifier@11..14 "i32"
                Whitespace@14..15 " "
                BlockExpression@15..20
                  OpenBraces@15..16 "{"
                  Whitespace@16..17 " "
                  Literal@17..18
                    Integer@17..18 "5"
                  Whitespace@18..19 " "
                  CloseBraces@19..20 "}""#]],
    );
}

#[test]
fn parse_function_definition_missing_type_id() {
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
    let errors = vec![
        SyntaxError::new(
            ParseError::new(vec![SyntaxKind::Identifier], None),
            TextRange::new(9.into(), 11.into()),
        ),
        SyntaxError::new(
            ParseError::new(vec![SyntaxKind::OpenBraces], None),
            TextRange::new(9.into(), 11.into()),
        ),
    ];
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
                Name@3..6
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
                NameReference@18..21
                  Identifier@18..21 "i32""#]],
    );
}

#[test]
fn parse_expression() {
    check(
        "fn id() -> i32 { 1 + -5 + 9 * a - 1 -2 -3  }",
        expect![[r#"
            EkitaiSource@0..44
              FunctionDefinition@0..44
                FnKw@0..2 "fn"
                Whitespace@2..3 " "
                Name@3..5
                  Identifier@3..5 "id"
                OpenParenthesis@5..6 "("
                CloseParenthesis@6..7 ")"
                Whitespace@7..8 " "
                Arrow@8..10 "->"
                Whitespace@10..11 " "
                NameReference@11..14
                  Identifier@11..14 "i32"
                Whitespace@14..15 " "
                BlockExpression@15..44
                  OpenBraces@15..16 "{"
                  Whitespace@16..17 " "
                  InfixExpression@17..41
                    InfixExpression@17..38
                      InfixExpression@17..35
                        InfixExpression@17..31
                          InfixExpression@17..23
                            Literal@17..18
                              Integer@17..18 "1"
                            Whitespace@18..19 " "
                            Plus@19..20 "+"
                            Whitespace@20..21 " "
                            PrefixExpression@21..23
                              Minus@21..22 "-"
                              Literal@22..23
                                Integer@22..23 "5"
                          Whitespace@23..24 " "
                          Plus@24..25 "+"
                          Whitespace@25..26 " "
                          InfixExpression@26..31
                            Literal@26..27
                              Integer@26..27 "9"
                            Whitespace@27..28 " "
                            Asterisk@28..29 "*"
                            Whitespace@29..30 " "
                            NameReference@30..31
                              Identifier@30..31 "a"
                        Whitespace@31..32 " "
                        Minus@32..33 "-"
                        Whitespace@33..34 " "
                        Literal@34..35
                          Integer@34..35 "1"
                      Whitespace@35..36 " "
                      Minus@36..37 "-"
                      Literal@37..38
                        Integer@37..38 "2"
                    Whitespace@38..39 " "
                    Minus@39..40 "-"
                    Literal@40..41
                      Integer@40..41 "3"
                  Whitespace@41..43 "  "
                  CloseBraces@43..44 "}""#]],
    );
}
