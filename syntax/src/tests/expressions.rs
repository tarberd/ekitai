use super::*;

fn check_str(actual: &str, expect: Expect) {
    let parse = Expression::parse(actual);
    expect.assert_eq(&parse.debug_dump());
}

#[test]
fn integer_literals() {
    check_str(
        "5",
        expect![[r#"
        Literal@0..1
          Integer@0..1 "5""#]],
    );
    check_str(
        "1239849128374981723948712938491283749871234",
        expect![[r#"
        Literal@0..43
          Integer@0..43 "123984912837498172394 ...""#]],
    );
}

#[test]
fn name_literals() {
    check_str(
        "my_id_",
        expect![[r#"
            NameReference@0..6
              Identifier@0..6 "my_id_""#]],
    );
}

#[test]
fn unary_operations() {
    check_str(
        "-5",
        expect![[r#"
        PrefixExpression@0..2
          Minus@0..1 "-"
          Literal@1..2
            Integer@1..2 "5""#]],
    );

    check_str(
        "(-a)",
        expect![[r#"
            ParenthesisExpression@0..4
              OpenParenthesis@0..1 "("
              PrefixExpression@1..3
                Minus@1..2 "-"
                NameReference@2..3
                  Identifier@2..3 "a"
              CloseParenthesis@3..4 ")""#]],
    );
}

#[test]
fn single_binary_operations() {
    check_str(
        "5 + a",
        expect![[r#"
            InfixExpression@0..5
              Literal@0..1
                Integer@0..1 "5"
              Whitespace@1..2 " "
              Plus@2..3 "+"
              Whitespace@3..4 " "
              NameReference@4..5
                Identifier@4..5 "a""#]],
    );
    check_str(
        "a - 5",
        expect![[r#"
            InfixExpression@0..5
              NameReference@0..1
                Identifier@0..1 "a"
              Whitespace@1..2 " "
              Minus@2..3 "-"
              Whitespace@3..4 " "
              Literal@4..5
                Integer@4..5 "5""#]],
    );
    check_str(
        "a * b",
        expect![[r#"
            InfixExpression@0..5
              NameReference@0..1
                Identifier@0..1 "a"
              Whitespace@1..2 " "
              Asterisk@2..3 "*"
              Whitespace@3..4 " "
              NameReference@4..5
                Identifier@4..5 "b""#]],
    );
    check_str(
        "a / b",
        expect![[r#"
            InfixExpression@0..5
              NameReference@0..1
                Identifier@0..1 "a"
              Whitespace@1..2 " "
              Slash@2..3 "/"
              Whitespace@3..4 " "
              NameReference@4..5
                Identifier@4..5 "b""#]],
    );
    check_str(
        "a % b",
        expect![[r#"
            InfixExpression@0..5
              NameReference@0..1
                Identifier@0..1 "a"
              Whitespace@1..2 " "
              Percent@2..3 "%"
              Whitespace@3..4 " "
              NameReference@4..5
                Identifier@4..5 "b""#]],
    );
}

#[test]
fn nested_expression() {
    check_str(
        "(a + b) * c",
        expect![[r#"
        InfixExpression@0..11
          ParenthesisExpression@0..7
            OpenParenthesis@0..1 "("
            InfixExpression@1..6
              NameReference@1..2
                Identifier@1..2 "a"
              Whitespace@2..3 " "
              Plus@3..4 "+"
              Whitespace@4..5 " "
              NameReference@5..6
                Identifier@5..6 "b"
            CloseParenthesis@6..7 ")"
          Whitespace@7..8 " "
          Asterisk@8..9 "*"
          Whitespace@9..10 " "
          NameReference@10..11
            Identifier@10..11 "c""#]],
    );

    check_str(
        "d + (a + b) * c",
        expect![[r#"
        InfixExpression@0..15
          NameReference@0..1
            Identifier@0..1 "d"
          Whitespace@1..2 " "
          Plus@2..3 "+"
          Whitespace@3..4 " "
          InfixExpression@4..15
            ParenthesisExpression@4..11
              OpenParenthesis@4..5 "("
              InfixExpression@5..10
                NameReference@5..6
                  Identifier@5..6 "a"
                Whitespace@6..7 " "
                Plus@7..8 "+"
                Whitespace@8..9 " "
                NameReference@9..10
                  Identifier@9..10 "b"
              CloseParenthesis@10..11 ")"
            Whitespace@11..12 " "
            Asterisk@12..13 "*"
            Whitespace@13..14 " "
            NameReference@14..15
              Identifier@14..15 "c""#]],
    );
}

#[test]
fn function_call() {
    check_str(
        "foo()",
        expect![[r#"
        CallExpression@0..5
          NameReference@0..3
            Identifier@0..3 "foo"
          ArgumentList@3..5
            OpenParenthesis@3..4 "("
            CloseParenthesis@4..5 ")""#]],
    );
    check_str(
        "(foo + a)()",
        expect![[r#"
        CallExpression@0..11
          ParenthesisExpression@0..9
            OpenParenthesis@0..1 "("
            InfixExpression@1..8
              NameReference@1..4
                Identifier@1..4 "foo"
              Whitespace@4..5 " "
              Plus@5..6 "+"
              Whitespace@6..7 " "
              NameReference@7..8
                Identifier@7..8 "a"
            CloseParenthesis@8..9 ")"
          ArgumentList@9..11
            OpenParenthesis@9..10 "("
            CloseParenthesis@10..11 ")""#]],
    );
    check_str(
        "((foo)())()",
        expect![[r#"
        CallExpression@0..11
          ParenthesisExpression@0..9
            OpenParenthesis@0..1 "("
            CallExpression@1..8
              ParenthesisExpression@1..6
                OpenParenthesis@1..2 "("
                NameReference@2..5
                  Identifier@2..5 "foo"
                CloseParenthesis@5..6 ")"
              ArgumentList@6..8
                OpenParenthesis@6..7 "("
                CloseParenthesis@7..8 ")"
            CloseParenthesis@8..9 ")"
          ArgumentList@9..11
            OpenParenthesis@9..10 "("
            CloseParenthesis@10..11 ")""#]],
    );
}

#[test]
fn function_call_with_operation_at_end() {
    check_str(
        "id() + foo()",
        expect![[r#"
            InfixExpression@0..12
              CallExpression@0..4
                NameReference@0..2
                  Identifier@0..2 "id"
                ArgumentList@2..4
                  OpenParenthesis@2..3 "("
                  CloseParenthesis@3..4 ")"
              Whitespace@4..5 " "
              Plus@5..6 "+"
              Whitespace@6..7 " "
              CallExpression@7..12
                NameReference@7..10
                  Identifier@7..10 "foo"
                ArgumentList@10..12
                  OpenParenthesis@10..11 "("
                  CloseParenthesis@11..12 ")""#]],
    );
}

#[test]
fn function_call_with_arguments() {
    check_str(
        "foo(5+5, a, b, -a)",
        expect![[r#"
        CallExpression@0..18
          NameReference@0..3
            Identifier@0..3 "foo"
          ArgumentList@3..18
            OpenParenthesis@3..4 "("
            InfixExpression@4..7
              Literal@4..5
                Integer@4..5 "5"
              Plus@5..6 "+"
              Literal@6..7
                Integer@6..7 "5"
            Comma@7..8 ","
            Whitespace@8..9 " "
            NameReference@9..10
              Identifier@9..10 "a"
            Comma@10..11 ","
            Whitespace@11..12 " "
            NameReference@12..13
              Identifier@12..13 "b"
            Comma@13..14 ","
            Whitespace@14..15 " "
            PrefixExpression@15..17
              Minus@15..16 "-"
              NameReference@16..17
                Identifier@16..17 "a"
            CloseParenthesis@17..18 ")""#]],
    );
    check_str(
        "my_f(-5,) * (my_f(a))",
        expect![[r#"
        InfixExpression@0..21
          CallExpression@0..9
            NameReference@0..4
              Identifier@0..4 "my_f"
            ArgumentList@4..9
              OpenParenthesis@4..5 "("
              PrefixExpression@5..7
                Minus@5..6 "-"
                Literal@6..7
                  Integer@6..7 "5"
              Comma@7..8 ","
              CloseParenthesis@8..9 ")"
          Whitespace@9..10 " "
          Asterisk@10..11 "*"
          Whitespace@11..12 " "
          ParenthesisExpression@12..21
            OpenParenthesis@12..13 "("
            CallExpression@13..20
              NameReference@13..17
                Identifier@13..17 "my_f"
              ArgumentList@17..20
                OpenParenthesis@17..18 "("
                NameReference@18..19
                  Identifier@18..19 "a"
                CloseParenthesis@19..20 ")"
            CloseParenthesis@20..21 ")""#]],
    );
}

#[test]
fn expression_blocks() {
    check_str(
        "{0}",
        expect![[r#"
            BlockExpression@0..3
              OpenBraces@0..1 "{"
              Literal@1..2
                Integer@1..2 "0"
              CloseBraces@2..3 "}""#]],
    );
    check_str(
        "{({{(foo())}})}",
        expect![[r#"
            BlockExpression@0..15
              OpenBraces@0..1 "{"
              ParenthesisExpression@1..14
                OpenParenthesis@1..2 "("
                BlockExpression@2..13
                  OpenBraces@2..3 "{"
                  BlockExpression@3..12
                    OpenBraces@3..4 "{"
                    ParenthesisExpression@4..11
                      OpenParenthesis@4..5 "("
                      CallExpression@5..10
                        NameReference@5..8
                          Identifier@5..8 "foo"
                        ArgumentList@8..10
                          OpenParenthesis@8..9 "("
                          CloseParenthesis@9..10 ")"
                      CloseParenthesis@10..11 ")"
                    CloseBraces@11..12 "}"
                  CloseBraces@12..13 "}"
                CloseParenthesis@13..14 ")"
              CloseBraces@14..15 "}""#]],
    );
}

#[test]
fn bollean_comparions() {
    check_str("a == a", expect![[r#"
        InfixExpression@0..6
          NameReference@0..1
            Identifier@0..1 "a"
          Whitespace@1..2 " "
          DoubleEquals@2..4 "=="
          Whitespace@4..5 " "
          NameReference@5..6
            Identifier@5..6 "a""#]]);
    check_str("a != a", expect![[r#"
        InfixExpression@0..6
          NameReference@0..1
            Identifier@0..1 "a"
          Whitespace@1..2 " "
          ExclamationEquals@2..4 "!="
          Whitespace@4..5 " "
          NameReference@5..6
            Identifier@5..6 "a""#]]);
    check_str("a > a", expect![[r#"
        InfixExpression@0..5
          NameReference@0..1
            Identifier@0..1 "a"
          Whitespace@1..2 " "
          Greater@2..3 ">"
          Whitespace@3..4 " "
          NameReference@4..5
            Identifier@4..5 "a""#]]);
    check_str("a >= a", expect![[r#"
        InfixExpression@0..6
          NameReference@0..1
            Identifier@0..1 "a"
          Whitespace@1..2 " "
          GreaterEquals@2..4 ">="
          Whitespace@4..5 " "
          NameReference@5..6
            Identifier@5..6 "a""#]]);
    check_str("a < a", expect![[r#"
        InfixExpression@0..5
          NameReference@0..1
            Identifier@0..1 "a"
          Whitespace@1..2 " "
          Less@2..3 "<"
          Whitespace@3..4 " "
          NameReference@4..5
            Identifier@4..5 "a""#]]);
    check_str("a <= a", expect![[r#"
        InfixExpression@0..6
          NameReference@0..1
            Identifier@0..1 "a"
          Whitespace@1..2 " "
          LessEquals@2..4 "<="
          Whitespace@4..5 " "
          NameReference@5..6
            Identifier@5..6 "a""#]]);
}

#[test]
fn expression_precedence() {
    check_str("a <= -a + 5 * 4", expect![[r#"
        InfixExpression@0..15
          NameReference@0..1
            Identifier@0..1 "a"
          Whitespace@1..2 " "
          LessEquals@2..4 "<="
          Whitespace@4..5 " "
          InfixExpression@5..15
            PrefixExpression@5..7
              Minus@5..6 "-"
              NameReference@6..7
                Identifier@6..7 "a"
            Whitespace@7..8 " "
            Plus@8..9 "+"
            Whitespace@9..10 " "
            InfixExpression@10..15
              Literal@10..11
                Integer@10..11 "5"
              Whitespace@11..12 " "
              Asterisk@12..13 "*"
              Whitespace@13..14 " "
              Literal@14..15
                Integer@14..15 "4""#]]);

    check_str("a + -b * c > a", expect![[r#"
        InfixExpression@0..14
          InfixExpression@0..10
            NameReference@0..1
              Identifier@0..1 "a"
            Whitespace@1..2 " "
            Plus@2..3 "+"
            Whitespace@3..4 " "
            InfixExpression@4..10
              PrefixExpression@4..6
                Minus@4..5 "-"
                NameReference@5..6
                  Identifier@5..6 "b"
              Whitespace@6..7 " "
              Asterisk@7..8 "*"
              Whitespace@8..9 " "
              NameReference@9..10
                Identifier@9..10 "c"
          Whitespace@10..11 " "
          Greater@11..12 ">"
          Whitespace@12..13 " "
          NameReference@13..14
            Identifier@13..14 "a""#]]);
}

#[test]
fn if_expression() {
    check_str(
        "if a != 0 { 0 } else { 5 }",
        expect![[r#"
            IfExpression@0..26
              IfKw@0..2 "if"
              Whitespace@2..3 " "
              InfixExpression@3..9
                NameReference@3..4
                  Identifier@3..4 "a"
                Whitespace@4..5 " "
                ExclamationEquals@5..7 "!="
                Whitespace@7..8 " "
                Literal@8..9
                  Integer@8..9 "0"
              Whitespace@9..10 " "
              BlockExpression@10..15
                OpenBraces@10..11 "{"
                Whitespace@11..12 " "
                Literal@12..13
                  Integer@12..13 "0"
                Whitespace@13..14 " "
                CloseBraces@14..15 "}"
              Whitespace@15..16 " "
              ElseKw@16..20 "else"
              Whitespace@20..21 " "
              BlockExpression@21..26
                OpenBraces@21..22 "{"
                Whitespace@22..23 " "
                Literal@23..24
                  Integer@23..24 "5"
                Whitespace@24..25 " "
                CloseBraces@25..26 "}""#]],
    );
}
