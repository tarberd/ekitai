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
