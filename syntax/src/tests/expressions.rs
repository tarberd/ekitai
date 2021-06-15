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
        "-a",
        expect![[r#"
        PrefixExpression@0..2
          Minus@0..1 "-"
          NameReference@1..2
            Identifier@1..2 "a""#]],
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
