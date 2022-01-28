use super::*;

fn check_file(test_file: &str) {
    super::check_file(
        &test_data_dir().join("block_expression"),
        &Expression::parse,
        test_file,
    );
}

#[test]
fn expression_blocks() {
    check_str(
        "{0}",
        expect![[r#"
            BlockExpression@0..3
              StatementList@0..3
                OpenBraces@0..1 "{"
                Literal@1..2
                  Integer@1..2 "0"
                CloseBraces@2..3 "}""#]],
    );
    check_str(
        "{({{(foo())}})}",
        expect![[r#"
            BlockExpression@0..15
              StatementList@0..15
                OpenBraces@0..1 "{"
                ParenthesisExpression@1..14
                  OpenParenthesis@1..2 "("
                  BlockExpression@2..13
                    StatementList@2..13
                      OpenBraces@2..3 "{"
                      BlockExpression@3..12
                        StatementList@3..12
                          OpenBraces@3..4 "{"
                          ParenthesisExpression@4..11
                            OpenParenthesis@4..5 "("
                            CallExpression@5..10
                              PathExpression@5..8
                                Path@5..8
                                  PathSegment@5..8
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
fn let_statements() {
    check_file("let_statements");
}

#[test]
fn expression_statements() {
    check_file("expr_statements");
}
