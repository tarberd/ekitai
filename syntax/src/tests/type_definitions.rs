use super::*;

#[test]
fn test_type_with_two_simple_constructors() {
    check_str(
        "type Data {
        No,
        Yes,
    }",
        expect![[r#"
            SourceFile@0..42
              TypeDefinition@0..42
                TypeKw@0..4 "type"
                Whitespace@4..5 " "
                Name@5..9
                  Identifier@5..9 "Data"
                Whitespace@9..10 " "
                ValueConstructorList@10..42
                  OpenBraces@10..11 "{"
                  Whitespace@11..20 "\n        "
                  ValueConstructor@20..22
                    Name@20..22
                      Identifier@20..22 "No"
                  Comma@22..23 ","
                  Whitespace@23..32 "\n        "
                  ValueConstructor@32..35
                    Name@32..35
                      Identifier@32..35 "Yes"
                  Comma@35..36 ","
                  Whitespace@36..41 "\n    "
                  CloseBraces@41..42 "}""#]],
    );
}

#[test]
fn test_type_and_function() {
    check_str(
        "type Data {
        No,
        Yes,
    }
    fn foo() -> i32 { 0i32 }",
        expect![[r#"
            SourceFile@0..71
              TypeDefinition@0..42
                TypeKw@0..4 "type"
                Whitespace@4..5 " "
                Name@5..9
                  Identifier@5..9 "Data"
                Whitespace@9..10 " "
                ValueConstructorList@10..42
                  OpenBraces@10..11 "{"
                  Whitespace@11..20 "\n        "
                  ValueConstructor@20..22
                    Name@20..22
                      Identifier@20..22 "No"
                  Comma@22..23 ","
                  Whitespace@23..32 "\n        "
                  ValueConstructor@32..35
                    Name@32..35
                      Identifier@32..35 "Yes"
                  Comma@35..36 ","
                  Whitespace@36..41 "\n    "
                  CloseBraces@41..42 "}"
              Whitespace@42..47 "\n    "
              FunctionDefinition@47..71
                FnKw@47..49 "fn"
                Whitespace@49..50 " "
                Name@50..53
                  Identifier@50..53 "foo"
                ParameterList@53..55
                  OpenParenthesis@53..54 "("
                  CloseParenthesis@54..55 ")"
                Whitespace@55..56 " "
                ThinArrow@56..58 "->"
                Whitespace@58..59 " "
                PathType@59..62
                  Path@59..62
                    PathSegment@59..62
                      NameReference@59..62
                        Identifier@59..62 "i32"
                Whitespace@62..63 " "
                BlockExpression@63..71
                  OpenBraces@63..64 "{"
                  Whitespace@64..65 " "
                  Literal@65..69
                    Integer@65..69 "0i32"
                  Whitespace@69..70 " "
                  CloseBraces@70..71 "}""#]],
    );
}
