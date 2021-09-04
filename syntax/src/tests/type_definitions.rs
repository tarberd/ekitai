use super::*;

#[test]
fn test_type_with_two_simple_constructors() {
    check_str(
        "type Data {
        No(),
        Yes(),
    }",
        expect![[r#"
            SourceFile@0..46
              TypeDefinition@0..46
                TypeKw@0..4 "type"
                Whitespace@4..5 " "
                Name@5..9
                  Identifier@5..9 "Data"
                Whitespace@9..10 " "
                ValueConstructorList@10..46
                  OpenBraces@10..11 "{"
                  Whitespace@11..20 "\n        "
                  ValueConstructor@20..24
                    Name@20..22
                      Identifier@20..22 "No"
                    ConstructorParameterList@22..24
                      OpenParenthesis@22..23 "("
                      CloseParenthesis@23..24 ")"
                  Comma@24..25 ","
                  Whitespace@25..34 "\n        "
                  ValueConstructor@34..39
                    Name@34..37
                      Identifier@34..37 "Yes"
                    ConstructorParameterList@37..39
                      OpenParenthesis@37..38 "("
                      CloseParenthesis@38..39 ")"
                  Comma@39..40 ","
                  Whitespace@40..45 "\n    "
                  CloseBraces@45..46 "}""#]],
    );
}

#[test]
fn test_type_and_function() {
    check_str(
        "type Data {
        No(),
        Yes(),
    }
    fn foo() -> i32 { 0i32 }",
        expect![[r#"
            SourceFile@0..75
              TypeDefinition@0..46
                TypeKw@0..4 "type"
                Whitespace@4..5 " "
                Name@5..9
                  Identifier@5..9 "Data"
                Whitespace@9..10 " "
                ValueConstructorList@10..46
                  OpenBraces@10..11 "{"
                  Whitespace@11..20 "\n        "
                  ValueConstructor@20..24
                    Name@20..22
                      Identifier@20..22 "No"
                    ConstructorParameterList@22..24
                      OpenParenthesis@22..23 "("
                      CloseParenthesis@23..24 ")"
                  Comma@24..25 ","
                  Whitespace@25..34 "\n        "
                  ValueConstructor@34..39
                    Name@34..37
                      Identifier@34..37 "Yes"
                    ConstructorParameterList@37..39
                      OpenParenthesis@37..38 "("
                      CloseParenthesis@38..39 ")"
                  Comma@39..40 ","
                  Whitespace@40..45 "\n    "
                  CloseBraces@45..46 "}"
              Whitespace@46..51 "\n    "
              FunctionDefinition@51..75
                FnKw@51..53 "fn"
                Whitespace@53..54 " "
                Name@54..57
                  Identifier@54..57 "foo"
                ParameterList@57..59
                  OpenParenthesis@57..58 "("
                  CloseParenthesis@58..59 ")"
                Whitespace@59..60 " "
                ThinArrow@60..62 "->"
                Whitespace@62..63 " "
                PathType@63..66
                  Path@63..66
                    PathSegment@63..66
                      NameReference@63..66
                        Identifier@63..66 "i32"
                Whitespace@66..67 " "
                BlockExpression@67..75
                  OpenBraces@67..68 "{"
                  Whitespace@68..69 " "
                  Literal@69..73
                    Integer@69..73 "0i32"
                  Whitespace@73..74 " "
                  CloseBraces@74..75 "}""#]],
    );
}

#[test]
fn test_type_with_parameters() {
    check_str(
        "type Data {
        No(i32, banana, Data),
        Yes(),
    }",
        expect![[r#"
            SourceFile@0..63
              TypeDefinition@0..63
                TypeKw@0..4 "type"
                Whitespace@4..5 " "
                Name@5..9
                  Identifier@5..9 "Data"
                Whitespace@9..10 " "
                ValueConstructorList@10..63
                  OpenBraces@10..11 "{"
                  Whitespace@11..20 "\n        "
                  ValueConstructor@20..41
                    Name@20..22
                      Identifier@20..22 "No"
                    ConstructorParameterList@22..41
                      OpenParenthesis@22..23 "("
                      PathType@23..26
                        Path@23..26
                          PathSegment@23..26
                            NameReference@23..26
                              Identifier@23..26 "i32"
                      Comma@26..27 ","
                      Whitespace@27..28 " "
                      PathType@28..34
                        Path@28..34
                          PathSegment@28..34
                            NameReference@28..34
                              Identifier@28..34 "banana"
                      Comma@34..35 ","
                      Whitespace@35..36 " "
                      PathType@36..40
                        Path@36..40
                          PathSegment@36..40
                            NameReference@36..40
                              Identifier@36..40 "Data"
                      CloseParenthesis@40..41 ")"
                  Comma@41..42 ","
                  Whitespace@42..51 "\n        "
                  ValueConstructor@51..56
                    Name@51..54
                      Identifier@51..54 "Yes"
                    ConstructorParameterList@54..56
                      OpenParenthesis@54..55 "("
                      CloseParenthesis@55..56 ")"
                  Comma@56..57 ","
                  Whitespace@57..62 "\n    "
                  CloseBraces@62..63 "}""#]],
    );
}
