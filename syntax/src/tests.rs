use super::*;
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
fn parse_fnkw() {
    check(
        "fn id() -> i32",
        expect![[r#"
            EkitaiSource@0..14
              FunctionDefinition@0..14
                FnKw@0..2 "fn"
                Whitespace@2..3 " "
                Identifier@3..5 "id"
                OpenParentesis@5..6 "("
                CloseParentesis@6..7 ")"
                Whitespace@7..8 " "
                Arrow@8..10 "->"
                Whitespace@10..11 " "
                Identifier@11..14 "i32""#]],
    );
}
