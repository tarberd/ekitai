use super::*;

#[test]
fn parse_empty() {
    check_str("", expect![[r#"SourceFile@0..0"#]]);
    // check_str("\t", expect![[r#"SourceFile@0..0"#]]);
    // check_str("\n", expect![[r#"SourceFile@0..0"#]]);
    // check_str("\t\n", expect![[r#"SourceFile@0..0"#]]);
}