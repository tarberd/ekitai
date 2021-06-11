use super::*;

#[test]
fn parse_empty() {
    check_str("", expect![[r#"SourceFile@0..0"#]]);
}