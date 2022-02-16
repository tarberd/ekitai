use super::*;

fn check_file(test_file: &str) {
    super::check_file(
        &test_data_dir().join("function_parameters"),
        &SourceFile::parse,
        test_file,
    );
}

#[test]
fn single_parameter() {
    check_file("single_parameter");
}

#[test]
fn multiple_parameters() {
    check_file("multiple_parameters");
}

#[test]
fn trailing_parameter_comma() {
    check_file("trailing_parameter_comma");
}

#[test]
fn missing_body() {
    check_file("missing_body");
}

#[test]
fn parameters_with_refinements() {
    check_file("parameter_with_refinements");
}
