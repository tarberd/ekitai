use super::*;

fn data_dir() -> PathBuf {
    test_data_dir().join("function_parameters")
}

#[test]
fn single_parameter() {
    check_file(&data_dir(), "single_parameter");
}

#[test]
fn multiple_parameters() {
    check_file(&data_dir(), "multiple_parameters");
}

#[test]
fn trailing_parameter_comma() {
    check_file(&data_dir(), "trailing_parameter_comma");
}

#[test]
fn missing_body() {
    check_file(&data_dir(), "missing_body");
}