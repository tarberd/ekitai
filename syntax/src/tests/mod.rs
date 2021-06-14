use std::path::{Path, PathBuf};

use super::*;
use expect_test::{expect, expect_file, Expect};

mod empty;
mod expressions;
mod function_parameters;

fn crate_root() -> PathBuf {
    let dir = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(dir).to_owned()
}

fn test_data_dir() -> PathBuf {
    crate_root().join("test_data")
}

fn check_str(actual: &str, expect: Expect) {
    let parse = SourceFile::parse(actual);
    expect.assert_eq(&parse.debug_dump());
}

fn check_file(path: &Path, test_name: &str) {
    let file_name = format!("{}.eki", test_name);
    let path = path.join(file_name);
    let source = std::fs::read_to_string(&path)
        .unwrap_or_else(|_| panic!("File at {:?} should be valid", path));
    let expected_file = path.with_extension("cst");
    let parse = SourceFile::parse(&source);
    expect_file![expected_file].assert_eq(&parse.debug_dump());
}
