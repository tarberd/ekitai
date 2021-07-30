use std::fs;
use std::path::PathBuf;

fn main() {
    let args: Vec<_> = std::env::args().into_iter().map(PathBuf::from).collect();

    if let Some(file_path) = args.get(1) {
        println!("Compiling file: {}", file_path.to_str().unwrap());
        match fs::read_to_string(file_path) {
            Ok(source) => drive(source),
            Err(err) => println!("{}", err),
        }
    }
}

fn drive(source: String) {
    codegen::compile_text(source);
}
