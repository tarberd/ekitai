use std::fs;
use std::path::PathBuf;
use syntax::cst::SourceFile;

fn main() {
    let args: Vec<_> = std::env::args()
        .into_iter()
        .map(|arg| PathBuf::from(arg))
        .collect();

    if let Some(file_path) = args.iter().nth(1) {
        println!("Compiling file: {}", file_path.to_str().unwrap());
        match fs::read_to_string(file_path) {
            Ok(source) => drive(source),
            Err(err) => println!("{}", err),
        }
    }
}

fn drive(source: String) {
    let parse = SourceFile::parse(&source);
    println!("{}", parse.debug_dump());
    println!(
        "{}",
        parse.ast_node()
    );
}
