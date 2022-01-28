use std::fs;
use std::path::PathBuf;

use codegen::{
    CodeGenTarget, CodeGenTargetABI, CodeGenTargetArch, CodeGenTargetSystem, CodeGenTargetTriple,
    CodeGenTargetVendor,
};

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
    let target = CodeGenTarget {
        triple: CodeGenTargetTriple {
            arch: CodeGenTargetArch::X86_64,
            vendor: CodeGenTargetVendor::PC,
            system: CodeGenTargetSystem::Linux,
            abi: CodeGenTargetABI::GNU,
        },
    };
    codegen::compile_text(source, target);
}
