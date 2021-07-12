use std::fs;
use std::path::PathBuf;
use syntax::cst::SourceFile;

fn main() {
    let args: Vec<_> = std::env::args()
        .into_iter()
        .map(PathBuf::from)
        .collect();

    if let Some(file_path) = args.get(1) {
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
    println!("Syntax Errors: {:#?}", parse.errors());

    let module = hir::Module::lower(parse.ast_node());
    println!("{:#?}", module);
    // println!("Lower Errors: {:#?}", errors);

    // let errors = hir::type_check::type_check_module(&module);

    // println!("Type Errors: {:#?}", errors);

    codegen::build_assembly_ir(&module);
}
