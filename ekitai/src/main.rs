use std::fs;
use std::path::PathBuf;
use syntax::cst::Expression;
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
    let source_file = parse.ast_node();
    println!("{}", source_file);
    let functions = source_file.functions();
    for fun in functions {
        if let Some(body) = fun.body() {
            println!("body: {}", body);
            let tail = body.tail_expression().unwrap();
            println!("tail: {}", tail);
            match tail {
                Expression::InfixExpression(x) => {
                    println!("({}, {})", x.lhs().unwrap(), x.rhs().unwrap())
                }
                _ => panic!("fuck u"),
            };
        }
    }
    let hir_root = hir::Module::lower(source_file);
    println!("{:#?}", hir_root);
}
