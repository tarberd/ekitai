trait Lower {
    // fn muda_hir(CstNode, HIR) -> HIR 
}

struct Root {

}
impl Root {
    // fn typeck(&self)  { 
        
    // }
}

// impl Lower<SourceFile> for Root {
//     fn lower(source_file: cst::SourceFile) Self {
//         Function::lower(source_file.functions());
//     }
// }

// impl Lower<SourceFile> for Expression {

// }

struct HIR {
    functions: Function[]
}

enum Type {
    All,
}

struct Context {}

impl Context {
    fn get_type_of_def_id()
}

mod test {
    #[test]
    fn typecheck() {
        let src = "fn foo() -> i32 { 5_i32 + 5_i32 }";
        use syntax::cst::source_file::SourceFile;
        let cst = SourceFile::parse(src).ast_node();
        let root = HirRoot::lower(cst);
        let ctx = Context;
        root.type_check(ctx)
    }
}