use hir::{HirDatabase, SourceDatabase, Type, TypeLocationId, Upcast};
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum, StructType},
};
use std::collections::HashMap;

#[salsa::database(
    hir::SourceDatabaseStorage,
    hir::InternDatabaseStorage,
    hir::DefinitionsDatabaseStorage,
    hir::HirDatabaseStorage
)]
#[derive(Default)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl Upcast<dyn hir::SourceDatabase> for Database {
    fn upcast(&self) -> &(dyn hir::SourceDatabase + 'static) {
        &*self
    }
}

impl Upcast<dyn hir::DefinitionsDatabase> for Database {
    fn upcast(&self) -> &(dyn hir::DefinitionsDatabase + 'static) {
        &*self
    }
}

impl salsa::Database for Database {}

fn inkwell_basic_type<'ink>(
    context: &'ink Context,
    type_map: &HashMap<TypeLocationId, StructType<'ink>>,
    ty: &Type,
) -> BasicTypeEnum<'ink> {
    match ty {
        Type::AbstractDataType(id) => type_map[id].into(),
        Type::FunctionDefinition(_id) => todo!(),
        Type::Scalar(scalar) => match scalar {
            hir::ScalarType::Integer(int_kind) => match int_kind {
                hir::IntegerKind::I32 => context.i32_type().into(),
                hir::IntegerKind::I64 => context.i64_type().into(),
            },
        },
    }
}

pub fn compile_text(source: String) {
    let mut db = Database::default();

    db.set_source_file_text(source);

    build_assembly_ir(&db)
}

pub fn build_assembly_ir(db: &dyn HirDatabase) {
    let context = Context::create();
    let llvm_module = context.create_module("ekitai_module");
    let _builder = context.create_builder();

    let mut type_map = HashMap::new();

    let def_map = db.source_file_definitions_map();

    for id in def_map.item_scope.definitions.iter() {
        match id {
            hir::LocationId::FunctionLocationId(_) => continue,
            hir::LocationId::TypeLocationId(ty_id) => {
                let ty = db.type_definition_data(*ty_id);
                let sty = context.opaque_struct_type(ty.name.id.as_str());
                sty.set_body(&[context.i128_type().into(); 3], false);
                type_map.insert(*ty_id, sty);
            }
        };
    }

    let mut function_map = HashMap::new();

    for id in def_map.item_scope.definitions.iter() {
        match id {
            hir::LocationId::FunctionLocationId(f_id) => {
                let fun = db.function_definition_data(*f_id);
                let sig = db.function_definition_signature(*f_id);

                let param_tys = sig
                    .parameter_types
                    .iter()
                    .map(|ty| inkwell_basic_type(&context, &type_map, ty));

                let ret_ty = inkwell_basic_type(&context, &type_map, &sig.return_type);

                let param_tys: Vec<_> = param_tys.collect();
                let llfty = ret_ty.fn_type(&param_tys, false);
                function_map.insert(*f_id, llfty);
                llvm_module.add_function(fun.name.id.as_str(), llfty, None);
            }
            hir::LocationId::TypeLocationId(_) => continue,
        }
    }

    println!("{}", llvm_module.print_to_string().to_string());
    let _ = llvm_module.print_to_file("out.ll");
}
