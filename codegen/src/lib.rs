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
            hir::ScalarType::Int(int_kind) => match int_kind {
                hir::IntKind::I32 => context.i32_type().into(),
                hir::IntKind::I64 => context.i64_type().into(),
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

    // let function_map =
    //     module
    //         .functions
    //         .iter()
    //         .fold(HashMap::new(), |mut function_map, (id, function)| {
    //             let ftype = &module_type_map.function_to_type[id];
    //             let ftype = inkwell_generate_function_type(&context, &ftype);
    //             let function = llvm_module.add_function(&function.name.id, ftype, None);
    //             function_map.insert(id, function);
    //             function_map
    //         });

    // for (fid, function) in module.functions.iter() {
    //     let llfunction = function_map[&fid];
    //     let llbody = context.append_basic_block(llfunction, "");
    //     builder.position_at_end(llbody);

    //     let body = &function.body;

    //     let mut name_map = HashMap::new();

    //     for (func_param_pos, (name_id, _)) in body.parameters.iter().enumerate() {
    //         let llfunction = function_map[&fid];
    //         let param = llfunction.get_nth_param(func_param_pos as u32).unwrap();
    //         let alloc = builder.build_alloca(param.get_type(), "");
    //         name_map.insert(name_id, alloc.into());
    //         builder.build_store(alloc, param);
    //     }

    //     let body_type_map = BodyTypeMap::new(module, &module_type_map, body);

    //     let return_value = build_expression(
    //         &context,
    //         &builder,
    //         &llfunction,
    //         &function_map,
    //         &name_map,
    //         module,
    //         body,
    //         &body_type_map,
    //         body.block,
    //     );

    //     builder.build_return(Some(&return_value));
    // }
}
