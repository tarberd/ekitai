use std::collections::HashMap;

use hir::type_check::{BodyTypeMap, IntegerType, ModuleTypeMap, Type};
use hir::{BinaryOperator, BlockExpression, Body, Expression, ExpressionId, FunctionId, Literal, Module, NameId};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValueEnum, FunctionValue};

fn inkwell_basic_type<'ink>(context: &'ink Context, ty: &Type) -> BasicTypeEnum<'ink> {
    match ty {
        Type::Integer(IntegerType::I32) => context.i32_type().into(),
        Type::Integer(IntegerType::I64) => context.i64_type().into(),
        Type::Function(_, _) => panic!("trying to lower function type"),
        Type::Unknown => panic!("trying to lower unknown type"),
    }
}

fn inkwell_generate_function_type<'ink>(context: &'ink Context, ty: &Type) -> FunctionType<'ink> {
    match ty {
        Type::Function(parameters, return_ty) => {
            let parameter_types: Vec<_> = parameters
                .iter()
                .map(|ty| inkwell_basic_type(context, ty))
                .collect();
            let return_type = inkwell_basic_type(context, return_ty);
            return_type.fn_type(parameter_types.as_slice(), false)
        }
        _ => panic!("trying to lower non function type to llvm function"),
    }
}

pub fn build_assembly_ir(module: &Module) {
    let context = Context::create();
    let llvm_module = context.create_module("ekitai_module");
    let builder = context.create_builder();

    let module_type_map = ModuleTypeMap::new(module);

    let function_map =
        module
            .functions
            .iter()
            .fold(HashMap::new(), |mut function_map, (id, function)| {
                let ftype = &module_type_map.function_to_type[id];
                let ftype = inkwell_generate_function_type(&context, &ftype);
                let function = llvm_module.add_function(&function.name.id, ftype, None);
                function_map.insert(id, function);
                function_map
            });

    for (fid, function) in module.functions.iter() {
        let llfunction = function_map[&fid];
        let llbody = context.append_basic_block(llfunction, "");
        builder.position_at_end(llbody);

        let body = &function.body;

        let mut name_map = HashMap::new();

        for (func_param_pos, (name_id, _)) in body.parameters.iter().enumerate() {
            let llfunction = function_map[&fid];
            let param = llfunction.get_nth_param(func_param_pos as u32).unwrap();
            let alloc = builder.build_alloca(param.get_type(), "");
            name_map.insert(name_id, alloc.into());
            builder.build_store(alloc, param);
        }

        let body_type_map = BodyTypeMap::new(module, &module_type_map, body);

        let return_value = build_expression(
            &context,
            &builder,
            &function_map,
            &name_map,
            module,
            body,
            &body_type_map,
            body.block,
        );

        builder.build_return(Some(&return_value));
    }

    println!("{}", llvm_module.print_to_string().to_string());
    match llvm_module.print_to_file("out.ll") {
        Ok(_) => {}
        Err(_) => {}
    }
}

fn build_block_expression<'ink>(
    context: &'ink Context,
    builder: &'ink Builder,
    function_map: &HashMap<FunctionId, FunctionValue<'ink>>,
    name_map: &HashMap<NameId, BasicValueEnum<'ink>>,
    module: &Module,
    body: &Body,
    body_type_map: &BodyTypeMap,
    block: &BlockExpression,
) -> BasicValueEnum<'ink> {
    build_expression(
        context,
        builder,
        function_map,
        name_map,
        module,
        body,
        body_type_map,
        block.tail_expression,
    )
}

fn build_expression<'ink>(
    context: &'ink Context,
    builder: &'ink Builder,
    function_map: &HashMap<FunctionId, FunctionValue<'ink>>,
    name_map: &HashMap<NameId, BasicValueEnum<'ink>>,
    module: &Module,
    body: &Body,
    body_type_map: &BodyTypeMap,
    expr_id: ExpressionId,
) -> BasicValueEnum<'ink> {
    let expr = &body.expressions[expr_id];
    match expr {
        Expression::BlockExpression(block) => build_block_expression(
            context,
            builder,
            function_map,
            name_map,
            module,
            body,
            body_type_map,
            block,
        ),
        Expression::BinaryExpression(op, lhs, rhs) => {
            let lllhs = build_expression(
                context,
                builder,
                function_map,
                name_map,
                module,
                body,
                body_type_map,
                *lhs,
            );
            let llrhs = build_expression(
                context,
                builder,
                function_map,
                name_map,
                module,
                body,
                body_type_map,
                *rhs,
            );

            let ty = &body_type_map.type_of_expression[expr_id];

            let llty = inkwell_basic_type(context, ty);

            match llty {
                BasicTypeEnum::ArrayType(_) => todo!(),
                BasicTypeEnum::FloatType(_) => todo!(),
                BasicTypeEnum::IntType(_int_ty) => {
                    let lllhs = lllhs.into_int_value();
                    let llrhs = llrhs.into_int_value();
                    match op {
                        BinaryOperator::Add => builder.build_int_add(lllhs, llrhs, "").into(),
                        BinaryOperator::Sub => builder.build_int_sub(lllhs, llrhs, "").into(),
                        BinaryOperator::Div => {
                            builder.build_int_signed_div(lllhs, llrhs, "").into()
                        }
                        BinaryOperator::Mul => builder.build_int_mul(lllhs, llrhs, "").into(),
                        BinaryOperator::Rem => {
                            builder.build_int_signed_rem(lllhs, llrhs, "").into()
                        }
                    }
                }
                BasicTypeEnum::PointerType(_) => todo!(),
                BasicTypeEnum::StructType(_) => todo!(),
                BasicTypeEnum::VectorType(_) => todo!(),
            }
        }
        Expression::UnaryExpression(_, _) => todo!(),
        Expression::Literal(lit) => {
            let ty = &body_type_map.type_of_expression[expr_id];
            let llty = inkwell_basic_type(context, ty);
            match llty {
                BasicTypeEnum::ArrayType(_) => todo!(),
                BasicTypeEnum::FloatType(_) => todo!(),
                BasicTypeEnum::IntType(int_ty) => match lit {
                    Literal::Integer(val, _) => int_ty.const_int(*val as u64, true).into(),
                },
                BasicTypeEnum::PointerType(_) => todo!(),
                BasicTypeEnum::StructType(_) => todo!(),
                BasicTypeEnum::VectorType(_) => todo!(),
            }
        }
        Expression::NameReference(name) => {
            if let Some(name_id) = body
                .names
                .iter()
                .find(|(_, name_to_find)| name.id == name_to_find.id)
                .map(|(id, _)| id)
            {
                let value = name_map[&name_id];
                builder.build_load(value.into_pointer_value(), "")
            } else {
                panic!("cant find {} in scope", name.id);
            }
        }
        Expression::Call(call) => {
            let callee = match &body.expressions[call.callee] {
                Expression::NameReference(name) => {
                    let function_id = module
                        .functions
                        .iter()
                        .find(|(_, function)| function.name.id == name.id)
                        .map(|(id, _)| id)
                        .unwrap_or_else(|| panic!("cant find function {} in scope", name.id));
                    function_id
                }
                _e => panic!("calling function pointers are not implemented"),
            };

            let callee = function_map[&callee];

            let arguments: Vec<_> = call
                .arguments
                .iter()
                .map(|argument| {
                    build_expression(
                        context,
                        builder,
                        function_map,
                        name_map,
                        module,
                        body,
                        body_type_map,
                        *argument,
                    )
                })
                .collect();

            builder
                .build_call(callee, arguments.as_slice(), "")
                .try_as_basic_value()
                .left()
                .unwrap()
        }
    }
}