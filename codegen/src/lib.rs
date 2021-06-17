use hir::{BinaryOperator, BlockExpression, Expression, Literal, Module};
use inkwell::builder::Builder;
use inkwell::context::Context;
use smol_str::SmolStr;

pub fn build_assembly_ir(module: &Module) {
    let context = Context::create();
    let llvm_module = context.create_module("ekitai_module");
    let builder = context.create_builder();

    let mut values = Vec::new();

    for function in &module.functions {
        let param_types = function
            .parameter_types
            .iter()
            .map(|ty| match ty.as_str() {
                "i32" => context.i32_type().into(),
                _ => panic!("other type"),
            })
            .collect::<Vec<_>>();
        let fn_type = context.i32_type().fn_type(&param_types, false);
        let llfunction = llvm_module.add_function(&function.name, fn_type, None);

        values.push((
            function.name.clone(),
            llfunction,
        ));
    }

    for function in &module.functions {
        let llfunction = values
            .iter()
            .find(|(name, _llfunction)| *name == *function.name)
            .map(|(_, llfunction)| llfunction)
            .unwrap();

        let body = context.append_basic_block(*llfunction, "");
        builder.position_at_end(body);

        let parameter_values = function
            .body
            .parameters
            .iter()
            .zip(llfunction.get_params())
            .map(|(param, llvalue)| {
                let alloca = builder.build_alloca(llvalue.get_type(), "");
                (param.name.clone(), alloca, llvalue)
            })
            .collect::<Vec<_>>()
            .into_iter()
            .map(|(name, alloca, llvalue)| {
                builder.build_store(alloca, llvalue);
                (name, alloca.into())
            })
            .collect();

        let value =
            build_block_expression(&context, &builder, &function.body.block, &parameter_values);

        builder.build_return(Some(&value));
    }

    println!("{}", llvm_module.print_to_string().to_string());
    match llvm_module.print_to_file("out.ll") {
        Ok(_) => {}
        Err(_) => {}
    }
}

fn build_block_expression<'context>(
    context: &'context Context,
    builder: &'context Builder,
    body: &BlockExpression,
    values: &'context Vec<(SmolStr, inkwell::values::BasicValueEnum)>,
) -> inkwell::values::BasicValueEnum<'context> {
    build_expression(context, builder, &body.tail_expression, &values)
}

fn build_expression<'context>(
    context: &'context Context,
    builder: &'context Builder,
    expression: &Expression,
    values: &'context Vec<(SmolStr, inkwell::values::BasicValueEnum)>,
) -> inkwell::values::BasicValueEnum<'context> {
    match expression {
        Expression::BlockExpression(block) => {
            build_block_expression(context, builder, block, &values)
        }
        Expression::BinaryExpression(lhs, op, rhs) => {
            let l_value = build_expression(context, builder, lhs, &values).into_int_value();
            let r_value = build_expression(context, builder, rhs, &values).into_int_value();
            match op {
                BinaryOperator::Add => builder
                    .build_int_add::<inkwell::values::IntValue>(l_value, r_value, "")
                    .into(),
                BinaryOperator::Sub => builder
                    .build_int_sub::<inkwell::values::IntValue>(l_value, r_value, "")
                    .into(),
                BinaryOperator::Div => builder
                    .build_int_signed_div::<inkwell::values::IntValue>(l_value, r_value, "")
                    .into(),
                BinaryOperator::Mul => builder
                    .build_int_mul::<inkwell::values::IntValue>(l_value, r_value, "")
                    .into(),
                BinaryOperator::Rem => builder
                    .build_int_signed_rem::<inkwell::values::IntValue>(l_value, r_value, "")
                    .into(),
            }
        }
        Expression::UnaryExpression(inner, op) => {
            let inner_value = build_expression(context, builder, inner, &values).into_int_value();
            match op {
                hir::UnaryOperator::Minus => builder.build_int_neg(inner_value, "").into(),
            }
        }
        Expression::Literal(lit) => match lit {
            Literal::Integer(value, _suffix) => {
                context.i32_type().const_int(*value as u64, true).into()
            }
        },
        Expression::NameReference(name) => {
            if let Some((_, value)) = values.iter().find(|(name_to_find, _)| name == name_to_find) {
                builder.build_load(value.into_pointer_value(), "")
            } else {
                todo!("missing id {}", name)
            }
        }
        Expression::Call(call) => {
            let _callee = build_expression(context, builder, &call.callee, &values);
            let _arguments = call
                .arguments
                .iter()
                .fold(Vec::new(), |mut arguments, argument| {
                    let argument = build_expression(context, builder, argument, values);
                    arguments.push(argument);
                    arguments
                });
            // builder
            //     .build_call(callee, arguments.as_slice(), "")
            //     .try_as_basic_value()
            //     .left()
            //     .unwrap()
            todo!()
        }
    }
}
