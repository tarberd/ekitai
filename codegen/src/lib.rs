use hir::{BinaryOperator, BlockExpression, Expression, Literal, Module};
use inkwell::builder::Builder;
use inkwell::context::Context;

pub fn build_assembly_ir(module: &Module) {
    let context = Context::create();
    let llvm_module = context.create_module("ekitai_module");
    let builder = context.create_builder();

    for function in &module.functions {
        let param_types = [];
        let fn_type = context.i32_type().fn_type(&param_types, false);
        let llfunction = llvm_module.add_function(&function.name, fn_type, None);
        let body = context.append_basic_block(llfunction, "entry");
        builder.position_at_end(body);

        let value = build_block_expression(&context, &builder, &function.body);

        builder.build_return(Some(&value));
    }

    println!("{}", llvm_module.print_to_string().to_string());
}

fn build_block_expression<'context>(
    context: &'context Context,
    builder: &'context Builder,
    body: &BlockExpression,
) -> inkwell::values::BasicValueEnum<'context> {
    build_expression(context, builder, &body.tail_expression)
}

fn build_expression<'context>(
    context: &'context Context,
    builder: &'context Builder,
    expression: &Expression,
) -> inkwell::values::BasicValueEnum<'context> {
    match expression {
        Expression::BlockExpression(block) => build_block_expression(context, builder, block),
        Expression::BinaryExpression(lhs, op, rhs) => {
            let l_value = build_expression(context, builder, lhs).into_int_value();
            let r_value = build_expression(context, builder, rhs).into_int_value();
            match op {
                BinaryOperator::Add => builder
                    .build_int_add::<inkwell::values::IntValue>(l_value, r_value, "add")
                    .into(),
                BinaryOperator::Sub => builder
                    .build_int_sub::<inkwell::values::IntValue>(l_value, r_value, "sub")
                    .into(),
                BinaryOperator::Div => builder
                    .build_int_signed_div::<inkwell::values::IntValue>(l_value, r_value, "div")
                    .into(),
                BinaryOperator::Mul => builder
                    .build_int_mul::<inkwell::values::IntValue>(l_value, r_value, "mul")
                    .into(),
                BinaryOperator::Rem => builder
                    .build_int_signed_rem::<inkwell::values::IntValue>(l_value, r_value, "rem")
                    .into(),
            }
        }
        Expression::UnaryExpression(inner, op) => {
            let inner_value = build_expression(context, builder, inner).into_int_value();
            match op {
                hir::UnaryOperator::Minus => builder.build_int_neg(inner_value, "neg").into(),
            }
        }
        Expression::Literal(lit) => match lit {
            Literal::Integer(value, _suffix) => {
                context.i32_type().const_int(*value as u64, true).into()
            }
        },
    }
}
