use super::{BlockExpression, Body, Expression, Function, IntegerKind, Literal, Module};

#[derive(Debug, PartialEq)]
pub enum TypeError {
    Error,
    TypeMismatch { expected: Type, actual: Type },
}

#[derive(Debug, PartialEq)]
pub enum Type {
    I32,
    Unknown,
}

pub fn type_check_module(module: &Module) -> Vec<TypeError> {
    let result = module
        .functions
        .iter()
        .fold(Vec::new(), |errors, function| {
            type_check_function(function, errors)
        });
    result
}

pub fn type_check_function(func: &Function, errors: Vec<TypeError>) -> Vec<TypeError> {
    let (_ty, errors) = type_check_body(&func.body, errors);
    errors
}

pub fn type_check_body(body: &Body, errors: Vec<TypeError>) -> (Option<Type>, Vec<TypeError>) {
    type_check_block(&body.block, errors)
}

pub fn type_check_block(
    block: &BlockExpression,
    errors: Vec<TypeError>,
) -> (Option<Type>, Vec<TypeError>) {
    type_check_expression(&block.tail_expression, errors)
}

pub fn type_check_expression(
    expr: &Expression,
    errors: Vec<TypeError>,
) -> (Option<Type>, Vec<TypeError>) {
    match expr {
        Expression::BlockExpression(e) => type_check_block(e, errors),
        Expression::BinaryExpression(lhs, _, rhs) => {
            let (left_type, errors) = type_check_expression(lhs, errors);
            let (right_type, mut errors) = type_check_expression(rhs, errors);
            match (left_type, right_type) {
                (Some(Type::Unknown), Some(r)) => {
                        (Some(r), errors)
                }
                (Some(l), Some(Type::Unknown)) => {
                        (Some(l), errors)
                }
                (Some(l), Some(r)) => {
                    if l == r {
                        (Some(l), errors)
                    } else {
                        errors.push(TypeError::TypeMismatch {
                            expected: l,
                            actual: r,
                        });
                        (None, errors)
                    }
                }
                _ => (None, errors),
            }
        }
        Expression::NameReference(..) => (Some(Type::I32), errors),
        Expression::UnaryExpression(inner, _) => type_check_expression(inner, errors),
        Expression::Literal(literal) => match literal {
            Literal::Integer(_, kind) => match kind {
                IntegerKind::I32 => (Some(Type::I32), errors),
                IntegerKind::Unsuffixed => (Some(Type::Unknown), errors)
            },
        },
        Expression::Call(..) => (Some(Type::I32), errors),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn typecheck() {
        let src = "fn foo() -> i32 { 5_i32 + 5_i32 }";
        let parse = syntax::cst::SourceFile::parse(&src);
        let (module, errors) = Module::lower(parse.ast_node());
        // println!("{:#?}", module);
        assert_eq!(errors, vec![]);

        let type_errors = type_check_module(&module);

        assert_eq!(type_errors, vec![]);
    }
}
