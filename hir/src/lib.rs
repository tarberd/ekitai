use std::{collections::HashMap, ops::Deref};
mod typestore;
mod typedef;
use syntax::cst::{CstNode, CstNodeEnum, expression::Expression, function::Function};
use crate::typestore::*;
use crate::typedef::*;


// used in HIR
#[derive(Debug, Clone)]
struct IncompleteExpression {
    func_name_ref: Option<String>,
    args: Vec<Type>,
    literal: Option<Type>,
}


// used in HIR
#[derive(Debug, Clone)]
struct IncompleteFunction {
    name: String,
    args: Vec<String>,
    ret_ref: Option<String>,
    expression: IncompleteExpression,
}

#[derive(Debug)]
struct Root {
    functions: Vec<IncompleteFunction>,
}

trait Lower {
    fn lower(node: CstNodeEnum, root: Root) -> Root;
}


impl Root {
    fn empty() -> Self {
        Root{ functions: vec![] }
    }
}


impl Lower for Root {
    fn lower(node: CstNodeEnum, root: Root) -> Self {
        let mut next_state = root;
        match node {
            CstNodeEnum::SourceFile(f) => {
                next_state.functions = f.functions().map(|f| lower_func(f)).collect();
            }
            _ => {}
        }
        next_state
    }
}

// impl Lower for Function {
fn lower_func(f: Function) -> IncompleteFunction {
    IncompleteFunction {
        // TODO:
        name: f.name().unwrap(),
        args: vec![],
        ret_ref: f.return_name_ref(),
        expression: lower_expr(f.body().unwrap().tail_expression().unwrap())
    }
}

// impl Lower for Expression {
fn lower_expr(expr: Expression) -> IncompleteExpression {
    match expr {
        Expression::InfixExpression(e) => {
            let a = e.lhs().unwrap();
            let b = e.rhs().unwrap();
            // TODO: assuming it has a type already
            let a_type = lower_expr(a).literal.unwrap();
            let b_type = lower_expr(b).literal.unwrap();
            IncompleteExpression {
                args: vec![a_type, b_type],
                // TODO: check from expression
                func_name_ref: Some("+".to_string()),
                literal: None,
            }
        },
        Expression::Literal(e) => IncompleteExpression {
            args: vec![],
            func_name_ref: None,
            // TODO: add check in Literal struct
            literal: Some(Type::Integer),
        },
        Expression::BlockExpression(e) => lower_expr(e.tail_expression().unwrap())
    }
}

struct Context<'a, Store: TypeStore> {
    type_store: &'a Store,
}

impl<'a, Store: TypeStore> Context<'a, Store> {
    fn prelude(store: &'a mut Store) -> Self {
        store.change("i32".to_string(), Type::Integer);
        store.change("+".to_string(), Type::Function {
            args: vec![Type::Integer, Type::Integer],
            ret: Box::new(Type::Integer),
        });

        Context {
            type_store: store,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    TypeMismatch,
    NotDefined,
}

trait Typechecker {
    fn check_type<Store: TypeStore>(&self, ctx: &Context<Store>) -> Result<Type, TypeError>;
}

impl Typechecker for Root {
    fn check_type<Store: TypeStore>(&self, ctx: &Context<Store>) -> Result<Type, TypeError> {
        for f in self.functions.clone() {
            f.check_type(ctx)?;
        }
        // TODO: Which type the module returns?
        Ok(Type::Integer)
    }
}

impl Typechecker for IncompleteFunction {
    fn check_type<Store: TypeStore>(&self, ctx: &Context<Store>) -> Result<Type, TypeError> {
        let expr_type = self.expression.check_type(ctx)?;
        let return_type_id = self.ret_ref.clone().unwrap();
        let return_type = ctx.type_store.get(return_type_id).unwrap();
        if expr_type == return_type {
            Ok(return_type)
        } else {
            Err(TypeError::TypeMismatch)
        }
    }
}


impl Typechecker for IncompleteExpression {
    fn check_type<Store: TypeStore>(&self, ctx: &Context<Store>) -> Result<Type, TypeError> {
        if let Some(func_ref) = self.func_name_ref.clone() {
            let func = ctx.type_store.get(func_ref).ok_or(TypeError::NotDefined)?;
            match func {
                Type::Function { args, ret } => {
                    if self.args == args {
                        Ok(*ret)
                    } else {
                        Err(TypeError::TypeMismatch)
                    }
                },
                _ => unreachable!(),
            }
        } else {
            Ok(self.literal.clone().unwrap())
        }
    }
}

mod test {
    use std::convert::TryInto;

    use syntax::cst::CstNodeEnum;

    use super::*;
    #[test]
    fn typecheck() {
        let src = "fn foo() -> i32 { 5_i32 + 5_i32 }";
        use syntax::cst::source_file::SourceFile;
        let cst = CstNodeEnum::SourceFile(SourceFile::parse(src).ast_node());
        println!("{:?}", cst);
        let root = Root::lower(cst, Root::empty());
        println!("{:?}", root);

        let mut store = BasicTypeStore::new();
        let ctx = Context::prelude(&mut store);
        root.check_type(&ctx).unwrap();
        let func = root.functions[0].clone();
        let func_type = func.check_type(&ctx).unwrap();
        assert_eq!(func_type, Type::Integer);
        println!("{:?} ---> {:?}", func, func_type);


        let expr = func.expression;
        let expr_type = expr.check_type(&ctx).unwrap();
        assert_eq!(expr_type, Type::Integer);
        println!("{:?} ---> {:?}", expr, expr_type);

    }
}