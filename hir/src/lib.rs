pub mod type_check;

use la_arena::{Arena, Idx};
use smol_str::SmolStr;
use syntax::cst;

#[derive(Debug, PartialEq)]
pub enum LowerError {
    IntegerTooBig,
    InvalidIntegerSuffix(SmolStr),
}

#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Function>,
}

// impl Module {
//     pub fn lower(source: cst::SourceFile) -> (Self, Vec<LowerError>) {
//         let lowered = source.functions().fold(
//             (Vec::new(), Vec::new()),
//             |(mut functions, diagnostics), function| {
//                 let lowered = Function::lower(function, diagnostics);
//                 if let Some(fun) = lowered.0 {
//                     functions.push(fun);
//                 }
//                 (functions, lowered.1)
//             },
//         );
//         (
//             Self {
//                 functions: lowered.0,
//             },
//             lowered.1,
//         )
//     }
// }

#[derive(Debug)]
pub struct Parameter {
    pub name: SmolStr,
    pub ty: SmolStr,
}

#[derive(Debug)]
pub struct Function {
    pub name: SmolStr,
    pub parameter_types: Vec<SmolStr>,
    pub return_type: SmolStr,
    pub body: Body,
}

// impl Function {
//     fn lower(f: cst::Function, diagnostics: Vec<LowerError>) -> (Option<Self>, Vec<LowerError>) {
//         let parameters = {
//             if let Some(param_list) = f.parameter_list() {
//                 let params = param_list.parameters();
//                 params
//                     .filter_map(|param| match (param.name(), param.type_name()) {
//                         (Some(n), Some(t)) => Some(Parameter {
//                             name: n.identifier().text().into(),
//                             ty: t.identifier().text().into(),
//                         }),
//                         (_, _) => None,
//                     })
//                     .collect()
//             } else {
//                 Vec::new()
//             }
//         };

//         let parameter_types = parameters
//             .iter()
//             .map(|Parameter { ty, .. }| ty.clone())
//             .collect();
//         let (block, diagnostics) = BlockExpression::lower(f.body().unwrap(), diagnostics);
//         let fun = match f.name() {
//             Some(name) => Some(Self {
//                 name: name.identifier().text().into(),
//                 parameter_types,
//                 return_type: "i32".into(),
//                 body: Body { parameters, block },
//             }),
//             None => None,
//         };
//         (fun, diagnostics)
//     }
// }

#[derive(Debug)]
pub struct Body {
    pub parameters: Vec<Parameter>,
    pub expressions: Arena<Expression>,
    pub block: ExpressionId,
}

impl Body {
    pub fn new(parameters: Vec<Parameter>, block: cst::BlockExpression) -> Self {
        let expressions = Arena::new();
        let (expressions, block) = Self::collect_block_expression(expressions, block);
        Self {
            parameters,
            expressions,
            block,
        }
    }

    fn collect_block_expression(
        expressions: Arena<Expression>,
        block: cst::BlockExpression,
    ) -> (Arena<Expression>, ExpressionId) {
        match block.tail_expression() {
            Some(tail) => {
                let (expressions, tail) = Self::collect_expression(expressions, tail);
                let block = BlockExpression {
                    tail_expression: tail,
                };
                let expr = Expression::BlockExpression(block);
                let id = expressions.alloc(expr);
                (expressions, id)
            }
            None => panic!("missing tail expression from block"),
        }
    }

    fn collect_expression(
        expressions: Arena<Expression>,
        expression: cst::Expression,
    ) -> (Arena<Expression>, ExpressionId) {
        match expression {
            cst::Expression::BlockExpression(block) => {
                Self::collect_block_expression(expressions, block)
            }
            cst::Expression::Literal(lit) => {
                let (lit, _) = Literal::lower(lit, vec![]);
                let literal = Expression::Literal(lit);
                let id = expressions.alloc(literal);
                (expressions, id)
            }
            cst::Expression::NameReference(name_ref) => {
                let name_ref = Expression::NameReference(name_ref.identifier().text().into());
                let id = expressions.alloc(name_ref);
                (expressions, id)
            }
            cst::Expression::InfixExpression(infix) => {
                Self::collect_infix_expression(expressions, infix)
            }
            cst::Expression::PrefixExpression(prefix) => {
                Self::collect_prefix_expression(expressions, prefix)
            }
            cst::Expression::ParenthesisExpression(inner) => Self::collect_expression(
                expressions,
                inner
                    .inner_expression()
                    .expect("missing expression from Parenthesis Expression"),
            ),
            cst::Expression::CallExpression(call) => {
                Self::collect_call_expression(expressions, call)
            }
        }
    }

    fn collect_infix_expression(
        expressions: Arena<Expression>,
        infix: cst::InfixExpression,
    ) -> (Arena<Expression>, ExpressionId) {
        let (expressions, lhs) = Self::collect_expression(
            expressions,
            infix.lhs().expect("missing lhs from infix expression"),
        );
        let (expressions, rhs) = Self::collect_expression(
            expressions,
            infix.rhs().expect("missing rhs from infix expression"),
        );

        let op = match infix
            .operator()
            .expect("missing operator from infix expression")
        {
            cst::BinaryOperator::Asterisk(_) => BinaryOperator::Mul,
            cst::BinaryOperator::Plus(_) => BinaryOperator::Add,
            cst::BinaryOperator::Minus(_) => BinaryOperator::Sub,
            cst::BinaryOperator::Slash(_) => BinaryOperator::Div,
            cst::BinaryOperator::Percent(_) => BinaryOperator::Rem,
        };

        let bin_expr = Expression::BinaryExpression(op, lhs, rhs);
        let id = expressions.alloc(bin_expr);
        (expressions, id)
    }

    fn collect_prefix_expression(
        expressions: Arena<Expression>,
        prefix: cst::PrefixExpression,
    ) -> (Arena<Expression>, ExpressionId) {
        let (expressions, inner) = Self::collect_expression(
            expressions,
            prefix
                .inner()
                .expect("missing inner expression from prefix expression"),
        );

        let op = match prefix
            .operator()
            .expect("missing operator from infix expression")
        {
            cst::UnaryOperator::Minus(_) => UnaryOperator::Minus,
        };

        let unary_expr = Expression::UnaryExpression(op, inner);
        let id = expressions.alloc(unary_expr);
        (expressions, id)
    }

    fn collect_call_expression(
        expressions: Arena<Expression>,
        call: cst::CallExpression,
    ) -> (Arena<Expression>, ExpressionId) {
        let (expressions, callee) = Self::collect_expression(
            expressions,
            call.callee().expect("missing callee from call expression"),
        );

        let (expression, arguments) = call
            .argument_list()
            .expect("missing argument list from call expression")
            .arguments()
            .fold(
                (expressions, Vec::new()),
                |(expressions, arguments), expression| {
                    let (expressions, argument) = Self::collect_expression(expressions, expression);
                    arguments.push(argument);
                    (expressions, arguments)
                },
            );

        let call = Expression::Call(Call { callee, arguments });
        let id = expressions.alloc(call);
        (expressions, id)
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Div,
    Mul,
    Rem,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Minus,
}

pub type ExpressionId = Idx<Expression>;

#[derive(Debug)]
pub enum Expression {
    BlockExpression(BlockExpression),
    BinaryExpression(BinaryOperator, ExpressionId, ExpressionId),
    UnaryExpression(UnaryOperator, ExpressionId),
    Literal(Literal),
    NameReference(SmolStr),
    Call(Call),
}

impl Expression {
    fn lower(expr: cst::Expression, diagnostics: Vec<LowerError>) -> (Self, Vec<LowerError>) {
        match expr {
            cst::Expression::BlockExpression(block) => {
                let lower = BlockExpression::lower(block, diagnostics);
                (Self::BlockExpression(Box::new(lower.0)), lower.1)
            }
            cst::Expression::Literal(literal) => {
                let lower = Literal::lower(literal, diagnostics);
                (Self::Literal(lower.0), lower.1)
            }
            cst::Expression::NameReference(name_ref) => (
                Self::NameReference(name_ref.identifier().text().into()),
                diagnostics,
            ),
            cst::Expression::InfixExpression(infix) => {
                let (lhs, diagnostics) = Expression::lower(
                    match infix.lhs() {
                        Some(expr) => expr,
                        None => todo!("todo missing lhs from infix expression"),
                    },
                    diagnostics,
                );
                let (rhs, diagnostics) = Expression::lower(
                    match infix.rhs() {
                        Some(expr) => expr,
                        None => todo!("todo missing rhs from infix expression"),
                    },
                    diagnostics,
                );
                let operator = match infix.operator() {
                    Some(op) => match op {
                        cst::BinaryOperator::Plus(_) => BinaryOperator::Add,
                        cst::BinaryOperator::Minus(_) => BinaryOperator::Sub,
                        cst::BinaryOperator::Asterisk(_) => BinaryOperator::Mul,
                        cst::BinaryOperator::Slash(_) => BinaryOperator::Div,
                        cst::BinaryOperator::Percent(_) => BinaryOperator::Rem,
                    },
                    None => todo!("missing infix expression operator"),
                };
                (
                    Self::BinaryExpression(Box::new(lhs), operator, Box::new(rhs)),
                    diagnostics,
                )
            }
            cst::Expression::PrefixExpression(prefix) => {
                let (inner, diagnostics) = Expression::lower(
                    prefix.inner().unwrap_or_else(|| {
                        todo!("todo missing inner expression on prefix expression")
                    }),
                    diagnostics,
                );
                let operator = match prefix.operator() {
                    Some(op) => match op {
                        cst::UnaryOperator::Minus(_) => UnaryOperator::Minus,
                    },
                    None => todo!("missing prefix expression operator"),
                };
                (
                    Self::UnaryExpression(Box::new(inner), operator),
                    diagnostics,
                )
            }
            cst::Expression::ParenthesisExpression(parenthesis) => Expression::lower(
                parenthesis
                    .inner_expression()
                    .unwrap_or_else(|| todo!("todo missing inner nested expression")),
                diagnostics,
            ),
            cst::Expression::CallExpression(call) => {
                let (call, diagnostics) = Call::lower(call, diagnostics);
                (Self::Call(Box::new(call)), diagnostics)
            }
        }
    }
}

#[derive(Debug)]
pub struct Call {
    pub callee: ExpressionId,
    pub arguments: Vec<ExpressionId>,
}

impl Call {
    fn lower(call: cst::CallExpression, diagnostics: Vec<LowerError>) -> (Self, Vec<LowerError>) {
        if let (Some(callee), Some(arguments)) = (call.callee(), call.argument_list()) {
            let (callee, diagnostics) = Expression::lower(callee, diagnostics);

            let (arguments, diagnostics) = arguments.arguments().fold(
                (Vec::new(), diagnostics),
                |(mut arguments, diagnostics), argument| {
                    let (argument, diagnostics) = Expression::lower(argument, diagnostics);
                    arguments.push(argument);
                    (arguments, diagnostics)
                },
            );

            (Self { callee, arguments }, diagnostics)
        } else {
            panic!("call expression must have callee and arguments");
        }
    }
}

#[derive(Debug)]
pub struct BlockExpression {
    pub tail_expression: ExpressionId,
}

impl BlockExpression {
    fn lower(b: cst::BlockExpression, diagnostics: Vec<LowerError>) -> (Self, Vec<LowerError>) {
        let lowered = match b.tail_expression() {
            Some(tail) => Expression::lower(tail, diagnostics),
            None => todo!("implement for missing tail expression"),
        };
        (
            Self {
                tail_expression: lowered.0,
            },
            lowered.1,
        )
    }
}

#[derive(Debug)]
pub enum Literal {
    Integer(u128, IntegerKind),
}

impl Literal {
    fn lower(lit: cst::Literal, mut diagnostics: Vec<LowerError>) -> (Self, Vec<LowerError>) {
        let lit = match lit.literal_kind() {
            cst::LiteralKind::Integer(integer) => {
                let (radical, suffix) = integer.radical_and_suffix();

                let kind = match suffix {
                    Some("i32") => IntegerKind::I32,
                    Some(invalid_suffix) => {
                        diagnostics.push(LowerError::InvalidIntegerSuffix(invalid_suffix.into()));
                        IntegerKind::Unsuffixed
                    }
                    None => IntegerKind::Unsuffixed,
                };

                let value = match radical
                    .chars()
                    .filter(|c| *c != '_')
                    .collect::<String>()
                    .parse::<u128>()
                    .ok()
                {
                    Some(value) => match kind {
                        IntegerKind::Unsuffixed => value,
                        IntegerKind::I32 => value,
                    },
                    None => {
                        // larger than u128
                        diagnostics.push(LowerError::IntegerTooBig);
                        0
                    }
                };

                Literal::Integer(value, kind)
            }
        };

        (lit, diagnostics)
    }
}

#[derive(Debug)]
pub enum IntegerKind {
    Unsuffixed,
    I32,
}
