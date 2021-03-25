pub mod type_check;

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

impl Module {
    pub fn lower(source: cst::SourceFile) -> (Self, Vec<LowerError>) {
        let lowered = source.functions().fold(
            (Vec::new(), Vec::new()),
            |(mut functions, diagnostics), function| {
                let lowered = Function::lower(function, diagnostics);
                if let Some(fun) = lowered.0 {
                    functions.push(fun);
                }
                (functions, lowered.1)
            },
        );
        (
            Self {
                functions: lowered.0,
            },
            lowered.1,
        )
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: BlockExpression,
}

impl Function {
    fn lower(f: cst::Function, diagnostics: Vec<LowerError>) -> (Option<Self>, Vec<LowerError>) {
        let lower = BlockExpression::lower(f.body().unwrap(), diagnostics);
        let fun = match f.name() {
            Some(name) => Some(Self {
                name: name.identifier().text().to_string(),
                body: lower.0,
            }),
            None => None,
        };
        (fun, lower.1)
    }
}

#[derive(Debug)]
pub enum InfixOperator {
    Add,
    Sub,
    Div,
    Mul,
    Rem,
}

#[derive(Debug)]
pub enum Expression {
    BlockExpression(Box<BlockExpression>),
    InfixExpression(Box<Expression>, InfixOperator, Box<Expression>),
    Literal(Literal),
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
                let operator = match infix.infix_operator() {
                    Some(op) => match op {
                        cst::InfixOperator::Plus(_) => InfixOperator::Add,
                        cst::InfixOperator::Minus(_) => InfixOperator::Sub,
                        cst::InfixOperator::Asterisk(_) => InfixOperator::Mul,
                        cst::InfixOperator::Slash(_) => InfixOperator::Div,
                        cst::InfixOperator::Percent(_) => InfixOperator::Rem,
                    },
                    None => todo!("missing infix expression operator"),
                };
                (
                    Self::InfixExpression(Box::new(lhs), operator, Box::new(rhs)),
                    diagnostics,
                )
            }
            cst::Expression::ParenthesisExpression(parenthesis) => Expression::lower(
                parenthesis
                    .inner_expression()
                    .unwrap_or_else(|| todo!("todo missing inner nested expression")),
                diagnostics,
            ),
        }
    }
}

#[derive(Debug)]
pub struct BlockExpression {
    pub tail_expression: Expression,
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
