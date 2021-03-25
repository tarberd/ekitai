pub mod type_check;

use smol_str::SmolStr;
use syntax::cst;

#[derive(Debug)]
pub enum LowerError {
    IntegerTooBig,
    InvalidIntegerSuffix(SmolStr),
}

#[derive(Debug)]
pub struct Module {
    functions: Vec<Function>,
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
    name: String,
    body: BlockExpression,
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
pub enum Expression {
    BlockExpression(Box<BlockExpression>),
    InfixExpression(Box<Expression>, Box<Expression>),
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
                let lower_lhs = Expression::lower(
                    match infix.lhs() {
                        Some(expr) => expr,
                        None => todo!("todo missing lhs form infix expression"),
                    },
                    diagnostics,
                );
                let lower_rhs = Expression::lower(
                    match infix.rhs() {
                        Some(expr) => expr,
                        None => todo!("todo missing rhs form infix expression"),
                    },
                    lower_lhs.1,
                );
                (
                    Self::InfixExpression(Box::new(lower_lhs.0), Box::new(lower_rhs.0)),
                    lower_rhs.1,
                )
            }
            cst::Expression::ParenthesisExpression(parenthesis) => {
                Expression::lower(parenthesis.inner_expression().unwrap_or_else(|| todo!("alskdhflakshd")), diagnostics)
            }
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
