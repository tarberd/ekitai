use syntax::cst;

#[derive(Debug)]
pub enum LowerError {
    IntegerTooBig,
    InvalidIntegerSufix,
}

#[derive(Debug)]
pub struct Module {
    functions: Vec<Function>,
}

impl Module {
    pub fn lower(source: cst::SourceFile) -> (Self, Vec<LowerError>) {
        let lower = source.functions().map(|f| Function::lower(f)).fold(
            (Vec::new(), Vec::new()),
            |(mut functions, mut diagnostics), (function, mut errors)| {
                diagnostics.append(&mut errors);
                if let Some(fun) = function {
                    functions.push(fun);
                }
                (functions, diagnostics)
            },
        );
        (Self { functions: lower.0 }, lower.1)
    }
}

#[derive(Debug)]
pub struct Function {
    name: String,
    body: BlockExpression,
}

impl Function {
    fn lower(f: cst::Function) -> (Option<Self>, Vec<LowerError>) {
        let mut lower = BlockExpression::lower(f.body().unwrap());
        let mut diagnostics = Vec::new();
        diagnostics.append(&mut lower.1);

        let fun = match f.name() {
            Some(name) => Some(Self {
                name: name.identifier().text().to_string(),
                body: lower.0,
            }),
            None => None,
        };
        (fun, diagnostics)
    }
}

#[derive(Debug)]
pub enum Expression {
    BlockExpression(Box<BlockExpression>),
    InfixExpression(Box<Expression>, Box<Expression>),
    Literal(Literal),
}

impl Expression {
    fn lower(expr: cst::Expression) -> (Self, Vec<LowerError>) {
        match expr {
            cst::Expression::BlockExpression(block) => {
                let lower = BlockExpression::lower(block);
                (Self::BlockExpression(Box::new(lower.0)), lower.1)
            }
            cst::Expression::Literal(literal) => {
                let lower = Literal::lower(literal);
                (Self::Literal(lower.0), lower.1)
            }
            cst::Expression::InfixExpression(infix) => {
                let mut diagnostics = Vec::new();
                let mut lower_lhs = Expression::lower(match infix.lhs() {
                    Some(expr) => expr,
                    None => todo!("todo missing lhs form infix expression"),
                });
                let mut lower_rhs = Expression::lower(match infix.rhs() {
                    Some(expr) => expr,
                    None => todo!("todo missing rhs form infix expression"),
                });
                diagnostics.append(&mut lower_lhs.1);
                diagnostics.append(&mut lower_rhs.1);
                (
                    Self::InfixExpression(Box::new(lower_lhs.0), Box::new(lower_rhs.0)),
                    diagnostics,
                )
            }
        }
    }
}

#[derive(Debug)]
pub struct BlockExpression {
    pub tail_expression: Expression,
}

impl BlockExpression {
    fn lower(b: cst::BlockExpression) -> (Self, Vec<LowerError>) {
        let mut diagnostics = Vec::new();
        let tail_expression = match b.tail_expression() {
            Some(tail) => {
                let mut lower = Expression::lower(tail);
                diagnostics.append(&mut lower.1);
                lower.0
            }
            None => todo!("implement for missing tail expression"),
        };
        (Self { tail_expression }, diagnostics)
    }
}

#[derive(Debug)]
pub enum Literal {
    Integer(u128, IntegerKind),
}

impl Literal {
    fn lower(lit: cst::Literal) -> (Self, Vec<LowerError>) {
        let mut diagnostics = Vec::new();

        let lit = match lit.literal_kind() {
            cst::LiteralKind::Integer(integer) => {
                let (radical, sufix) = integer.radical_and_sufix();

                let value = match radical
                    .chars()
                    .filter(|c| *c != '_')
                    .collect::<String>()
                    .parse()
                    .ok()
                {
                    Some(parse) => parse,
                    None => {
                        // larger than u128
                        diagnostics.push(LowerError::IntegerTooBig);
                        0
                    }
                };

                match sufix {
                    Some("i32") => Literal::Integer(value, IntegerKind::I32),
                    _ => {
                        // invalid sufix
                        diagnostics.push(LowerError::InvalidIntegerSufix);
                        Literal::Integer(value, IntegerKind::Unsufixed)
                    }
                }
            }
        };

        (lit, diagnostics)
    }
}

#[derive(Debug)]
pub enum IntegerKind {
    Unsufixed,
    I32,
}
