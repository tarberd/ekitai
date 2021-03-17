use syntax::cst;

#[derive(Debug)]
pub struct Module {
    functions: Vec<Function>,
}

impl Module {
    pub fn lower(source: cst::SourceFile) -> Self {
        let functions = source
            .functions()
            .filter_map(|f| Function::lower(f))
            .collect();
        Self { functions }
    }
}

#[derive(Debug)]
pub struct Function {
    name: String,
    body: BlockExpression,
}

impl Function {
    fn lower(f: cst::Function) -> Option<Self> {
        Some(Self {
            name: f.name()?.identifier()?.text().to_string(),
            body: BlockExpression::lower(f.body()?)?,
        })
    }
}

#[derive(Debug)]
pub enum Expression {
    BlockExpression(Box<BlockExpression>),
    InfixExpression(Box<Expression>, Box<Expression>),
    Literal(Literal),
}

impl Expression {
    fn lower(expr: cst::Expression) -> Option<Self> {
        Some(match expr {
            cst::Expression::BlockExpression(block) => {
                Self::BlockExpression(Box::new(BlockExpression::lower(block)?))
            }
            cst::Expression::Literal(literal) => Self::Literal(Literal::lower(literal)?),
            cst::Expression::InfixExpression(infix) => Self::InfixExpression(
                Box::new(Expression::lower(infix.lhs()?)?),
                Box::new(Expression::lower(infix.rhs()?)?),
            ),
        })
    }
}

#[derive(Debug)]
pub struct BlockExpression {
    pub tail_expression: Expression,
}

impl BlockExpression {
    fn lower(b: cst::BlockExpression) -> Option<Self> {
        Some(Self {
            tail_expression: Expression::lower(b.tail_expression()?)?,
        })
    }
}

#[derive(Debug)]
pub enum Literal {
    Integer(u128, IntegerKind),
}

impl Literal {
    fn lower(lit: cst::Literal) -> Option<Self> {
        match lit.literal_kind()? {
            cst::LiteralKind::Integer(_) => None,
        }
    }
}

#[derive(Debug)]
pub enum IntegerKind {
    I32,
}
