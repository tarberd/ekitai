pub mod type_check;

use la_arena::{Arena, ArenaMap, Idx};
use smol_str::SmolStr;
use syntax::cst;

#[derive(Debug, PartialEq)]
pub enum LowerError {
    IntegerTooBig,
    InvalidIntegerSuffix(SmolStr),
}

#[derive(Debug)]
pub struct Module {
    pub functions: Arena<Function>,
}

impl Module {
    pub fn lower(source: cst::SourceFile) -> Self {
        let functions = source
            .functions()
            .fold(Arena::new(), |mut functions, function| {
                let fun = Function::lower(function);
                functions.alloc(fun);
                functions
            });

        Self { functions }
    }
}

pub type FunctionId = Idx<Function>;

#[derive(Debug)]
pub struct Function {
    pub name: Name,
    pub parameter_types: Vec<TypeReference>,
    pub return_type: TypeReference,
    pub body: Body,
}

impl Function {
    fn lower(f: cst::Function) -> Self {
        let parameters = {
            if let Some(param_list) = f.parameter_list() {
                let params = param_list.parameters();
                params
                    .filter_map(|param| match (param.name(), param.type_name()) {
                        (Some(n), Some(t)) => Some((
                            Name {
                                id: n.identifier().text().into(),
                            },
                            TypeReference {
                                id: t.identifier().text().into(),
                            },
                        )),
                        (_, _) => None,
                    })
                    .collect()
            } else {
                Vec::new()
            }
        };

        let parameter_types = parameters.iter().map(|(_, ty)| ty.clone()).collect();

        let body = Body::new(
            parameters,
            f.body()
                .expect("Missing block expression from function body."),
        );
        let name = Name {
            id: f
                .name()
                .expect("missing name from function declaration.")
                .identifier()
                .text()
                .into(),
        };
        let return_type = TypeReference {
            id: f
                .return_type()
                .expect("missin return type from function")
                .identifier()
                .text()
                .into(),
        };
        Self {
            name,
            parameter_types,
            return_type,
            body,
        }
    }
}

pub type NameId = Idx<Name>;

#[derive(Debug, Clone)]
pub struct Name {
    pub id: SmolStr,
}

#[derive(Debug, Clone)]
pub struct NameReference {
    pub id: SmolStr,
}

#[derive(Debug, Clone)]
pub struct TypeReference {
    pub id: SmolStr,
}

#[derive(Debug)]
pub struct Body {
    pub names: Arena<Name>,
    pub types: Arena<TypeReference>,
    pub parameters: ArenaMap<Idx<Name>, Idx<TypeReference>>,
    pub expressions: Arena<Expression>,
    pub block: Idx<Expression>,
}

impl Body {
    pub fn new(parameters: Vec<(Name, TypeReference)>, block: cst::BlockExpression) -> Self {
        let (names, types, parameters) = parameters.into_iter().fold(
            (Arena::new(), Arena::new(), ArenaMap::default()),
            |(mut names, mut types, mut parameters), (name, typeref)| {
                let name_id = names.alloc(name);
                let type_id = types.alloc(typeref);
                parameters.insert(name_id, type_id);
                (names, types, parameters)
            },
        );
        let (expressions, block) = Self::collect_block_expression(Arena::new(), block);
        Self {
            names,
            types,
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
                let (mut expressions, tail) = Self::collect_expression(expressions, tail);
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
        mut expressions: Arena<Expression>,
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
                let name_ref = Expression::NameReference(NameReference {
                    id: name_ref.identifier().text().into(),
                });
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
            cst::Expression::IfExpression(if_expr) => {
                let (expressions, condition) =
                    Self::collect_expression(expressions, if_expr.contidion().unwrap());
                let (expressions, then_branch) =
                    Self::collect_expression(expressions, if_expr.then_branch().unwrap());
                let (mut expressions, else_branch) =
                    Self::collect_expression(expressions, if_expr.else_branch().unwrap());

                let id = expressions.alloc(Expression::IfExpression(IfExpression {
                    condition,
                    then_branch,
                    else_branch,
                }));

                (expressions, id)
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
        let (mut expressions, rhs) = Self::collect_expression(
            expressions,
            infix.rhs().expect("missing rhs from infix expression"),
        );

        let op = match infix
            .operator()
            .expect("missing operator from infix expression")
        {
            cst::BinaryOperator::Asterisk(_) => BinaryOperator::Arithmetic(ArithmeticOperator::Mul),
            cst::BinaryOperator::Plus(_) => BinaryOperator::Arithmetic(ArithmeticOperator::Add),
            cst::BinaryOperator::Minus(_) => BinaryOperator::Arithmetic(ArithmeticOperator::Sub),
            cst::BinaryOperator::Slash(_) => BinaryOperator::Arithmetic(ArithmeticOperator::Div),
            cst::BinaryOperator::Percent(_) => BinaryOperator::Arithmetic(ArithmeticOperator::Rem),
            cst::BinaryOperator::DoubleEquals(_) => {
                BinaryOperator::Compare(CompareOperator::Equality { negated: false })
            }
            cst::BinaryOperator::ExclamationEquals(_) => {
                BinaryOperator::Compare(CompareOperator::Equality { negated: true })
            }
            cst::BinaryOperator::Less(_) => BinaryOperator::Compare(CompareOperator::Order {
                ordering: Ordering::Less,
                strict: true,
            }),
            cst::BinaryOperator::LessEquals(_) => BinaryOperator::Compare(CompareOperator::Order {
                ordering: Ordering::Less,
                strict: false,
            }),
            cst::BinaryOperator::Greater(_) => BinaryOperator::Compare(CompareOperator::Order {
                ordering: Ordering::Greater,
                strict: true,
            }),
            cst::BinaryOperator::GreaterEquals(_) => {
                BinaryOperator::Compare(CompareOperator::Order {
                    ordering: Ordering::Greater,
                    strict: false,
                })
            }
        };

        let bin_expr = Expression::BinaryExpression(op, lhs, rhs);
        let id = expressions.alloc(bin_expr);
        (expressions, id)
    }

    fn collect_prefix_expression(
        expressions: Arena<Expression>,
        prefix: cst::PrefixExpression,
    ) -> (Arena<Expression>, ExpressionId) {
        let (mut expressions, inner) = Self::collect_expression(
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

        let (mut expressions, arguments) = call
            .argument_list()
            .expect("missing argument list from call expression")
            .arguments()
            .fold(
                (expressions, Vec::new()),
                |(expressions, mut arguments), expression| {
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
    Arithmetic(ArithmeticOperator),
    Compare(CompareOperator),
}

#[derive(Debug)]
pub enum ArithmeticOperator {
    Add,
    Sub,
    Div,
    Mul,
    Rem,
}

#[derive(Debug)]
pub enum CompareOperator {
    Equality { negated: bool },
    Order { ordering: Ordering, strict: bool },
}

#[derive(Debug)]
pub enum Ordering {
    Less,
    Greater,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Minus,
}

pub type ExpressionId = Idx<Expression>;

#[derive(Debug)]
pub enum Expression {
    BlockExpression(BlockExpression),
    IfExpression(IfExpression),
    BinaryExpression(BinaryOperator, ExpressionId, ExpressionId),
    UnaryExpression(UnaryOperator, ExpressionId),
    Literal(Literal),
    NameReference(NameReference),
    Call(Call),
}

#[derive(Debug)]
pub struct Call {
    pub callee: ExpressionId,
    pub arguments: Vec<ExpressionId>,
}

#[derive(Debug)]
pub struct BlockExpression {
    pub tail_expression: ExpressionId,
}

#[derive(Debug)]
pub struct IfExpression {
    pub condition: ExpressionId,
    pub then_branch: ExpressionId,
    pub else_branch: ExpressionId,
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
                    Some("i64") => IntegerKind::I64,
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
                        IntegerKind::I64 => value,
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

#[derive(Debug, Clone, PartialEq)]
pub enum IntegerKind {
    Unsuffixed,
    I32,
    I64,
}
