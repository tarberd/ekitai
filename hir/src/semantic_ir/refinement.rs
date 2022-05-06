use crate::{
    semantic_ir::term::{ArithmeticOperator, LogicOperator, Ordering},
    BinaryOperator, CompareOperator, Name,
};
use syntax::ast;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Predicate {
    Variable(Name),
    Boolean(bool),
    Integer(u128),
    Binary(BinaryOperator, Box<Predicate>, Box<Predicate>),
    Unary(UnaryOperator, Box<Predicate>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Minus,
    Negation,
}

impl Predicate {
    pub(crate) fn lower(expr: ast::Expression) -> Self {
        match expr {
            ast::Expression::Literal(lit) => match lit.literal_kind() {
                ast::TokenLiteral::Integer(int) => {
                    let value = int
                        .radical_and_suffix()
                        .0
                        .chars()
                        .filter(|c| *c != '_')
                        .collect::<String>()
                        .parse::<u128>()
                        .unwrap();
                    Self::Integer(value)
                }
                ast::TokenLiteral::Boolean(bool) => Self::Boolean(match bool {
                    ast::Boolean::True(_) => true,
                    ast::Boolean::False(_) => false,
                }),
            },
            ast::Expression::PathExpression(path) => {
                let name = Name::from_ast_nameref(
                    path.path()
                        .unwrap()
                        .path_segment()
                        .unwrap()
                        .name_reference()
                        .unwrap(),
                );
                match path.path().unwrap().path() {
                    Some(_) => panic!("path variables not supported in refinement predicates."),
                    None => (),
                };
                Self::Variable(name)
            }
            ast::Expression::InfixExpression(infix) => {
                let left = Self::lower(infix.lhs().unwrap());
                let right = Self::lower(infix.rhs().unwrap());
                let bin_op = match infix.operator().unwrap() {
                    ast::BinaryOperator::Asterisk(_) => {
                        BinaryOperator::Arithmetic(ArithmeticOperator::Mul)
                    }
                    ast::BinaryOperator::Plus(_) => {
                        BinaryOperator::Arithmetic(ArithmeticOperator::Add)
                    }
                    ast::BinaryOperator::Minus(_) => {
                        BinaryOperator::Arithmetic(ArithmeticOperator::Sub)
                    }
                    ast::BinaryOperator::Slash(_) => {
                        BinaryOperator::Arithmetic(ArithmeticOperator::Div)
                    }
                    ast::BinaryOperator::Percent(_) => {
                        BinaryOperator::Arithmetic(ArithmeticOperator::Rem)
                    }
                    ast::BinaryOperator::DoubleEquals(_) => {
                        BinaryOperator::Compare(CompareOperator::Equality { negated: false })
                    }
                    ast::BinaryOperator::ExclamationEquals(_) => {
                        BinaryOperator::Compare(CompareOperator::Equality { negated: true })
                    }
                    ast::BinaryOperator::Less(_) => {
                        BinaryOperator::Compare(CompareOperator::Order {
                            ordering: Ordering::Less,
                            strict: true,
                        })
                    }
                    ast::BinaryOperator::LessEquals(_) => {
                        BinaryOperator::Compare(CompareOperator::Order {
                            ordering: Ordering::Less,
                            strict: false,
                        })
                    }
                    ast::BinaryOperator::Greater(_) => {
                        BinaryOperator::Compare(CompareOperator::Order {
                            ordering: Ordering::Greater,
                            strict: true,
                        })
                    }
                    ast::BinaryOperator::GreaterEquals(_) => {
                        BinaryOperator::Compare(CompareOperator::Order {
                            ordering: Ordering::Greater,
                            strict: false,
                        })
                    }
                    ast::BinaryOperator::DoubleAmpersand(_) => {
                        BinaryOperator::Logic(LogicOperator::And)
                    }
                    ast::BinaryOperator::DoublePipe(_) => BinaryOperator::Logic(LogicOperator::Or),
                };
                Self::Binary(bin_op, Box::new(left), Box::new(right))
            }
            ast::Expression::PrefixExpression(prefix) => {
                let inner = Self::lower(prefix.inner().unwrap());
                let op = match prefix.operator().unwrap() {
                    ast::UnaryOperator::Minus(_) => UnaryOperator::Minus,
                    ast::UnaryOperator::Exclamation(_) => UnaryOperator::Negation,
                    ast::UnaryOperator::Asterisk(_) | ast::UnaryOperator::Ampersand(_) => {
                        panic!("Cannot reference and dereference in refinements.")
                    }
                };
                Self::Unary(op, Box::new(inner))
            }
            ast::Expression::ParenthesisExpression(inner) => {
                Self::lower(inner.inner_expression().unwrap())
            }
            x @ ast::Expression::BlockExpression(_)
            | x @ ast::Expression::CallExpression(_)
            | x @ ast::Expression::IfExpression(_)
            | x @ ast::Expression::MatchExpression(_)
            | x @ ast::Expression::NewExpression(_) => {
                panic!("{:?} not implemented for refinements", x)
            }
        }
    }
}
