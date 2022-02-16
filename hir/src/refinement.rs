use crate::{ArithmeticOperator, BinaryOperator, CompareOperator, LogicOperator, Name, Ordering};
use syntax::cst;

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
    pub(crate) fn lower(expr: cst::Expression) -> Self {
        match expr {
            cst::Expression::Literal(lit) => match lit.literal_kind() {
                cst::TokenLiteral::Integer(int) => {
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
                cst::TokenLiteral::Boolean(bool) => Self::Boolean(match bool {
                    cst::Boolean::True(_) => true,
                    cst::Boolean::False(_) => false,
                }),
            },
            cst::Expression::PathExpression(path) => {
                let name = Name::lower_nameref(
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
            cst::Expression::InfixExpression(infix) => {
                let left = Self::lower(infix.lhs().unwrap());
                let right = Self::lower(infix.rhs().unwrap());
                let bin_op = match infix.operator().unwrap() {
                    cst::BinaryOperator::Asterisk(_) => {
                        BinaryOperator::Arithmetic(ArithmeticOperator::Mul)
                    }
                    cst::BinaryOperator::Plus(_) => {
                        BinaryOperator::Arithmetic(ArithmeticOperator::Add)
                    }
                    cst::BinaryOperator::Minus(_) => {
                        BinaryOperator::Arithmetic(ArithmeticOperator::Sub)
                    }
                    cst::BinaryOperator::Slash(_) => {
                        BinaryOperator::Arithmetic(ArithmeticOperator::Div)
                    }
                    cst::BinaryOperator::Percent(_) => {
                        BinaryOperator::Arithmetic(ArithmeticOperator::Rem)
                    }
                    cst::BinaryOperator::DoubleEquals(_) => {
                        BinaryOperator::Compare(CompareOperator::Equality { negated: false })
                    }
                    cst::BinaryOperator::ExclamationEquals(_) => {
                        BinaryOperator::Compare(CompareOperator::Equality { negated: true })
                    }
                    cst::BinaryOperator::Less(_) => {
                        BinaryOperator::Compare(CompareOperator::Order {
                            ordering: Ordering::Less,
                            strict: true,
                        })
                    }
                    cst::BinaryOperator::LessEquals(_) => {
                        BinaryOperator::Compare(CompareOperator::Order {
                            ordering: Ordering::Less,
                            strict: false,
                        })
                    }
                    cst::BinaryOperator::Greater(_) => {
                        BinaryOperator::Compare(CompareOperator::Order {
                            ordering: Ordering::Greater,
                            strict: true,
                        })
                    }
                    cst::BinaryOperator::GreaterEquals(_) => {
                        BinaryOperator::Compare(CompareOperator::Order {
                            ordering: Ordering::Greater,
                            strict: false,
                        })
                    }
                    cst::BinaryOperator::DoubleAmpersand(_) => {
                        BinaryOperator::Logic(LogicOperator::And)
                    }
                    cst::BinaryOperator::DoublePipe(_) => BinaryOperator::Logic(LogicOperator::Or),
                };
                Self::Binary(bin_op, Box::new(left), Box::new(right))
            }
            cst::Expression::PrefixExpression(prefix) => {
                let inner = Self::lower(prefix.inner().unwrap());
                let op = match prefix.operator().unwrap() {
                    cst::UnaryOperator::Minus(_) => UnaryOperator::Minus,
                    cst::UnaryOperator::Exclamation(_) => UnaryOperator::Negation,
                    cst::UnaryOperator::Asterisk(_) | cst::UnaryOperator::Ampersand(_) => {
                        panic!("Cannot reference and dereference in refinements.")
                    }
                };
                Self::Unary(op, Box::new(inner))
            }
            cst::Expression::ParenthesisExpression(inner) => {
                Self::lower(inner.inner_expression().unwrap())
            }
            x @ cst::Expression::BlockExpression(_)
            | x @ cst::Expression::CallExpression(_)
            | x @ cst::Expression::IfExpression(_)
            | x @ cst::Expression::MatchExpression(_)
            | x @ cst::Expression::NewExpression(_) => {
                panic!("{:?} not implemented for refinements", x)
            }
        }
    }
}
