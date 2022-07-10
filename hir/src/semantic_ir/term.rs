use la_arena::{Arena, Idx};
use syntax::ast;

use super::{intrinsic::BuiltinInteger, name::Name, path::Path};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub patterns: Arena<Pattern>,
    pub parameters: Vec<PatternId>,
    pub expressions: Arena<Term>,
    pub root_expression: TermId,
}

impl Body {
    pub fn lower(function: ast::FunctionDefinition) -> Self {
        let (
            BodyFold {
                expressions,
                patterns,
                parameters,
                ..
            },
            root_expression,
        ) = BodyFold::new()
            .fold_function_parameters(&function)
            .fold_block_expression(function.body().unwrap());

        Self {
            patterns,
            parameters,
            expressions,
            root_expression,
        }
    }
}

pub type TermId = Idx<Term>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Block {
        statements: Vec<Statement>,
        trailing_expression: TermId,
    },
    If {
        condition: TermId,
        then_branch: TermId,
        else_branch: TermId,
    },
    Match {
        matchee: TermId,
        case_list: Vec<(PatternId, TermId)>,
    },
    Call {
        callee: TermId,
        arguments: Vec<TermId>,
    },
    New(TermId),
    Binary(BinaryOperator, TermId, TermId),
    Unary(UnaryOperator, TermId),
    Path(Path),
    Literal(Literal),
}

pub type PatternId = Idx<Pattern>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Deconstructor(Path, Vec<PatternId>),
    Bind(Name),
}

impl Pattern {
    pub fn lower(pattern: ast::Pattern) -> Self {
        match pattern {
            ast::Pattern::DeconstructorPattern(pat) => {
                Self::Deconstructor(Path::from_ast(pat.path().unwrap()), Vec::new())
            }
            ast::Pattern::BindingPattern(pat) => {
                Self::Bind(Name::from_ast_name(pat.name().unwrap()))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(PatternId, TermId),
    Expression(TermId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Integer(u128, Option<BuiltinInteger>),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Arithmetic(ArithmeticOperator),
    Logic(LogicOperator),
    Compare(CompareOperator),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArithmeticOperator {
    Add,
    Sub,
    Div,
    Mul,
    Rem,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LogicOperator {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompareOperator {
    Equality { negated: bool },
    Order { ordering: Ordering, strict: bool },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ordering {
    Less,
    Greater,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Minus,
    Negation,
    Reference,
    Dereference,
}

#[derive(Default)]
struct BodyFold {
    pub expressions: Arena<Term>,
    pub patterns: Arena<Pattern>,
    pub parameters: Vec<PatternId>,
    pub argument_binder_count: u128,
}

impl BodyFold {
    fn new() -> Self {
        BodyFold::default()
    }

    fn fold_function_parameters(self, function: &ast::FunctionDefinition) -> Self {
        function
            .parameter_list()
            .unwrap()
            .parameters()
            .fold(self, |fold, param| {
                let (mut fold, pattern_id) = fold.fold_pattern(param.pattern().unwrap());
                fold.parameters.push(pattern_id);
                fold
            })
    }

    fn fold_pattern(self, pattern: ast::Pattern) -> (Self, PatternId) {
        let (mut fold, pattern) = match pattern {
            ast::Pattern::DeconstructorPattern(deconstructor) => {
                let subpatterns = deconstructor.pattern_list().unwrap().patterns();
                let (fold, subpatterns) =
                    subpatterns.fold((self, Vec::new()), |(fold, mut subpatterns), pattern| {
                        let (fold, subpattern) = fold.fold_pattern(pattern);
                        subpatterns.push(subpattern);
                        (fold, subpatterns)
                    });
                (
                    fold,
                    Pattern::Deconstructor(
                        Path::from_ast(deconstructor.path().unwrap()),
                        subpatterns,
                    ),
                )
            }
            ast::Pattern::BindingPattern(pat) => (
                self,
                Pattern::Bind(Name::from_ast_name(pat.name().unwrap())),
            ),
        };
        let pattern_id = fold.patterns.alloc(pattern);
        (fold, pattern_id)
    }

    fn fold_expression(self, expression: ast::Expression) -> (Self, TermId) {
        match expression {
            ast::Expression::Literal(literal) => self.fold_literal_expression(literal),
            ast::Expression::PathExpression(path) => self.fold_path_expression(path),
            ast::Expression::BlockExpression(block) => self.fold_block_expression(block),
            ast::Expression::InfixExpression(infix) => self.fold_infix_expression(infix),
            ast::Expression::PrefixExpression(prefix) => self.fold_prefix_expression(prefix),
            ast::Expression::ParenthesisExpression(paren) => {
                self.fold_expression(paren.inner_expression().unwrap())
            }
            ast::Expression::CallExpression(call) => self.fold_call_expression(call),
            ast::Expression::IfExpression(if_expr) => self.fold_if_expression(if_expr),
            ast::Expression::MatchExpression(match_expr) => self.fold_match_expression(match_expr),
            ast::Expression::NewExpression(new_expr) => self.fold_new_expression(new_expr),
        }
    }

    fn fold_block_expression(self, block: ast::BlockExpression) -> (Self, TermId) {
        let statement_list = block.statement_list().unwrap();
        let (fold, statements) = statement_list.statements().fold(
            (self, Vec::new()),
            |(fold, mut statements), statement| {
                let (fold, statement) = fold.fold_statement(statement);
                statements.push(statement);
                (fold, statements)
            },
        );

        let (mut fold, trailing_expression) = fold.fold_expression(
            statement_list
                .tail_expression()
                .expect("missing tail expression from block"),
        );
        let expr = Term::Block {
            statements,
            trailing_expression,
        };
        let id = fold.expressions.alloc(expr);
        (fold, id)
    }

    fn fold_statement(self, statement: ast::Statement) -> (BodyFold, Statement) {
        match statement {
            ast::Statement::Let(let_statement) => {
                let pattern = let_statement
                    .pattern()
                    .expect("missing pattern on let statement");
                let (fold, pattern_id) = self.fold_pattern(pattern);
                let expression = let_statement
                    .expression()
                    .expect("missing expression on let statement");
                let (fold, expression_id) = fold.fold_expression(expression);
                (fold, Statement::Let(pattern_id, expression_id))
            }
            ast::Statement::Expression(expr_statement) => {
                let expression = expr_statement
                    .expression()
                    .expect("missing expression on let statement");
                let (fold, expression_id) = self.fold_expression(expression);
                (fold, Statement::Expression(expression_id))
            }
        }
    }

    fn fold_infix_expression(self, infix: ast::InfixExpression) -> (Self, TermId) {
        let (fold, lhs) =
            self.fold_expression(infix.lhs().expect("missing lhs from infix expression"));
        let (mut fold, rhs) =
            fold.fold_expression(infix.rhs().expect("missing rhs from infix expression"));

        let (lhs_pattern_id, lhs_path) = fold.make_argument_pattern();
        let (rhs_pattern_id, rhs_path) = fold.make_argument_pattern();
        let statements = vec![
            Statement::Let(lhs_pattern_id, lhs),
            Statement::Let(rhs_pattern_id, rhs),
        ];

        let op = match infix
            .operator()
            .expect("missing operator from infix expression")
        {
            ast::BinaryOperator::Asterisk(_) => BinaryOperator::Arithmetic(ArithmeticOperator::Mul),
            ast::BinaryOperator::Plus(_) => BinaryOperator::Arithmetic(ArithmeticOperator::Add),
            ast::BinaryOperator::Minus(_) => BinaryOperator::Arithmetic(ArithmeticOperator::Sub),
            ast::BinaryOperator::Slash(_) => BinaryOperator::Arithmetic(ArithmeticOperator::Div),
            ast::BinaryOperator::Percent(_) => BinaryOperator::Arithmetic(ArithmeticOperator::Rem),
            ast::BinaryOperator::DoubleEquals(_) => {
                BinaryOperator::Compare(CompareOperator::Equality { negated: false })
            }
            ast::BinaryOperator::ExclamationEquals(_) => {
                BinaryOperator::Compare(CompareOperator::Equality { negated: true })
            }
            ast::BinaryOperator::Less(_) => BinaryOperator::Compare(CompareOperator::Order {
                ordering: Ordering::Less,
                strict: true,
            }),
            ast::BinaryOperator::LessEquals(_) => BinaryOperator::Compare(CompareOperator::Order {
                ordering: Ordering::Less,
                strict: false,
            }),
            ast::BinaryOperator::Greater(_) => BinaryOperator::Compare(CompareOperator::Order {
                ordering: Ordering::Greater,
                strict: true,
            }),
            ast::BinaryOperator::GreaterEquals(_) => {
                BinaryOperator::Compare(CompareOperator::Order {
                    ordering: Ordering::Greater,
                    strict: false,
                })
            }
            ast::BinaryOperator::DoubleAmpersand(_) => BinaryOperator::Logic(LogicOperator::And),
            ast::BinaryOperator::DoublePipe(_) => BinaryOperator::Logic(LogicOperator::Or),
        };

        let lhs = Term::Path(lhs_path);
        let lhs = fold.expressions.alloc(lhs);
        let rhs = Term::Path(rhs_path);
        let rhs = fold.expressions.alloc(rhs);

        let bin_expr = Term::Binary(op, lhs, rhs);
        let bin_expr = fold.expressions.alloc(bin_expr);

        let bin_expr_block = Term::Block {
            statements,
            trailing_expression: bin_expr,
        };
        let bin_expr_block = fold.expressions.alloc(bin_expr_block);
        (fold, bin_expr_block)
    }

    fn fold_prefix_expression(self, prefix: ast::PrefixExpression) -> (Self, TermId) {
        let (mut fold, inner) = self.fold_expression(
            prefix
                .inner()
                .expect("missing inner expression from prefix expression"),
        );

        let (pattern_id, path) = fold.make_argument_pattern();
        let statements = vec![Statement::Let(pattern_id, inner)];

        let op = match prefix
            .operator()
            .expect("missing operator from infix expression")
        {
            ast::UnaryOperator::Minus(_) => UnaryOperator::Minus,
            ast::UnaryOperator::Exclamation(_) => UnaryOperator::Negation,
            ast::UnaryOperator::Asterisk(_) => UnaryOperator::Dereference,
            ast::UnaryOperator::Ampersand(_) => UnaryOperator::Reference,
        };

        let argument = Term::Path(path);
        let inner_id = fold.expressions.alloc(argument);

        let unary_expr = Term::Unary(op, inner_id);
        let unary_expr_id = fold.expressions.alloc(unary_expr);

        let call_block = Term::Block {
            statements,
            trailing_expression: unary_expr_id,
        };
        let call_block_id = fold.expressions.alloc(call_block);
        (fold, call_block_id)
    }

    fn fold_if_expression(self, if_expr: ast::IfExpression) -> (Self, TermId) {
        let (fold, condition) = self.fold_expression(if_expr.condition().unwrap());
        let (fold, then_branch) = fold.fold_expression(if_expr.then_branch().unwrap());
        let (mut fold, else_branch) = fold.fold_expression(if_expr.else_branch().unwrap());

        let id = fold.expressions.alloc(Term::If {
            condition,
            then_branch,
            else_branch,
        });

        (fold, id)
    }

    fn fold_match_expression(self, match_expr: ast::MatchExpression) -> (Self, TermId) {
        let (fold, matchee) = self.fold_expression(match_expr.matchee().unwrap());

        let (mut fold, case_list) = match_expr.case_list().unwrap().cases().fold(
            (fold, Vec::new()),
            |(fold, mut case_list), case| {
                let (fold, case_expr) = fold.fold_expression(case.expression().unwrap());
                let (fold, pattern) = fold.fold_pattern(case.pattern().unwrap());
                case_list.push((pattern, case_expr));
                (fold, case_list)
            },
        );

        let match_expr = Term::Match { matchee, case_list };
        let id = fold.expressions.alloc(match_expr);
        (fold, id)
    }

    fn fold_call_expression(self, call: ast::CallExpression) -> (Self, TermId) {
        let (fold, callee) =
            self.fold_expression(call.callee().expect("missing callee from call expression"));

        let (mut fold, arguments) = call
            .argument_list()
            .expect("missing argument list from call expression")
            .arguments()
            .fold(
                (fold, Vec::new()),
                |(expressions, mut arguments), expression| {
                    let (expressions, argument) = Self::fold_expression(expressions, expression);
                    arguments.push(argument);
                    (expressions, arguments)
                },
            );

        let call = Term::Call { callee, arguments };
        let id = fold.expressions.alloc(call);
        (fold, id)
    }

    fn fold_path_expression(mut self, path: ast::PathExpression) -> (Self, TermId) {
        let path_expr = Term::Path(Path::from_ast(path.path().unwrap()));
        let id = self.expressions.alloc(path_expr);
        (self, id)
    }

    fn fold_literal_expression(mut self, literal: ast::Literal) -> (Self, TermId) {
        let literal = match literal.literal_kind() {
            ast::TokenLiteral::Integer(integer) => {
                let (radical, suffix) = integer.radical_and_suffix();

                let kind = match suffix {
                    Some("i32") => Some(BuiltinInteger::I32),
                    Some("i64") => Some(BuiltinInteger::I64),
                    Some(_invalid_suffix) => None,
                    None => None,
                };

                let value = match radical
                    .chars()
                    .filter(|c| *c != '_')
                    .collect::<String>()
                    .parse::<u128>()
                    .ok()
                {
                    Some(value) => match kind {
                        None => value,
                        Some(BuiltinInteger::I32) => value,
                        Some(BuiltinInteger::I64) => value,
                    },
                    None => {
                        // larger than u128
                        0
                    }
                };

                Literal::Integer(value, kind)
            }
            ast::TokenLiteral::Boolean(bool_token) => match bool_token {
                ast::Boolean::True(_) => Literal::Bool(true),
                ast::Boolean::False(_) => Literal::Bool(false),
            },
        };

        let literal = Term::Literal(literal);
        let id = self.expressions.alloc(literal);
        (self, id)
    }

    fn fold_new_expression(self, new_expr: ast::NewExpression) -> (Self, Idx<Term>) {
        let (mut fold, inner_id) = self.fold_call_expression(new_expr.call_expression().unwrap());
        let new_expr = Term::New(inner_id);
        let id = fold.expressions.alloc(new_expr);
        (fold, id)
    }

    fn make_argument_pattern(&mut self) -> (Idx<Pattern>, Path) {
        let name = Name::new_inline(format!("__arg{}", self.argument_binder_count).as_str());
        self.argument_binder_count += 1;
        let pattern = Pattern::Bind(name.clone());
        let pattern_id = self.patterns.alloc(pattern);
        (
            pattern_id,
            Path {
                segments: vec![name],
            },
        )
    }
}
