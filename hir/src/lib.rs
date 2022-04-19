mod ast_node_map;
mod definitions_db;
mod definitions_map;
mod item_tree;
pub mod refinement;
mod source_db;

use std::{collections::HashMap, fmt::Debug};

use ast_node_map::AstNodeMap;
pub use definitions_db::{DefinitionsDatabase, DefinitionsDatabaseStorage};
pub use definitions_map::{
    BuiltinInteger, BuiltinType, DefinitionsMap, FunctionLocationId, TypeLocationId,
    TypeNamespaceItem, ValueConstructor, ValueConstructorId, ValueNamespaceItem,
};
pub use definitions_map::{InternDatabase, InternDatabaseStorage};
use item_tree::{FunctionDefinition, TypeDefinition};
use la_arena::{Arena, ArenaMap, Idx};
use refinement::Predicate;
use smol_str::SmolStr;
pub use source_db::{SourceDatabase, SourceDatabaseStorage};
use syntax::ast::{self, AstToken};

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: DefinitionsDatabase + Upcast<dyn DefinitionsDatabase> {
    fn type_of_definition(&self, definition: TypeableDefinition) -> Type;

    fn type_of_value(&self, id: TypeableValueDefinitionId) -> Type;

    fn callable_definition_signature(&self, callable: CallableDefinitionId) -> FunctionSignature;

    fn infer_body_expression_types(&self, function: FunctionLocationId) -> InferenceResult;
}

fn type_of_definition(_db: &dyn HirDatabase, definition: TypeableDefinition) -> Type {
    match definition {
        TypeableDefinition::Type(type_def_location) => Type::AbstractDataType(type_def_location),
        TypeableDefinition::Builtin(builtin) => match builtin {
            BuiltinType::Integer(int) => match int {
                BuiltinInteger::I32 => Type::Scalar(ScalarType::Integer(IntegerKind::I32)),
                BuiltinInteger::I64 => Type::Scalar(ScalarType::Integer(IntegerKind::I64)),
            },
            BuiltinType::Boolean => Type::Scalar(ScalarType::Boolean),
        },
    }
}

fn type_of_value(_db: &dyn HirDatabase, value: TypeableValueDefinitionId) -> Type {
    match value {
        TypeableValueDefinitionId::Function(function_location_id) => Type::FunctionDefinition(
            CallableDefinitionId::FunctionDefinition(function_location_id),
        ),
        TypeableValueDefinitionId::ValueConstructor(value_constructor_id) => {
            Type::FunctionDefinition(CallableDefinitionId::ValueConstructor(value_constructor_id))
        }
    }
}

fn callable_definition_signature(
    db: &dyn HirDatabase,
    callable: CallableDefinitionId,
) -> FunctionSignature {
    match callable {
        CallableDefinitionId::FunctionDefinition(function_id) => {
            function_definition_signature(db, function_id)
        }
        CallableDefinitionId::ValueConstructor(value_constructor_id) => {
            value_constructor_signature(db, value_constructor_id)
        }
    }
}

fn function_definition_signature(
    db: &dyn HirDatabase,
    function_id: FunctionLocationId,
) -> FunctionSignature {
    let resolver = Resolver::new_for_function(db, function_id);
    let function = db.function_definition_data(function_id);

    let typeref_resolver = TypeReferenceResolver::new(db, &resolver);

    let parameter_types = function
        .parameter_types
        .iter()
        .map(|type_reference| {
            typeref_resolver
                .resolve_type_reference(type_reference)
                .expect("missing function definition argument type")
        })
        .collect();

    let return_type = typeref_resolver
        .resolve_type_reference(&function.return_type)
        .expect("missing function definition return type");

    FunctionSignature {
        parameter_types,
        return_type,
    }
}

fn value_constructor_signature(
    db: &dyn HirDatabase,
    value_constructor_id: ValueConstructorId,
) -> FunctionSignature {
    let resolver = Resolver::new_for_type(db, value_constructor_id.parrent_id);
    let typeref_resolver = TypeReferenceResolver::new(db, &resolver);
    let type_data = db.type_definition_data(value_constructor_id.parrent_id);
    let constructor = type_data.value_constructor(value_constructor_id.id);

    let parameter_types = constructor
        .parameters
        .iter()
        .map(|type_reference| {
            typeref_resolver
                .resolve_type_reference(type_reference)
                .expect("missing value constructor definition data type")
        })
        .collect();

    let return_type = Type::AbstractDataType(value_constructor_id.parrent_id);

    FunctionSignature {
        parameter_types,
        return_type,
    }
}

fn infer_body_expression_types(
    db: &dyn HirDatabase,
    function_id: FunctionLocationId,
) -> InferenceResult {
    InferenceResult::new(db, function_id)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeableDefinition {
    Type(TypeLocationId),
    Builtin(BuiltinType),
}

impl From<TypeNamespaceItem> for TypeableDefinition {
    fn from(typeable_item: TypeNamespaceItem) -> Self {
        match typeable_item {
            TypeNamespaceItem::TypeDefinition(ty_def) => Self::Type(ty_def),
            TypeNamespaceItem::Builtin(builtin) => Self::Builtin(builtin),
        }
    }
}

impl From<TypeLocationId> for TypeableDefinition {
    fn from(from: TypeLocationId) -> Self {
        Self::Type(from)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeableValueDefinitionId {
    Function(FunctionLocationId),
    ValueConstructor(ValueConstructorId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    AbstractDataType(TypeLocationId),
    FunctionDefinition(CallableDefinitionId),
    Refinement(Box<Type>, Name, Predicate),
    Pointer(Box<Type>),
    Scalar(ScalarType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallableDefinitionId {
    FunctionDefinition(FunctionLocationId),
    ValueConstructor(ValueConstructorId),
}

impl From<FunctionLocationId> for CallableDefinitionId {
    fn from(id: FunctionLocationId) -> Self {
        Self::FunctionDefinition(id)
    }
}

impl From<ValueConstructorId> for CallableDefinitionId {
    fn from(id: ValueConstructorId) -> Self {
        Self::ValueConstructor(id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarType {
    Integer(IntegerKind),
    Boolean,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerKind {
    I32,
    I64,
}

pub type TypeDefinitionId = Idx<TypeDefinition>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinitionData {
    pub name: Name,
    pub value_constructors: Arena<ValueConstructor>,
}

impl TypeDefinitionData {
    pub fn value_constructor(&self, id: Idx<ValueConstructor>) -> &ValueConstructor {
        &self.value_constructors[id]
    }
}

impl TypeDefinition {
    fn lower(ast_node_map: &AstNodeMap, ty: ast::TypeDefinition) -> Self {
        TypeDefinition {
            name: Name::lower(ty.name().unwrap()),
            value_constructors: ty
                .value_constructor_list()
                .unwrap()
                .constructors()
                .map(ValueConstructor::lower)
                .collect(),
            ast_node_id: ast_node_map.ast_id(&ty),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinitionData {
    pub name: Name,
    pub parameter_types: Vec<TypeReference>,
    pub return_type: TypeReference,
}

impl FunctionDefinition {
    fn lower(ast_node_map: &AstNodeMap, f: ast::FunctionDefinition) -> Self {
        let parameter_types = {
            if let Some(param_list) = f.parameter_list() {
                let params = param_list.parameters();
                params
                    .map(|param| {
                        TypeReference::lower(
                            param.ty().expect("missing type for function parameter"),
                        )
                    })
                    .collect()
            } else {
                Vec::new()
            }
        };
        let name = Name::lower(f.name().expect("missing name from function declaration."));
        let return_type =
            TypeReference::lower(f.return_type().expect("missin return type from function"));
        let ast_node_id = ast_node_map.ast_id(&f);
        Self {
            name,
            parameter_types,
            return_type,
            ast_node_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub patterns: Arena<Pattern>,
    pub parameters: Vec<PatternId>,
    pub expressions: Arena<Expression>,
    pub root_expression: ExpressionId,
}

impl Body {
    pub fn lower(function: ast::FunctionDefinition) -> Self {
        let (
            BodyFold {
                expressions,
                patterns,
                parameters,
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

    pub fn lower_refinement_body(refinement: ast::RefinementType) -> Self {
        let (
            BodyFold {
                expressions,
                patterns,
                parameters,
            },
            root_expression,
        ) = BodyFold::new()
            .fold_refinement_binding(&refinement)
            .fold_expression(refinement.predicate().unwrap());

        Self {
            patterns,
            parameters,
            expressions,
            root_expression,
        }
    }
}

#[derive(Default)]
struct BodyFold {
    pub expressions: Arena<Expression>,
    pub patterns: Arena<Pattern>,
    pub parameters: Vec<PatternId>,
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

    fn fold_refinement_binding(self, refinement: &ast::RefinementType) -> Self {
        let pattern = refinement.inner_pattern().unwrap();
        let (mut fold, pattern_id) = self.fold_pattern(pattern);
        fold.parameters.push(pattern_id);
        fold
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
                    Pattern::Deconstructor(Path::lower(deconstructor.path().unwrap()), subpatterns),
                )
            }
            ast::Pattern::BindingPattern(pat) => {
                (self, Pattern::Bind(Name::lower(pat.name().unwrap())))
            }
        };
        let pattern_id = fold.patterns.alloc(pattern);
        (fold, pattern_id)
    }

    fn fold_expression(self, expression: ast::Expression) -> (Self, ExpressionId) {
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

    fn fold_block_expression(self, block: ast::BlockExpression) -> (Self, ExpressionId) {
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
        let expr = Expression::Block {
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

    fn fold_infix_expression(self, infix: ast::InfixExpression) -> (Self, ExpressionId) {
        let (fold, lhs) =
            self.fold_expression(infix.lhs().expect("missing lhs from infix expression"));
        let (mut fold, rhs) =
            fold.fold_expression(infix.rhs().expect("missing rhs from infix expression"));

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

        let bin_expr = Expression::Binary(op, lhs, rhs);
        let id = fold.expressions.alloc(bin_expr);
        (fold, id)
    }

    fn fold_prefix_expression(self, prefix: ast::PrefixExpression) -> (Self, ExpressionId) {
        let (mut fold, inner) = self.fold_expression(
            prefix
                .inner()
                .expect("missing inner expression from prefix expression"),
        );

        let op = match prefix
            .operator()
            .expect("missing operator from infix expression")
        {
            ast::UnaryOperator::Minus(_) => UnaryOperator::Minus,
            ast::UnaryOperator::Exclamation(_) => UnaryOperator::Negation,
            ast::UnaryOperator::Asterisk(_) => UnaryOperator::Dereference,
            ast::UnaryOperator::Ampersand(_) => UnaryOperator::Reference,
        };

        let unary_expr = Expression::Unary(op, inner);
        let id = fold.expressions.alloc(unary_expr);
        (fold, id)
    }

    fn fold_if_expression(self, if_expr: ast::IfExpression) -> (Self, ExpressionId) {
        let (fold, condition) = self.fold_expression(if_expr.condition().unwrap());
        let (fold, then_branch) = fold.fold_expression(if_expr.then_branch().unwrap());
        let (mut fold, else_branch) = fold.fold_expression(if_expr.else_branch().unwrap());

        let id = fold.expressions.alloc(Expression::If {
            condition,
            then_branch,
            else_branch,
        });

        (fold, id)
    }

    fn fold_match_expression(self, match_expr: ast::MatchExpression) -> (Self, ExpressionId) {
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

        let match_expr = Expression::Match { matchee, case_list };
        let id = fold.expressions.alloc(match_expr);
        (fold, id)
    }

    fn fold_call_expression(self, call: ast::CallExpression) -> (Self, ExpressionId) {
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

        let call = Expression::Call { callee, arguments };
        let id = fold.expressions.alloc(call);
        (fold, id)
    }

    fn fold_path_expression(mut self, path: ast::PathExpression) -> (Self, ExpressionId) {
        let path_expr = Expression::Path(Path::lower(path.path().unwrap()));
        let id = self.expressions.alloc(path_expr);
        (self, id)
    }

    fn fold_literal_expression(mut self, literal: ast::Literal) -> (Self, ExpressionId) {
        let literal = match literal.literal_kind() {
            ast::TokenLiteral::Integer(integer) => {
                let (radical, suffix) = integer.radical_and_suffix();

                let kind = match suffix {
                    Some("i32") => Some(IntegerKind::I32),
                    Some("i64") => Some(IntegerKind::I64),
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
                        Some(IntegerKind::I32) => value,
                        Some(IntegerKind::I64) => value,
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

        let literal = Expression::Literal(literal);
        let id = self.expressions.alloc(literal);
        (self, id)
    }

    fn fold_new_expression(self, new_expr: ast::NewExpression) -> (Self, Idx<Expression>) {
        let (mut fold, inner_id) = self.fold_call_expression(new_expr.call_expression().unwrap());
        let new_expr = Expression::New(inner_id);
        let id = fold.expressions.alloc(new_expr);
        (fold, id)
    }
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
                Self::Deconstructor(Path::lower(pat.path().unwrap()), Vec::new())
            }
            ast::Pattern::BindingPattern(pat) => Self::Bind(Name::lower(pat.name().unwrap())),
        }
    }
}

pub type ExpressionId = Idx<Expression>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Block {
        statements: Vec<Statement>,
        trailing_expression: ExpressionId,
    },
    If {
        condition: ExpressionId,
        then_branch: ExpressionId,
        else_branch: ExpressionId,
    },
    Match {
        matchee: ExpressionId,
        case_list: Vec<(PatternId, ExpressionId)>,
    },
    Call {
        callee: ExpressionId,
        arguments: Vec<ExpressionId>,
    },
    New(ExpressionId),
    Binary(BinaryOperator, ExpressionId, ExpressionId),
    Unary(UnaryOperator, ExpressionId),
    Path(Path),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(PatternId, ExpressionId),
    Expression(ExpressionId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Integer(u128, Option<IntegerKind>),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub id: SmolStr,
}

impl Name {
    fn lower(name: ast::Name) -> Self {
        Name {
            id: name.identifier().text().into(),
        }
    }

    fn lower_nameref(name: ast::NameReference) -> Self {
        Name {
            id: name.identifier().text().into(),
        }
    }

    const fn new_inline(name: &str) -> Self {
        Self {
            id: SmolStr::new_inline(name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub segments: Vec<Name>,
}

impl Path {
    fn lower(path: ast::Path) -> Self {
        let segments = Self::collect_segments(Vec::new(), path);
        Self { segments }
    }

    fn collect_segments(mut segments: Vec<Name>, path: ast::Path) -> Vec<Name> {
        let segment = path.path_segment().unwrap();
        if let Some(path) = path.path() {
            let mut segments = Self::collect_segments(segments, path);
            segments.push(Name::lower_nameref(segment.name_reference().unwrap()));
            segments
        } else {
            segments.push(Name::lower_nameref(segment.name_reference().unwrap()));
            segments
        }
    }
}

pub type TypeReferenceId = Idx<TypeReference>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeReference {
    Path(Path),
    Refinement(Box<TypeReference>, Name, Predicate),
    Pointer(Box<TypeReference>),
}

impl TypeReference {
    fn lower(ty: ast::Type) -> Self {
        match ty {
            ast::Type::PathType(path_ty) => Self::Path(Path::lower(path_ty.path().unwrap())),
            ast::Type::PointerType(ptr_ty) => {
                Self::Pointer(Box::new(Self::lower(ptr_ty.inner_type().unwrap())))
            }
            ast::Type::RefinementType(refinement_ty) => Self::Refinement(
                Box::new(Self::lower(refinement_ty.inner_type().unwrap())),
                Name::lower(match refinement_ty.inner_pattern().unwrap() {
                    ast::Pattern::DeconstructorPattern(_) => todo!(),
                    ast::Pattern::BindingPattern(binding) => binding.name().unwrap(),
                }),
                Predicate::lower(refinement_ty.predicate().unwrap()),
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
}

pub struct Resolver {
    scopes: Vec<Scope>,
}

impl Resolver {
    pub fn new_empty() -> Self {
        Resolver { scopes: Vec::new() }
    }

    pub fn new_root_resolver(db: &dyn HirDatabase) -> Self {
        let scopes = vec![Scope::Module {
            definitions_map: db.source_file_definitions_map(),
        }];

        Self { scopes }
    }

    pub fn new_for_function(db: &dyn HirDatabase, _fid: FunctionLocationId) -> Self {
        Self::new_root_resolver(db)
    }

    pub fn new_for_type(db: &dyn HirDatabase, _id: TypeLocationId) -> Self {
        Self::new_root_resolver(db)
    }

    pub fn new_for_expression(
        db: &dyn HirDatabase,
        function_id: FunctionLocationId,
        expression_id: ExpressionId,
    ) -> Self {
        let resolver = Self::new_root_resolver(db);
        let expr_scope_map = db.expression_scope_map(function_id);

        let resolver = expr_scope_map
            .expression_scope_ids(expression_id)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .fold(resolver, |mut resolver, scope_id| {
                resolver.scopes.push(Scope::Expression {
                    scope_map: expr_scope_map.clone(),
                    scope_id,
                });
                resolver
            });

        resolver
    }

    pub fn resolve_path_in_type_namespace(
        &self,
        db: &dyn DefinitionsDatabase,
        path: &Path,
    ) -> Option<TypeNamespaceItem> {
        path.segments
            .first()
            .map(|_| {
                self.scopes.iter().rev().find_map(|scope| match scope {
                    Scope::Module { definitions_map } => {
                        definitions_map.resolve_path(db, path).in_type_namespace()
                    }
                    Scope::Expression { .. } => None,
                })
            })
            .expect("empty path")
    }

    pub fn resolve_path_in_value_namespace(
        &self,
        db: &dyn DefinitionsDatabase,
        path: &Path,
    ) -> Option<ValueNamespaceItem> {
        path.segments
            .first()
            .map(|first_name| {
                self.scopes.iter().rev().find_map(|scope| match scope {
                    Scope::Module { definitions_map } => {
                        definitions_map.resolve_path(db, path).in_value_namespace()
                    }
                    Scope::Expression {
                        scope_map,
                        scope_id,
                    } if path.segments.len() == 1 => {
                        let scope = &scope_map.scopes[*scope_id];

                        scope.entries.iter().find_map(|(name, pattern_id)| {
                            match name == first_name {
                                true => Some(ValueNamespaceItem::LocalBinding(*pattern_id)),
                                false => None,
                            }
                        })
                    }
                    _ => None,
                })
            })
            .expect("empty path")
    }
}

impl From<TypeLocationId> for TypeNamespaceItem {
    fn from(type_location: TypeLocationId) -> Self {
        Self::TypeDefinition(type_location)
    }
}

impl From<BuiltinType> for TypeNamespaceItem {
    fn from(builtin: BuiltinType) -> Self {
        Self::Builtin(builtin)
    }
}

pub enum Scope {
    Module {
        definitions_map: DefinitionsMap,
    },
    Expression {
        scope_map: ExpressionScopeMap,
        scope_id: ExpressionScopeId,
    },
}

static BUILTIN_SCOPE: &[(Name, BuiltinType)] = &[
    (Name::new_inline("bool"), BuiltinType::Boolean),
    (
        Name::new_inline("i32"),
        BuiltinType::Integer(BuiltinInteger::I32),
    ),
    (
        Name::new_inline("i64"),
        BuiltinType::Integer(BuiltinInteger::I64),
    ),
];

type ExpressionScopeId = Idx<ExpressionScope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionScope {
    pub parent: Option<ExpressionScopeId>,
    pub entries: Vec<(Name, PatternId)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionScopeMap {
    pub scopes: Arena<ExpressionScope>,
    pub scope_map: HashMap<ExpressionId, ExpressionScopeId>,
}

impl ExpressionScopeMap {
    pub fn new(body: &Body) -> Self {
        let mut scopes = Arena::default();

        let root = scopes.alloc(Self::build_root_scope(body));

        let ExpressionScopeFold {
            scopes, scope_map, ..
        } = ExpressionScopeFold {
            body,
            scopes,
            scope_map: HashMap::new(),
        }
        .fold_expression(body.root_expression, root);

        Self { scopes, scope_map }
    }

    pub fn expression_scope_ids(
        &self,
        expression_id: ExpressionId,
    ) -> impl Iterator<Item = ExpressionScopeId> + '_ {
        let scope_id = self.scope_map[&expression_id];
        std::iter::successors(Some(scope_id), move |&scope| self.scopes[scope].parent)
    }

    fn build_root_scope(body: &Body) -> ExpressionScope {
        let entries = body
            .parameters
            .iter()
            .flat_map(|pattern_id| {
                let pattern = &body.patterns[*pattern_id];
                match pattern {
                    Pattern::Deconstructor(_, _) => todo!(),
                    Pattern::Bind(bind) => Some((bind.clone(), *pattern_id)),
                }
            })
            .collect();

        ExpressionScope {
            parent: None,
            entries,
        }
    }
}

struct ExpressionScopeFold<'body> {
    pub body: &'body Body,
    pub scopes: Arena<ExpressionScope>,
    pub scope_map: HashMap<ExpressionId, ExpressionScopeId>,
}

impl<'body> ExpressionScopeFold<'body> {
    fn fold_expression(mut self, expr_id: ExpressionId, scope_id: ExpressionScopeId) -> Self {
        self.scope_map.insert(expr_id, scope_id);
        match &self.body.expressions[expr_id] {
            Expression::Block {
                statements,
                trailing_expression,
            } => {
                let (fold, scope_id) =
                    statements
                        .iter()
                        .fold(
                            (self, scope_id),
                            |(fold, scope_id), statement| match statement {
                                Statement::Let(pattern_id, expr_id) => {
                                    let mut fold = fold.fold_expression(*expr_id, scope_id);
                                    let entries = fold.fold_pattern_bindings(*pattern_id);
                                    let scope_id = fold.scopes.alloc(ExpressionScope {
                                        parent: Some(scope_id),
                                        entries,
                                    });
                                    (fold, scope_id)
                                }
                                Statement::Expression(expr_id) => {
                                    (fold.fold_expression(*expr_id, scope_id), scope_id)
                                }
                            },
                        );
                fold.fold_expression(*trailing_expression, scope_id)
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => self
                .fold_expression(*condition, scope_id)
                .fold_expression(*then_branch, scope_id)
                .fold_expression(*else_branch, scope_id),
            Expression::Match { matchee, case_list } => case_list.iter().fold(
                self.fold_expression(*matchee, scope_id),
                |mut fold, (pattern_id, expr_id)| {
                    let entries = fold.fold_pattern_bindings(*pattern_id);
                    let scope_id = fold.scopes.alloc(ExpressionScope {
                        parent: Some(scope_id),
                        entries,
                    });
                    fold.fold_expression(*expr_id, scope_id)
                },
            ),
            Expression::Call { callee, arguments } => arguments
                .iter()
                .fold(self.fold_expression(*callee, scope_id), |fold, argument| {
                    fold.fold_expression(*argument, scope_id)
                }),
            Expression::Binary(_, lhs, rhs) => self
                .fold_expression(*lhs, scope_id)
                .fold_expression(*rhs, scope_id),
            Expression::Unary(_, expr) => self.fold_expression(*expr, scope_id),
            Expression::Path(_) => self,
            Expression::Literal(_) => self,
            Expression::New(inner) => self.fold_expression(*inner, scope_id),
        }
    }

    fn fold_pattern_bindings(&self, pattern_id: PatternId) -> Vec<(Name, PatternId)> {
        self.fold_pattern_bindings_0(pattern_id, Vec::new())
    }

    fn fold_pattern_bindings_0(
        &self,
        pattern_id: PatternId,
        mut entries: Vec<(Name, PatternId)>,
    ) -> Vec<(Name, PatternId)> {
        match &self.body.patterns[pattern_id] {
            Pattern::Deconstructor(_, subpatterns) => {
                subpatterns.iter().fold(entries, |entries, pattern| {
                    self.fold_pattern_bindings_0(*pattern, entries)
                })
            }
            Pattern::Bind(name) => {
                entries.push((name.clone(), pattern_id));
                entries
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferenceResult {
    pub type_of_expression: ArenaMap<ExpressionId, Type>,
    pub type_of_pattern: ArenaMap<PatternId, Type>,
}

impl InferenceResult {
    pub fn new(db: &dyn HirDatabase, function_id: FunctionLocationId) -> Self {
        let body = &db.body_of_definition(function_id);
        InferenceResultFold::fold_function(db, function_id, body).inference_result
    }
}

struct InferenceResultFold<'s> {
    pub db: &'s dyn HirDatabase,
    pub function_id: FunctionLocationId,
    pub body: &'s Body,
    pub resolver: Resolver,
    pub inference_result: InferenceResult,
}

impl<'s> InferenceResultFold<'s> {
    fn new(db: &'s dyn HirDatabase, function_id: FunctionLocationId, body: &'s Body) -> Self {
        Self {
            db,
            function_id,
            body,
            resolver: Resolver::new_root_resolver(db),
            inference_result: InferenceResult {
                type_of_expression: ArenaMap::default(),
                type_of_pattern: ArenaMap::default(),
            },
        }
    }

    pub fn fold_function(
        db: &'s dyn HirDatabase,
        function_id: FunctionLocationId,
        body: &'s Body,
    ) -> Self {
        Self::new(db, function_id, body)
            .fold_function_parameters()
            .fold_body_root_expression()
    }

    fn fold_function_parameters(mut self) -> Self {
        let function = self.db.function_definition_data(self.function_id);
        self.resolver = Resolver::new_for_function(self.db, self.function_id);
        let ty_resolver = TypeReferenceResolver::new(self.db, &self.resolver);
        let parameter_types = function
            .parameter_types
            .iter()
            .map(|type_reference| {
                ty_resolver
                    .resolve_type_reference(type_reference)
                    .expect("missing parameter type")
            })
            .collect::<Vec<_>>();

        self = self
            .body
            .parameters
            .iter()
            .zip(parameter_types)
            .fold(self, |fold, (pattern_id, ty)| {
                fold.fold_pattern(*pattern_id, ty)
            });

        self
    }

    fn fold_pattern(self, pattern_id: PatternId, expected_type: Type) -> Self {
        let (mut fold, ty) = match &self.body.patterns[pattern_id] {
            Pattern::Deconstructor(path, subpatterns) => {
                let path_resolver =
                    ValuePathResolver::new(self.db, &self.inference_result, &self.resolver);
                let expected_type = path_resolver.resolve_type_for_value_path(path);

                let subpattern_types = match expected_type {
                    Type::FunctionDefinition(CallableDefinitionId::ValueConstructor(
                        constructor_id,
                    )) => {
                        let signature =
                            self.db.callable_definition_signature(constructor_id.into());
                        signature.parameter_types
                    }
                    _ => panic!(),
                };

                let fold = subpatterns
                    .iter()
                    .zip(subpattern_types.into_iter())
                    .fold(self, |fold, (pattern_id, pattern_type)| {
                        fold.fold_pattern(*pattern_id, pattern_type)
                    });

                (fold, expected_type)
            }
            Pattern::Bind(_) => (self, expected_type),
        };
        fold.inference_result.type_of_pattern.insert(pattern_id, ty);
        fold
    }

    fn fold_body_root_expression(self) -> Self {
        let root_expression = self.body.root_expression;
        let (fold, ty) = self.fold_expression_type(root_expression);
        let ret_ty = fold
            .db
            .callable_definition_signature(fold.function_id.into())
            .return_type;
        if ty != ret_ty {
            panic!("expected {:?} found {:?}", ret_ty, ty);
        }
        fold
    }

    fn fold_expression_type(self, expr_id: ExpressionId) -> (Self, Type) {
        let expr = &self.body.expressions[expr_id];
        let (mut fold, ty) = match expr {
            Expression::Block {
                statements,
                trailing_expression,
            } => {
                let fold = statements
                    .iter()
                    .fold(self, |fold, statement| fold.fold_statement(statement));
                fold.fold_expression_type(*trailing_expression)
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let (fold, condition_ty) = self.fold_expression_type(*condition);
                let (fold, then_ty) = fold.fold_expression_type(*then_branch);
                let (fold, else_ty) = fold.fold_expression_type(*else_branch);

                if condition_ty != Type::Scalar(ScalarType::Boolean) {
                    panic!("condition is not boolean");
                }

                if then_ty != else_ty {
                    panic!("mismatching types of then and else branches");
                }

                (fold, then_ty)
            }
            Expression::Binary(op, lhs, rhs) => {
                let (fold, lhs_ty) = self.fold_expression_type(*lhs);
                let (fold, rhs_ty) = fold.fold_expression_type(*rhs);

                if lhs_ty != rhs_ty {
                    panic!()
                }

                let ret_ty = match op {
                    BinaryOperator::Arithmetic(_) => match rhs_ty {
                        Type::Scalar(ScalarType::Integer(_)) => rhs_ty,
                        _ => panic!(),
                    },
                    BinaryOperator::Compare(_) => Type::Scalar(ScalarType::Boolean),
                    BinaryOperator::Logic(_) => Type::Scalar(ScalarType::Boolean),
                };

                (fold, ret_ty)
            }
            Expression::Unary(op, expr) => match op {
                UnaryOperator::Minus => self.fold_expression_type(*expr),
                UnaryOperator::Negation => self.fold_expression_type(*expr),
                UnaryOperator::Reference => {
                    let (fold, inner) = self.fold_expression_type(*expr);
                    (fold, Type::Pointer(Box::new(inner)))
                }
                UnaryOperator::Dereference => {
                    let (fold, inner) = self.fold_expression_type(*expr);
                    match inner {
                        Type::Pointer(inner) => (fold, *inner),
                        ty => panic!("Can not dereference type: {:?}", ty),
                    }
                }
            },
            Expression::Match { matchee, case_list } => {
                let (fold, matchee_type) = self.fold_expression_type(*matchee);

                let (fold, case_types) = case_list.iter().fold(
                    (fold, Vec::new()),
                    |(mut fold, mut case_types), (pattern_id, case)| {
                        fold.resolver =
                            Resolver::new_for_expression(fold.db, fold.function_id, expr_id);
                        let fold = fold.fold_pattern(*pattern_id, matchee_type.clone());
                        let (fold, case_type) = fold.fold_expression_type(*case);
                        case_types.push(case_type);
                        (fold, case_types)
                    },
                );

                let mut case_types = case_types.iter();

                if let Some(first_case_type) = case_types.next() {
                    for case_type in case_types {
                        if case_type != first_case_type {
                            panic!(
                                "on match case: Expected {:?} found {:?}",
                                first_case_type, case_type
                            );
                        }
                    }

                    (fold, first_case_type.clone())
                } else {
                    panic!("empty match case list")
                }
            }
            Expression::Literal(lit) => {
                let ty = match lit {
                    Literal::Integer(value, some_kind) => {
                        let inner = match some_kind {
                            Some(int_kind) => Type::Scalar(ScalarType::Integer(int_kind.clone())),
                            None => Type::Scalar(ScalarType::Integer(IntegerKind::I64)),
                        };

                        let name = Name { id: "lit".into() };
                        let predicate = Predicate::Binary(
                            BinaryOperator::Compare(CompareOperator::Equality { negated: false }),
                            Box::new(Predicate::Variable(name.clone())),
                            Box::new(Predicate::Integer(*value)),
                        );
                        Type::Refinement(Box::new(inner), name, predicate)
                    }
                    Literal::Bool(value) => {
                        let inner = Type::Scalar(ScalarType::Boolean);
                        let name = Name { id: "lit".into() };
                        let predicate = Predicate::Binary(
                            BinaryOperator::Compare(CompareOperator::Equality { negated: false }),
                            Box::new(Predicate::Variable(name.clone())),
                            Box::new(Predicate::Boolean(*value)),
                        );
                        Type::Refinement(Box::new(inner), name, predicate)
                    }
                };
                (self, ty)
            }
            Expression::Path(path) => {
                let resolver = Resolver::new_for_expression(self.db, self.function_id, expr_id);
                let path_resolver =
                    ValuePathResolver::new(self.db, &self.inference_result, &resolver);
                let ty = path_resolver.resolve_type_for_value_path(path);
                (self, ty)
            }
            Expression::Call { callee, arguments } => {
                let (fold, callee_type) = self.fold_expression_type(*callee);
                match callee_type {
                    Type::FunctionDefinition(f_id) => {
                        let sig = fold.db.callable_definition_signature(f_id);
                        let (fold, arg_tys) = arguments.iter().fold(
                            (fold, Vec::new()),
                            |(fold, mut arguments), arg| {
                                let (fold, arg_ty) = fold.fold_expression_type(*arg);
                                arguments.push(arg_ty);
                                (fold, arguments)
                            },
                        );
                        if sig.parameter_types != arg_tys {
                            panic!("type of parameters to function call do not match the parameters in the function definition")
                        };
                        (fold, sig.return_type)
                    }
                    x => panic!("function call not implemented for {:?} type", x),
                }
            }
            Expression::New(inner) => {
                let (fold, inner) = self.fold_expression_type(*inner);
                (fold, Type::Pointer(Box::new(inner)))
            }
        };
        fold.inference_result
            .type_of_expression
            .insert(expr_id, ty.clone());
        (fold, ty)
    }

    fn fold_statement(self, statement: &'s Statement) -> InferenceResultFold {
        match statement {
            Statement::Let(pattern_id, expr_id) => {
                let (fold, expr_type) = self.fold_expression_type(*expr_id);
                let fold = fold.fold_pattern(*pattern_id, expr_type);
                fold
            }
            Statement::Expression(expr_id) => {
                let (mut fold, ty) = self.fold_expression_type(*expr_id);
                fold.inference_result
                    .type_of_expression
                    .insert(*expr_id, ty);
                fold
            }
        }
    }
}

struct TypeReferenceResolver<'d> {
    db: &'d dyn HirDatabase,
    resolver: &'d Resolver,
}

impl<'d> TypeReferenceResolver<'d> {
    pub fn new(db: &'d dyn HirDatabase, resolver: &'d Resolver) -> Self {
        Self { db, resolver }
    }

    pub fn resolve_type_reference(&self, type_reference: &TypeReference) -> Option<Type> {
        let ty = match type_reference {
            TypeReference::Path(path) => {
                let typed_item = self
                    .resolver
                    .resolve_path_in_type_namespace(self.db.upcast(), path)?;

                self.db.type_of_definition(typed_item.into())
            }
            TypeReference::Pointer(inner) => {
                let inner = self.resolve_type_reference(inner)?;
                Type::Pointer(Box::new(inner))
            }
            TypeReference::Refinement(inner, name, predicate) => {
                let inner = self.resolve_type_reference(inner)?;
                Type::Refinement(Box::new(inner), name.clone(), predicate.clone())
            }
        };
        Some(ty)
    }
}

struct ValuePathResolver<'d> {
    db: &'d dyn HirDatabase,
    resolver: &'d Resolver,
    inference_result: &'d InferenceResult,
}

impl<'d> ValuePathResolver<'d> {
    pub fn new(
        db: &'d dyn HirDatabase,
        inference_result: &'d InferenceResult,
        resolver: &'d Resolver,
    ) -> Self {
        Self {
            db,
            resolver,
            inference_result,
        }
    }

    pub fn resolve_type_for_value_path(&self, path: &Path) -> Type {
        let value_item = self
            .resolver
            .resolve_path_in_value_namespace(self.db.upcast(), path)
            .unwrap();

        let ty = match value_item {
            ValueNamespaceItem::LocalBinding(pattern_id) => self
                .inference_result
                .type_of_pattern
                .get(pattern_id)
                .expect("pattern has no type")
                .clone(),
            non_local_item => {
                let typeable_value = match non_local_item {
                    ValueNamespaceItem::Function(id) => TypeableValueDefinitionId::Function(id),
                    ValueNamespaceItem::ValueConstructor(id) => {
                        TypeableValueDefinitionId::ValueConstructor(id)
                    }
                    _ => panic!(),
                };

                self.db.type_of_value(typeable_value)
            }
        };

        ty
    }
}
