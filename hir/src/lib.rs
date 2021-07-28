use la_arena::{Arena, Idx};
use smol_str::SmolStr;
use std::{collections::HashMap, convert::TryFrom, marker::PhantomData};
use syntax::{
    cst::{self, raw::SyntaxNodePointer, CstNode},
    Parse,
};

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase {
    #[salsa::input]
    fn source_file_text(&self) -> String;

    fn source_file_parse(&self) -> Parse<cst::SourceFile>;

    fn source_file_cst_id_map(&self) -> CstIdMap;
}

fn source_file_parse(source_db: &dyn SourceDatabase) -> Parse<cst::SourceFile> {
    let source = source_db.source_file_text();
    cst::SourceFile::parse(source.as_str())
}

fn source_file_cst_id_map(source_db: &dyn SourceDatabase) -> CstIdMap {
    let parse = source_db.source_file_parse();
    CstIdMap::from_source_file(parse.ast_node())
}

#[salsa::query_group(InternDatabaseStorage)]
pub trait InternDatabase {
    #[salsa::interned]
    fn intern_function(&self, loc: FunctionLocation) -> FunctionLocationId;

    #[salsa::interned]
    fn intern_type(&self, loc: TypeLocation) -> TypeLocationId;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionLocationId(salsa::InternId);

impl salsa::InternKey for FunctionLocationId {
    fn from_intern_id(id: salsa::InternId) -> Self {
        Self(id)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeLocationId(salsa::InternId);

impl salsa::InternKey for TypeLocationId {
    fn from_intern_id(id: salsa::InternId) -> Self {
        Self(id)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionLocation {
    pub id: FunctionDefinitionId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeLocation {
    pub id: TypeDefinitionId,
}

#[salsa::query_group(DefinitionsDatabaseStorage)]
pub trait DefinitionsDatabase:
    SourceDatabase + InternDatabase + Upcast<dyn SourceDatabase>
{
    fn source_file_item_tree(&self) -> ItemTree;

    fn source_file_definitions_map(&self) -> DefinitionsMap;

    fn body_of_definition(&self, def: FunctionLocationId) -> Body;

    fn function_definition_data(&self, id: FunctionLocationId) -> FunctionDefinitionData;

    fn type_definition_data(&self, id: TypeLocationId) -> TypeDefinitionData;
}

fn source_file_item_tree(def_db: &dyn DefinitionsDatabase) -> ItemTree {
    let parse = def_db.source_file_parse();
    let cst_id_map = def_db.source_file_cst_id_map();

    let source = parse.ast_node();

    let functions = source
        .module_items()
        .flat_map(|item| match item {
            cst::ModuleItem::FunctionDefinition(f) => {
                Some(FunctionDefinition::lower(&cst_id_map, f))
            }
            _ => None,
        })
        .collect();

    let types = source
        .module_items()
        .flat_map(|item| match item {
            cst::ModuleItem::TypeDefinition(t) => Some(TypeDefinition::lower(&cst_id_map, t)),
            _ => None,
        })
        .collect();

    ItemTree { functions, types }
}

fn source_file_definitions_map(def_db: &dyn DefinitionsDatabase) -> DefinitionsMap {
    let item_tree = def_db.source_file_item_tree();

    let mut types = HashMap::new();
    let mut values = HashMap::new();
    let mut definitions = Vec::new();

    for (f_id, function) in item_tree.functions.iter() {
        let function_location = FunctionLocation { id: f_id };
        let function_location_id = def_db.intern_function(function_location);
        let location_id = LocationId::FunctionLocationId(function_location_id);

        definitions.push(location_id.clone());
        values.insert(function.name.clone(), location_id);
    }

    for (t_id, type_def) in item_tree.types.iter() {
        let type_location = TypeLocation { id: t_id };
        let type_location_id = def_db.intern_type(type_location);
        let location_id = LocationId::TypeLocationId(type_location_id);

        definitions.push(location_id.clone());
        types.insert(type_def.name.clone(), location_id);
    }

    let item_scope = ItemScope {
        types,
        values,
        definitions,
    };

    DefinitionsMap { item_scope }
}

fn body_of_definition(db: &dyn DefinitionsDatabase, id: FunctionLocationId) -> Body {
    let source_file = db.source_file_parse();
    let cst_map = db.source_file_cst_id_map();
    let package_defs = db.source_file_item_tree();
    let location = db.lookup_intern_function(id);

    let fun_def = &package_defs.functions[location.id];
    let source = &cst_map.arena[fun_def.cst_node_id.cst_id];
    let function_syntax_node = source.get_syntax_node(&source_file.syntax_node());
    let function_cst_node = cst::FunctionDefinition::try_from(function_syntax_node)
        .ok()
        .unwrap();

    Body::lower(function_cst_node)
}

fn function_definition_data(
    db: &dyn DefinitionsDatabase,
    id: FunctionLocationId,
) -> FunctionDefinitionData {
    let loc = db.lookup_intern_function(id);
    let item_tree = db.source_file_item_tree();
    let FunctionDefinition {
        name,
        parameter_types,
        return_type,
        ..
    } = item_tree.functions[loc.id].clone();

    FunctionDefinitionData {
        name,
        parameter_types,
        return_type,
    }
}

fn type_definition_data(db: &dyn DefinitionsDatabase, id: TypeLocationId) -> TypeDefinitionData {
    let loc = db.lookup_intern_type(id);
    let item_tree = db.source_file_item_tree();
    let TypeDefinition {
        name,
        value_constructors,
        ..
    } = item_tree.types[loc.id].clone();

    TypeDefinitionData {
        name,
        value_constructors,
    }
}

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: DefinitionsDatabase + Upcast<dyn DefinitionsDatabase> {
    fn type_of_definition(&self, definition: TypableDefinitionId) -> Type;

    fn type_of_value(&self, id: TypableValueDefinitionId) -> Type;

    // fn value_constructor_filds(&self, id: ValueConstructorId) -> ArenaMap<FildId, TypeKind>;

    fn function_definition_signature(&self, function: FunctionLocationId) -> FunctionSignature;
}

fn type_of_definition(_db: &dyn HirDatabase, definition: TypableDefinitionId) -> Type {
    match definition {
        TypableDefinitionId::Type(type_def_location) => Type::AbstractDataType(type_def_location),
    }
}

fn type_of_value(_db: &dyn HirDatabase, value: TypableValueDefinitionId) -> Type {
    match value {
        TypableValueDefinitionId::Function(function_location_id) => {
            Type::FunctionDefinition(function_location_id)
        }
    }
}

fn function_definition_signature(
    db: &dyn HirDatabase,
    function_id: FunctionLocationId,
) -> FunctionSignature {
    let resolver = Resolver::new_for_function(db, function_id);
    let function = db.function_definition_data(function_id);

    let parameter_types = function
        .parameter_types
        .iter()
        .map(|type_reference| match type_reference {
            TypeReference::Path(path) => resolver.resolve_path_as_type(db.upcast(), path),
        })
        .collect();

    let return_type = match &function.return_type {
        TypeReference::Path(path) => resolver.resolve_path_as_type(db.upcast(), path),
    };

    FunctionSignature {
        parameter_types,
        return_type,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CstIdMap {
    arena: Arena<SyntaxNodePointer>,
}
impl CstIdMap {
    fn from_source_file(source_file: cst::SourceFile) -> Self {
        let arena = source_file
            .module_items()
            .map(|item| SyntaxNodePointer::new(item.as_syntax_node().clone()))
            .collect();

        Self { arena }
    }

    fn cst_id<N: cst::CstNode>(&self, node: &N) -> CstId<N> {
        let node_ptr = SyntaxNodePointer::new(node.as_syntax_node().clone());
        let cst_id = self
            .arena
            .iter()
            .find(|(_, ptr)| **ptr == node_ptr)
            .map(|(id, _)| id)
            .expect("cant find node");

        CstId {
            cst_id,
            phantom: PhantomData,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CstId<N: cst::CstNode> {
    pub cst_id: Idx<SyntaxNodePointer>,
    pub phantom: PhantomData<fn() -> N>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    pub functions: Arena<FunctionDefinition>,
    pub types: Arena<TypeDefinition>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionsMap {
    pub item_scope: ItemScope,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemScope {
    pub types: HashMap<Name, LocationId>,
    pub values: HashMap<Name, LocationId>,
    pub definitions: Vec<LocationId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LocationId {
    FunctionLocationId(FunctionLocationId),
    TypeLocationId(TypeLocationId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypableDefinitionId {
    Type(TypeLocationId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypableValueDefinitionId {
    Function(FunctionLocationId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    AbstractDataType(TypeLocationId),
    FunctionDefinition(FunctionLocationId),
    Scalar(ScalarType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalarType {
    Integer(IntegerKind),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntegerKind {
    I32,
    I64,
}

pub type TypeDefinitionId = Idx<TypeDefinition>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition {
    pub name: Name,
    pub value_constructors: Vec<ValueConstructor>,
    pub cst_id: CstId<cst::TypeDefinition>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinitionData {
    pub name: Name,
    pub value_constructors: Vec<ValueConstructor>,
}

impl TypeDefinition {
    fn lower(cst_map: &CstIdMap, ty: cst::TypeDefinition) -> Self {
        TypeDefinition {
            name: Name::lower(ty.name().unwrap()),
            value_constructors: ty
                .value_constructor_list()
                .unwrap()
                .constructors()
                .map(ValueConstructor::lower)
                .collect(),
            cst_id: cst_map.cst_id(&ty),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueConstructor {
    pub name: Name,
}

impl ValueConstructor {
    fn lower(ctor: cst::ValueConstructor) -> Self {
        ValueConstructor {
            name: Name::lower(ctor.name().unwrap()),
        }
    }
}

pub type FunctionDefinitionId = Idx<FunctionDefinition>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinition {
    pub name: Name,
    pub parameter_types: Vec<TypeReference>,
    pub return_type: TypeReference,
    pub cst_node_id: CstId<cst::FunctionDefinition>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinitionData {
    pub name: Name,
    pub parameter_types: Vec<TypeReference>,
    pub return_type: TypeReference,
}

impl FunctionDefinition {
    fn lower(cstmap: &CstIdMap, f: cst::FunctionDefinition) -> Self {
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
        let cst_node_id = cstmap.cst_id(&f);
        Self {
            name,
            parameter_types,
            return_type,
            cst_node_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub patterns: Arena<Pattern>,
    pub types: Arena<TypeReference>,
    pub parameter_bindings: Vec<(PatternId, TypeReferenceId)>,
    pub return_type: TypeReference,
    pub expressions: Arena<Expression>,
    pub root_expression: ExpressionId,
}

struct BodyExpressionFold {
    pub expressions: Arena<Expression>,
    pub patterns: Arena<Pattern>,
}

impl BodyExpressionFold {
    fn fold_expression(self, expression: cst::Expression) -> (Self, ExpressionId) {
        match expression {
            cst::Expression::Literal(literal) => self.fold_literal_expression(literal),
            cst::Expression::PathExpression(path) => self.fold_path_expression(path),
            cst::Expression::BlockExpression(block) => self.fold_block_expression(block),
            cst::Expression::InfixExpression(infix) => self.fold_infix_expression(infix),
            cst::Expression::PrefixExpression(prefix) => self.fold_prefix_expression(prefix),
            cst::Expression::ParenthesisExpression(paren) => {
                self.fold_expression(paren.inner_expression().unwrap())
            }
            cst::Expression::CallExpression(call) => self.fold_call_expression(call),
            cst::Expression::IfExpression(if_expr) => self.fold_if_expression(if_expr),
            cst::Expression::MatchExpression(match_expr) => self.fold_match_expression(match_expr),
        }
    }

    fn fold_block_expression(self, block: cst::BlockExpression) -> (Self, ExpressionId) {
        let (mut fold, trailing_expression) = self.fold_expression(
            block
                .tail_expression()
                .expect("missing tail expression from block"),
        );
        let expr = Expression::Block {
            trailing_expression,
        };
        let id = fold.expressions.alloc(expr);
        (fold, id)
    }

    fn fold_infix_expression(self, infix: cst::InfixExpression) -> (Self, ExpressionId) {
        let (fold, lhs) =
            self.fold_expression(infix.lhs().expect("missing lhs from infix expression"));
        let (mut fold, rhs) =
            fold.fold_expression(infix.rhs().expect("missing rhs from infix expression"));

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

        let bin_expr = Expression::Binary(op, lhs, rhs);
        let id = fold.expressions.alloc(bin_expr);
        (fold, id)
    }

    fn fold_prefix_expression(self, prefix: cst::PrefixExpression) -> (Self, ExpressionId) {
        let (mut fold, inner) = self.fold_expression(
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

        let unary_expr = Expression::Unary(op, inner);
        let id = fold.expressions.alloc(unary_expr);
        (fold, id)
    }

    fn fold_if_expression(self, if_expr: cst::IfExpression) -> (Self, ExpressionId) {
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

    fn fold_match_expression(self, match_expr: cst::MatchExpression) -> (Self, ExpressionId) {
        let (fold, matchee) = self.fold_expression(match_expr.matchee().unwrap());

        let (mut fold, case_list) = match_expr.case_list().unwrap().cases().fold(
            (fold, Vec::new()),
            |(fold, mut case_list), case| {
                let (mut fold, case_expr) = fold.fold_expression(case.expression().unwrap());
                let pattern = fold.patterns.alloc(Pattern::lower(case.pattern().unwrap()));
                case_list.push((pattern, case_expr));
                (fold, case_list)
            },
        );

        let match_expr = Expression::Match { matchee, case_list };
        let id = fold.expressions.alloc(match_expr);
        (fold, id)
    }

    fn fold_call_expression(self, call: cst::CallExpression) -> (Self, ExpressionId) {
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

    fn fold_path_expression(mut self, path: cst::PathExpression) -> (Self, ExpressionId) {
        let path_expr = Expression::Path(Path::lower(path.path().unwrap()));
        let id = self.expressions.alloc(path_expr);
        (self, id)
    }

    fn fold_literal_expression(mut self, literal: cst::Literal) -> (Self, ExpressionId) {
        let literal = match literal.literal_kind() {
            cst::LiteralKind::Integer(integer) => {
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
        };

        let literal = Expression::Literal(literal);
        let id = self.expressions.alloc(literal);
        (self, id)
    }
}

impl Body {
    pub fn lower(function: cst::FunctionDefinition) -> Self {
        let params = function
            .parameter_list()
            .unwrap()
            .parameters()
            .map(|param| {
                (
                    Pattern::lower(param.pattern().unwrap()),
                    TypeReference::lower(param.ty().unwrap()),
                )
            });

        let mut patterns = Arena::default();
        let mut types = Arena::default();
        let mut parameter_bindings = Vec::new();

        for (pattern, ty) in params {
            let pattern_id = patterns.alloc(pattern);
            let ty_id = types.alloc(ty);
            parameter_bindings.push((pattern_id, ty_id));
        }

        let return_type = function.return_type().map(TypeReference::lower).unwrap();

        let (
            BodyExpressionFold {
                patterns,
                expressions,
            },
            root_expression,
        ) = BodyExpressionFold {
            patterns,
            expressions: Arena::default(),
        }
        .fold_block_expression(function.body().unwrap());

        Self {
            patterns,
            types,
            parameter_bindings,
            return_type,
            expressions,
            root_expression,
        }
    }
}

pub type PatternId = Idx<Pattern>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Path(Path),
    Bind(Name),
}

impl Pattern {
    pub fn lower(pattern: cst::Pattern) -> Self {
        match pattern {
            cst::Pattern::PathPattern(pat) => Self::Path(Path::lower(pat.path().unwrap())),
            cst::Pattern::IdentifierPattern(pat) => Self::Bind(Name::lower(pat.name().unwrap())),
        }
    }
}

pub type ExpressionId = Idx<Expression>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Block {
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
    Binary(BinaryOperator, ExpressionId, ExpressionId),
    Unary(UnaryOperator, ExpressionId),
    Path(Path),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Integer(u128, Option<IntegerKind>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Arithmetic(ArithmeticOperator),
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub id: SmolStr,
}

impl Name {
    fn lower(name: cst::Name) -> Self {
        Name {
            id: name.identifier().text().into(),
        }
    }

    fn lower_nameref(name: cst::NameReference) -> Self {
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
    fn lower(path: cst::Path) -> Self {
        let segments = Self::collect_segments(Vec::new(), path);
        Self { segments }
    }

    fn collect_segments(mut segments: Vec<Name>, path: cst::Path) -> Vec<Name> {
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
}

impl TypeReference {
    fn lower(ty: cst::Type) -> Self {
        match ty {
            cst::Type::PathType(path_ty) => Self::Path(Path::lower(path_ty.path().unwrap())),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
}

pub struct Resolver {}

impl Resolver {
    pub fn new_empty() -> Self {
        Resolver {}
    }

    pub fn new_root_resolver(_db: &dyn HirDatabase) -> Self {
        Resolver::new_empty()
    }

    pub fn new_for_function(db: &dyn HirDatabase, _fid: FunctionLocationId) -> Self {
        Self::new_root_resolver(db)
    }

    pub fn resolve_path_as_type(&self, db: &dyn DefinitionsDatabase, path: &Path) -> Type {
        let def_map = db.source_file_definitions_map();

        let mut segments = path.segments.iter();
        let type_name = segments.next().unwrap();
        def_map
            .item_scope
            .types
            .get(type_name)
            .map(|loc| match loc {
                LocationId::FunctionLocationId(_) => panic!(),
                LocationId::TypeLocationId(type_loc) => Type::AbstractDataType(*type_loc),
            })
            .or_else(|| {
                BUILTIN_SCOPE
                    .iter()
                    .find_map(|(name, ty)| if name == type_name { Some(ty) } else { None })
                    .cloned()
            })
            .unwrap()
    }
}

static BUILTIN_SCOPE: &[(Name, Type)] = &[
    (
        Name::new_inline("i32"),
        Type::Scalar(ScalarType::Integer(IntegerKind::I32)),
    ),
    (
        Name::new_inline("i64"),
        Type::Scalar(ScalarType::Integer(IntegerKind::I64)),
    ),
];
