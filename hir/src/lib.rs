use la_arena::{Arena, ArenaMap, Idx};
use smol_str::SmolStr;
use std::{collections::HashMap, convert::TryFrom, fmt::Debug, marker::PhantomData};
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

    fn expression_scope_map(&self, def: FunctionLocationId) -> ExpressionScopeMap;

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

fn expression_scope_map(
    db: &dyn DefinitionsDatabase,
    def: FunctionLocationId,
) -> ExpressionScopeMap {
    let body = db.body_of_definition(def);
    ExpressionScopeMap::new(&body)
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

impl DefinitionsMap {
    fn resolve_path(&self, db: &dyn DefinitionsDatabase, path: &Path) -> NamespaceResolution {
        path.segments
            .first()
            .map(|name| self.resolve_name(name))
            .and_then(|resolution| {
                path.segments
                    .iter()
                    .skip(1)
                    .try_fold(resolution, |resolution, name| {
                        resolution
                            .in_type_namespace()
                            .map(|typeable_item| match typeable_item {
                                TypeNamespaceItem::TypeDefinition(type_id) => {
                                    let ty_data = db.type_definition_data(type_id);
                                    let value = ty_data
                                        .value_constructors
                                        .iter()
                                        .find(|(_, constructor)| constructor.name == *name)
                                        .map(|(id, _)| {
                                            ValueNamespaceItem::ValueConstructor(
                                                ValueConstructorId {
                                                    parrent_id: type_id,
                                                    id,
                                                },
                                            )
                                        });

                                    NamespaceResolution::new(None, value)
                                }
                                TypeNamespaceItem::Builtin(_) => todo!(),
                            })
                    })
            })
            .unwrap_or_default()
    }

    fn resolve_name(&self, name: &Name) -> NamespaceResolution {
        let builtin_type = BUILTIN_SCOPE
            .iter()
            .find_map(|(builtin_name, builtin_type)| match builtin_name == name {
                true => Some((*builtin_type).into()),
                false => None,
            });
        self.item_scope
            .get(name)
            .or(NamespaceResolution::new(builtin_type, None))
    }
}

#[derive(Default)]
struct NamespaceResolution {
    type_resolution: Option<TypeNamespaceItem>,
    value_resolution: Option<ValueNamespaceItem>,
}

impl NamespaceResolution {
    fn new(
        type_resolution: Option<TypeNamespaceItem>,
        value_resolution: Option<ValueNamespaceItem>,
    ) -> Self {
        Self {
            type_resolution,
            value_resolution,
        }
    }

    fn in_type_namespace(self) -> Option<TypeNamespaceItem> {
        self.type_resolution
    }

    fn in_value_namespace(self) -> Option<ValueNamespaceItem> {
        self.value_resolution
    }

    fn or(self, resolution: Self) -> Self {
        Self {
            type_resolution: self.type_resolution.or(resolution.type_resolution),
            value_resolution: self.value_resolution.or(resolution.value_resolution),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemScope {
    pub types: HashMap<Name, LocationId>,
    pub values: HashMap<Name, LocationId>,
    pub definitions: Vec<LocationId>,
}

impl ItemScope {
    fn get(&self, name: &Name) -> NamespaceResolution {
        NamespaceResolution::new(
            self.types.get(name).and_then(|loc| match loc {
                LocationId::TypeLocationId(id) => Some(TypeNamespaceItem::TypeDefinition(*id)),
                _ => None,
            }),
            self.values.get(name).and_then(|loc| match loc {
                LocationId::FunctionLocationId(id) => Some(ValueNamespaceItem::Function(*id)),
                _ => None,
            }),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LocationId {
    FunctionLocationId(FunctionLocationId),
    TypeLocationId(TypeLocationId),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeableValueDefinitionId {
    Function(FunctionLocationId),
    ValueConstructor(ValueConstructorId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    AbstractDataType(TypeLocationId),
    FunctionDefinition(CallableDefinitionId),
    Scalar(ScalarType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalarType {
    Integer(IntegerKind),
    Boolean,
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
    pub value_constructors: Arena<ValueConstructor>,
    pub cst_id: CstId<cst::TypeDefinition>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinitionData {
    pub name: Name,
    pub value_constructors: Arena<ValueConstructor>,
}

impl TypeDefinitionData {
    pub(crate) fn value_constructor(&self, id: Idx<ValueConstructor>) -> &ValueConstructor {
        &self.value_constructors[id]
    }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueConstructorId {
    pub parrent_id: TypeLocationId,
    pub id: Idx<ValueConstructor>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueConstructor {
    pub name: Name,
    pub parameters: Vec<TypeReference>,
}

impl ValueConstructor {
    fn lower(ctor: cst::ValueConstructor) -> Self {
        ValueConstructor {
            name: Name::lower(ctor.name().unwrap()),
            parameters: ctor
                .constructor_parameter_list()
                .unwrap()
                .types()
                .map(TypeReference::lower)
                .collect(),
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
    pub parameters: Vec<PatternId>,
    pub expressions: Arena<Expression>,
    pub root_expression: ExpressionId,
}

impl Body {
    pub fn lower(function: cst::FunctionDefinition) -> Self {
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

    fn fold_function_parameters(self, function: &cst::FunctionDefinition) -> Self {
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

    fn fold_pattern(self, pattern: cst::Pattern) -> (Self, PatternId) {
        let (mut fold, pattern) = match pattern {
            cst::Pattern::DeconstructorPattern(deconstructor) => {
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
            cst::Pattern::BindingPattern(pat) => {
                (self, Pattern::Bind(Name::lower(pat.name().unwrap())))
            }
        };
        let pattern_id = fold.patterns.alloc(pattern);
        (fold, pattern_id)
    }

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
            cst::BinaryOperator::DoubleAmpersand(_) => BinaryOperator::Logic(LogicOperator::And),
            cst::BinaryOperator::DoublePipe(_) => BinaryOperator::Logic(LogicOperator::Or),
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
            cst::UnaryOperator::Exclamation(_) => UnaryOperator::Negation,
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
            cst::TokenLiteral::Integer(integer) => {
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
            cst::TokenLiteral::Boolean(bool_token) => match bool_token {
                cst::Boolean::True(_) => Literal::Bool(true),
                cst::Boolean::False(_) => Literal::Bool(false),
            },
        };

        let literal = Expression::Literal(literal);
        let id = self.expressions.alloc(literal);
        (self, id)
    }
}

pub type PatternId = Idx<Pattern>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Deconstructor(Path, Vec<PatternId>),
    Bind(Name),
}

impl Pattern {
    pub fn lower(pattern: cst::Pattern) -> Self {
        match pattern {
            cst::Pattern::DeconstructorPattern(pat) => {
                Self::Deconstructor(Path::lower(pat.path().unwrap()), Vec::new())
            }
            cst::Pattern::BindingPattern(pat) => Self::Bind(Name::lower(pat.name().unwrap())),
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

pub enum TypeNamespaceItem {
    TypeDefinition(TypeLocationId),
    Builtin(BuiltinType),
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

#[derive(Debug)]
pub enum ValueNamespaceItem {
    Function(FunctionLocationId),
    ValueConstructor(ValueConstructorId),
    /// local binding in expression body
    LocalBinding(PatternId),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Integer(BuiltinInteger),
    Boolean,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinInteger {
    I32,
    I64,
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
                trailing_expression,
            } => self.fold_expression(*trailing_expression, scope_id),
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
    pub inference_result: InferenceResult,
}

impl<'s> InferenceResultFold<'s> {
    fn new(db: &'s dyn HirDatabase, function_id: FunctionLocationId, body: &'s Body) -> Self {
        Self {
            db,
            function_id,
            body,
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
        let resolver = Resolver::new_for_function(self.db, self.function_id);
        let ty_resolver = TypeReferenceResolver::new(self.db, &resolver);
        let parameter_types = function.parameter_types.iter().map(|type_reference| {
            ty_resolver
                .resolve_type_reference(type_reference)
                .expect("missing parameter type")
        });

        self = self
            .body
            .parameters
            .iter()
            .zip(parameter_types)
            .fold(self, |fold, (pattern_id, ty)| {
                fold.fold_pattern(&resolver, *pattern_id, ty)
            });

        self
    }

    fn fold_pattern(
        mut self,
        resolver: &Resolver,
        pattern_id: PatternId,
        expected_type: Type,
    ) -> Self {
        let ty = match &self.body.patterns[pattern_id] {
            Pattern::Deconstructor(path, _) => {
                let path_resolver =
                    ValuePathResolver::new(self.db, &self.inference_result, resolver);
                path_resolver.resolve_type_for_value_path(path)
            }
            Pattern::Bind(_) => expected_type,
        };
        self.inference_result.type_of_pattern.insert(pattern_id, ty);
        self
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
                trailing_expression,
            } => self.fold_expression_type(*trailing_expression),
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
            },
            Expression::Match { matchee, case_list } => {
                let (fold, matchee_type) = self.fold_expression_type(*matchee);

                let (fold, case_types) = case_list.iter().fold(
                    (fold, Vec::new()),
                    |(fold, mut case_types), (pattern_id, case)| {
                        let resolver =
                            Resolver::new_for_expression(fold.db, fold.function_id, expr_id);
                        let fold = fold.fold_pattern(&resolver, *pattern_id, matchee_type.clone());
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
                    Literal::Integer(_, some_kind) => match some_kind {
                        Some(int_kind) => Type::Scalar(ScalarType::Integer(int_kind.clone())),
                        None => Type::Scalar(ScalarType::Integer(IntegerKind::I64)),
                    },
                    Literal::Bool(_) => Type::Scalar(ScalarType::Boolean),
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
        };
        fold.inference_result
            .type_of_expression
            .insert(expr_id, ty.clone());
        (fold, ty)
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
