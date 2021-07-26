use la_arena::{Arena, Idx};
use smol_str::SmolStr;
use std::{collections::HashMap, marker::PhantomData};
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
    Int(IntKind),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntKind {
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
        Type::Scalar(ScalarType::Int(IntKind::I32)),
    ),
    (
        Name::new_inline("i64"),
        Type::Scalar(ScalarType::Int(IntKind::I64)),
    ),
];
