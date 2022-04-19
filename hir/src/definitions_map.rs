use std::collections::HashMap;

use la_arena::Idx;
use syntax::ast;

use crate::{
    item_tree::{FunctionDefinition, ItemId, TypeDefinition},
    DefinitionsDatabase, Path, BUILTIN_SCOPE, Name, PatternId, TypeReference,
};

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
    pub id: ItemId<FunctionDefinition>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeLocation {
    pub id: ItemId<TypeDefinition>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionsMap {
    pub item_scope: ItemScope,
}

impl DefinitionsMap {
    pub(crate) fn resolve_path(&self, db: &dyn DefinitionsDatabase, path: &Path) -> NamespaceResolution {
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

    pub(crate) fn resolve_name(&self, name: &Name) -> NamespaceResolution {
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

    pub fn iter_type_locations(&self) -> impl Iterator<Item = &TypeLocationId> {
        let mut iter = self.definitions.iter();
        std::iter::from_fn(move || {
            while let Some(def) = iter.next() {
                match def {
                    LocationId::TypeLocationId(id) => return Some(id),
                    _ => continue,
                }
            }
            None
        })
    }

    pub fn iter_function_locations(&self) -> impl Iterator<Item = &FunctionLocationId> {
        let mut iter = self.definitions.iter();
        std::iter::from_fn(move || {
            while let Some(def) = iter.next() {
                match def {
                    LocationId::FunctionLocationId(id) => return Some(id),
                    _ => continue,
                }
            }
            None
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LocationId {
    FunctionLocationId(FunctionLocationId),
    TypeLocationId(TypeLocationId),
}

#[derive(Default)]
pub struct NamespaceResolution {
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

    pub(crate) fn in_type_namespace(self) -> Option<TypeNamespaceItem> {
        self.type_resolution
    }

    pub(crate) fn in_value_namespace(self) -> Option<ValueNamespaceItem> {
        self.value_resolution
    }

    fn or(self, resolution: Self) -> Self {
        Self {
            type_resolution: self.type_resolution.or(resolution.type_resolution),
            value_resolution: self.value_resolution.or(resolution.value_resolution),
        }
    }
}

pub enum TypeNamespaceItem {
    TypeDefinition(TypeLocationId),
    Builtin(BuiltinType),
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

#[derive(Debug)]
pub enum ValueNamespaceItem {
    Function(FunctionLocationId),
    ValueConstructor(ValueConstructorId),
    /// local binding in expression body
    LocalBinding(PatternId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    pub(crate) fn lower(ctor: ast::ValueConstructor) -> Self {
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
