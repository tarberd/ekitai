use std::collections::HashMap;

use la_arena::{Arena, Idx};

use crate::DefinitionsDatabase;

use super::{
    intrinsic::BUILTIN_SCOPE,
    item::{FunctionDefinition, Item, TypeDefinition, ValueConstructor},
    item_tree::{ItemTree, ItemTreeNodeId},
    name::Name,
    path::Path,
    path_resolver::{TypeNamespaceItem, ValueNamespaceItem},
    type_reference::TypeReference,
};

#[salsa::query_group(InternerStorage)]
pub trait Interner {
    #[salsa::interned]
    fn intern_function(&self, loc: FunctionDefinitionLocation) -> FunctionDefinitionId;

    #[salsa::interned]
    fn intern_type(&self, loc: TypeDefinitionLocation) -> TypeDefinitionId;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionDefinitionId(salsa::InternId);

impl salsa::InternKey for FunctionDefinitionId {
    fn from_intern_id(id: salsa::InternId) -> Self {
        Self(id)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeDefinitionId(salsa::InternId);

impl salsa::InternKey for TypeDefinitionId {
    fn from_intern_id(id: salsa::InternId) -> Self {
        Self(id)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDefinitionLocation {
    item_id: ItemTreeNodeId<FunctionDefinition>,
}

impl<'a> FunctionDefinitionLocation {
    pub(crate) fn in_item_tree(&self, item_tree: &'a ItemTree) -> &'a FunctionDefinition {
        item_tree.get(self.item_id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDefinitionLocation {
    pub item_id: ItemTreeNodeId<TypeDefinition>,
}

impl<'a> TypeDefinitionLocation {
    pub(crate) fn in_item_tree(&self, item_tree: &'a ItemTree) -> &'a TypeDefinition {
        item_tree.get(self.item_id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionMap {
    root_module_scope: ItemScope,
}

impl DefinitionMap {
    pub(crate) fn new(item_tree: ItemTree, interner: &dyn Interner) -> Self {
        let mut types = HashMap::new();
        let mut values = HashMap::new();
        let mut definitions = Vec::new();

        for item in item_tree.root_items() {
            match item {
                Item::Function(id) => {
                    let function = item_tree.get(*id);
                    let function_location = FunctionDefinitionLocation { item_id: *id };
                    let function_location_id = interner.intern_function(function_location);
                    let location_id = LocationId::FunctionLocationId(function_location_id);

                    definitions.push(location_id.clone());
                    values.insert(function.name.clone(), location_id);
                }
                Item::Type(id) => {
                    let ty = item_tree.get(*id);
                    let type_location = TypeDefinitionLocation { item_id: *id };
                    let type_location_id = interner.intern_type(type_location);
                    let location_id = LocationId::TypeLocationId(type_location_id);

                    definitions.push(location_id.clone());
                    types.insert(ty.name.clone(), location_id);
                }
            }
        }

        let item_scope = ItemScope {
            types,
            values,
            definitions,
        };

        DefinitionMap {
            root_module_scope: item_scope,
        }
    }

    pub fn root_module_item_scope(&self) -> &ItemScope {
        &self.root_module_scope
    }

    pub(crate) fn resolve_path(
        &self,
        db: &dyn DefinitionsDatabase,
        path: &Path,
    ) -> NamespaceResolution {
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
                                                    type_definition_id: type_id,
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
        self.root_module_scope
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

    pub fn iter_type_locations(&self) -> impl Iterator<Item = &TypeDefinitionId> {
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

    pub fn iter_function_locations(&self) -> impl Iterator<Item = &FunctionDefinitionId> {
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
    FunctionLocationId(FunctionDefinitionId),
    TypeLocationId(TypeDefinitionId),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueConstructorId {
    pub type_definition_id: TypeDefinitionId,
    pub id: Idx<ValueConstructor>,
}

impl From<ValueConstructorId> for CallableDefinitionId {
    fn from(id: ValueConstructorId) -> Self {
        Self::ValueConstructor(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeableValueDefinitionId {
    Function(FunctionDefinitionId),
    ValueConstructor(ValueConstructorId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallableDefinitionId {
    FunctionDefinition(FunctionDefinitionId),
    ValueConstructor(ValueConstructorId),
}

impl From<FunctionDefinitionId> for CallableDefinitionId {
    fn from(id: FunctionDefinitionId) -> Self {
        Self::FunctionDefinition(id)
    }
}

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

impl From<TypeDefinition> for TypeDefinitionData {
    fn from(ty: TypeDefinition) -> Self {
        let TypeDefinition {
            name,
            value_constructors,
            ..
        } = ty;

        TypeDefinitionData {
            name,
            value_constructors,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinitionData {
    pub name: Name,
    pub parameter_types: Vec<TypeReference>,
    pub return_type: TypeReference,
}

impl From<FunctionDefinition> for FunctionDefinitionData {
    fn from(function: FunctionDefinition) -> Self {
        let FunctionDefinition {
            name,
            parameter_types,
            return_type,
            ..
        } = function;

        FunctionDefinitionData {
            name,
            parameter_types,
            return_type,
        }
    }
}
