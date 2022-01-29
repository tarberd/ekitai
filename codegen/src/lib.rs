use by_address::ByAddress;
use hir::{
    BinaryOperator, Body, CallableDefinitionId, ExpressionId, FunctionLocationId, HirDatabase,
    InferenceResult, Literal, Name, PatternId, Resolver, ScalarType, SourceDatabase, Statement,
    Type, TypeLocationId, UnaryOperator, Upcast, ValueConstructorId,
};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{
        CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine, TargetTriple,
    },
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};
use std::{
    borrow::BorrowMut,
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
    iter::FromIterator,
    sync::Arc,
};

mod amd64_c_abi;

#[salsa::query_group(CodeGenDatabaseStorage)]
pub trait CodeGenDatabase: HirDatabase + Upcast<dyn HirDatabase> {
    #[salsa::input]
    fn target(&self) -> CodeGenTarget;

    fn target_machine(&self) -> ByAddress<Arc<TargetMachine>>;

    fn build_assembly_ir(&self) -> ();
}

fn target_machine(db: &dyn CodeGenDatabase) -> ByAddress<Arc<TargetMachine>> {
    let target = db.target();

    match target.triple.arch {
        CodeGenTargetArch::X86_64 => {
            Target::initialize_x86(&InitializationConfig::default());
        }
    };

    let target_triple = TargetTriple::create(&target.triple.to_string());
    let target = Target::from_triple(&target_triple).unwrap_or_else(|err| {
        panic!(
            "Could not create LLVM target from target triple {}: {err:?}",
            target.triple
        )
    });
    let opt = OptimizationLevel::None;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let cpu = "";
    let features = "";
    let target_machine = target
        .create_target_machine(&target_triple, cpu, features, opt, reloc, model)
        .unwrap();

    ByAddress(Arc::new(target_machine))
}

#[salsa::database(
    hir::SourceDatabaseStorage,
    hir::InternDatabaseStorage,
    hir::DefinitionsDatabaseStorage,
    hir::HirDatabaseStorage,
    CodeGenDatabaseStorage
)]
#[derive(Default)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl Upcast<dyn hir::SourceDatabase> for Database {
    fn upcast(&self) -> &(dyn hir::SourceDatabase + 'static) {
        &*self
    }
}

impl Upcast<dyn hir::DefinitionsDatabase> for Database {
    fn upcast(&self) -> &(dyn hir::DefinitionsDatabase + 'static) {
        &*self
    }
}

impl Upcast<dyn hir::HirDatabase> for Database {
    fn upcast(&self) -> &(dyn hir::HirDatabase + 'static) {
        &*self
    }
}

impl salsa::Database for Database {}

#[derive(Clone)]
struct LocalBindingStack<'ink> {
    stack: Vec<Scope<'ink>>,
}

impl<'ink> LocalBindingStack<'ink> {
    pub fn get(&self, name: &Name) -> Option<&BindingValue<'ink>> {
        self.stack.iter().rev().find_map(|scope| scope.get(name))
    }

    pub fn push(&mut self, scope: Scope<'ink>) {
        self.stack.push(scope);
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }
}

impl<'ink> FromIterator<Scope<'ink>> for LocalBindingStack<'ink> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Scope<'ink>>,
    {
        let stack = iter.into_iter().collect();
        Self { stack }
    }
}

#[derive(Clone)]
struct Scope<'ink> {
    scope: Vec<Binding<'ink>>,
}

impl<'ink> Scope<'ink> {
    pub fn get(&self, name: &Name) -> Option<&BindingValue<'ink>> {
        self.scope
            .iter()
            .find_map(|binding| match &binding.name == name {
                true => Some(&binding.value),
                false => None,
            })
    }
}

impl<'ink> FromIterator<Binding<'ink>> for Scope<'ink> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Binding<'ink>>,
    {
        let scope = iter.into_iter().collect();
        Self { scope }
    }
}

#[derive(Clone)]
struct Binding<'ink> {
    pub name: Name,
    pub value: BindingValue<'ink>,
}

#[derive(Clone)]
struct BindingValue<'ink> {
    pub kind: ValueKind,
    pub value: BasicValueEnum<'ink>,
}

#[derive(Debug, Clone, Copy)]
pub enum CodeGenTargetArch {
    X86_64,
}

impl Display for CodeGenTargetArch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Self::X86_64 => "x86_64",
        };
        f.write_str(string)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CodeGenTargetVendor {
    PC,
}

impl Display for CodeGenTargetVendor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Self::PC => "pc",
        };
        f.write_str(string)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CodeGenTargetSystem {
    Linux,
}

impl Display for CodeGenTargetSystem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Self::Linux => "linux",
        };
        f.write_str(string)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CodeGenTargetABI {
    GNU,
}

impl Display for CodeGenTargetABI {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Self::GNU => "gnu",
        };
        f.write_str(string)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CodeGenTargetTriple {
    pub arch: CodeGenTargetArch,
    pub vendor: CodeGenTargetVendor,
    pub system: CodeGenTargetSystem,
    pub abi: CodeGenTargetABI,
}

impl Display for CodeGenTargetTriple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}-{}-{}-{}",
            self.arch, self.vendor, self.system, self.abi
        ))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CodeGenTarget {
    pub triple: CodeGenTargetTriple,
}

pub struct CodeGenTypeCache<'db, 'context> {
    db: &'db dyn CodeGenDatabase,
    context: &'context Context,
    target_data: TargetData,
    adt_map: RefCell<HashMap<TypeLocationId, (StructType<'context>, TypeInfo<'context>)>>,
    adt_variant_map: RefCell<HashMap<ValueConstructorId, StructType<'context>>>,
}

impl<'db, 'context> CodeGenTypeCache<'db, 'context> {
    pub(crate) fn new(db: &'db dyn CodeGenDatabase, context: &'context Context) -> Self {
        Self {
            db,
            context,
            target_data: db.target_machine().get_target_data(),
            adt_map: RefCell::new(HashMap::new()),
            adt_variant_map: RefCell::new(HashMap::new()),
        }
    }

    fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'context> {
        match ty {
            Type::AbstractDataType(ty_loc_id) => self.adt_struct_type(*ty_loc_id).into(),
            Type::FunctionDefinition(_) => todo!(),
            Type::Scalar(scalar) => self.scalar_type(scalar),
        }
    }

    fn type_info(&self, ty: &Type) -> TypeInfo<'context> {
        match ty {
            Type::AbstractDataType(ty_loc_id) => self.adt_struct_info(*ty_loc_id),
            Type::FunctionDefinition(_) => todo!(),
            Type::Scalar(_) => todo!(),
        }
    }

    fn bit_size(&self, ty: &Type) -> usize {
        match ty {
            Type::AbstractDataType(type_id) => {
                let struct_ir = self.adt_struct_type(*type_id);
                self.target_data.get_bit_size(&struct_ir.as_any_type_enum()) as usize
            }
            Type::FunctionDefinition(_) => todo!(),
            Type::Scalar(scalar) => {
                let scalar = self.scalar_type(scalar);
                self.target_data.get_bit_size(&scalar.as_any_type_enum()) as usize
            }
        }
    }

    fn adt_struct_info(&self, ty_loc_id: TypeLocationId) -> TypeInfo<'context> {
        self.adt_struct(ty_loc_id).1
    }

    fn adt_struct_type(&self, ty_loc_id: TypeLocationId) -> StructType<'context> {
        self.adt_struct(ty_loc_id).0
    }

    fn adt_struct(&self, ty_loc_id: TypeLocationId) -> (StructType<'context>, TypeInfo<'context>) {
        self.adt_map
            .borrow_mut()
            .entry(ty_loc_id)
            .or_insert_with(|| {
                let ty_data = self.db.type_definition_data(ty_loc_id);
                let opaque = self.context.opaque_struct_type(ty_data.name.id.as_str());

                let tag_bit_len = {
                    let variant_count = ty_data.value_constructors.len();
                    let mut bit_len = 0;
                    while 1 << bit_len < variant_count {
                        bit_len += 1;
                    }
                    bit_len
                };

                let tag_type = match tag_bit_len {
                    0 => None,
                    1 => Some(self.context.bool_type()),
                    2..=8 => Some(self.context.i8_type()),
                    9..=16 => Some(self.context.i16_type()),
                    17..=32 => Some(self.context.i32_type()),
                    33..=64 => Some(self.context.i64_type()),
                    65..=128 => Some(self.context.i128_type()),
                    _ => panic!("Tag for ADT too big! {tag_bit_len:?}"),
                };

                let value_constructor_ids = ty_data
                    .value_constructors
                    .iter()
                    .map(|(id, _)| ValueConstructorId {
                        parrent_id: ty_loc_id,
                        id,
                    })
                    .collect::<Vec<_>>();

                let ty_constructors = value_constructor_ids
                    .iter()
                    .cloned()
                    .map(|id| self.value_constructor_struct(id));

                let bigest = ty_constructors
                    .max_by_key(|struct_type| {
                        self.target_data
                            .get_abi_size(&struct_type.as_any_type_enum())
                    })
                    .expect("Failed to get memsize of type variant");

                let field_types = tag_type
                    .into_iter()
                    .map(Into::into)
                    .chain(std::iter::once(bigest.into()))
                    .collect::<Vec<_>>();

                opaque.set_body(field_types.as_slice(), false);
                let tag_map = value_constructor_ids
                    .into_iter()
                    .map(|id| (id, u32::from(id.id.into_raw()) as usize))
                    .collect();
                let adt_info = TypeInfo {
                    tag: tag_type,
                    tag_map,
                };
                (opaque, adt_info)
            })
            .clone()
    }

    fn value_constructor_struct(&self, constructor_id: ValueConstructorId) -> StructType<'context> {
        if let Some(struct_type) = self.adt_variant_map.borrow().get(&constructor_id) {
            return *struct_type;
        }

        let constructor = self.db.callable_definition_signature(constructor_id.into());
        let struct_data = constructor
            .parameter_types
            .iter()
            .map(|ty| match ty {
                Type::AbstractDataType(id) => self.adt_struct_type(*id).into(),
                Type::FunctionDefinition(_) => todo!(),
                Type::Scalar(scalar) => self.scalar_type(scalar),
            })
            .collect::<Vec<_>>();

        let ty_data = self.db.type_definition_data(constructor_id.parrent_id);
        let variant_data = ty_data.value_constructor(constructor_id.id);

        let opaque = self
            .context
            .opaque_struct_type(format!("{}::{}", ty_data.name.id, variant_data.name.id).as_str());

        opaque.set_body(&struct_data, false);

        self.adt_variant_map
            .borrow_mut()
            .insert(constructor_id, opaque);
        opaque
    }

    fn scalar_type(&self, scalar: &ScalarType) -> BasicTypeEnum<'context> {
        match scalar {
            ScalarType::Integer(int_kind) => match int_kind {
                hir::IntegerKind::I32 => self.context.i32_type().into(),
                hir::IntegerKind::I64 => self.context.i64_type().into(),
            },
            ScalarType::Boolean => self.context.bool_type().into(),
        }
    }
}

#[derive(Clone)]
struct TypeInfo<'context> {
    pub tag: Option<IntType<'context>>,
    pub tag_map: HashMap<ValueConstructorId, usize>,
}

// impl<'context> TypeInfo<'context> {
//     fn adt_tag_value(
//         &self,
//         builder: &Builder<'context>,
//         matchee_value: BasicValueEnum<'context>,
//     ) -> Option<BasicValueEnum<'context>> {
//         match self.tag_index {
//             Some((offset, tag_type)) => match matchee_value {
//                 BasicValueEnum::ArrayValue(_) => todo!(),
//                 BasicValueEnum::IntValue(_) => todo!(),
//                 BasicValueEnum::FloatValue(_) => todo!(),
//                 BasicValueEnum::PointerValue(pointer) => unsafe {
//                     Some(
//                         builder
//                             .build_in_bounds_gep(pointer, &[], "")
//                             .as_basic_value_enum(),
//                     )
//                 },
//                 BasicValueEnum::StructValue(struct_value) => todo!(),
//                 BasicValueEnum::VectorValue(_) => todo!(),
//             },
//             None => todo!(),
//         }
//     }
// }

struct CodeGenFunctionInfoCache<'db, 'context, 'type_cache> {
    db: &'db dyn CodeGenDatabase,
    context: &'context Context,
    type_cache: &'type_cache CodeGenTypeCache<'db, 'context>,
}

impl<'db, 'context, 'type_cache> CodeGenFunctionInfoCache<'db, 'context, 'type_cache> {
    pub(crate) fn new(
        db: &'db dyn CodeGenDatabase,
        context: &'context Context,
        type_cache: &'type_cache CodeGenTypeCache<'db, 'context>,
    ) -> Self {
        Self {
            db,
            context,
            type_cache,
        }
    }

    pub(crate) fn llvm_function_type(
        &self,
        function_id: &FunctionLocationId,
    ) -> FunctionType<'context> {
        let function_info = self.function_info(function_id);
        let function_signature = self.db.callable_definition_signature((*function_id).into());
        let parameter_types = function_signature
            .parameter_types
            .iter()
            .zip(function_info.parameter_kinds)
            .map(|(param_type, param_kind)| {
                let llvm_param_type = self.type_cache.llvm_type(param_type);
                match param_kind {
                    ValueKind::Indirect => llvm_param_type
                        .ptr_type(AddressSpace::Generic)
                        .as_basic_type_enum(),
                    ValueKind::Direct => llvm_param_type,
                }
            });
        let return_type = {
            let return_type = function_signature.return_type;
            self.type_cache.llvm_type(&return_type)
        };
        match function_info.return_kind {
            ValueKind::Indirect => {
                let return_type = return_type
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum();
                let parameter_types = std::iter::once(return_type).chain(parameter_types);
                self.context.void_type().fn_type(
                    parameter_types
                        .map(Into::into)
                        .collect::<Vec<_>>()
                        .as_slice(),
                    false,
                )
            }
            ValueKind::Direct => return_type.fn_type(
                &parameter_types
                    .into_iter()
                    .map(Into::into)
                    .collect::<Vec<_>>(),
                false,
            ),
        }
    }

    pub(crate) fn function_info(&self, function_id: &FunctionLocationId) -> FunctionInfo {
        let free_integer_registers = 6;
        let function_signature = self.db.callable_definition_signature((*function_id).into());
        let return_bitsize = self.type_cache.bit_size(&function_signature.return_type);
        let (free_integer_registers, return_kind) = match return_bitsize {
            0..=128 => (free_integer_registers, ValueKind::Direct),
            _ => (free_integer_registers - 1, ValueKind::Indirect),
        };

        let (_, parameter_kinds) = function_signature.parameter_types.iter().fold(
            (free_integer_registers, Vec::new()),
            |(free_integer_registers, mut param_kinds), param_type| match free_integer_registers {
                0 => {
                    let bitsize = self.type_cache.bit_size(param_type);
                    if bitsize <= 64 {
                        param_kinds.push(ValueKind::Direct)
                    } else {
                        param_kinds.push(ValueKind::Indirect);
                    }
                    (free_integer_registers, param_kinds)
                }
                _ => {
                    let param_size = self.type_cache.bit_size(param_type);
                    let (free_integer_registers, param_kind) = match param_size {
                        1..=64 => (free_integer_registers - 1, ValueKind::Direct),
                        65..=128 => (free_integer_registers - 2, ValueKind::Direct),
                        129.. => (free_integer_registers, ValueKind::Indirect),
                        x => panic!("parameter size {x} not supported"),
                    };
                    param_kinds.push(param_kind);
                    (free_integer_registers, param_kinds)
                }
            },
        );
        FunctionInfo {
            return_kind,
            parameter_kinds,
        }
    }
}

struct FunctionInfo {
    pub return_kind: ValueKind,
    pub parameter_kinds: Vec<ValueKind>,
}

impl FunctionInfo {
    pub(crate) fn skip_return_param<T>(
        &self,
        iter: impl Iterator<Item = T>,
    ) -> impl Iterator<Item = T> {
        iter.skip(match self.return_kind {
            ValueKind::Indirect => 1,
            ValueKind::Direct => 0,
        })
    }
}

struct CodeGenFunctionValueCache<'db, 'context, 'module, 'type_cache, 'function_info_cache> {
    db: &'db dyn CodeGenDatabase,
    context: &'context Context,
    module: &'module Module<'context>,
    type_cache: &'type_cache CodeGenTypeCache<'db, 'context>,
    function_info_cache: &'function_info_cache CodeGenFunctionInfoCache<'db, 'context, 'type_cache>,
    function_value_map: RefCell<HashMap<FunctionLocationId, FunctionValue<'context>>>,
}

impl<'db, 'context, 'module, 'type_cache, 'function_info_cache>
    CodeGenFunctionValueCache<'db, 'context, 'module, 'type_cache, 'function_info_cache>
{
    pub(crate) fn new(
        db: &'db dyn CodeGenDatabase,
        context: &'context Context,
        module: &'module Module<'context>,
        type_cache: &'type_cache CodeGenTypeCache<'db, 'context>,
        function_info_cache: &'function_info_cache CodeGenFunctionInfoCache<
            'db,
            'context,
            'type_cache,
        >,
    ) -> Self {
        Self {
            db,
            context,
            module,
            type_cache,
            function_info_cache,
            function_value_map: RefCell::new(HashMap::new()),
        }
    }

    fn llvm_function_value(&self, function_id: &FunctionLocationId) -> FunctionValue<'context> {
        if let Some(function_value) = self.function_value_map.borrow().get(function_id) {
            return *function_value;
        }

        let name = {
            let f_data = self.db.function_definition_data(*function_id);
            f_data.name.id
        };
        let llvm_function_type = self.function_info_cache.llvm_function_type(function_id);
        let function_value = self
            .module
            .add_function(name.as_str(), llvm_function_type, None);

        let function_info = self.function_info_cache.function_info(function_id);

        match function_info.return_kind {
            ValueKind::Indirect => {
                let ret_ty = llvm_function_type
                    .get_param_types()
                    .first()
                    .unwrap()
                    .into_pointer_type()
                    .get_element_type();
                let kind_id = Attribute::get_named_enum_kind_id("sret");
                let sret_attribute = self
                    .context
                    .create_type_attribute(kind_id, ret_ty.as_any_type_enum());
                let kind_id = Attribute::get_named_enum_kind_id("noalias");
                let noalias_attribute = self.context.create_enum_attribute(kind_id, 0);
                function_value.add_attribute(AttributeLoc::Param(0), sret_attribute);
                function_value.add_attribute(AttributeLoc::Param(0), noalias_attribute);
            }
            _ => (),
        };

        for ((param_index, param_type), param_kind) in function_info
            .skip_return_param(llvm_function_type.get_param_types().iter().enumerate())
            .zip(function_info.parameter_kinds)
        {
            if let ValueKind::Indirect = param_kind {
                let kind_id = Attribute::get_named_enum_kind_id("byval");
                let byval_attribute = self.context.create_type_attribute(
                    kind_id,
                    param_type
                        .into_pointer_type()
                        .get_element_type()
                        .as_any_type_enum(),
                );
                function_value.add_attribute(
                    AttributeLoc::Param(param_index.try_into().unwrap()),
                    byval_attribute,
                )
            }
        }
        self.function_value_map
            .borrow_mut()
            .insert(*function_id, function_value);
        function_value
    }
}

struct CodeGenFunctionBodyLoweringContext<
    'db,
    'context,
    'module,
    'type_cache,
    'function_info_cache,
    'function_value_cache,
> {
    db: &'db dyn CodeGenDatabase,
    context: &'context Context,
    module: &'module Module<'context>,
    type_cache: &'type_cache CodeGenTypeCache<'db, 'context>,
    function_info_cache: &'function_info_cache CodeGenFunctionInfoCache<'db, 'context, 'type_cache>,
    function_value_cache: &'function_value_cache CodeGenFunctionValueCache<
        'db,
        'context,
        'module,
        'type_cache,
        'function_info_cache,
    >,
}

impl<'db, 'context, 'module, 'type_cache, 'function_info_cache, 'function_value_cache>
    CodeGenFunctionBodyLoweringContext<
        'db,
        'context,
        'module,
        'type_cache,
        'function_info_cache,
        'function_value_cache,
    >
{
    pub(crate) fn new(
        db: &'db dyn CodeGenDatabase,
        context: &'context Context,
        module: &'module Module<'context>,
        type_cache: &'type_cache CodeGenTypeCache<'db, 'context>,
        function_info_cache: &'function_info_cache CodeGenFunctionInfoCache<
            'db,
            'context,
            'type_cache,
        >,
        function_value_cache: &'function_value_cache CodeGenFunctionValueCache<
            'db,
            'context,
            'module,
            'type_cache,
            'function_info_cache,
        >,
    ) -> Self {
        Self {
            db,
            context,
            module,
            type_cache,
            function_info_cache,
            function_value_cache,
        }
    }

    pub(crate) fn build_function_body(&self, function_id: &FunctionLocationId) {
        let llvm_function_value = self.function_value_cache.llvm_function_value(function_id);
        let llvm_body = self.context.append_basic_block(llvm_function_value, "");
        let alloca_builder = self.context.create_builder();
        alloca_builder.position_at_end(llvm_body);
        let body = self.db.body_of_definition(*function_id);

        let function_signature = self.db.callable_definition_signature((*function_id).into());
        let function_info = self.function_info_cache.function_info(function_id);
        let parameter_scope = body
            .parameters
            .iter()
            .zip(
                function_info
                    .skip_return_param(llvm_function_value.get_param_iter())
                    .zip(function_info.parameter_kinds)
                    .zip(function_signature.parameter_types),
            )
            .map(|(pattern_id, ((parameter, parameter_kind), param_ty))| {
                let pattern = &body.patterns[*pattern_id];
                match pattern {
                    hir::Pattern::Deconstructor(_, _) => todo!("Unsing unsuported path pattern"),
                    hir::Pattern::Bind(name) => {
                        parameter.set_name(&name.id);
                        let binding_value = match parameter_kind {
                            ValueKind::Indirect => BindingValue {
                                kind: parameter_kind,
                                value: parameter,
                            },
                            ValueKind::Direct => {
                                if self.type_cache.bit_size(&param_ty) <= 64 {
                                    BindingValue {
                                        kind: ValueKind::Direct,
                                        value: parameter,
                                    }
                                } else {
                                    let param_ptr =
                                        alloca_builder.build_alloca(parameter.get_type(), &name.id);
                                    alloca_builder.build_store(param_ptr, parameter);
                                    BindingValue {
                                        kind: ValueKind::Indirect,
                                        value: param_ptr.into(),
                                    }
                                }
                            }
                        };
                        Binding {
                            name: name.clone(),
                            value: binding_value,
                        }
                    }
                }
            })
            .collect::<Scope>();

        let binding_stack = std::iter::once(parameter_scope).collect();

        let return_value_ptr = match function_info.return_kind {
            ValueKind::Indirect => Some(
                llvm_function_value
                    .get_first_param()
                    .unwrap()
                    .into_pointer_value(),
            ),
            ValueKind::Direct => None,
        };

        let builder = self.context.create_builder();
        builder.position_at_end(llvm_body);

        let return_value = ExpressionLowerer::new(
            self.db,
            &self.context,
            &builder,
            alloca_builder,
            &self.type_cache,
            &self.function_info_cache,
            &self.function_value_cache,
            *function_id,
            binding_stack,
        )
        .fold_expression(return_value_ptr, body.root_expression);

        let return_value = match function_info.return_kind {
            ValueKind::Indirect => None,
            ValueKind::Direct => Some(return_value),
        };

        builder.build_return(return_value.as_ref().map(|val| val as &dyn BasicValue));
    }
}

struct CodeGenFunctionArgumentInfo {
    kind: ArgumentKind,
}

enum ArgumentKind {
    Direct,
    Indirect,
}

#[derive(Clone, Copy)]
enum ReturnKind {
    ArgumentPointer,
    RegisterValue,
}

#[derive(Debug, Clone, Copy)]
enum ValueKind {
    Indirect,
    Direct,
}

struct FunctionIr<'ink> {
    pub value: FunctionValue<'ink>,
    pub return_kind: ReturnKind,
    pub parameters_kind: Vec<ValueKind>,
}

impl FunctionIr<'_> {
    fn parameters(&self) -> impl Iterator<Item = (BasicValueEnum, &ValueKind)> {
        match self.return_kind {
            ReturnKind::ArgumentPointer => self
                .value
                .get_param_iter()
                .zip(self.parameters_kind.iter())
                .skip(1),
            ReturnKind::RegisterValue => self
                .value
                .get_param_iter()
                .zip(self.parameters_kind.iter())
                .skip(0),
        }
    }
}

// enum ArgumentMappingKind {
//     Direct,
//     Indirect,
// }

// struct FunctionHirToLLVM {
//     arguments_hir_to_llvm: Vec<ArgumentMappingKind>,
//     return_kind: ArgumentMappingKind,
// }

// struct X86_64CAbi<'db> {
//     db: &'db dyn CodeGenDatabase,
// }

// #[derive(PartialEq, Eq)]
// enum Class {
//     NoClass,
//     Memory,
//     Integer,
// }

// impl Default for Class {
//     fn default() -> Self {
//         Self::NoClass
//     }
// }

// #[derive(Default)]
// struct Classification {
//     low: Class,
//     high: Class,
// }

// impl Classification {
//     fn is_memory(&self) -> bool {
//         self.low == Class::Memory
//     }

//     fn add_field(&mut self, offset: usize, class: Class) {
//         if self.is_memory() {
//             return;
//         }

//         let is_offset_low = offset < 8;
//         let class_at_offset = if is_offset_low { self.low } else { self.high };
//         let merged_class = Self::merge_classes(class_at_offset, class);

//         if is_offset_low {
//             if self.low != merged_class {
//                 self.low = merged_class;
//             }
//         } else {
//             if self.high != merged_class {
//                 self.high = merged_class;

//                 if merged_class == Class::Memory {
//                     self.low = Class::Memory;
//                 }
//             }
//         }
//     }

//     fn merge_classes(first: Class, second: Class) -> Class {
//         match (first, second) {
//             (f, s) if f == s => f,
//             (f, Class::NoClass) => f,
//             (Class::NoClass, s) => s,
//             (Class::Memory, _) | (_, Class::Memory) => Class::Memory,
//             (Class::Integer, _) | (_, Class::Integer) => Class::Integer,
//         }
//     }

//     fn classify_type(
//         &mut self,
//         type_: BasicTypeEnum,
//         classification: Classification,
//         offset: usize,
//     ) {
//         match type_ {
//             BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_) => {
//                 self.add_field(offset, Class::Integer);
//                 return;
//             }
//             BasicTypeEnum::StructType(struct_type) => {
//                 let members = struct_type.get_field_types();

//                 let mut struct_offset = 0;
//                 for member in &members {
//                     if
//                 }
//             }
//             _ => panic!()
//         }
//     }
// }

// impl<'db, 'context> X86_64CAbi<'db> {
//     fn create_call(
//         &self,
//         builder: Builder,
//         arguments: &[BasicValueEnum],
//         create_call: &dyn Fn(&[BasicValueEnum]) -> BasicValueEnum<'context>,
//     ) -> BasicValueEnum<'context> {
//         create_call(arguments)
//     }

//     fn classify(&self, type_: BasicTypeEnum) -> Classification {
//         let target_data = self.db.target_machine().get_target_data();

//         let mut classification = Classification::default();
//         if target_data.get_abi_alignment(&type_.as_any_type_enum()) > 4 * 32 {
//             classification.add_field(0, Class::Memory);
//             return classification;
//         }

//         classification
//     }

//     fn create_function_type(
//         &self,
//         type_cache: &CodeGenTypeCache,
//         function: FunctionSignature,
//     ) -> FunctionType {
//         let target_data = self.db.target_machine().get_target_data();
//         let return_type = type_cache.llvm_type(&function.return_type);

//         let free_integer_registers = 6;
//         let (llvm_return_type, return_kind, free_integer_registers) = {
//             let return_type_abi_size = target_data.get_abi_size(&return_type.as_any_type_enum());
//             if return_type_abi_size > 2 * 8 {
//                 (
//                     return_type
//                         .ptr_type(AddressSpace::Generic)
//                         .as_basic_type_enum(),
//                     ArgumentMappingKind::Indirect,
//                     free_integer_registers - 1,
//                 )
//             } else {
//                 (
//                     return_type,
//                     ArgumentMappingKind::Direct,
//                     free_integer_registers
//                         - if return_type_abi_size % 8 == 0 {
//                             return_type_abi_size / 8
//                         } else {
//                             return_type_abi_size / 8 + 1
//                         },
//                 )
//             }
//         };

//         let mut free_integer_registers = free_integer_registers;
//         let param_tys = function.parameter_types.iter().map(|ty| {
//             let arg_type = type_cache.llvm_type(ty);
//             let mut required_abi_size = target_data.get_abi_size(&arg_type.as_any_type_enum());
//             if free_integer_registers >= required_abi_size {
//             } else {
//             }
//         });

//         todo!()
//     }
// }

pub fn compile_text(source: String, target: CodeGenTarget) {
    let mut db = Database::default();
    db.set_source_file_text(source);
    db.set_target(target);
    db.build_assembly_ir()
}

pub fn build_assembly_ir(db: &dyn CodeGenDatabase) {
    let context = Context::create();

    let target_machine = db.target_machine();
    let target_data = target_machine.get_target_data();

    let llvm_module = context.create_module("ekitai_module");
    llvm_module.set_data_layout(&target_data.get_data_layout());
    llvm_module.set_triple(&target_machine.get_triple());

    let type_cache = CodeGenTypeCache::new(db, &context);
    let function_info_cache = CodeGenFunctionInfoCache::new(db, &context, &type_cache);
    let function_value_cache = CodeGenFunctionValueCache::new(
        db,
        &context,
        &llvm_module,
        &type_cache,
        &function_info_cache,
    );

    let function_body_builder = CodeGenFunctionBodyLoweringContext::new(
        db,
        &context,
        &llvm_module,
        &type_cache,
        &function_info_cache,
        &function_value_cache,
    );

    let def_map = db.source_file_definitions_map();
    for function_id in def_map.item_scope.iter_function_locations() {
        function_body_builder.build_function_body(function_id)
    }

    println!("{}", llvm_module.print_to_string().to_string());
    if let Err(err) = llvm_module.verify() {
        let err = err.to_string();
        panic!("Could not verify llvm: {err}");
    };
    let _ = llvm_module.print_to_file("out.ll");
}

struct ExpressionLowerer<
    'db,
    'context,
    'module,
    'builder,
    'type_cache,
    'function_info_cache,
    'function_value_cache,
> {
    db: &'db dyn CodeGenDatabase,
    context: &'context Context,
    builder: &'builder Builder<'context>,
    alloca_builder: Builder<'context>,
    type_cache: &'type_cache CodeGenTypeCache<'db, 'context>,
    function_info_cache: &'function_info_cache CodeGenFunctionInfoCache<'db, 'context, 'type_cache>,
    function_value_cache: &'function_value_cache CodeGenFunctionValueCache<
        'db,
        'context,
        'module,
        'type_cache,
        'function_info_cache,
    >,
    function_id: FunctionLocationId,
    body: Body,
    binding_stack: RefCell<LocalBindingStack<'context>>,
    inference: InferenceResult,
}

impl<
        'db,
        'context,
        'module,
        'builder,
        'type_cache,
        'function_info_cache,
        'function_value_cache,
    >
    ExpressionLowerer<
        'db,
        'context,
        'module,
        'builder,
        'type_cache,
        'function_info_cache,
        'function_value_cache,
    >
{
    pub fn new(
        db: &'db dyn CodeGenDatabase,
        context: &'context Context,
        builder: &'builder Builder<'context>,
        alloca_builder: Builder<'context>,
        type_cache: &'type_cache CodeGenTypeCache<'db, 'context>,
        function_info_cache: &'function_info_cache CodeGenFunctionInfoCache<
            'db,
            'context,
            'type_cache,
        >,
        function_value_cache: &'function_value_cache CodeGenFunctionValueCache<
            'db,
            'context,
            'module,
            'type_cache,
            'function_info_cache,
        >,
        function_id: FunctionLocationId,
        binding_stack: LocalBindingStack<'context>,
    ) -> Self {
        Self {
            db,
            context,
            builder,
            alloca_builder,
            type_cache,
            function_info_cache,
            function_value_cache,
            function_id,
            body: db.body_of_definition(function_id),
            binding_stack: RefCell::new(binding_stack),
            inference: db.infer_body_expression_types(function_id),
        }
    }

    pub fn get_owener_function_value(&self) -> FunctionValue {
        self.function_value_cache
            .llvm_function_value(&self.function_id)
    }

    pub fn fold_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        expr_id: ExpressionId,
    ) -> Option<BasicValueEnum<'context>> {
        let expr = &self.body.expressions[expr_id];
        match expr {
            hir::Expression::Block {
                statements,
                trailing_expression,
            } => {
                for statement in statements {
                    match statement {
                        hir::Statement::Let(pattern_id, expr_id) => {
                            let pattern = &self.body.patterns[*pattern_id];
                            let expr_ty = &self.inference.type_of_expression[*expr_id];
                            let bitsize = self.type_cache.bit_size(expr_ty);
                            let value = if bitsize <= 64 {
                                let value = self.fold_expression(None, *expr_id).unwrap();
                                BindingValue {
                                    kind: ValueKind::Direct,
                                    value,
                                }
                            } else {
                                let ty = self.type_cache.llvm_type(expr_ty);
                                let ptr = self.alloca_builder.build_alloca(ty, "");
                                self.fold_expression(Some(ptr), *expr_id);
                                BindingValue {
                                    kind: ValueKind::Indirect,
                                    value: ptr.as_basic_value_enum(),
                                }
                            };

                            let binding = match pattern {
                                hir::Pattern::Deconstructor(_, _) => todo!(),
                                hir::Pattern::Bind(name) => Binding {
                                    name: name.clone(),
                                    value,
                                },
                            };
                            self.binding_stack
                                .borrow_mut()
                                .push(std::iter::once(binding).collect());
                        }
                        hir::Statement::Expression(expr_id) => {
                            self.fold_expression(None, *expr_id);
                        }
                    }
                }
                let value = self.fold_expression(indirect_value, *trailing_expression);
                for statement in statements {
                    if let Statement::Let(_, _) = statement {
                        self.binding_stack.borrow_mut().pop();
                    };
                }
                value
            }
            hir::Expression::If {
                condition,
                then_branch,
                else_branch,
            } => self.fold_if_expression(indirect_value, condition, then_branch, else_branch),
            hir::Expression::Match { matchee, case_list } => {
                let resolver =
                    Resolver::new_for_expression(self.db.upcast(), self.function_id, expr_id);
                self.fold_match_expression(indirect_value, *matchee, case_list, resolver)
            }
            hir::Expression::Call { callee, arguments } => {
                self.fold_call_expression(indirect_value, callee, arguments)
            }
            hir::Expression::Binary(operator, lhs, rhs) => {
                self.fold_binary_expression(indirect_value, operator, lhs, rhs)
            }
            hir::Expression::Unary(op, expr) => {
                self.fold_unary_expression(indirect_value, op, expr)
            }
            hir::Expression::Path(path) => {
                let resolver =
                    Resolver::new_for_expression(self.db.upcast(), self.function_id, expr_id);
                self.fold_path_expression(indirect_value, path, resolver)
            }
            hir::Expression::Literal(literal) => {
                self.fold_literal_expression(indirect_value, expr_id, literal)
            }
        }
    }

    fn fold_if_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        condition: &ExpressionId,
        then_branch: &ExpressionId,
        else_branch: &ExpressionId,
    ) -> Option<BasicValueEnum<'context>> {
        let comparison = self
            .fold_expression(None, *condition)
            .unwrap()
            .into_int_value();

        let then_block = self
            .context
            .append_basic_block(self.get_owener_function_value(), "then");
        let else_block = self
            .context
            .append_basic_block(self.get_owener_function_value(), "else");
        let merge_block = self
            .context
            .append_basic_block(self.get_owener_function_value(), "merge");

        self.builder
            .build_conditional_branch(comparison, then_block, else_block);

        self.builder.position_at_end(then_block);
        let then_value = self.fold_expression(indirect_value, *then_branch);
        self.builder.build_unconditional_branch(merge_block);

        self.builder.position_at_end(else_block);
        let else_value = self.fold_expression(indirect_value, *else_branch);
        self.builder.build_unconditional_branch(merge_block);

        self.builder.position_at_end(merge_block);
        match indirect_value {
            Some(_) => None,
            None => {
                let phi = self
                    .builder
                    .build_phi(then_value.unwrap().get_type(), "phi");
                phi.add_incoming(&[
                    (&then_value.unwrap(), then_block),
                    (&else_value.unwrap(), else_block),
                ]);
                Some(phi.as_basic_value())
            }
        }
    }

    fn fold_match_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        matchee: ExpressionId,
        case_list: &[(PatternId, ExpressionId)],
        resolver: Resolver,
    ) -> Option<BasicValueEnum<'context>> {
        let matchee_value = self.fold_expression(None, matchee).unwrap();

        let matchee_type = &self.inference.type_of_expression[matchee];
        let TypeInfo {
            tag: tag_type,
            tag_map,
        } = self.type_cache.type_info(matchee_type);

        let tag_value = match matchee_type {
            Type::AbstractDataType(_) => match tag_type {
                Some(_) => {
                    let value = match matchee_value.get_type() {
                        BasicTypeEnum::PointerType(_) => {
                            let tag_ptr = self
                                .builder
                                .build_struct_gep(matchee_value.into_pointer_value(), 0, "")
                                .unwrap();
                            let tag_value = self.builder.build_load(tag_ptr, "");
                            tag_value.into_int_value()
                        }
                        BasicTypeEnum::StructType(_) => {
                            let tag_value = self
                                .builder
                                .build_extract_value(matchee_value.into_struct_value(), 0, "")
                                .unwrap()
                                .into_int_value();
                            tag_value
                        }
                        basic_type => panic!("match of {basic_type:?} not supported"),
                    };
                    Some(value)
                }
                None => None,
            },
            Type::FunctionDefinition(_) => todo!("match on function not supported"),
            Type::Scalar(_) => todo!("match on scalar not supported"),
        };

        tag_value.map_or_else(
            || todo!("tagless"),
            |tag_value| {
                let (patterns_per_tag_value, else_patterns) = case_list
                    .iter()
                    .cloned()
                    .map(|(pattern_id, expression_id)| {
                        let case_pattern = &self.body.patterns[pattern_id];
                        match case_pattern {
                            hir::Pattern::Deconstructor(path, _) => {
                                let constructor_id = resolver
                                    .resolve_path_in_value_namespace(self.db.upcast(), path)
                                    .map(|item| match item {
                                        hir::ValueNamespaceItem::ValueConstructor(id) => id,
                                        _ => panic!(),
                                    })
                                    .unwrap();
                                let tag_value = tag_map.get(&constructor_id).unwrap();
                                ((pattern_id, expression_id), Some(*tag_value))
                            }
                            hir::Pattern::Bind(_) => ((pattern_id, expression_id), None),
                        }
                    })
                    .fold(
                        (BTreeMap::new(), Vec::new()),
                        |(mut tag_patterns_map, mut else_patterns), (case, tag)| match tag {
                            None => {
                                else_patterns.push(case);
                                (tag_patterns_map, else_patterns)
                            }
                            Some(tag) => {
                                tag_patterns_map.entry(tag).or_insert(vec![case]).push(case);
                                (tag_patterns_map, else_patterns)
                            }
                        },
                    );

                let else_block = self
                    .context
                    .append_basic_block(self.get_owener_function_value(), "case.else");
                let merge_block = self
                    .context
                    .append_basic_block(self.get_owener_function_value(), "case.merge");

                let (switch_cases, cases_and_blocks) = patterns_per_tag_value
                    .into_iter()
                    .map(|(tag, patterns)| {
                        let tag_value = tag_type.unwrap().const_int(tag as u64, false);
                        let case_block = self
                            .context
                            .prepend_basic_block(merge_block, format!("br.{}.tag", tag).as_str());
                        ((tag_value, case_block), (case_block, patterns))
                    })
                    .unzip::<_, (BasicBlock, Vec<(PatternId, ExpressionId)>), Vec<_>, Vec<_>>();

                self.builder
                    .build_switch(tag_value, else_block, switch_cases.as_slice().as_ref());

                self.builder.position_at_end(else_block);
                self.builder.build_unreachable();

                let case_values = cases_and_blocks
                    .into_iter()
                    .map(|(case_block, cases)| {
                        self.builder.position_at_end(case_block);
                        let (pattern_id, expression_id) = cases.first().unwrap();
                        let pattern = &self.body.patterns[*pattern_id];
                        match pattern {
                            hir::Pattern::Deconstructor(_, sub_patterns) => {
                                if sub_patterns.is_empty() {
                                    let case_value =
                                        self.fold_expression(indirect_value, *expression_id);
                                    self.builder.build_unconditional_branch(merge_block);
                                    (case_value, case_block)
                                } else {
                                    let inner_value = match matchee_value.get_type() {
                                        BasicTypeEnum::PointerType(_) => {
                                            let inner_ptr = self
                                                .builder
                                                .build_struct_gep(
                                                    matchee_value.into_pointer_value(),
                                                    1,
                                                    "",
                                                )
                                                .unwrap();
                                            inner_ptr.as_basic_value_enum()
                                        }
                                        BasicTypeEnum::StructType(_) => {
                                            let inner_value = self
                                                .builder
                                                .build_extract_value(
                                                    matchee_value.into_struct_value(),
                                                    1,
                                                    "",
                                                )
                                                .unwrap();
                                            inner_value
                                        }
                                        _ => panic!(),
                                    };

                                    let scope = sub_patterns
                                        .iter()
                                        .enumerate()
                                        .map(|(index, sub_pattern_id)| {
                                            let sub_pattern = &self.body.patterns[*sub_pattern_id];
                                            match sub_pattern {
                                                hir::Pattern::Bind(name) => {
                                                    match inner_value.get_type() {
                                                        BasicTypeEnum::PointerType(_) => {
                                                            let bind_ptr = self
                                                                .builder
                                                                .build_struct_gep(
                                                                    inner_value
                                                                        .into_pointer_value(),
                                                                    index as u32,
                                                                    "",
                                                                )
                                                                .unwrap();
                                                            Binding {
                                                                name: name.clone(),
                                                                value: BindingValue {
                                                                    kind: ValueKind::Indirect,
                                                                    value: bind_ptr
                                                                        .as_basic_value_enum(),
                                                                },
                                                            }
                                                        }
                                                        BasicTypeEnum::StructType(_) => {
                                                            let value = self
                                                                .builder
                                                                .build_extract_value(
                                                                    inner_value.into_struct_value(),
                                                                    index as u32,
                                                                    "",
                                                                )
                                                                .unwrap();
                                                            Binding {
                                                                name: name.clone(),
                                                                value: BindingValue {
                                                                    kind: ValueKind::Direct,
                                                                    value,
                                                                },
                                                            }
                                                        }
                                                        _ => panic!(),
                                                    }
                                                }
                                                hir::Pattern::Deconstructor(_, _) => todo!(),
                                            }
                                        })
                                        .collect();

                                    self.binding_stack.borrow_mut().push(scope);
                                    let case_value =
                                        self.fold_expression(indirect_value, *expression_id);
                                    self.builder.build_unconditional_branch(merge_block);
                                    self.binding_stack.borrow_mut().pop();
                                    (case_value, case_block)
                                }
                            }
                            hir::Pattern::Bind(_) => todo!(),
                        }
                    })
                    .collect::<Vec<_>>();

                self.builder.position_at_end(merge_block);
                match indirect_value {
                    Some(ptr) => ptr.into(),
                    None => {
                        let phi = self
                            .builder
                            .build_phi(case_values.first().unwrap().0.get_type(), "phi");
                        phi.add_incoming(
                            case_values
                                .iter()
                                .map(|(case_value, case_block)| {
                                    (case_value as &dyn BasicValue, *case_block)
                                })
                                .collect::<Vec<_>>()
                                .as_slice(),
                        );
                        phi.as_basic_value()
                    }
                }
            },
        )
    }

    fn fold_call_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        callee: &ExpressionId,
        arguments: &[ExpressionId],
    ) -> Option<BasicValueEnum<'context>> {
        let callee_type = &self.inference.type_of_expression[*callee];
        let callable_definition = match callee_type {
            Type::FunctionDefinition(callable) => callable,
            _ => panic!("call has no callable type."),
        };
        match callable_definition {
            CallableDefinitionId::FunctionDefinition(id) => {
                // let function_info = self.function_info_cache.function_info(id);
                // let function_value = self.function_value_cache.llvm_function_value(id);
                // let arguments: Vec<_> = match function_info.return_kind {
                //     ValueKind::Direct => None,
                //     ValueKind::Indirect => Some(stack_value.unwrap().as_basic_value_enum()),
                // }
                // .into_iter()
                // .chain(
                //     arguments
                //         .iter()
                //         .zip(
                //             function_info
                //                 .skip_return_param(function_value.get_param_iter()),
                //         )
                //         .map(|(arg, (_, kind))| {
                //             let arg_stack_value = match kind {
                //                 ValueKind::Indirect => {
                //                     let ty = &self.inference.type_of_expression[*arg];
                //                     let ty = self.type_cache.llvm_type(ty);
                //                     let arg_ptr = self.builder.build_alloca(ty, "");
                //                     Some(arg_ptr)
                //                 }
                //                 ValueKind::Direct => stack_value,
                //             };
                //             let arg_value = self.fold_expression(arg_stack_value, *arg);

                //             match kind {
                //                 ValueKind::Indirect => {
                //                     let _ = self.builder.build_memcpy(
                //                         arg_stack_value.unwrap(),
                //                         4,
                //                         arg_value.into_pointer_value(),
                //                         4,
                //                         self.context.i32_type().const_int(16, false),
                //                     );
                //                 }
                //                 ValueKind::Direct => todo!(),
                //             };
                //             arg_value
                //         }),
                // )
                // .collect();

                // let call_value = self.builder.build_call(
                //     function.value,
                //     arguments
                //         .into_iter()
                //         .map(|x| x.into())
                //         .collect::<Vec<_>>()
                //         .as_slice(),
                //     "",
                // );

                // match function.return_kind {
                //     ReturnKind::ArgumentPointer => stack_value.unwrap().into(),
                //     ReturnKind::RegisterValue => {
                //         call_value.try_as_basic_value().unwrap_left()
                //     }
                // }
                todo!()
            }
            CallableDefinitionId::ValueConstructor(constructor_id) => {
                let (struct_type, type_info) =
                    self.type_cache.adt_struct(constructor_id.parrent_id);
                match indirect_value {
                    Some(ptr) => {
                        if let Some(tag_type) = type_info.tag {
                            let tag_value = type_info.tag_map[constructor_id];
                            let tag_value = tag_type.const_int(tag_value as u64, false);
                            let tag_ptr = self.builder.build_struct_gep(ptr, 0, "").unwrap();
                            self.builder.build_store(tag_ptr, tag_value);
                        } else {
                            todo!()
                        }
                        None
                    }
                    None => {
                        let constructed_value = struct_type.const_zero();
                        let TypeInfo { tag, tag_map } = type_info;
                        let tag_value = tag.map(|tag_type| {
                            tag_type.const_int(tag_map[constructor_id] as u64, true)
                        });
                        let value = if let Some(tag_value) = tag_value {
                            self.builder
                                .build_insert_value(constructed_value, tag_value, 0, "")
                                .unwrap()
                                .as_basic_value_enum()
                        } else {
                            todo!()
                        };
                        Some(value)
                    }
                }
            }
        }
    }

    fn fold_binary_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        operator: &BinaryOperator,
        lhs: &ExpressionId,
        rhs: &ExpressionId,
    ) -> Option<BasicValueEnum<'context>> {
        let lhs = self.fold_expression(None, *lhs).unwrap().into_int_value();
        let rhs = self.fold_expression(None, *rhs).unwrap().into_int_value();
        let int_value = match operator {
            hir::BinaryOperator::Arithmetic(arithmetic_op) => match arithmetic_op {
                hir::ArithmeticOperator::Add => self.builder.build_int_add(lhs, rhs, ""),
                hir::ArithmeticOperator::Sub => self.builder.build_int_sub(lhs, rhs, ""),
                hir::ArithmeticOperator::Div => self.builder.build_int_signed_div(lhs, rhs, ""),
                hir::ArithmeticOperator::Mul => self.builder.build_int_mul(lhs, rhs, ""),
                hir::ArithmeticOperator::Rem => self.builder.build_int_signed_rem(lhs, rhs, ""),
            },
            hir::BinaryOperator::Compare(compare_op) => {
                let predicate = match compare_op {
                    hir::CompareOperator::Equality { negated } => match negated {
                        true => IntPredicate::NE,
                        false => IntPredicate::EQ,
                    },
                    hir::CompareOperator::Order { ordering, strict } => match (ordering, strict) {
                        (hir::Ordering::Less, true) => IntPredicate::SLT,
                        (hir::Ordering::Less, false) => IntPredicate::SLE,
                        (hir::Ordering::Greater, true) => IntPredicate::SGT,
                        (hir::Ordering::Greater, false) => IntPredicate::SGE,
                    },
                };
                self.builder.build_int_compare(predicate, lhs, rhs, "")
            }
            hir::BinaryOperator::Logic(logic_op) => match logic_op {
                hir::LogicOperator::And => self.builder.build_and(lhs, rhs, ""),
                hir::LogicOperator::Or => self.builder.build_or(lhs, rhs, ""),
            },
        };
        match indirect_value {
            Some(ptr) => {
                self.builder
                    .build_store(ptr, int_value.as_basic_value_enum());
                None
            }
            None => Some(int_value.into()),
        }
    }

    fn fold_unary_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        operator: &UnaryOperator,
        expression: &ExpressionId,
    ) -> Option<BasicValueEnum<'context>> {
        let expr = self.fold_expression(None, *expression).unwrap();
        let value = match operator {
            hir::UnaryOperator::Minus => self
                .builder
                .build_int_sub(
                    expr.get_type().into_int_type().const_zero(),
                    expr.into_int_value(),
                    "",
                )
                .into(),
            hir::UnaryOperator::Negation => {
                self.builder.build_not(expr.into_int_value(), "").into()
            }
        };
        match indirect_value {
            Some(ptr) => {
                self.builder.build_store(ptr, value);
                None
            }
            None => Some(value),
        }
    }

    fn fold_path_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        path: &hir::Path,
        resolver: Resolver,
    ) -> Option<BasicValueEnum<'context>> {
        let item = resolver
            .resolve_path_in_value_namespace(self.db.upcast(), path)
            .unwrap();

        match item {
            hir::ValueNamespaceItem::Function(_) => todo!(),
            hir::ValueNamespaceItem::ValueConstructor(_) => todo!(),
            hir::ValueNamespaceItem::LocalBinding(pattern_id) => {
                match &self.body.patterns[pattern_id] {
                    hir::Pattern::Deconstructor(_, _) => todo!(),
                    hir::Pattern::Bind(name) => match indirect_value {
                        Some(ptr) => {
                            let source = self
                                .binding_stack
                                .borrow()
                                .get(name)
                                .unwrap()
                                .value
                                .into_pointer_value();
                            let _ = self.builder.build_memcpy(
                                ptr,
                                4,
                                source,
                                4,
                                self.context.i32_type().const_int(4, false),
                            );
                            None
                        }
                        None => Some(self.binding_stack.borrow().get(name).unwrap().value),
                    },
                }
            }
        }
    }

    fn fold_literal_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        literal_id: ExpressionId,
        literal: &Literal,
    ) -> Option<BasicValueEnum<'context>> {
        let literal_type = &self.inference.type_of_expression[literal_id];
        let value = match literal_type {
            Type::AbstractDataType(_) => todo!(),
            Type::FunctionDefinition(_) => todo!(),
            Type::Scalar(ScalarType::Integer(kind)) => {
                let value = match literal {
                    hir::Literal::Integer(value, _) => *value,
                    _ => panic!(),
                };
                match kind {
                    hir::IntegerKind::I32 => {
                        self.context.i32_type().const_int(value as u64, true).into()
                    }
                    hir::IntegerKind::I64 => {
                        self.context.i64_type().const_int(value as u64, true).into()
                    }
                }
            }
            Type::Scalar(ScalarType::Boolean) => match literal {
                hir::Literal::Bool(bool) => match bool {
                    true => self.context.bool_type().const_all_ones().into(),
                    false => self.context.bool_type().const_zero().into(),
                },
                _ => panic!(),
            },
        };
        match indirect_value {
            Some(ptr) => {
                self.builder.build_store(ptr, value);
                None
            }
            None => Some(value),
        }
    }
}
