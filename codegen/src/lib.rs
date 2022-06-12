use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    fmt::Display,
    iter::FromIterator,
    sync::Arc,
};

use by_address::ByAddress;
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{
        CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine, TargetTriple,
    },
    types::{AnyType, BasicType, BasicTypeEnum, FunctionType, IntType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};

use hir::{
    check::type_inference::InferenceResult,
    check::{IntegerKind, ScalarType, Type},
    semantic_ir::{
        definition_map::{CallableDefinitionId, ValueConstructorId},
        name::Name,
        path_resolver::{Resolver, ValueNamespaceItem},
    },
    semantic_ir::{
        definition_map::{FunctionDefinitionId, TypeDefinitionId},
        path::Path,
        term::{
            ArithmeticOperator, BinaryOperator, Body, CompareOperator, Literal, LogicOperator,
            Ordering, Pattern, BodyPatternId, Statement, Term, BodyTermId, UnaryOperator,
        },
    },
    HirDatabase, SourceDatabase, Upcast,
};

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
    hir::InternerStorage,
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

impl Upcast<dyn hir::Interner> for Database {
    fn upcast(&self) -> &(dyn hir::Interner + 'static) {
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
    pub fn get(&self, name: &Name) -> Option<Value<'ink>> {
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
    pub fn get(&self, name: &Name) -> Option<Value<'ink>> {
        self.scope
            .iter()
            .find_map(|binding| match &binding.name == name {
                true => Some(binding.value.clone()),
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
    pub value: Value<'ink>,
}

#[derive(Debug, Clone)]
struct Value<'a> {
    pub kind: ValueKind,
    pub value: BasicValueEnum<'a>,
}

impl<'a> Value<'a> {
    fn new(kind: ValueKind, value: BasicValueEnum<'a>) -> Self {
        Self { kind, value }
    }
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
    adt_map: RefCell<HashMap<TypeDefinitionId, (StructType<'context>, TypeInfo<'context>)>>,
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
            Type::Pointer(inner) => {
                let inner = self.llvm_type(inner);
                let ptr_type = inner.ptr_type(AddressSpace::Generic);
                ptr_type.as_basic_type_enum()
            }
            Type::Refinement(inner, _, _) => self.llvm_type(inner),
        }
    }

    fn type_info(&self, ty: &Type) -> TypeInfo<'context> {
        match ty {
            Type::AbstractDataType(ty_loc_id) => self.adt_struct_info(*ty_loc_id),
            Type::FunctionDefinition(_) => todo!(),
            Type::Scalar(_) => todo!(),
            Type::Pointer(_) => todo!(),
            Type::Refinement(inner, _, _) => self.type_info(inner),
        }
    }

    fn bit_size(&self, ty: &Type) -> usize {
        let llvm_type = self.llvm_type(ty);
        self.target_data.get_bit_size(&llvm_type.as_any_type_enum()) as usize
    }

    fn target_data(&self) -> &TargetData {
        &self.target_data
    }

    fn adt_struct_info(&self, ty_loc_id: TypeDefinitionId) -> TypeInfo<'context> {
        self.adt_struct(ty_loc_id).1
    }

    fn adt_struct_type(&self, ty_loc_id: TypeDefinitionId) -> StructType<'context> {
        self.adt_struct(ty_loc_id).0
    }

    fn adt_struct(
        &self,
        ty_loc_id: TypeDefinitionId,
    ) -> (StructType<'context>, TypeInfo<'context>) {
        if let Some(value) = self.adt_map.borrow().get(&ty_loc_id) {
            return value.clone();
        };
        let ty_data = self.db.type_definition_data(ty_loc_id);
        let opaque = self.context.opaque_struct_type(ty_data.name.id.as_str());
        self.adt_map
            .borrow_mut()
            .insert(ty_loc_id, (opaque, TypeInfo::default()));

        let tag_bit_len = {
            let variant_count = ty_data.value_constructors.len();
            let mut bit_len = 0;
            while 1 << bit_len < variant_count {
                bit_len += 1;
            }
            bit_len
        };

        let tag_type = match tag_bit_len {
            0 => Some(self.context.i64_type()),
            1 => Some(self.context.i64_type()),
            2..=8 => Some(self.context.i64_type()),
            9..=16 => Some(self.context.i64_type()),
            17..=32 => Some(self.context.i64_type()),
            33..=64 => Some(self.context.i64_type()),
            _ => panic!("Tag for ADT too big! {tag_bit_len:?}"),
        };

        let value_constructor_ids = ty_data
            .value_constructors
            .iter()
            .map(|(id, _)| ValueConstructorId {
                type_definition_id: ty_loc_id,
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
        let value = (opaque, adt_info);
        self.adt_map.borrow_mut().insert(ty_loc_id, value.clone());
        value
    }

    fn value_constructor_struct(&self, constructor_id: ValueConstructorId) -> StructType<'context> {
        if let Some(struct_type) = self.adt_variant_map.borrow().get(&constructor_id) {
            return *struct_type;
        }

        let constructor = self.db.callable_definition_signature(constructor_id.into());
        let struct_data = constructor
            .parameter_types
            .iter()
            .map(|ty| self.llvm_type(ty))
            .collect::<Vec<_>>();

        let ty_data = self
            .db
            .type_definition_data(constructor_id.type_definition_id);
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
                IntegerKind::I32 => self.context.i32_type().into(),
                IntegerKind::I64 => self.context.i64_type().into(),
            },
            ScalarType::Boolean => self.context.bool_type().into(),
        }
    }
}

#[derive(Default, Clone)]
struct TypeInfo<'context> {
    pub tag: Option<IntType<'context>>,
    pub tag_map: HashMap<ValueConstructorId, usize>,
}

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
        function_id: &FunctionDefinitionId,
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

    pub(crate) fn function_info(&self, function_id: &FunctionDefinitionId) -> FunctionInfo {
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

    pub(crate) fn get_return_type<'ctx>(
        &self,
        function_value: FunctionValue<'ctx>,
    ) -> BasicTypeEnum<'ctx> {
        match self.return_kind {
            ValueKind::Indirect => function_value.get_type().get_param_types()[0],
            ValueKind::Direct => function_value.get_type().get_return_type().unwrap(),
        }
    }
}

struct CodeGenFunctionValueCache<'db, 'context, 'module, 'type_cache, 'function_info_cache> {
    db: &'db dyn CodeGenDatabase,
    context: &'context Context,
    module: &'module Module<'context>,
    type_cache: &'type_cache CodeGenTypeCache<'db, 'context>,
    function_info_cache: &'function_info_cache CodeGenFunctionInfoCache<'db, 'context, 'type_cache>,
    function_value_map: RefCell<HashMap<FunctionDefinitionId, FunctionValue<'context>>>,
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

    fn llvm_function_value(&self, function_id: &FunctionDefinitionId) -> FunctionValue<'context> {
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

    pub(crate) fn build_function_body(&self, function_id: &FunctionDefinitionId) {
        let llvm_function_value = self.function_value_cache.llvm_function_value(function_id);
        let llvm_body = self.context.append_basic_block(llvm_function_value, "");
        let builder = self.context.create_builder();
        builder.position_at_end(llvm_body);
        let body = self.db.body_of_definition(*function_id);
        let function_signature = self.db.callable_definition_signature((*function_id).into());
        let function_info = self.function_info_cache.function_info(function_id);

        let return_value_ptr = match function_info.return_kind {
            ValueKind::Indirect => Some(
                llvm_function_value
                    .get_first_param()
                    .unwrap()
                    .into_pointer_value(),
            ),
            ValueKind::Direct => {
                let ret_type = llvm_function_value.get_type().get_return_type().unwrap();
                let bit_size = self.type_cache.target_data().get_bit_size(&ret_type);
                if bit_size <= 64 {
                    None
                } else {
                    Some(builder.build_alloca(ret_type, ""))
                }
            }
        };

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
                    Pattern::Deconstructor(_, _) => todo!("Unsing unsuported path pattern"),
                    Pattern::Bind(name) => {
                        parameter.set_name(&name.id);
                        let binding_value = match parameter_kind {
                            ValueKind::Indirect => Value {
                                kind: parameter_kind,
                                value: parameter,
                            },
                            ValueKind::Direct => {
                                if self.type_cache.bit_size(&param_ty) <= 64 {
                                    Value {
                                        kind: ValueKind::Direct,
                                        value: parameter,
                                    }
                                } else {
                                    let param_ptr =
                                        builder.build_alloca(parameter.get_type(), &name.id);
                                    builder.build_store(param_ptr, parameter);
                                    Value {
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

        let builder = self.context.create_builder();
        builder.position_at_end(llvm_body);

        let return_value = ExpressionLowerer::new(
            self.db,
            &self.context,
            &builder,
            &self.type_cache,
            &self.function_info_cache,
            &self.function_value_cache,
            *function_id,
            binding_stack,
        )
        .fold_expression(return_value_ptr, body.root_expression);

        let return_value = match return_value_ptr {
            None => {
                let Value { kind, value } = return_value.unwrap();
                let value = match kind {
                    ValueKind::Indirect => builder.build_load(value.into_pointer_value(), ""),
                    ValueKind::Direct => value,
                };
                Some(value)
            }
            Some(ptr) => match function_info.return_kind {
                ValueKind::Indirect => None,
                ValueKind::Direct => Some(builder.build_load(ptr, "")),
            },
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
    for function_id in def_map.root_module_item_scope().iter_function_locations() {
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
    type_cache: &'type_cache CodeGenTypeCache<'db, 'context>,
    function_info_cache: &'function_info_cache CodeGenFunctionInfoCache<'db, 'context, 'type_cache>,
    function_value_cache: &'function_value_cache CodeGenFunctionValueCache<
        'db,
        'context,
        'module,
        'type_cache,
        'function_info_cache,
    >,
    function_id: FunctionDefinitionId,
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
        function_id: FunctionDefinitionId,
        binding_stack: LocalBindingStack<'context>,
    ) -> Self {
        Self {
            db,
            context,
            builder,
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

    fn get_alloca_builder(&self) -> Builder<'context> {
        let builder = self.context.create_builder();
        let block = self
            .get_owener_function_value()
            .get_first_basic_block()
            .unwrap();
        match block.get_first_instruction() {
            Some(instruction) => builder.position_before(&instruction),
            None => builder.position_at_end(block),
        };
        builder
    }

    pub fn fold_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        expr_id: BodyTermId,
    ) -> Option<Value<'context>> {
        let expr = &self.body.expressions[expr_id];
        match expr {
            Term::Block {
                statements,
                trailing_expression,
            } => {
                for statement in statements {
                    match statement {
                        Statement::Let(pattern_id, expr_id) => {
                            let pattern = &self.body.patterns[*pattern_id];
                            let value = self.fold_expression(None, *expr_id).unwrap();
                            let binding = match pattern {
                                Pattern::Deconstructor(_, _) => todo!(),
                                Pattern::Bind(name) => Binding {
                                    name: name.clone(),
                                    value,
                                },
                            };
                            self.binding_stack
                                .borrow_mut()
                                .push(std::iter::once(binding).collect());
                        }
                        Statement::Expression(expr_id) => {
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
            Term::If {
                condition,
                then_branch,
                else_branch,
            } => self.fold_if_expression(indirect_value, condition, then_branch, else_branch),
            Term::Match { matchee, case_list } => {
                let resolver =
                    Resolver::new_for_expression(self.db.upcast(), self.function_id, expr_id);
                self.fold_match_expression(indirect_value, *matchee, case_list, resolver)
            }
            Term::Call { callee, arguments } => {
                self.fold_call_expression(indirect_value, callee, arguments)
            }
            Term::Binary(operator, lhs, rhs) => {
                self.fold_binary_expression(indirect_value, operator, lhs, rhs)
            }
            Term::Unary(op, expr) => self.fold_unary_expression(indirect_value, op, expr),
            Term::Path(path) => {
                let resolver =
                    Resolver::new_for_expression(self.db.upcast(), self.function_id, expr_id);
                self.fold_path_expression(indirect_value, path, resolver)
            }
            Term::Literal(literal) => {
                self.fold_literal_expression(indirect_value, expr_id, literal)
            }
            Term::New(inner) => self.fold_new_expression(indirect_value, *inner),
        }
    }

    fn fold_if_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        condition: &BodyTermId,
        then_branch: &BodyTermId,
        else_branch: &BodyTermId,
    ) -> Option<Value<'context>> {
        let comparison = self
            .fold_expression(None, *condition)
            .unwrap()
            .value
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
        let then_block = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(merge_block);

        self.builder.position_at_end(else_block);
        let else_value = self.fold_expression(indirect_value, *else_branch);
        let else_block = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(merge_block);

        self.builder.position_at_end(merge_block);
        match indirect_value {
            Some(ptr) => None,
            None => {
                let (
                    Value {
                        kind: then_kind,
                        value: then_value,
                    },
                    Value {
                        kind: else_kind,
                        value: else_value,
                    },
                ) = (then_value.unwrap(), else_value.unwrap());
                match (then_kind, else_kind) {
                    (ValueKind::Direct, ValueKind::Direct)
                    | (ValueKind::Indirect, ValueKind::Indirect) => {
                        let phi = self.builder.build_phi(then_value.get_type(), "phi");
                        phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
                        Some(Value::new(then_kind, phi.as_basic_value()))
                    }
                    _ => panic!(),
                }
            }
        }
    }

    fn fold_match_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        matchee: BodyTermId,
        case_list: &[(BodyPatternId, BodyTermId)],
        resolver: Resolver,
    ) -> Option<Value<'context>> {
        let Value {
            kind: matchee_kind,
            value: matchee_value,
        } = self.fold_expression(None, matchee).unwrap();

        let matchee_type = &self.inference.type_of_expression[matchee];
        let TypeInfo { tag, tag_map } = self.type_cache.type_info(matchee_type);

        let tag_value = match matchee_kind {
            ValueKind::Indirect => {
                let matchee_ptr = matchee_value.into_pointer_value();
                match tag {
                    Some(_) => {
                        let tag_ptr = self.builder.build_struct_gep(matchee_ptr, 0, "").unwrap();
                        Some(self.builder.build_load(tag_ptr, "").into_int_value())
                    }
                    None => None,
                }
            }
            ValueKind::Direct => match tag {
                Some(_) => Some(
                    self.builder
                        .build_extract_value(matchee_value.into_struct_value(), 0, "")
                        .unwrap()
                        .into_int_value(),
                ),
                None => None,
            },
        };

        match tag_value {
            Some(tag_value) => {
                let (patterns_per_tag_value, else_patterns) = case_list
                    .iter()
                    .cloned()
                    .map(|(pattern_id, expression_id)| {
                        let case_pattern = &self.body.patterns[pattern_id];
                        match case_pattern {
                            Pattern::Deconstructor(path, _) => {
                                let constructor_id = resolver
                                    .resolve_path_in_value_namespace(self.db.upcast(), path)
                                    .map(|item| match item {
                                        ValueNamespaceItem::ValueConstructor(id) => id,
                                        _ => panic!(),
                                    })
                                    .unwrap();
                                let tag_value = tag_map.get(&constructor_id).unwrap();
                                ((pattern_id, expression_id), Some(*tag_value))
                            }
                            Pattern::Bind(_) => ((pattern_id, expression_id), None),
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
                        let tag_value = tag_value.get_type().const_int(tag as u64, false);
                        let case_block = self
                            .context
                            .prepend_basic_block(merge_block, format!("br.{}.tag", tag).as_str());
                        ((tag_value, case_block), (case_block, patterns))
                    })
                    .unzip::<_, (BasicBlock, Vec<(BodyPatternId, BodyTermId)>), Vec<_>, Vec<_>>();

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
                            Pattern::Deconstructor(_, sub_patterns) => {
                                if sub_patterns.is_empty() {
                                    let case_value =
                                        self.fold_expression(indirect_value, *expression_id);
                                    self.builder.build_unconditional_branch(merge_block);
                                    let case_block = self.builder.get_insert_block().unwrap();
                                    (case_value, case_block)
                                } else {
                                    let inner_value = match matchee_kind {
                                        ValueKind::Indirect => {
                                            let inner_ptr = self
                                                .builder
                                                .build_struct_gep(
                                                    matchee_value.into_pointer_value(),
                                                    1,
                                                    "",
                                                )
                                                .unwrap();
                                            Value::new(
                                                ValueKind::Indirect,
                                                inner_ptr.as_basic_value_enum(),
                                            )
                                        }
                                        ValueKind::Direct => {
                                            let inner_value = self
                                                .builder
                                                .build_extract_value(
                                                    matchee_value.into_struct_value(),
                                                    1,
                                                    "",
                                                )
                                                .unwrap();
                                            Value::new(ValueKind::Direct, inner_value)
                                        }
                                    };

                                    let scope = sub_patterns
                                        .iter()
                                        .enumerate()
                                        .map(|(index, sub_pattern_id)| {
                                            let sub_pattern = &self.body.patterns[*sub_pattern_id];
                                            match sub_pattern {
                                                Pattern::Bind(name) => match inner_value.kind {
                                                    ValueKind::Indirect => {
                                                        let bind_ptr = self
                                                            .builder
                                                            .build_struct_gep(
                                                                inner_value
                                                                    .value
                                                                    .into_pointer_value(),
                                                                index as u32,
                                                                "",
                                                            )
                                                            .unwrap();
                                                        let value = if self
                                                            .type_cache
                                                            .target_data()
                                                            .get_bit_size(
                                                                &bind_ptr
                                                                    .get_type()
                                                                    .get_element_type(),
                                                            )
                                                            <= 64
                                                        {
                                                            Value::new(
                                                                ValueKind::Direct,
                                                                self.builder
                                                                    .build_load(bind_ptr, ""),
                                                            )
                                                        } else {
                                                            Value::new(
                                                                ValueKind::Indirect,
                                                                bind_ptr.as_basic_value_enum(),
                                                            )
                                                        };
                                                        Binding {
                                                            name: name.clone(),
                                                            value,
                                                        }
                                                    }
                                                    ValueKind::Direct => {
                                                        let value = self
                                                            .builder
                                                            .build_extract_value(
                                                                inner_value
                                                                    .value
                                                                    .into_struct_value(),
                                                                index as u32,
                                                                "",
                                                            )
                                                            .unwrap();
                                                        Binding {
                                                            name: name.clone(),
                                                            value: Value {
                                                                kind: ValueKind::Direct,
                                                                value,
                                                            },
                                                        }
                                                    }
                                                },
                                                Pattern::Deconstructor(_, _) => todo!(),
                                            }
                                        })
                                        .collect();

                                    self.binding_stack.borrow_mut().push(scope);
                                    let case_value =
                                        self.fold_expression(indirect_value, *expression_id);
                                    self.builder.build_unconditional_branch(merge_block);
                                    self.binding_stack.borrow_mut().pop();
                                    let case_block = self.builder.get_insert_block().unwrap();
                                    (case_value, case_block)
                                }
                            }
                            Pattern::Bind(_) => todo!(),
                        }
                    })
                    .collect::<Vec<_>>();

                self.builder.position_at_end(merge_block);
                match indirect_value {
                    Some(_) => None,
                    None => {
                        let first_value = case_values.first().unwrap().0.as_ref().unwrap();
                        let phi = self.builder.build_phi(first_value.value.get_type(), "phi");
                        phi.add_incoming(
                            case_values
                                .iter()
                                .map(|(case_value, case_block)| {
                                    (
                                        &case_value.as_ref().unwrap().value as &dyn BasicValue,
                                        *case_block,
                                    )
                                })
                                .collect::<Vec<_>>()
                                .as_slice(),
                        );

                        Some(Value::new(first_value.kind, phi.as_basic_value()))
                    }
                }
            }

            None => todo!(),
        }
    }

    fn fold_call_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        callee: &BodyTermId,
        arguments: &[BodyTermId],
    ) -> Option<Value<'context>> {
        let callee_type = &self.inference.type_of_expression[*callee];
        let callable_definition = match callee_type {
            Type::FunctionDefinition(callable) => callable,
            _ => panic!("call has no callable type."),
        };
        match callable_definition {
            CallableDefinitionId::FunctionDefinition(id) => {
                let function_info = self.function_info_cache.function_info(&id);
                let function_value = self.function_value_cache.llvm_function_value(&id);

                let return_type = function_info.get_return_type(function_value);

                let indirect_function_return = match function_info.return_kind {
                    ValueKind::Direct => None,
                    ValueKind::Indirect => match indirect_value {
                        Some(ptr) => Some(ptr.as_basic_value_enum()),
                        None => Some(
                            self.get_alloca_builder()
                                .build_alloca(
                                    BasicTypeEnum::try_from(
                                        return_type.into_pointer_type().get_element_type(),
                                    )
                                    .unwrap(),
                                    "",
                                )
                                .as_basic_value_enum(),
                        ),
                    },
                };

                let arguments = indirect_function_return.into_iter().chain(
                    arguments
                        .iter()
                        .zip(function_info.parameter_kinds.iter())
                        .map(|(argument_expr, parameter_kind)| {
                            let Value {
                                kind: argument_kind,
                                value: argument_value,
                            } = self.fold_expression(None, *argument_expr).unwrap();
                            let argument_value = match (parameter_kind, argument_kind) {
                                (ValueKind::Indirect, ValueKind::Indirect)
                                | (ValueKind::Direct, ValueKind::Direct) => argument_value,
                                (ValueKind::Direct, ValueKind::Indirect) => self
                                    .builder
                                    .build_load(argument_value.into_pointer_value(), ""),
                                (ValueKind::Indirect, ValueKind::Direct) => {
                                    let ptr = self
                                        .get_alloca_builder()
                                        .build_alloca(argument_value.get_type(), "");
                                    self.builder.build_store(ptr, argument_value);
                                    ptr.as_basic_value_enum()
                                }
                            };
                            argument_value
                        }),
                );

                let call_value = self.builder.build_call(
                    function_value,
                    arguments.map(|x| x.into()).collect::<Vec<_>>().as_slice(),
                    "",
                );

                match indirect_value {
                    Some(_) => None,
                    None => match function_info.return_kind {
                        ValueKind::Indirect => Some(Value::new(
                            ValueKind::Indirect,
                            indirect_function_return.unwrap(),
                        )),
                        ValueKind::Direct => Some(Value::new(
                            ValueKind::Direct,
                            call_value.try_as_basic_value().unwrap_left(),
                        )),
                    },
                }
            }
            CallableDefinitionId::ValueConstructor(constructor_id) => {
                let (struct_type, type_info) = self
                    .type_cache
                    .adt_struct(constructor_id.type_definition_id);
                match indirect_value {
                    Some(ptr) => {
                        if let Some(tag_type) = type_info.tag {
                            let tag_value = type_info.tag_map[&constructor_id];
                            let tag_value = tag_type.const_int(tag_value as u64, false);
                            let tag_ptr = self.builder.build_struct_gep(ptr, 0, "").unwrap();
                            self.builder.build_store(tag_ptr, tag_value);
                            let variant_ptr = self.builder.build_struct_gep(ptr, 1, "").unwrap();
                            let variant_ptr = self
                                .builder
                                .build_bitcast(
                                    variant_ptr,
                                    self.type_cache
                                        .value_constructor_struct(*constructor_id)
                                        .ptr_type(AddressSpace::Generic),
                                    "",
                                )
                                .into_pointer_value();
                            let arguments = arguments
                                .iter()
                                .map(|argument| self.fold_expression(None, *argument).unwrap());
                            for (index, argument) in arguments.enumerate() {
                                let argument_ptr = self
                                    .builder
                                    .build_struct_gep(variant_ptr, index as u32, "")
                                    .unwrap();
                                let value = match argument.kind {
                                    ValueKind::Indirect => {
                                        if !argument_ptr
                                            .get_type()
                                            .get_element_type()
                                            .is_pointer_type()
                                        {
                                            self.builder
                                                .build_load(argument.value.into_pointer_value(), "")
                                        } else {
                                            argument.value
                                        }
                                    }
                                    ValueKind::Direct => argument.value,
                                };
                                self.builder.build_store(argument_ptr, value);
                            }
                        } else {
                            todo!()
                        }
                        None
                    }
                    None => {
                        let type_size = self
                            .type_cache
                            .target_data()
                            .get_bit_size(&struct_type.as_any_type_enum());

                        let value = struct_type.const_zero();
                        let TypeInfo { tag, tag_map } = type_info;
                        let tag_value = tag.map(|tag_type| {
                            tag_type.const_int(tag_map[&constructor_id] as u64, true)
                        });
                        let value = if let Some(tag_value) = tag_value {
                            if type_size <= 64 {
                                let value = self
                                    .builder
                                    .build_insert_value(value, tag_value, 0, "")
                                    .unwrap()
                                    .as_basic_value_enum();
                                Value::new(ValueKind::Direct, value)
                            } else {
                                let ptr = self.get_alloca_builder().build_alloca(struct_type, "");
                                self.builder.build_store(
                                    ptr,
                                    ptr.get_type()
                                        .get_element_type()
                                        .into_struct_type()
                                        .const_zero(),
                                );
                                let tag_ptr = self.builder.build_struct_gep(ptr, 0, "").unwrap();
                                self.builder.build_store(tag_ptr, tag_value);
                                let variant_ptr =
                                    self.builder.build_struct_gep(ptr, 1, "").unwrap();
                                let variant_ptr = self
                                    .builder
                                    .build_bitcast(
                                        variant_ptr,
                                        self.type_cache
                                            .value_constructor_struct(*constructor_id)
                                            .ptr_type(AddressSpace::Generic),
                                        "",
                                    )
                                    .into_pointer_value();
                                let arguments = arguments
                                    .iter()
                                    .map(|argument| self.fold_expression(None, *argument).unwrap());
                                for (index, argument) in arguments.enumerate() {
                                    let argument_ptr = self
                                        .builder
                                        .build_struct_gep(variant_ptr, index as u32, "")
                                        .unwrap();
                                    let value = match argument.kind {
                                        ValueKind::Indirect => self
                                            .builder
                                            .build_load(argument.value.into_pointer_value(), ""),
                                        ValueKind::Direct => argument.value,
                                    };
                                    self.builder.build_store(argument_ptr, value);
                                }
                                Value::new(ValueKind::Indirect, ptr.as_basic_value_enum())
                            }
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
        lhs: &BodyTermId,
        rhs: &BodyTermId,
    ) -> Option<Value<'context>> {
        let lhs = self
            .fold_expression(None, *lhs)
            .unwrap()
            .value
            .into_int_value();
        let rhs = self
            .fold_expression(None, *rhs)
            .unwrap()
            .value
            .into_int_value();
        let int_value = match operator {
            BinaryOperator::Arithmetic(arithmetic_op) => match arithmetic_op {
                ArithmeticOperator::Add => self.builder.build_int_add(lhs, rhs, ""),
                ArithmeticOperator::Sub => self.builder.build_int_sub(lhs, rhs, ""),
                ArithmeticOperator::Div => self.builder.build_int_signed_div(lhs, rhs, ""),
                ArithmeticOperator::Mul => self.builder.build_int_mul(lhs, rhs, ""),
                ArithmeticOperator::Rem => self.builder.build_int_signed_rem(lhs, rhs, ""),
            },
            BinaryOperator::Compare(compare_op) => {
                let predicate = match compare_op {
                    CompareOperator::Equality { negated } => match negated {
                        true => IntPredicate::NE,
                        false => IntPredicate::EQ,
                    },
                    CompareOperator::Order { ordering, strict } => match (ordering, strict) {
                        (Ordering::Less, true) => IntPredicate::SLT,
                        (Ordering::Less, false) => IntPredicate::SLE,
                        (Ordering::Greater, true) => IntPredicate::SGT,
                        (Ordering::Greater, false) => IntPredicate::SGE,
                    },
                };
                self.builder.build_int_compare(predicate, lhs, rhs, "")
            }
            BinaryOperator::Logic(logic_op) => match logic_op {
                LogicOperator::And => self.builder.build_and(lhs, rhs, ""),
                LogicOperator::Or => self.builder.build_or(lhs, rhs, ""),
            },
        };
        match indirect_value {
            Some(ptr) => {
                self.builder
                    .build_store(ptr, int_value.as_basic_value_enum());
                None
            }
            None => Some(Value::new(ValueKind::Direct, int_value.into())),
        }
    }

    fn fold_unary_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        operator: &UnaryOperator,
        expression: &BodyTermId,
    ) -> Option<Value<'context>> {
        let Value { value: expr, kind } = self.fold_expression(None, *expression).unwrap();
        let (kind, value) = match operator {
            UnaryOperator::Minus => (
                kind,
                self.builder
                    .build_int_sub(
                        expr.get_type().into_int_type().const_zero(),
                        expr.into_int_value(),
                        "",
                    )
                    .into(),
            ),
            UnaryOperator::Negation => (
                kind,
                self.builder.build_not(expr.into_int_value(), "").into(),
            ),
            UnaryOperator::Reference => {
                todo!("reference operator lowering to llvm is not implemented")
            }
            UnaryOperator::Dereference => match kind {
                ValueKind::Indirect => {
                    expr.into_pointer_value();
                    (kind, expr)
                }
                ValueKind::Direct => (ValueKind::Indirect, expr),
            },
        };
        match indirect_value {
            Some(ptr) => {
                self.builder.build_store(ptr, value);
                None
            }
            None => Some(Value::new(kind, value)),
        }
    }

    fn fold_path_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        path: &Path,
        resolver: Resolver,
    ) -> Option<Value<'context>> {
        let item = resolver
            .resolve_path_in_value_namespace(self.db.upcast(), path)
            .unwrap();

        match item {
            ValueNamespaceItem::Function(_) => todo!(),
            ValueNamespaceItem::ValueConstructor(_) => todo!(),
            ValueNamespaceItem::LocalBinding(pattern_id) => match &self.body.patterns[pattern_id] {
                Pattern::Deconstructor(_, _) => todo!(),
                Pattern::Bind(name) => {
                    let value = self
                        .binding_stack
                        .borrow()
                        .get(name)
                        .expect("missing id {name:?} from scope.");
                    match indirect_value {
                        Some(ptr) => {
                            match value.kind {
                                    ValueKind::Indirect => {
                                        let source = value.value.into_pointer_value();
                                        let _ = self.builder.build_memcpy(
                                            ptr,
                                            4,
                                            source,
                                            4,
                                            self.context.i8_type().const_int(16, false),
                                        );
                                    }
                                    ValueKind::Direct => panic!("trying to put direct value into indirect value at bind {name:?}."),
                                }
                            None
                        }
                        None => Some(value.clone()),
                    }
                }
            },
        }
    }

    fn fold_literal_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        literal_id: BodyTermId,
        literal: &Literal,
    ) -> Option<Value<'context>> {
        let literal_type = &self.inference.type_of_expression[literal_id];
        let value = match literal_type {
            Type::AbstractDataType(_) => todo!(),
            Type::FunctionDefinition(_) => todo!(),
            Type::Scalar(ScalarType::Integer(kind)) => {
                let value = match literal {
                    Literal::Integer(value, _) => *value,
                    _ => panic!(),
                };
                match kind {
                    IntegerKind::I32 => {
                        self.context.i32_type().const_int(value as u64, true).into()
                    }
                    IntegerKind::I64 => {
                        self.context.i64_type().const_int(value as u64, true).into()
                    }
                }
            }
            Type::Scalar(ScalarType::Boolean) => match literal {
                Literal::Bool(bool) => match bool {
                    true => self.context.bool_type().const_all_ones().into(),
                    false => self.context.bool_type().const_zero().into(),
                },
                _ => panic!(),
            },
            Type::Pointer(_) => todo!(),
            Type::Refinement(_, _, _) => todo!(),
        };
        match indirect_value {
            Some(ptr) => {
                self.builder.build_store(ptr, value);
                None
            }
            None => Some(Value::new(ValueKind::Direct, value)),
        }
    }

    fn fold_new_expression(
        &self,
        indirect_value: Option<PointerValue<'context>>,
        expr: BodyTermId,
    ) -> Option<Value<'context>> {
        let ty = &self.inference.type_of_expression[expr];
        let ty = self.type_cache.llvm_type(ty);
        let ptr = self.builder.build_malloc(ty, "").unwrap();
        let _ = self.fold_expression(Some(ptr), expr).is_none();
        match indirect_value {
            Some(return_ptr) => {
                let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::Generic);
                let source = self
                    .builder
                    .build_bitcast(ptr, i8_ptr_type, "")
                    .into_pointer_value();
                let source_type = &ptr.get_type().get_element_type().as_any_type_enum();
                let source_alignment = self.type_cache.target_data().get_abi_alignment(source_type);
                let sink = self
                    .builder
                    .build_bitcast(return_ptr, i8_ptr_type, "")
                    .into_pointer_value();
                let sink_alignment = self.type_cache.target_data().get_abi_alignment(
                    &return_ptr.get_type().get_element_type().as_any_type_enum(),
                );
                let size = self.type_cache.target_data().get_abi_size(source_type);
                let size = self
                    .context
                    .ptr_sized_int_type(self.type_cache.target_data(), None)
                    .const_int(size, false);
                let _ =
                    self.builder
                        .build_memcpy(sink, sink_alignment, source, source_alignment, size);
                None
            }
            None => Some(Value::new(ValueKind::Direct, ptr.as_basic_value_enum())),
        }
    }
}
