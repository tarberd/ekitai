use hir::{
    Body, ExpressionId, FunctionLocationId, HirDatabase, InferenceResult, Name, Resolver,
    SourceDatabase, Type, TypeLocationId, Upcast, ValueConstructorId,
};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    builder::Builder,
    context::Context,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetTriple},
    types::{AnyType, BasicType, BasicTypeEnum, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};
use std::collections::HashMap;

#[salsa::database(
    hir::SourceDatabaseStorage,
    hir::InternDatabaseStorage,
    hir::DefinitionsDatabaseStorage,
    hir::HirDatabaseStorage
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

impl salsa::Database for Database {}

#[derive(Clone, Copy)]
enum ReturnKind {
    ArgumentPointer,
    RegisterValue,
}

#[derive(Clone, Copy)]
enum ValueKind {
    Stack,
    Register,
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

fn inkwell_basic_type<'ink>(
    context: &'ink Context,
    type_map: &HashMap<TypeLocationId, StructType<'ink>>,
    ty: &Type,
) -> BasicTypeEnum<'ink> {
    match ty {
        Type::AbstractDataType(id) => type_map[id].into(),
        Type::FunctionDefinition(_id) => todo!(),
        Type::Scalar(scalar) => match scalar {
            hir::ScalarType::Integer(int_kind) => match int_kind {
                hir::IntegerKind::I32 => context.i32_type().into(),
                hir::IntegerKind::I64 => context.i64_type().into(),
            },
            hir::ScalarType::Boolean => context.bool_type().into(),
        },
    }
}

pub fn compile_text(source: String) {
    let mut db = Database::default();

    db.set_source_file_text(source);

    build_assembly_ir(&db)
}

pub fn build_assembly_ir(db: &dyn HirDatabase) {
    Target::initialize_x86(&InitializationConfig::default());
    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64-pc-linux-gnu"),
            "x86-64",
            "",
            opt,
            reloc,
            model,
        )
        .unwrap();

    let target_data = target_machine.get_target_data();

    let context = Context::create();
    let llvm_module = context.create_module("ekitai_module");
    llvm_module.set_data_layout(&target_data.get_data_layout());
    llvm_module.set_triple(&target_machine.get_triple());

    let mut type_map = HashMap::new();

    let def_map = db.source_file_definitions_map();

    for id in def_map.item_scope.definitions.iter() {
        match id {
            hir::LocationId::FunctionLocationId(_) => continue,
            hir::LocationId::TypeLocationId(ty_id) => {
                let ty = db.type_definition_data(*ty_id);
                let sty = context.opaque_struct_type(ty.name.id.as_str());
                sty.set_body(&[context.i32_type().into()], false);
                type_map.insert(*ty_id, sty);
            }
        };
    }

    let mut function_map = HashMap::new();

    for id in def_map.item_scope.definitions.iter() {
        match id {
            hir::LocationId::FunctionLocationId(f_id) => {
                let fun = db.function_definition_data(*f_id);
                let sig = db.function_definition_signature(*f_id);

                let param_tys = sig.parameter_types.iter().map(|ty| {
                    match inkwell_basic_type(&context, &type_map, ty) {
                        BasicTypeEnum::StructType(s) => {
                            (ValueKind::Stack, s.ptr_type(AddressSpace::Generic).into())
                        }
                        other => (ValueKind::Register, other),
                    }
                });

                let ret_ty = inkwell_basic_type(&context, &type_map, &sig.return_type);

                let return_kind = match &sig.return_type {
                    Type::AbstractDataType(_) => ReturnKind::ArgumentPointer,
                    _ => ReturnKind::RegisterValue,
                };

                let (parameters_kind, function_type) = match return_kind {
                    ReturnKind::ArgumentPointer => {
                        let (param_kinds, param_types): (Vec<_>, Vec<_>) = Some((
                            ValueKind::Stack,
                            ret_ty.ptr_type(AddressSpace::Generic).into(),
                        ))
                        .into_iter()
                        .chain(param_tys)
                        .unzip();
                        let function_type = context.void_type().fn_type(&param_types, false);
                        (param_kinds, function_type)
                    }
                    ReturnKind::RegisterValue => {
                        let (param_kinds, param_types): (Vec<_>, Vec<_>) = param_tys.unzip();
                        let function_type = ret_ty.fn_type(&param_types, false);
                        (param_kinds, function_type)
                    }
                };
                let function_value =
                    llvm_module.add_function(fun.name.id.as_str(), function_type, None);
                match return_kind {
                    ReturnKind::ArgumentPointer => {
                        let kind_id = Attribute::get_named_enum_kind_id("sret");
                        let byval_attribute =
                            context.create_type_attribute(kind_id, ret_ty.as_any_type_enum());
                        function_value.add_attribute(AttributeLoc::Param(0), byval_attribute);
                        let kind_id = Attribute::get_named_enum_kind_id("noalias");
                        let byval_attribute = context.create_enum_attribute(kind_id, 0);
                        function_value.add_attribute(AttributeLoc::Param(0), byval_attribute);
                    }
                    ReturnKind::RegisterValue => (),
                };
                function_map.insert(
                    *f_id,
                    FunctionIr {
                        value: function_value,
                        return_kind,
                        parameters_kind,
                    },
                );
            }
            hir::LocationId::TypeLocationId(_) => continue,
        }
    }

    for (function_id, function) in function_map.iter() {
        let llvm_body = context.append_basic_block(function.value, "");
        let builder = context.create_builder();
        builder.position_at_end(llvm_body);

        let body = db.body_of_definition(*function_id);
        let local_bindings: HashMap<_, _> = body
            .parameter_bindings
            .iter()
            .zip(function.parameters())
            .map(|(pattern_id, (parameter, parameter_kind))| {
                let pattern = &body.patterns[*pattern_id];
                match pattern {
                    hir::Pattern::Path(_) => todo!("Unsing unsuported path pattern"),
                    hir::Pattern::Bind(name) => {
                        parameter.set_name(&name.id);
                        (name.clone(), (parameter, *parameter_kind))
                    }
                }
            })
            .collect();

        let return_value_ptr = match function.return_kind {
            ReturnKind::ArgumentPointer => Some(
                function
                    .value
                    .get_first_param()
                    .unwrap()
                    .into_pointer_value(),
            ),
            ReturnKind::RegisterValue => None,
        };

        let return_value = ExpressionLowerer::new(
            &context,
            &builder,
            db,
            &type_map,
            &function_map,
            &local_bindings,
            *function_id,
            &body,
        )
        .fold_expression(return_value_ptr, body.root_expression);

        let return_value = match function.return_kind {
            ReturnKind::ArgumentPointer => None,
            ReturnKind::RegisterValue => Some(return_value),
        };

        builder.build_return(return_value.as_ref().map(|val| val as &dyn BasicValue));
    }

    println!("{}", llvm_module.print_to_string().to_string());
    let _ = llvm_module.print_to_file("out.ll");
}

struct ExpressionLowerer<'ink> {
    context: &'ink Context,
    builder: &'ink Builder<'ink>,
    db: &'ink dyn HirDatabase,
    function_id: FunctionLocationId,
    body: &'ink Body,
    type_map: &'ink HashMap<TypeLocationId, StructType<'ink>>,
    function_map: &'ink HashMap<FunctionLocationId, FunctionIr<'ink>>,
    local_bindings: &'ink HashMap<Name, (BasicValueEnum<'ink>, ValueKind)>,
    inference: InferenceResult,
}

impl<'ink> ExpressionLowerer<'ink> {
    pub fn new(
        context: &'ink Context,
        builder: &'ink Builder,
        db: &'ink dyn HirDatabase,
        type_map: &'ink HashMap<TypeLocationId, StructType<'ink>>,
        function_map: &'ink HashMap<FunctionLocationId, FunctionIr<'ink>>,
        local_bindings: &'ink HashMap<Name, (BasicValueEnum<'ink>, ValueKind)>,
        function_id: FunctionLocationId,
        body: &'ink Body,
    ) -> Self {
        Self {
            context,
            builder,
            db,
            type_map,
            function_map,
            function_id,
            local_bindings,
            body,
            inference: db.infer_body_expression_types(function_id),
        }
    }

    pub fn get_owener_function_value(&self) -> FunctionValue {
        self.get_owener_function_ir().value
    }

    pub fn get_owener_function_ir(&self) -> &FunctionIr<'ink> {
        &self.function_map[&self.function_id]
    }

    pub fn fold_expression(
        &self,
        stack_value: Option<PointerValue<'ink>>,
        expr_id: ExpressionId,
    ) -> BasicValueEnum<'ink> {
        let expr = &self.body.expressions[expr_id];
        match expr {
            hir::Expression::Block {
                trailing_expression,
            } => self.fold_expression(stack_value, *trailing_expression),
            hir::Expression::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let comparison = self.fold_expression(None, *condition).into_int_value();

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
                let then_value = self.fold_expression(stack_value, *then_branch);
                self.builder.build_unconditional_branch(merge_block);

                self.builder.position_at_end(else_block);
                let else_value = self.fold_expression(stack_value, *else_branch);
                self.builder.build_unconditional_branch(merge_block);

                self.builder.position_at_end(merge_block);
                match stack_value {
                    Some(ptr) => ptr.into(),
                    None => {
                        let phi = self.builder.build_phi(then_value.get_type(), "phi");
                        phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
                        phi.as_basic_value()
                    }
                }
            }
            hir::Expression::Match { matchee, case_list } => {
                let matchee_value = self.fold_expression(None, *matchee);

                let matchee_ty = &self.inference.type_of_expression[*matchee];

                match matchee_ty {
                    Type::AbstractDataType(_) => {
                        let merge_block = self
                            .context
                            .append_basic_block(self.get_owener_function_value(), "case.merge");

                        let tag_ptr = self.builder.build_bitcast(
                            matchee_value,
                            self.context.i32_type().ptr_type(AddressSpace::Generic),
                            "",
                        );
                        let tag = self.builder.build_load(tag_ptr.into_pointer_value(), "");
                        let resolver =
                            Resolver::new_for_expression(self.db, self.function_id, expr_id);
                        let (cases, case_expressions): (Vec<_>, Vec<ExpressionId>) = case_list.iter().map(|(pattern_id, case_expression)| {
                            let case_pattern = &self.body.patterns[*pattern_id];
                            match case_pattern {
                                hir::Pattern::Path(path) => {
                                    let item = resolver
                                        .resolve_path_in_value_namespace(self.db.upcast(), path)
                                        .unwrap();

                                    let tag = match item {
                                        hir::ValueNamespaceItem::ValueConstructor(ValueConstructorId{id, ..}) => {
                                            let tag = u32::from(id.into_raw());
                                            let tag_value = self.context.i32_type().const_int(tag as u64, false);
                                            (tag, tag_value)
                                        },
                                        item => panic!("namespace resolution must be constructor but found :{:?}, missing typecheck?", item),
                                    };

                                    (tag, case_expression)
                                }
                                hir::Pattern::Bind(_) => todo!(),
                            }
                        })
                        .map(|((tag, tag_value), case_expression)| {
                            let case_block = self.context.prepend_basic_block(
                                merge_block,
                                format!("case.{}tag", tag).as_str(),
                            );
                            ((tag_value, case_block), case_expression)
                        }).unzip();

                        let else_block = self
                            .context
                            .prepend_basic_block(cases.first().unwrap().1, "case.else");

                        self.builder.build_switch(
                            tag.into_int_value(),
                            else_block,
                            cases.as_slice().as_ref(),
                        );

                        self.builder.position_at_end(else_block);
                        self.builder.build_unreachable();

                        let mut case_values = Vec::new();
                        for ((_, case_block), case_expression) in
                            cases.iter().zip(case_expressions.iter())
                        {
                            self.builder.position_at_end(*case_block);
                            let case_value = self.fold_expression(stack_value, *case_expression);
                            case_values.push(case_value);
                            self.builder.build_unconditional_branch(merge_block);
                        }
                        self.builder.position_at_end(merge_block);
                        match stack_value {
                            Some(ptr) => ptr.into(),
                            None => {
                                let phi = self
                                    .builder
                                    .build_phi(case_values.first().unwrap().get_type(), "phi");
                                phi.add_incoming(
                                    case_values
                                        .iter()
                                        .map(|basic_value| basic_value as &dyn BasicValue)
                                        .zip(cases.iter().map(|(_, block)| block).cloned())
                                        .collect::<Vec<_>>()
                                        .as_slice(),
                                );
                                phi.as_basic_value()
                            }
                        }
                    }
                    _ => panic!(),
                }
            }
            hir::Expression::Call { callee, arguments } => {
                let callee_type = &self.inference.type_of_expression[*callee];
                match callee_type {
                    Type::FunctionDefinition(id) => {
                        let function = &self.function_map[id];

                        let arguments: Vec<_> = match function.return_kind {
                            ReturnKind::RegisterValue => None,
                            ReturnKind::ArgumentPointer => {
                                Some(stack_value.unwrap().as_basic_value_enum())
                            }
                        }
                        .into_iter()
                        .chain(arguments.iter().zip(function.parameters()).map(
                            |(arg, (_, kind))| {
                                let arg_stack_value = match kind {
                                    ValueKind::Stack => {
                                        let ty = &self.inference.type_of_expression[*arg];
                                        let ty =
                                            inkwell_basic_type(self.context, self.type_map, ty);
                                        let arg_ptr = self.builder.build_alloca(ty, "");
                                        Some(arg_ptr)
                                    }
                                    ValueKind::Register => stack_value,
                                };
                                let arg_value = self.fold_expression(arg_stack_value, *arg);

                                match kind {
                                    ValueKind::Stack => {
                                        let _ = self.builder.build_memcpy(
                                            arg_stack_value.unwrap(),
                                            4,
                                            arg_value.into_pointer_value(),
                                            4,
                                            self.context.i32_type().const_int(16, false),
                                        );
                                    }
                                    ValueKind::Register => todo!(),
                                };
                                arg_value
                            },
                        ))
                        .collect();

                        let call_value =
                            self.builder
                                .build_call(function.value, arguments.as_slice(), "");

                        match function.return_kind {
                            ReturnKind::ArgumentPointer => stack_value.unwrap().into(),
                            ReturnKind::RegisterValue => {
                                call_value.try_as_basic_value().unwrap_left()
                            }
                        }
                    }
                    _ => panic!(),
                }
            }
            hir::Expression::Binary(operator, lhs, rhs) => {
                let lhs = self.fold_expression(None, *lhs).into_int_value();
                let rhs = self.fold_expression(None, *rhs).into_int_value();
                let int_value = match operator {
                    hir::BinaryOperator::Arithmetic(arithmetic_op) => match arithmetic_op {
                        hir::ArithmeticOperator::Add => self.builder.build_int_add(lhs, rhs, ""),
                        hir::ArithmeticOperator::Sub => self.builder.build_int_sub(lhs, rhs, ""),
                        hir::ArithmeticOperator::Div => {
                            self.builder.build_int_signed_div(lhs, rhs, "")
                        }
                        hir::ArithmeticOperator::Mul => self.builder.build_int_mul(lhs, rhs, ""),
                        hir::ArithmeticOperator::Rem => {
                            self.builder.build_int_signed_rem(lhs, rhs, "")
                        }
                    },
                    hir::BinaryOperator::Compare(compare_op) => {
                        let predicate = match compare_op {
                            hir::CompareOperator::Equality { negated } => match negated {
                                true => IntPredicate::NE,
                                false => IntPredicate::EQ,
                            },
                            hir::CompareOperator::Order { ordering, strict } => {
                                match (ordering, strict) {
                                    (hir::Ordering::Less, true) => IntPredicate::SLT,
                                    (hir::Ordering::Less, false) => IntPredicate::SLE,
                                    (hir::Ordering::Greater, true) => IntPredicate::SGT,
                                    (hir::Ordering::Greater, false) => IntPredicate::SGE,
                                }
                            }
                        };
                        self.builder.build_int_compare(predicate, lhs, rhs, "")
                    }
                };
                int_value.into()
            }
            hir::Expression::Unary(op, expr) => {
                let expr = self.fold_expression(None, *expr);
                match op {
                    hir::UnaryOperator::Minus => self
                        .builder
                        .build_int_sub(
                            expr.get_type().into_int_type().const_zero(),
                            expr.into_int_value(),
                            "",
                        )
                        .into(),
                }
            }
            hir::Expression::Path(path) => {
                let resolver = Resolver::new_for_expression(self.db, self.function_id, expr_id);
                let item = resolver
                    .resolve_path_in_value_namespace(self.db.upcast(), path)
                    .unwrap();

                match item {
                    hir::ValueNamespaceItem::Function(_) => todo!(),
                    hir::ValueNamespaceItem::ValueConstructor(ValueConstructorId {
                        id, ..
                    }) => {
                        let struct_ptr = stack_value.unwrap();

                        let tag_type = self.context.i32_type();
                        let tag_ptr = self.builder.build_struct_gep(struct_ptr, 0, "").unwrap();

                        let id = id.into_raw();
                        let tag = tag_type.const_int(u32::from(id) as u64, false);

                        self.builder.build_store(tag_ptr, tag);

                        struct_ptr.into()
                    }
                    hir::ValueNamespaceItem::LocalBinding(binding) => {
                        match &self.body.patterns[binding] {
                            hir::Pattern::Path(_) => todo!(),
                            hir::Pattern::Bind(name) => match stack_value {
                                Some(ptr) => {
                                    let source = self.local_bindings[name].0.into_pointer_value();
                                    let _ = self.builder.build_memcpy(
                                        ptr,
                                        4,
                                        source,
                                        4,
                                        self.context.i32_type().const_int(4, false),
                                    );
                                    source.into()
                                }
                                None => self.local_bindings[name].0,
                            },
                        }
                    }
                }
            }
            hir::Expression::Literal(literal) => match literal {
                hir::Literal::Integer(value, kind) => match kind {
                    Some(kind) => match kind {
                        hir::IntegerKind::I32 => self
                            .context
                            .i32_type()
                            .const_int(*value as u64, true)
                            .into(),
                        hir::IntegerKind::I64 => self
                            .context
                            .i64_type()
                            .const_int(*value as u64, true)
                            .into(),
                    },
                    None => todo!(),
                },
                hir::Literal::Bool(value) => match value {
                    true => self.context.bool_type().const_all_ones().into(),
                    false => self.context.bool_type().const_zero().into(),
                },
            },
        }
    }
}
