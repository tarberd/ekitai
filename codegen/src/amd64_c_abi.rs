use super::CodeGenTypeCache;
use hir::Type;

enum Class {
    Integer,
    SSE,
    SSEUp,
    X87,
    X87Up,
    ComplexX87,
    NoClass,
    Memory,
}

fn merge(accumulative: Class, field: Class) -> Class {
    todo!()
}

fn post_merge(aggregate_size: usize, low: Class, hi: Class) -> (Class, Class) {
    todo!()
}

fn classify(
    type_cache: &CodeGenTypeCache,
    ty: Type,
    offset_base: usize,
    low: &mut Class,
    high: &mut Class,
    is_named_argument: bool,
) {
    let current = match offset_base < 64 {
        true => low,
        false => high,
    };

    *current = Class::Memory;

    match ty {
        Type::Scalar(scalar) => match scalar {
            hir::ScalarType::Integer(hir::IntegerKind::I32 | hir::IntegerKind::I64)
            | hir::ScalarType::Boolean => {
                *current = Class::Integer;
            }
        },
        Type::AbstractDataType(test) => {
            let struct_type = type_cache.adt_struct(test);
            let size = type_cache.bit_size(&ty);

            // AMD64-ABI 3.2.3p2: Rule 1. If the size of an object is larger
            // than eight eightbytes, ..., it has class MEMORY.
            if size > 8 * 64 {
                return;
            }
        }
        Type::FunctionDefinition(_) => todo!(),
        Type::Pointer(_) => todo!(),
    }
}

fn classify_return_type(type_cache: &CodeGenTypeCache, return_type: Type) {
    let (mut low, mut high) = (Class::NoClass, Class::NoClass);
    classify(type_cache, return_type, 0, &mut low, &mut high, true);
}

fn compute_info(type_cache: &CodeGenTypeCache, return_type: Type) {
    let mut free_integer_registers = 6;
    let mut free_sse_registers = 8;

    classify_return_type(type_cache, return_type);
}
