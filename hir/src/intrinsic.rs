use crate::name::Name;

pub(crate) static BUILTIN_SCOPE: &[(Name, BuiltinType)] = &[
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
