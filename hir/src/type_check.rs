use super::Module;

#[derive(Debug)]
pub enum TypeError {
    Error,
}

pub fn type_check_module(_module: &Module) -> ((), Vec<TypeError>) {
    ((), vec![])
}
