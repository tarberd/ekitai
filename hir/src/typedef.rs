
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Integer,
    Function {
        args: Vec<Type>,
        ret: Box<Type>,
    }
}