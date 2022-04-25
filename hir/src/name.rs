use smol_str::SmolStr;
use syntax::ast::{self, AstToken};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub id: SmolStr,
}

impl Name {
    pub(crate) fn from_ast_name(name: ast::Name) -> Self {
        Name {
            id: name.identifier().text().into(),
        }
    }

    pub(crate) fn from_ast_nameref(name: ast::NameReference) -> Self {
        Name {
            id: name.identifier().text().into(),
        }
    }

    pub(crate) const fn new_inline(name: &str) -> Self {
        Self {
            id: SmolStr::new_inline(name),
        }
    }
}
