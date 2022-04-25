use syntax::ast;

use crate::{name::Name, path::Path, refinement::Predicate};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeReference {
    Path(Path),
    Refinement(Box<TypeReference>, Name, Predicate),
    Pointer(Box<TypeReference>),
}

impl TypeReference {
    pub(crate) fn from_ast(ty: ast::Type) -> Self {
        match ty {
            ast::Type::PathType(path_ty) => Self::Path(Path::from_ast(path_ty.path().unwrap())),
            ast::Type::PointerType(ptr_ty) => {
                Self::Pointer(Box::new(Self::from_ast(ptr_ty.inner_type().unwrap())))
            }
            ast::Type::RefinementType(refinement_ty) => Self::Refinement(
                Box::new(Self::from_ast(refinement_ty.inner_type().unwrap())),
                Name::from_ast_name(match refinement_ty.inner_pattern().unwrap() {
                    ast::Pattern::DeconstructorPattern(_) => todo!(),
                    ast::Pattern::BindingPattern(binding) => binding.name().unwrap(),
                }),
                Predicate::lower(refinement_ty.predicate().unwrap()),
            ),
        }
    }
}
