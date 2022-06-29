use syntax::ast;

use super::name::Name;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub segments: Vec<Name>,
}

impl Path {
    pub(crate) fn from_ast(path: ast::Path) -> Self {
        let segments = Self::collect_segments(Vec::new(), path);
        Self { segments }
    }

    fn collect_segments(mut segments: Vec<Name>, path: ast::Path) -> Vec<Name> {
        let segment = path.path_segment().unwrap();
        if let Some(path) = path.path() {
            let mut segments = Self::collect_segments(segments, path);
            segments.push(Name::from_ast_nameref(segment.name_reference().unwrap()));
            segments
        } else {
            segments.push(Name::from_ast_nameref(segment.name_reference().unwrap()));
            segments
        }
    }

    pub(crate) fn as_name(&self) -> Name {
        Name::new_inline(
            self.segments
                .iter()
                .fold(String::new(), |mut fold, segment| {
                    if fold.is_empty() {
                        fold.push_str(segment.id.as_str());
                        fold
                    } else {
                        let segment = &segment.id;
                        format!("{fold}::{segment}")
                    }
                })
                .as_str(),
        )
    }
}
