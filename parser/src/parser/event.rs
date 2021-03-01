use crate::syntax_kind::SyntaxKind;
use crate::TreeSink;
use std::mem;

#[derive(Debug, PartialEq, Eq)]
pub enum Event {
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    FinishNode,
    AddToken,
    Placeholder,
}

pub(crate) fn sink_events<Sink>(mut events: Vec<Event>, mut sink: Sink) -> Sink
where
    Sink: TreeSink,
{
    for i in 0..events.len() {
        match mem::replace(&mut events[i], Event::Placeholder) {
            Event::StartNode {
                forward_parent,
                kind,
            } => {
                let mut forward_parents = vec![kind];

                let mut idx = i;
                let mut forward_parent = forward_parent;

                while let Some(fp) = forward_parent {
                    idx += fp;
                    forward_parent = match mem::replace(&mut events[idx], Event::Placeholder) {
                        Event::StartNode {
                            kind,
                            forward_parent,
                        } => {
                            forward_parents.push(kind);
                            forward_parent
                        }
                        _ => unreachable!(),
                    };
                }

                for kind in forward_parents.into_iter().rev() {
                    sink.start_node(kind);
                }
            }
            Event::FinishNode => sink.finish_node(),
            Event::AddToken => sink.add_token(),
            Event::Placeholder => {}
        }
    }
    sink
}
