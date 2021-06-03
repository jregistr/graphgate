use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::ops::{Deref, DerefMut};

use indexmap::IndexMap;
use serde::{Serialize, Serializer};
use value::{ConstValue, Name, Variables};

use crate::types::{FetchQuery, VariablesRef, SelectionRef};
use crate::Request;
use std::borrow::Borrow;

#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum PlanNode<'a> {
    Sequence(SequenceNode<'a>),
    Parallel(ParallelNode<'a>),
    Introspection(IntrospectionNode),
    Fetch(FetchNode<'a>),
    Flatten(FlattenNode<'a>),
}

impl<'a> PlanNode<'a> {
    pub(crate) fn flatten(self) -> Self {
        match self {
            PlanNode::Sequence(mut node) if node.nodes.len() == 1 => node.nodes.remove(0),
            PlanNode::Parallel(mut node) if node.nodes.len() == 1 => node.nodes.remove(0),
            _ => self,
        }
    }
}

impl ToString for RootNode<'_> {
    fn to_string(&self) -> String {
        match self {
            RootNode::Subscribe(_) => "".to_string(),
            RootNode::Query(plan) => plan.to_string()
        }
    }
}

impl ToString for PlanNode<'_> {
    fn to_string(&self) -> String {
        fn stringify_nodes(output: &mut String, nodes: &[&PlanNode], indentation: &str) -> () {
            if nodes.len() > 0 {
                let next_indent = format!("{}{}", indentation, "  ");
                for (index, node) in nodes.iter().enumerate() {
                    stringify_node(output, &node, next_indent.as_str());
                    if index < nodes.len() - 1 {
                        output.push_str(",\n")
                    }
                }
            }
        }

        fn stringify_node(output: &mut String, node: &PlanNode, indentation: &str) -> () {
            let next_indent = format!("{}{}", indentation, " ");
            let next_indent = next_indent.as_str();
            match node {
                PlanNode::Sequence(seq) => {
                    output.push_str(format!("{}Sequence {{", indentation).as_str());
                    let nodes: Vec<&PlanNode> = seq.nodes.iter().map(|n| n).collect();
                    let nodes = nodes.as_slice();
                    stringify_nodes(output, nodes, next_indent);
                    output.push_str("\n}");
                }
                PlanNode::Parallel(par) => {
                    output.push_str(format!("{}Parallel {{", indentation).as_str());
                    let nodes: Vec<&PlanNode> = par.nodes.iter().map(|n| n).collect();
                    let nodes = nodes.as_slice();
                    stringify_nodes(output, nodes, next_indent);
                    output.push_str("\n}");
                }
                PlanNode::Introspection(_) => {}
                PlanNode::Fetch(fetch) => {
                    output.push_str(format!("{indent}Fetch(service: {svc}) {{", indent = indentation, svc = fetch.service).as_str());
                    let query = &fetch.query.selection_set.0;
                    for select in query.iter() {
                        match select {
                            SelectionRef::FieldRef(field) => {
                                output.push_str(format!("{:?}", field.field.selection_set).as_str())
                            }
                            SelectionRef::IntrospectionTypename => {}
                            SelectionRef::RequiredRef(req) => {
                                // output.push_str(format!("{:?}", req.fields).as_str())
                            }
                            SelectionRef::InlineFragment { type_condition, selection_set } => {
                                // output.push_str("Inline Fragmentation!!!!")
                            }
                        }
                    }
                }
                PlanNode::Flatten(_) => {}
            };
        }

        let mut output = String::from("QueryPlan { \n");
        stringify_nodes(&mut output, &[&self], "");
        output.push_str("\n}\n");
        output
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct PathSegment<'a> {
    pub name: &'a str,
    pub is_list: bool,
    pub possible_type: Option<&'a str>,
}

#[derive(Clone, Default, Hash, Eq, PartialEq)]
pub struct ResponsePath<'a>(Vec<PathSegment<'a>>);

impl<'a> Debug for ResponsePath<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        for (idx, segment) in self.0.iter().enumerate() {
            if idx > 0 {
                write!(f, ".")?;
            }
            if segment.is_list {
                write!(f, "[{}]", segment.name)?;
            } else {
                write!(f, "{}", segment.name)?;
            }
            if let Some(possible_type) = segment.possible_type {
                write!(f, "({})", possible_type)?;
            }
        }
        Ok(())
    }
}

impl<'a> Display for ResponsePath<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Debug::fmt(self, f)
    }
}

impl<'a> Serialize for ResponsePath<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'a> Deref for ResponsePath<'a> {
    type Target = Vec<PathSegment<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for ResponsePath<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Default, Debug, Serialize)]
pub struct SequenceNode<'a> {
    pub nodes: Vec<PlanNode<'a>>,
}

#[derive(Default, Debug, Serialize)]
pub struct ParallelNode<'a> {
    pub nodes: Vec<PlanNode<'a>>,
}

#[derive(Debug, Serialize)]
pub struct IntrospectionDirective {
    pub name: Name,

    #[serde(skip_serializing_if = "IndexMap::is_empty")]
    pub arguments: IndexMap<Name, ConstValue>,
}

#[derive(Debug, Serialize)]
pub struct IntrospectionField {
    pub name: Name,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub alias: Option<Name>,

    #[serde(skip_serializing_if = "IndexMap::is_empty")]
    pub arguments: IndexMap<Name, ConstValue>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub directives: Vec<IntrospectionDirective>,

    pub selection_set: IntrospectionSelectionSet,
}

#[derive(Debug, Default, Serialize)]
#[serde(transparent)]
pub struct IntrospectionSelectionSet(pub Vec<IntrospectionField>);

#[derive(Debug, Serialize)]
#[serde(transparent)]
pub struct IntrospectionNode {
    pub selection_set: IntrospectionSelectionSet,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FetchNode<'a> {
    pub service: &'a str,
    #[serde(skip_serializing_if = "VariablesRef::is_empty")]
    pub variables: VariablesRef<'a>,
    pub query: FetchQuery<'a>,
}

impl<'a> FetchNode<'a> {
    pub fn to_request(&self) -> Request {
        Request::new(self.query.to_string()).variables(self.variables.to_variables())
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FlattenNode<'a> {
    pub path: ResponsePath<'a>,
    pub prefix: usize,
    pub service: &'a str,
    #[serde(skip_serializing_if = "VariablesRef::is_empty")]
    pub variables: VariablesRef<'a>,
    pub query: FetchQuery<'a>,
}

impl<'a> FlattenNode<'a> {
    pub fn to_request(&self, representations: Variables) -> Request {
        Request::new(self.query.to_string())
            .variables(representations)
            .extend_variables(self.variables.to_variables())
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SubscribeNode<'a> {
    pub subscribe_nodes: Vec<FetchNode<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub flatten_node: Option<PlanNode<'a>>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum RootNode<'a> {
    Subscribe(SubscribeNode<'a>),
    Query(PlanNode<'a>),
}
