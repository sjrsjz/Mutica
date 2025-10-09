pub mod ast;
pub mod lexer;
pub use ast::TypeAst;

use std::{collections::HashMap, fmt::Debug};

use crate::{parser::{ast::LinearTypeAst, lexer::Span}, types::Type};

#[derive(Debug, Clone)]
pub enum ParseError {
    UseBeforeDeclaration(LinearTypeAst, String),
    RedeclaredPattern(LinearTypeAst, String),
    UnusedVariable(LinearTypeAst, Vec<String>),
    AmbiguousPattern(LinearTypeAst),
    PatternOutOfParameterDefinition(LinearTypeAst),
    MissingBranch(LinearTypeAst),
    InternalError(String),
    AstGenerationError(Span),
}

pub struct ParseContext {
    pub declared_variables: Vec<HashMap<String, usize>>,
}
pub enum ContextError {
    NotUsed(Vec<String>),
    NotDeclared(String),
    EmptyContext,
}
impl ParseContext {
    const NOT_USED: usize = 0usize;

    pub fn new() -> Self {
        Self {
            declared_variables: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.declared_variables.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) -> Result<(), ContextError> {
        if let Some(current_scope) = self.declared_variables.last() {
            let unused_vars: Vec<String> = current_scope
                .iter()
                .filter_map(|(name, &count)| {
                    if count == Self::NOT_USED {
                        Some(name.clone())
                    } else {
                        None
                    }
                })
                .collect();
            if !unused_vars.is_empty() {
                return Err(ContextError::NotUsed(unused_vars));
            }
        } else {
            return Err(ContextError::EmptyContext);
        }
        self.declared_variables.pop();
        Ok(())
    }

    pub fn declare_variable(&mut self, name: String) -> Result<(), ContextError> {
        if let Some(current_scope) = self.declared_variables.last_mut() {
            if current_scope.contains_key(&name) && current_scope[&name] == Self::NOT_USED {
                return Err(ContextError::NotUsed(vec![name]));
            }
            current_scope.insert(name, Self::NOT_USED);
            return Ok(());
        }
        Err(ContextError::EmptyContext)
    }

    pub fn use_variable(&mut self, name: &str) -> Result<(), ContextError> {
        for scope in self.declared_variables.iter_mut().rev() {
            if let Some(count) = scope.get_mut(name) {
                *count += 1;
                return Ok(());
            }
        }
        Err(ContextError::NotDeclared(name.to_string()))
    }
}

pub struct BuildContextLayer {
    fixpoint_mapping: Vec<(String, Type)>,
    pattern_index_mapping: HashMap<String, isize>,
    captured_index_mapping: HashMap<String, isize>,
    pattern_count: usize,
}

impl BuildContextLayer {
    pub fn new() -> Self {
        Self {
            fixpoint_mapping: Vec::new(),
            pattern_index_mapping: HashMap::new(),
            captured_index_mapping: HashMap::new(),
            pattern_count: 0,
        }
    }

    pub fn enter_fixpoint(&mut self, name: String, t: Type) {
        self.fixpoint_mapping.push((name, t));
    }

    pub fn exit_fixpoint(&mut self) {
        self.fixpoint_mapping.pop();
    }

    pub fn get(&self, name: &str) -> Option<Result<&Type, isize>> {
        for (n, t) in self.fixpoint_mapping.iter().rev() {
            if n == name {
                return Some(Ok(t));
            }
        }
        if let Some(&index) = self.pattern_index_mapping.get(name) {
            return Some(Err(index));
        }
        if let Some(&index) = self.captured_index_mapping.get(name) {
            return Some(Err(index));
        }
        None
    }

    pub fn push_pattern(&mut self, name: String) -> Option<usize> {
        if self.pattern_index_mapping.contains_key(&name) {
            return None;
        }
        let index = self.pattern_index_mapping.len();
        self.pattern_index_mapping.insert(name, index as isize);
        Some(index)
    }

    pub fn push_captured(&mut self, name: String) -> Option<usize> {
        if self.captured_index_mapping.contains_key(&name) {
            return None;
        }
        let index = self.captured_index_mapping.len();
        self.captured_index_mapping
            .insert(name, -1 - index as isize);
        Some(index)
    }

    pub fn pattern_count(&self) -> usize {
        self.pattern_index_mapping.len()
    }

    pub fn captured_count(&self) -> usize {
        self.captured_index_mapping.len()
    }

    pub fn inc_pattern_count(&mut self) -> usize {
        self.pattern_count += 1;
        self.pattern_count - 1
    }
}

impl Default for BuildContextLayer {
    fn default() -> Self {
        Self::new()
    }
}

pub struct BuildContext {
    layers: Vec<BuildContextLayer>,
}

impl BuildContext {
    pub fn new() -> Self {
        Self {
            layers: vec![BuildContextLayer::new()],
        }
    }

    pub fn enter_layer(&mut self) {
        self.layers.push(BuildContextLayer::new());
    }

    pub fn exit_layer(&mut self) -> BuildContextLayer {
        self.layers
            .pop()
            .expect("There should always be at least one layer")
    }

    pub fn current_layer_mut(&mut self) -> &mut BuildContextLayer {
        self.layers
            .last_mut()
            .expect("There should always be at least one layer")
    }

    pub fn current_layer(&self) -> &BuildContextLayer {
        self.layers
            .last()
            .expect("There should always be at least one layer")
    }
}
