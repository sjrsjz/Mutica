use crate::parser::lexer::{LexerToken, LexicalError};
use crate::parser::{
    BuildContext, BuildContextLayer, ContextError, MultiFileBuilder, MultiFileBuilderError,
    ParseContext, ParseError, SourceLocation, WithLocation,
};
use lalrpop_util::ErrorRecovery;
use mutica_core::arc_gc::gc::GC;
use mutica_core::as_type;
use mutica_core::types::allof::AllOf;
use mutica_core::types::anyof::AnyOf;
use mutica_core::types::character::Character;
use mutica_core::types::character_value::CharacterValue;
use mutica_core::types::closure::Closure;
use mutica_core::types::constraint::Constraint;
use mutica_core::types::eqof::EqOf;
use mutica_core::types::fixpoint::FixPoint;
use mutica_core::types::float::Float;
use mutica_core::types::float_value::FloatValue;
use mutica_core::types::invoke::Invoke;
use mutica_core::types::lazy::Lazy;
use mutica_core::types::namespace::Namespace;
use mutica_core::types::opcode::{Opcode, OpcodeKind};
use mutica_core::types::ordered_type::OrderedType;
use mutica_core::types::sequence::Sequence;
use mutica_core::types::subof::SubOf;
use mutica_core::types::type_bound::TypeBound;
use mutica_core::types::unify::EnvironmentVarState;
use mutica_core::types::{GcAllocObject, Type, TypeError};
use mutica_core::util::rootstack::RootStack;
use std::collections::HashMap;
use std::num::NonZero;
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum AtomicOpcode {
    Opcode,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    Greater,
    Is,
    Neg,
    Set,
    BuildFixPoint,
    IO(String),
}

#[derive(Debug, Clone)]
pub enum TypeAst {
    ParseError(ErrorRecovery<usize, LexerToken, LexicalError>),
    Import(String),
    Range {
        ty: Box<WithLocation<TypeAst>>,
        min: usize,
        delta: Option<usize>,
    },
    Float,
    Char,
    Top,
    Bottom,
    DiscardPattern,
    OrderedType(usize),
    FloatLiteral(f64),
    CharLiteral(char),
    Variable(String),
    Tuple(Vec<(WithLocation<TypeAst>, NonZero<usize>)>),
    Cons {
        head: Vec<(WithLocation<TypeAst>, NonZero<usize>)>,
        tail: Box<WithLocation<TypeAst>>,
    },
    List {
        head: Vec<(WithLocation<TypeAst>, NonZero<usize>)>,
        tail: Box<WithLocation<TypeAst>>,
    },
    Generalize(Vec<WithLocation<TypeAst>>),
    Specialize(Vec<WithLocation<TypeAst>>),
    Invoke {
        func: Box<WithLocation<TypeAst>>,
        arg: Box<WithLocation<TypeAst>>,
        continuation: Option<Box<WithLocation<TypeAst>>>,
        perform_handler: Option<Box<WithLocation<TypeAst>>>,
    },
    HandleWith {
        closure: Box<WithLocation<TypeAst>>,
        init_val: Box<WithLocation<TypeAst>>,
        handler: Box<WithLocation<TypeAst>>,
    },
    #[allow(clippy::type_complexity)]
    Expression {
        binding_patterns: Vec<(
            Vec<WithLocation<String>>,
            WithLocation<TypeAst>,
            (WithLocation<TypeAst>, WithLocation<TypeAst>),
        )>,
        binding_types: Vec<WithLocation<TypeAst>>,
        body: Box<WithLocation<TypeAst>>,
    },
    #[allow(clippy::type_complexity)]
    Match {
        branches: Vec<(
            Vec<WithLocation<String>>,
            WithLocation<TypeAst>,
            (WithLocation<TypeAst>, WithLocation<TypeAst>),
            WithLocation<TypeAst>,
        )>, // pattern, expr
    },
    Apply {
        func: Box<WithLocation<TypeAst>>,
        arg: Box<WithLocation<TypeAst>>,
    },
    Eq {
        left: Box<WithLocation<TypeAst>>,
        right: Box<WithLocation<TypeAst>>,
    },
    Neq {
        left: Box<WithLocation<TypeAst>>,
        right: Box<WithLocation<TypeAst>>,
    },
    Not {
        value: Box<WithLocation<TypeAst>>,
    },
    AtomicOpcode(AtomicOpcode),
    FixPoint {
        param_name: String,
        expr: Box<WithLocation<TypeAst>>,
    },
    StaticFixPoint {
        param_name: String,
        expr: Box<WithLocation<TypeAst>>,
    },
    Namespace {
        tag: String,
        expr: Box<WithLocation<TypeAst>>,
    },
    Generic {
        generic_vars: Vec<WithLocation<String>>,
        expr: Box<WithLocation<TypeAst>>,
        constraint: Box<(WithLocation<TypeAst>, WithLocation<TypeAst>)>,
    },
    Literal(Box<WithLocation<TypeAst>>),
    EqOf {
        value: Box<WithLocation<TypeAst>>,
    },
    SubOf {
        value: Box<WithLocation<TypeAst>>,
    },
}

#[derive(Debug, Clone)]
pub enum BasicTypeAst {
    Range {
        ty: Box<WithLocation<BasicTypeAst>>,
        min: usize,
        delta: Option<usize>,
    },
    Float,
    Char,
    Top,
    Bottom,
    FloatLiteral(f64),
    CharLiteral(char),
    OrderedType(usize),
    Variable(String),
    Tuple(Vec<(WithLocation<BasicTypeAst>, NonZero<usize>)>),
    List {
        head: Vec<(WithLocation<BasicTypeAst>, NonZero<usize>)>,
        tail: Box<WithLocation<BasicTypeAst>>,
    },
    Cons {
        head: Vec<(WithLocation<BasicTypeAst>, NonZero<usize>)>,
        tail: Box<WithLocation<BasicTypeAst>>,
    },
    Generalize(Vec<WithLocation<BasicTypeAst>>),
    Specialize(Vec<WithLocation<BasicTypeAst>>),
    Invoke {
        func: Box<WithLocation<BasicTypeAst>>,
        arg: Box<WithLocation<BasicTypeAst>>,
        continuation: Option<Box<WithLocation<BasicTypeAst>>>,
        perform_handler: Option<Box<WithLocation<BasicTypeAst>>>,
    },
    #[allow(clippy::type_complexity)]
    Match {
        branches: Vec<(
            Vec<WithLocation<String>>,
            WithLocation<BasicTypeAst>,
            (WithLocation<BasicTypeAst>, WithLocation<BasicTypeAst>),
            WithLocation<BasicTypeAst>,
        )>, // pattern, expr
    },
    Apply {
        func: Box<WithLocation<BasicTypeAst>>,
        arg: Box<WithLocation<BasicTypeAst>>,
        handler: Option<Box<WithLocation<BasicTypeAst>>>,
    },
    AtomicOpcode(AtomicOpcode),
    Namespace {
        tag: String,
        expr: Box<WithLocation<BasicTypeAst>>,
    },
    Generic {
        generic_vars: Vec<WithLocation<String>>,
        expr: Box<WithLocation<BasicTypeAst>>,
        constraint: Box<(WithLocation<BasicTypeAst>, WithLocation<BasicTypeAst>)>,
    },
    Literal(Box<WithLocation<BasicTypeAst>>),
    EqOf {
        value: Box<WithLocation<BasicTypeAst>>,
    },
    SubOf {
        value: Box<WithLocation<BasicTypeAst>>,
    },
    StaticFixPoint {
        param_name: String,
        expr: Box<WithLocation<BasicTypeAst>>,
    },
}

pub struct LinearizeContext {
    invoke_tmpvar_counter: usize, // 用于生成唯一的
}

impl Default for LinearizeContext {
    fn default() -> Self {
        Self::new()
    }
}

impl LinearizeContext {
    pub fn new() -> Self {
        Self { invoke_tmpvar_counter: 0 }
    }

    fn allocate_tmpvar(&mut self) -> usize {
        let index = self.invoke_tmpvar_counter;
        self.invoke_tmpvar_counter += 1;
        index
    }

    pub fn allocate_tmpvar_name(&mut self) -> String {
        let index = self.allocate_tmpvar();
        format!("invoke#tmp#{}", index)
    }
}

#[derive(Debug)]
pub struct LinearizeResult<'ast> {
    #[allow(clippy::type_complexity)]
    bindings: Vec<(
        WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
        WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
        Option<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
        String,
    )>, // (func, arg, handler, tmpvar_name)
    tail_type: WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
}

impl<'ast> LinearizeResult<'ast> {
    pub fn new_simple(ty: WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>) -> Self {
        Self { bindings: Vec::new(), tail_type: ty }
    }

    #[allow(clippy::type_complexity)]
    pub fn new_with_binding(
        bindings: Vec<(
            WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
            WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
            Option<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
            String,
        )>,
        ty: WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
    ) -> Self {
        Self { bindings, tail_type: ty }
    }

    pub fn new_apply(
        func: LinearizeResult<'ast>,
        arg: LinearizeResult<'ast>,
        handler: Option<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
        allocated_tmpvar_name: String,
    ) -> Self {
        let mut bindings = func.bindings;
        bindings.extend(arg.bindings);
        bindings.push((func.tail_type, arg.tail_type, handler, allocated_tmpvar_name.clone()));
        Self { bindings, tail_type: LinearTypeAst::Variable(allocated_tmpvar_name).into() }
    }

    #[allow(clippy::type_complexity)]
    pub fn bindings(
        &self,
    ) -> &Vec<(
        WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
        WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
        Option<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
        String,
    )> {
        &self.bindings
    }

    pub fn tail_type(&self) -> &WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>> {
        &self.tail_type
    }

    pub fn finalize(self) -> WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>> {
        let mut ty = self.tail_type;
        for (f, a, handler, tmpvar) in self.bindings.into_iter().rev() {
            let f_loc = f.location().cloned();
            let continuation = if let LinearTypeAst::Variable(v) = ty.value()
                && v.eq(&tmpvar)
            {
                None // TCO（尾调用优化）
            } else {
                Some(
                    WithLocation::new(
                        LinearTypeAst::Match {
                            auto_captures: HashMap::new(),
                            branches: vec![(
                                vec![WithLocation::new(tmpvar.clone(), ty.location())],
                                WithLocation::new(
                                    LinearTypeAst::Variable(tmpvar.clone()),
                                    ty.location(),
                                ),
                                (
                                    WithLocation::new(LinearTypeAst::Tuple(vec![]), ty.location()),
                                    WithLocation::new(LinearTypeAst::Tuple(vec![]), ty.location()),
                                ),
                                ty.clone(),
                            )],
                        },
                        ty.location(),
                    )
                    .into(),
                )
            };
            ty = WithLocation::new(
                LinearTypeAst::Invoke {
                    func: Box::new(f),
                    arg: Box::new(a),
                    continuation,
                    perform_handler: handler.map(Box::new),
                },
                f_loc.as_ref(),
            )
        }
        ty
    }
}

impl BasicTypeAst {
    #[stacksafe::stacksafe]
    pub fn linearize<'a>(
        &'a self,
        ctx: &mut LinearizeContext,
        loc: Option<&SourceLocation>,
    ) -> LinearizeResult<'a> {
        match self {
            BasicTypeAst::Range { ty, min, delta } => {
                let ty_result = ty.linearize(ctx, ty.location());
                let tail_ty = ty_result.tail_type().clone();
                LinearizeResult::new_with_binding(
                    ty_result.bindings,
                    WithLocation::new(
                        LinearTypeAst::Range { ty: Box::new(tail_ty), min: *min, delta: *delta },
                        loc,
                    ),
                )
            }
            BasicTypeAst::Float => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::Float, loc))
            }
            BasicTypeAst::Char => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::Char, loc))
            }
            BasicTypeAst::Top => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::Top, loc))
            }
            BasicTypeAst::Bottom => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::Bottom, loc))
            }
            BasicTypeAst::FloatLiteral(v) => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::FloatLiteral(*v), loc))
            }
            BasicTypeAst::CharLiteral(v) => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::CharLiteral(*v), loc))
            }
            BasicTypeAst::OrderedType(v) => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::OrderedType(*v), loc))
            }
            BasicTypeAst::Variable(v) => LinearizeResult::new_simple(WithLocation::new(
                LinearTypeAst::Variable(v.clone()),
                loc,
            )),
            BasicTypeAst::Tuple(v) => {
                let elements =
                    v.iter().map(|(e, n)| (e.linearize(ctx, e.location()), *n)).collect::<Vec<_>>();
                let ty = LinearTypeAst::Tuple(
                    elements.iter().map(|(e, n)| (e.tail_type().clone(), *n)).collect(),
                );

                LinearizeResult::new_with_binding(
                    elements.into_iter().flat_map(|(e, _)| e.bindings.into_iter()).collect(),
                    WithLocation::new(ty, loc),
                )
            }
            BasicTypeAst::List { head, tail } => {
                let head_results = head
                    .iter()
                    .map(|(e, n)| (e.linearize(ctx, e.location()), *n))
                    .collect::<Vec<_>>();
                let tail_result = tail.linearize(ctx, tail.location());
                let mut bindings = Vec::new();
                for hr in &head_results {
                    bindings.extend(hr.0.bindings.clone());
                }
                bindings.extend(tail_result.bindings.clone());
                let head_types = head_results
                    .into_iter()
                    .map(|(r, n)| (r.tail_type().clone(), n))
                    .collect::<Vec<_>>();
                let ty = LinearTypeAst::List {
                    head: head_types,
                    tail: Box::new(tail_result.tail_type().clone()),
                };
                LinearizeResult::new_with_binding(bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Cons { head, tail } => {
                let head_results = head
                    .iter()
                    .map(|(e, n)| (e.linearize(ctx, e.location()), *n))
                    .collect::<Vec<_>>();
                let tail_result = tail.linearize(ctx, tail.location());
                let mut bindings = Vec::new();
                for hr in &head_results {
                    bindings.extend(hr.0.bindings.clone());
                }
                bindings.extend(tail_result.bindings.clone());
                let head_types = head_results
                    .into_iter()
                    .map(|(r, n)| (r.tail_type().clone(), n))
                    .collect::<Vec<_>>();
                let ty = LinearTypeAst::Cons {
                    head: head_types,
                    tail: Box::new(tail_result.tail_type().clone()),
                };
                LinearizeResult::new_with_binding(bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Generalize(v) => {
                let elements = v.iter().map(|e| e.linearize(ctx, e.location())).collect::<Vec<_>>();
                let ty = LinearTypeAst::Generalize(
                    elements.iter().map(|e| e.tail_type().clone()).collect(),
                );
                LinearizeResult::new_with_binding(
                    elements.into_iter().flat_map(|e| e.bindings.into_iter()).collect(),
                    WithLocation::new(ty, loc),
                )
            }
            BasicTypeAst::Specialize(v) => {
                let elements = v.iter().map(|e| e.linearize(ctx, e.location())).collect::<Vec<_>>();
                let ty = LinearTypeAst::Specialize(
                    elements.iter().map(|e| e.tail_type().clone()).collect(),
                );
                LinearizeResult::new_with_binding(
                    elements.into_iter().flat_map(|e| e.bindings.into_iter()).collect(),
                    WithLocation::new(ty, loc),
                )
            }
            BasicTypeAst::Invoke { func, arg, continuation, perform_handler } => {
                let func = func.linearize(ctx, func.location());
                let arg = arg.linearize(ctx, arg.location());
                let continuation = continuation.as_ref().map(|continuation| {
                    Box::new(continuation.linearize(ctx, continuation.location()))
                });
                let perform_handler = perform_handler.as_ref().map(|perform_handler| {
                    Box::new(perform_handler.linearize(ctx, perform_handler.location()))
                });
                let ty = LinearTypeAst::Invoke {
                    func: func.tail_type().clone().into(),
                    arg: arg.tail_type().clone().into(),
                    continuation: continuation.as_ref().map(|c| c.tail_type().clone().into()),
                    perform_handler: perform_handler.as_ref().map(|r| r.tail_type().clone().into()),
                };
                let mut bindings = func.bindings;
                bindings.extend(arg.bindings);
                if let Some(continuation) = continuation {
                    bindings.extend(continuation.bindings);
                }
                if let Some(perform_handler) = perform_handler {
                    bindings.extend(perform_handler.bindings);
                }
                LinearizeResult::new_with_binding(bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Match { branches } => {
                let mut linearized_branches = Vec::new();
                let mut bindings = Vec::new();
                for (vars, p, (f, g), expr) in branches {
                    let pat = p.linearize(ctx, p.location());
                    let f = f.linearize(ctx, f.location());
                    let g = g.linearize(ctx, g.location());
                    let expr = expr.linearize(ctx, expr.location()).finalize(); // expr 是严格独立上下文的，因此直接线性化不参与CPS
                    bindings.extend(pat.bindings.clone());
                    bindings.extend(f.bindings.clone());
                    bindings.extend(g.bindings.clone());
                    linearized_branches.push((
                        vars.clone(),
                        pat.tail_type().clone(),
                        (f.tail_type().clone(), g.tail_type().clone()),
                        expr,
                    ));
                }
                let ty = LinearTypeAst::Match {
                    auto_captures: HashMap::new(),
                    branches: linearized_branches,
                };
                LinearizeResult::new_with_binding(bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Apply { func, arg, handler } => {
                let func = func.linearize(ctx, func.location());
                let arg = arg.linearize(ctx, arg.location());
                let allocated_tmpvar_name = ctx.allocate_tmpvar_name();
                match handler {
                    Some(handler) => {
                        let handler = handler.linearize(ctx, handler.location());
                        let result = LinearizeResult::new_apply(
                            func,
                            arg,
                            Some(handler.tail_type().clone()),
                            allocated_tmpvar_name,
                        );
                        let mut bindings = result.bindings;
                        bindings.extend(handler.bindings);
                        LinearizeResult::new_with_binding(bindings, result.tail_type.clone())
                    }
                    None => LinearizeResult::new_apply(func, arg, None, allocated_tmpvar_name),
                }
            }
            BasicTypeAst::AtomicOpcode(atomic_opcode) => LinearizeResult::new_simple(
                WithLocation::new(LinearTypeAst::AtomicOpcode(atomic_opcode.clone()), loc),
            ),
            BasicTypeAst::Namespace { tag, expr } => {
                let expr = expr.linearize(ctx, expr.location());
                let ty = LinearTypeAst::Namespace {
                    tag: tag.clone(),
                    expr: Box::new(expr.tail_type().clone()),
                };
                LinearizeResult::new_with_binding(expr.bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Generic { generic_vars, expr, constraint } => {
                let expr = expr.linearize(ctx, expr.location());
                let mut bindings = expr.bindings.clone();
                let constraint_f = constraint.0.linearize(ctx, constraint.0.location());
                let constraint_g = constraint.1.linearize(ctx, constraint.1.location());
                bindings.extend(constraint_f.bindings.clone());
                bindings.extend(constraint_g.bindings.clone());

                let ty = LinearTypeAst::Generic {
                    generic_vars: generic_vars.clone(),
                    expr: Box::new(expr.tail_type().clone()),
                    constraint: Box::new((
                        constraint_f.tail_type().clone(),
                        constraint_g.tail_type().clone(),
                    )),
                };
                LinearizeResult::new_with_binding(bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Literal(inner) => LinearizeResult::new_simple(WithLocation::new(
                LinearTypeAst::Literal(Box::new(inner.linearize(ctx, inner.location()).finalize())),
                loc,
            )),
            BasicTypeAst::EqOf { value } => {
                let value = value.linearize(ctx, value.location());
                let ty = LinearTypeAst::EqOf { value: Box::new(value.tail_type().clone()) };
                LinearizeResult::new_with_binding(value.bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::SubOf { value } => {
                let value = value.linearize(ctx, value.location());
                let ty = LinearTypeAst::SubOf { value: Box::new(value.tail_type().clone()) };
                LinearizeResult::new_with_binding(value.bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::StaticFixPoint { param_name, expr } => {
                let expr = expr.linearize(ctx, expr.location());
                let ty = LinearTypeAst::StaticFixPoint {
                    param_name: param_name.clone(),
                    expr: Box::new(expr.tail_type().clone()),
                };
                LinearizeResult::new_with_binding(expr.bindings, WithLocation::new(ty, loc))
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct FlowedMetaData<'ast> {
    reference: Option<WithLocation<Option<&'ast LinearTypeAst<'ast>>>>,
    variable_context: Vec<WithLocation<String>>,
}

impl<'ast> FlowedMetaData<'ast> {
    pub fn reference(&self) -> Option<&WithLocation<Option<&'ast LinearTypeAst<'ast>>>> {
        self.reference.as_ref()
    }

    pub fn variable_context(&self) -> &Vec<WithLocation<String>> {
        &self.variable_context
    }

    pub fn with_reference(
        self,
        reference: Option<WithLocation<Option<&'ast LinearTypeAst<'ast>>>>,
    ) -> Self {
        Self { reference, ..self }
    }

    pub fn with_variable_context(self, variable_context: Vec<WithLocation<String>>) -> Self {
        Self { variable_context, ..self }
    }
}

#[derive(Debug, Clone)]
pub enum LinearTypeAst<'ast> {
    Range {
        ty: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
        min: usize,
        delta: Option<usize>,
    },
    Char,
    Float,
    Top,
    Bottom,
    FloatLiteral(f64),
    CharLiteral(char),
    OrderedType(usize),
    Variable(String), // None 表示续体
    Tuple(Vec<(WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>, NonZero<usize>)>),
    List {
        head: Vec<(WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>, NonZero<usize>)>,
        tail: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
    },
    Cons {
        head: Vec<(WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>, NonZero<usize>)>,
        tail: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
    },
    Generalize(Vec<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>),
    Specialize(Vec<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>),
    #[allow(clippy::type_complexity)]
    Match {
        auto_captures: HashMap<String, WithLocation<()>>,
        branches: Vec<(
            Vec<WithLocation<String>>,
            WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
            (
                WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
                WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
            ),
            WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
        )>, // pattern, expr
    },
    Invoke {
        func: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
        arg: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
        continuation: Option<Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>>,
        perform_handler: Option<Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>>,
    },
    AtomicOpcode(AtomicOpcode),
    Namespace {
        tag: String,
        expr: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
    },
    Generic {
        generic_vars: Vec<WithLocation<String>>,
        expr: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
        constraint: Box<(
            WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
            WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
        )>,
    },
    Literal(Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>),
    EqOf {
        value: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
    },
    SubOf {
        value: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
    },
    StaticFixPoint {
        param_name: String,
        expr: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
    },
}

impl TypeAst {
    // 把高级抽象语法转换为基础抽象语法
    #[stacksafe::stacksafe]
    pub fn into_basic(
        &self,
        multifile_builder: &mut MultiFileBuilder,
        loc: Option<&SourceLocation>,
    ) -> WithLocation<BasicTypeAst> {
        match self {
            TypeAst::ParseError(span) => {
                panic!("Cannot convert TypeAst::ParseError to BasicTypeAst: {:?}", span)
            }
            TypeAst::Range { ty, min, delta } => WithLocation::new(
                BasicTypeAst::Range {
                    ty: Box::new(ty.into_basic(multifile_builder, ty.location())),
                    min: *min,
                    delta: *delta,
                },
                loc,
            ),
            TypeAst::Float => WithLocation::new(BasicTypeAst::Float, loc),
            TypeAst::Char => WithLocation::new(BasicTypeAst::Char, loc),
            TypeAst::OrderedType(v) => WithLocation::new(BasicTypeAst::OrderedType(*v), loc),
            TypeAst::Top => WithLocation::new(BasicTypeAst::Top, loc),
            TypeAst::Bottom => WithLocation::new(BasicTypeAst::Bottom, loc),
            TypeAst::DiscardPattern => WithLocation::new(BasicTypeAst::Tuple(vec![]), loc), // discard 只允许丢弃unit
            TypeAst::FloatLiteral(v) => WithLocation::new(BasicTypeAst::FloatLiteral(*v), loc),
            TypeAst::CharLiteral(v) => WithLocation::new(BasicTypeAst::CharLiteral(*v), loc),
            TypeAst::Variable(name) => WithLocation::new(BasicTypeAst::Variable(name.clone()), loc),
            TypeAst::Tuple(elements) => WithLocation::new(
                BasicTypeAst::Tuple(
                    elements
                        .iter()
                        .map(|(e, count)| (e.into_basic(multifile_builder, e.location()), *count))
                        .collect(),
                ),
                loc,
            ),
            TypeAst::Cons { head, tail } => WithLocation::new(
                BasicTypeAst::Cons {
                    head: head
                        .iter()
                        .map(|(e, count)| (e.into_basic(multifile_builder, e.location()), *count))
                        .collect(),
                    tail: Box::new(tail.into_basic(multifile_builder, tail.location())),
                },
                loc,
            ),
            TypeAst::List { head, tail } => WithLocation::new(
                BasicTypeAst::List {
                    head: head
                        .iter()
                        .map(|(e, count)| (e.into_basic(multifile_builder, e.location()), *count))
                        .collect(),
                    tail: Box::new(tail.into_basic(multifile_builder, tail.location())),
                },
                loc,
            ),
            TypeAst::Generalize(elements) => WithLocation::new(
                BasicTypeAst::Generalize(
                    elements
                        .iter()
                        .map(|e| e.into_basic(multifile_builder, e.location()))
                        .collect(),
                ),
                loc,
            ),
            TypeAst::Specialize(elements) => WithLocation::new(
                BasicTypeAst::Specialize(
                    elements
                        .iter()
                        .map(|e| e.into_basic(multifile_builder, e.location()))
                        .collect(),
                ),
                loc,
            ),
            TypeAst::Invoke { func, arg, continuation, perform_handler } => WithLocation::new(
                BasicTypeAst::Invoke {
                    func: Box::new(func.into_basic(multifile_builder, func.location())),
                    arg: Box::new(arg.into_basic(multifile_builder, arg.location())),
                    continuation: continuation
                        .as_ref()
                        .map(|c| Box::new(c.into_basic(multifile_builder, c.location()))),
                    perform_handler: perform_handler
                        .as_ref()
                        .map(|h| Box::new(h.into_basic(multifile_builder, h.location()))),
                },
                loc,
            ),
            TypeAst::HandleWith { closure, init_val, handler } => WithLocation::new(
                BasicTypeAst::Apply {
                    func: Box::new(closure.into_basic(multifile_builder, closure.location())),
                    arg: Box::new(init_val.into_basic(multifile_builder, init_val.location())),
                    handler: Some(Box::new(
                        handler.into_basic(multifile_builder, handler.location()),
                    )),
                },
                loc,
            ),
            TypeAst::Expression { binding_patterns, binding_types, body } => {
                // 转换为嵌套的闭包和应用
                let mut expr = body.into_basic(multifile_builder, body.location());
                for ((vars, p, (f, g)), ty) in
                    binding_patterns.iter().rev().zip(binding_types.iter().rev())
                {
                    expr = WithLocation::new(
                        BasicTypeAst::Apply {
                            func: Box::new(WithLocation::new(
                                BasicTypeAst::Match {
                                    branches: vec![(
                                        vars.clone(),
                                        p.into_basic(multifile_builder, p.location()),
                                        (
                                            f.into_basic(multifile_builder, f.location()),
                                            g.into_basic(multifile_builder, g.location()),
                                        ),
                                        expr,
                                    )],
                                },
                                p.location(),
                            )),
                            arg: Box::new(ty.into_basic(multifile_builder, ty.location())),
                            handler: None,
                        },
                        ty.location(),
                    ); // 应用的位置信息不重要
                }
                expr
            }
            TypeAst::Match { branches } => WithLocation::new(
                BasicTypeAst::Match {
                    branches: branches
                        .iter()
                        .map(|(vars, p, (f, g), expr)| {
                            (
                                vars.clone(),
                                p.into_basic(multifile_builder, p.location()),
                                (
                                    f.into_basic(multifile_builder, f.location()),
                                    g.into_basic(multifile_builder, g.location()),
                                ),
                                expr.into_basic(multifile_builder, expr.location()),
                            )
                        })
                        .collect(),
                },
                loc,
            ),
            TypeAst::Apply { func, arg } => WithLocation::new(
                BasicTypeAst::Apply {
                    func: Box::new(func.into_basic(multifile_builder, func.location())),
                    arg: Box::new(arg.into_basic(multifile_builder, arg.location())),
                    handler: None,
                },
                loc,
            ),
            TypeAst::Eq { left, right } => WithLocation::new(
                BasicTypeAst::Apply {
                    func: Box::new(WithLocation::new(
                        BasicTypeAst::Match {
                            branches: vec![
                                (
                                    vec![WithLocation::new("_eq#x".into(), loc)],
                                    WithLocation::new(
                                        BasicTypeAst::Tuple(vec![
                                            (
                                                WithLocation::new(
                                                    BasicTypeAst::Variable("_eq#x".into()),
                                                    loc,
                                                ),
                                                NonZero::new(1).unwrap(),
                                            ),
                                            (
                                                WithLocation::new(
                                                    BasicTypeAst::Variable("_eq#x".into()),
                                                    loc,
                                                ),
                                                NonZero::new(1).unwrap(),
                                            ),
                                        ]),
                                        loc,
                                    ),
                                    (
                                        WithLocation::new(BasicTypeAst::Tuple(vec![]), loc),
                                        WithLocation::new(BasicTypeAst::Tuple(vec![]), loc),
                                    ),
                                    WithLocation::new(
                                        BasicTypeAst::Variable("op#true".into()),
                                        loc,
                                    ),
                                ),
                                (
                                    vec![],
                                    WithLocation::new(BasicTypeAst::Top, loc),
                                    (
                                        WithLocation::new(BasicTypeAst::Tuple(vec![]), loc),
                                        WithLocation::new(BasicTypeAst::Tuple(vec![]), loc),
                                    ),
                                    WithLocation::new(
                                        BasicTypeAst::Variable("op#false".into()),
                                        loc,
                                    ),
                                ),
                            ],
                        },
                        loc,
                    )),
                    arg: Box::new(WithLocation::new(
                        BasicTypeAst::Tuple(vec![
                            (
                                left.into_basic(multifile_builder, left.location()),
                                NonZero::new(1).unwrap(),
                            ),
                            (
                                right.into_basic(multifile_builder, right.location()),
                                NonZero::new(1).unwrap(),
                            ),
                        ]),
                        loc,
                    )),
                    handler: None,
                },
                loc,
            ),
            TypeAst::Neq { left, right } => WithLocation::new(
                BasicTypeAst::Apply {
                    func: Box::new(WithLocation::new(
                        BasicTypeAst::Match {
                            branches: vec![
                                (
                                    vec![WithLocation::new("_neq#x".into(), loc)],
                                    WithLocation::new(
                                        BasicTypeAst::Tuple(vec![
                                            (
                                                WithLocation::new(
                                                    BasicTypeAst::Variable("_neq#x".into()),
                                                    loc,
                                                ),
                                                NonZero::new(1).unwrap(),
                                            ),
                                            (
                                                WithLocation::new(
                                                    BasicTypeAst::Variable("_neq#x".into()),
                                                    loc,
                                                ),
                                                NonZero::new(1).unwrap(),
                                            ),
                                        ]),
                                        loc,
                                    ),
                                    (
                                        WithLocation::new(BasicTypeAst::Tuple(vec![]), loc),
                                        WithLocation::new(BasicTypeAst::Tuple(vec![]), loc),
                                    ),
                                    WithLocation::new(
                                        BasicTypeAst::Variable("op#false".into()),
                                        loc,
                                    ),
                                ),
                                (
                                    vec![],
                                    WithLocation::new(BasicTypeAst::Top, loc),
                                    (
                                        WithLocation::new(BasicTypeAst::Tuple(vec![]), loc),
                                        WithLocation::new(BasicTypeAst::Tuple(vec![]), loc),
                                    ),
                                    WithLocation::new(
                                        BasicTypeAst::Variable("op#true".into()),
                                        loc,
                                    ),
                                ),
                            ],
                        },
                        loc,
                    )),
                    arg: Box::new(WithLocation::new(
                        BasicTypeAst::Tuple(vec![
                            (
                                left.into_basic(multifile_builder, left.location()),
                                NonZero::new(1).unwrap(),
                            ),
                            (
                                right.into_basic(multifile_builder, right.location()),
                                NonZero::new(1).unwrap(),
                            ),
                        ]),
                        loc,
                    )),
                    handler: None,
                },
                loc,
            ),
            TypeAst::Not { value } => WithLocation::new(
                BasicTypeAst::Apply {
                    func: WithLocation::new(BasicTypeAst::Variable("op#not".to_string()), loc)
                        .into(),
                    arg: value.into_basic(multifile_builder, value.location()).into(),
                    handler: None,
                },
                loc,
            ),
            TypeAst::AtomicOpcode(binary_op) => {
                WithLocation::new(BasicTypeAst::AtomicOpcode(binary_op.clone()), loc)
            }
            TypeAst::FixPoint { param_name, expr } => {
                let inner_lambda = WithLocation::new(
                    BasicTypeAst::Match {
                        branches: vec![(
                            vec![WithLocation::new(param_name.clone(), loc)],
                            WithLocation::new(BasicTypeAst::Variable(param_name.clone()), loc),
                            (
                                WithLocation::new(BasicTypeAst::Variable(param_name.clone()), loc),
                                WithLocation::new(BasicTypeAst::Top, loc),
                            ),
                            expr.into_basic(multifile_builder, expr.location()),
                        )],
                    },
                    loc,
                );
                WithLocation::new(
                    BasicTypeAst::Apply {
                        func: Box::new(WithLocation::new(
                            BasicTypeAst::AtomicOpcode(AtomicOpcode::BuildFixPoint),
                            loc,
                        )),
                        arg: Box::new(inner_lambda),
                        handler: None,
                    },
                    loc,
                )
            }
            TypeAst::Namespace { tag, expr } => WithLocation::new(
                BasicTypeAst::Namespace {
                    tag: tag.clone(),
                    expr: Box::new(expr.into_basic(multifile_builder, expr.location())),
                },
                loc,
            ),
            TypeAst::Generic { generic_vars, expr, constraint } => WithLocation::new(
                BasicTypeAst::Generic {
                    generic_vars: generic_vars.clone(),
                    expr: Box::new(expr.into_basic(multifile_builder, expr.location())),
                    constraint: Box::new((
                        constraint.0.into_basic(multifile_builder, constraint.0.location()),
                        constraint.1.into_basic(multifile_builder, constraint.1.location()),
                    )),
                },
                loc,
            ),
            TypeAst::Literal(inner) => WithLocation::new(
                BasicTypeAst::Literal(Box::new(
                    inner.into_basic(multifile_builder, inner.location()),
                )),
                loc,
            ),
            TypeAst::Import(import_path) => {
                // read from file
                let path = PathBuf::from(import_path);
                let path = path.canonicalize().unwrap_or(path);

                match std::fs::read_to_string(&path) {
                    Ok(content) => multifile_builder
                        .build(path, content)
                        .0
                        .map(|r| r.0)
                        .unwrap_or(WithLocation::new(BasicTypeAst::Bottom, loc)),
                    Err(e) => {
                        multifile_builder
                            .errors
                            .push(WithLocation::new(MultiFileBuilderError::IOError(e), loc));
                        WithLocation::new(BasicTypeAst::Bottom, loc)
                    }
                }
            }
            TypeAst::EqOf { value } => WithLocation::new(
                BasicTypeAst::EqOf {
                    value: Box::new(value.into_basic(multifile_builder, value.location())),
                },
                loc,
            ),
            TypeAst::SubOf { value } => WithLocation::new(
                BasicTypeAst::SubOf {
                    value: Box::new(value.into_basic(multifile_builder, value.location())),
                },
                loc,
            ),
            TypeAst::StaticFixPoint { param_name, expr } => WithLocation::new(
                BasicTypeAst::StaticFixPoint {
                    param_name: param_name.clone(),
                    expr: Box::new(expr.into_basic(multifile_builder, expr.location())),
                },
                loc,
            ),
        }
    }

    pub fn collect_errors(&self, errors: &mut Vec<ErrorRecovery<usize, LexerToken, LexicalError>>) {
        match self {
            TypeAst::ParseError(span) => {
                errors.push(span.clone());
            }
            TypeAst::Float
            | TypeAst::Char
            | TypeAst::Top
            | TypeAst::Bottom
            | TypeAst::DiscardPattern
            | TypeAst::FloatLiteral(_)
            | TypeAst::CharLiteral(_)
            | TypeAst::OrderedType(_)
            | TypeAst::Variable(_)
            | TypeAst::Import(_) => {}
            TypeAst::Range { ty, .. } => {
                ty.collect_errors(errors);
            }
            TypeAst::Tuple(elements) => {
                for (elem, _) in elements {
                    elem.collect_errors(errors);
                }
            }
            TypeAst::Generalize(elements) | TypeAst::Specialize(elements) => {
                for elem in elements {
                    elem.collect_errors(errors);
                }
            }
            TypeAst::Invoke { func, arg, continuation, perform_handler } => {
                func.collect_errors(errors);
                arg.collect_errors(errors);
                if let Some(continuation) = continuation {
                    continuation.collect_errors(errors);
                }
                if let Some(perform_handler) = perform_handler {
                    perform_handler.collect_errors(errors);
                }
            }
            TypeAst::HandleWith { closure, init_val, handler: catch } => {
                closure.collect_errors(errors);
                init_val.collect_errors(errors);
                catch.collect_errors(errors);
            }
            TypeAst::Expression { binding_patterns, binding_types, body } => {
                for (_, p, (f, g)) in binding_patterns {
                    p.collect_errors(errors);
                    f.collect_errors(errors);
                    g.collect_errors(errors);
                }
                for ty in binding_types {
                    ty.collect_errors(errors);
                }
                body.collect_errors(errors);
            }
            TypeAst::Match { branches } => {
                for (_, p, (f, g), e) in branches {
                    p.collect_errors(errors);
                    f.collect_errors(errors);
                    g.collect_errors(errors);
                    e.collect_errors(errors);
                }
            }
            TypeAst::Apply { func, arg } => {
                func.collect_errors(errors);
                arg.collect_errors(errors);
            }
            TypeAst::Eq { left, right } | TypeAst::Neq { left, right } => {
                left.collect_errors(errors);
                right.collect_errors(errors);
            }
            TypeAst::Not { value } => {
                value.collect_errors(errors);
            }
            TypeAst::AtomicOpcode(_) => {}
            TypeAst::FixPoint { expr, .. } => {
                expr.collect_errors(errors);
            }
            TypeAst::Namespace { expr, .. } => {
                expr.collect_errors(errors);
            }
            TypeAst::Generic { expr, .. } => {
                expr.collect_errors(errors);
            }
            TypeAst::Literal(inner) => {
                inner.collect_errors(errors);
            }
            TypeAst::Cons { head, tail } => {
                for (elem, _) in head {
                    elem.collect_errors(errors);
                }
                tail.collect_errors(errors);
            }
            TypeAst::List { head, tail } => {
                for (elem, _) in head {
                    elem.collect_errors(errors);
                }
                tail.collect_errors(errors);
            }
            TypeAst::EqOf { value } => {
                value.collect_errors(errors);
            }
            TypeAst::SubOf { value } => {
                value.collect_errors(errors);
            }
            TypeAst::StaticFixPoint { expr, .. } => {
                expr.collect_errors(errors);
            }
        }
    }

    pub fn sanitize(ast: WithLocation<Self>) -> WithLocation<Self> {
        ast.map(|ast| match ast {
            TypeAst::ParseError(_) => TypeAst::Bottom,
            TypeAst::Float
            | TypeAst::Char
            | TypeAst::Top
            | TypeAst::Bottom
            | TypeAst::DiscardPattern
            | TypeAst::FloatLiteral(_)
            | TypeAst::CharLiteral(_)
            | TypeAst::OrderedType(_)
            | TypeAst::Variable(_)
            | TypeAst::Import(_) => ast,
            TypeAst::Range { ty, min, delta } => {
                TypeAst::Range { ty: Box::new(Self::sanitize(*ty)), min, delta }
            }
            TypeAst::Tuple(elements) => TypeAst::Tuple(
                elements.into_iter().map(|(e, count)| (Self::sanitize(e), count)).collect(),
            ),
            TypeAst::Cons { head, tail } => TypeAst::Cons {
                head: head.into_iter().map(|(e, count)| (Self::sanitize(e), count)).collect(),
                tail: Box::new(Self::sanitize(*tail)),
            },
            TypeAst::List { head, tail } => TypeAst::List {
                head: head.into_iter().map(|(e, count)| (Self::sanitize(e), count)).collect(),
                tail: Box::new(Self::sanitize(*tail)),
            },
            TypeAst::Generalize(elements) => {
                TypeAst::Generalize(elements.into_iter().map(Self::sanitize).collect())
            }
            TypeAst::Specialize(elements) => {
                TypeAst::Specialize(elements.into_iter().map(Self::sanitize).collect())
            }
            TypeAst::Invoke { func, arg, continuation, perform_handler } => TypeAst::Invoke {
                func: Box::new(Self::sanitize(*func)),
                arg: Box::new(Self::sanitize(*arg)),
                continuation: continuation.map(|c| Box::new(Self::sanitize(*c))),
                perform_handler: perform_handler.map(|h| Box::new(Self::sanitize(*h))),
            },
            TypeAst::HandleWith { closure, init_val, handler: catch } => TypeAst::HandleWith {
                closure: Box::new(Self::sanitize(*closure)),
                init_val: Box::new(Self::sanitize(*init_val)),
                handler: Box::new(Self::sanitize(*catch)),
            },
            TypeAst::Expression { binding_patterns, binding_types, body } => TypeAst::Expression {
                binding_patterns: binding_patterns
                    .into_iter()
                    .map(|(vars, p, (f, g))| {
                        (vars, Self::sanitize(p), (Self::sanitize(f), Self::sanitize(g)))
                    })
                    .collect(),
                binding_types: binding_types.into_iter().map(Self::sanitize).collect(),
                body: Box::new(Self::sanitize(*body)),
            },
            TypeAst::Match { branches } => TypeAst::Match {
                branches: branches
                    .into_iter()
                    .map(|(vars, p, (f, g), e)| {
                        (
                            vars,
                            Self::sanitize(p),
                            (Self::sanitize(f), Self::sanitize(g)),
                            Self::sanitize(e),
                        )
                    })
                    .collect(),
            },
            TypeAst::Apply { func, arg } => TypeAst::Apply {
                func: Box::new(Self::sanitize(*func)),
                arg: Box::new(Self::sanitize(*arg)),
            },
            TypeAst::Eq { left, right } => TypeAst::Eq {
                left: Box::new(Self::sanitize(*left)),
                right: Box::new(Self::sanitize(*right)),
            },
            TypeAst::Neq { left, right } => TypeAst::Neq {
                left: Box::new(Self::sanitize(*left)),
                right: Box::new(Self::sanitize(*right)),
            },
            TypeAst::Not { value } => TypeAst::Not { value: Box::new(Self::sanitize(*value)) },
            TypeAst::AtomicOpcode(op) => TypeAst::AtomicOpcode(op),
            TypeAst::FixPoint { param_name, expr } => {
                TypeAst::FixPoint { param_name, expr: Box::new(Self::sanitize(*expr)) }
            }
            TypeAst::Namespace { tag, expr } => {
                TypeAst::Namespace { tag, expr: Box::new(Self::sanitize(*expr)) }
            }
            TypeAst::Generic { generic_vars, expr, constraint } => TypeAst::Generic {
                generic_vars,
                expr: Box::new(Self::sanitize(*expr)),
                constraint: Box::new((Self::sanitize(constraint.0), Self::sanitize(constraint.1))),
            },
            TypeAst::Literal(inner) => TypeAst::Literal(Box::new(Self::sanitize(*inner))),
            TypeAst::EqOf { value } => TypeAst::EqOf { value: Box::new(Self::sanitize(*value)) },
            TypeAst::SubOf { value } => TypeAst::SubOf { value: Box::new(Self::sanitize(*value)) },
            TypeAst::StaticFixPoint { param_name, expr } => {
                TypeAst::StaticFixPoint { param_name, expr: Box::new(Self::sanitize(*expr)) }
            }
        })
    }
}

#[derive(Debug)]
pub struct GenericEnv {
    declared: HashMap<String, WithLocation<()>>, // 已声明的模式变量
}

impl Deref for GenericEnv {
    type Target = HashMap<String, WithLocation<()>>;
    fn deref(&self) -> &Self::Target {
        &self.declared
    }
}

impl Default for GenericEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl GenericEnv {
    pub fn new() -> Self {
        GenericEnv { declared: HashMap::new() }
    }

    pub fn extend(&mut self, names: impl IntoIterator<Item = WithLocation<String>>) {
        for name in names {
            if self.declared.contains_key(name.value()) {
                continue; // 重复声明的模式变量忽略
            }
            self.declared.insert(name.value().clone(), WithLocation::new((), name.location()));
        }
    }
}

impl IntoIterator for GenericEnv {
    type Item = (String, WithLocation<()>);
    type IntoIter = std::collections::hash_map::IntoIter<String, WithLocation<()>>;

    fn into_iter(self) -> Self::IntoIter {
        self.declared.into_iter()
    }
}

pub struct FlowResult<'ast> {
    ty: WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>, // flow后的类型
    captures: HashMap<String, WithLocation<()>>,                 // 该类型所捕获的自由变量
}

impl<'ast> FlowResult<'ast> {
    pub fn simple(ty: WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>) -> Self {
        FlowResult { ty: ty.with_payload(FlowedMetaData::default()), captures: HashMap::new() }
    }

    pub fn complex(
        ty: WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
        captures: HashMap<String, WithLocation<()>>,
    ) -> Self {
        FlowResult { ty: ty.with_payload(FlowedMetaData::default()), captures }
    }

    pub fn ty(&self) -> &WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>> {
        &self.ty
    }

    pub fn captures(&self) -> &HashMap<String, WithLocation<()>> {
        &self.captures
    }

    pub fn with_payload(self, payload: FlowedMetaData<'ast>) -> Self {
        FlowResult { ty: self.ty.with_payload(payload), captures: self.captures }
    }
}

impl<'ast> LinearTypeAst<'ast> {
    #[stacksafe::stacksafe]
    pub fn flow(
        &self,
        ctx: &mut ParseContext,
        loc: Option<&SourceLocation>,
        errors: &mut Vec<WithLocation<ParseError<'ast>>>,
    ) -> FlowResult<'ast> {
        match self {
            LinearTypeAst::Range { ty, min, delta } => {
                let ty_res = ty.flow(ctx, ty.location(), errors);
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::Range { ty: Box::new(ty_res.ty), min: *min, delta: *delta },
                        loc,
                    ),
                    ty_res.captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Float => FlowResult::simple(
                WithLocation::new(LinearTypeAst::Float, loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::Char => FlowResult::simple(
                WithLocation::new(LinearTypeAst::Char, loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::OrderedType(v) => FlowResult::simple(
                WithLocation::new(LinearTypeAst::OrderedType(*v), loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::Top => FlowResult::simple(
                WithLocation::new(LinearTypeAst::Top, loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::Bottom => FlowResult::simple(
                WithLocation::new(LinearTypeAst::Bottom, loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::FloatLiteral(v) => FlowResult::simple(
                WithLocation::new(LinearTypeAst::FloatLiteral(*v), loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::CharLiteral(v) => FlowResult::simple(
                WithLocation::new(LinearTypeAst::CharLiteral(*v), loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::Variable(name) => match ctx.use_variable(name) {
                Ok(var_loc) => {
                    let mut captures = HashMap::new();
                    captures.insert(name.clone(), var_loc.clone());
                    FlowResult::complex(
                        WithLocation::new(LinearTypeAst::Variable(name.clone()), loc),
                        captures,
                    )
                    .with_payload(
                        FlowedMetaData::default()
                            .with_reference(Some(var_loc.clone().map(|_| None)))
                            .with_variable_context(ctx.capture()),
                    )
                }
                Err(context_error) => match context_error {
                    ContextError::NotDeclared(name) => {
                        errors.push(WithLocation::new(
                            ParseError::UseBeforeDeclaration(
                                WithLocation::new(self.clone(), loc),
                                name,
                            ),
                            loc,
                        ));
                        FlowResult::simple(
                            WithLocation::new(LinearTypeAst::Bottom, loc).with_payload(
                                FlowedMetaData::default().with_variable_context(ctx.capture()),
                            ),
                        )
                    }
                    _ => unreachable!(),
                },
            },
            LinearTypeAst::Tuple(elements) => {
                let mut new_elements = Vec::new();
                let mut all_captures = HashMap::new();
                for (elem, count) in elements {
                    let res = elem.flow(ctx, elem.location(), errors);
                    new_elements.push((res.ty, *count));
                    all_captures.extend(res.captures);
                }
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::Tuple(new_elements), loc),
                    all_captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::List { head, tail } => {
                let mut new_heads = Vec::new();
                let mut all_captures = HashMap::new();
                for (h, count) in head {
                    let res = h.flow(ctx, h.location(), errors);
                    new_heads.push((res.ty, *count));
                    all_captures.extend(res.captures);
                }
                let tail_res = tail.flow(ctx, tail.location(), errors);
                all_captures.extend(tail_res.captures);
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::List { head: new_heads, tail: Box::new(tail_res.ty) },
                        loc,
                    ),
                    all_captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Cons { head, tail } => {
                let mut new_heads = Vec::new();
                let mut all_captures = HashMap::new();
                for (h, count) in head {
                    let res = h.flow(ctx, h.location(), errors);
                    new_heads.push((res.ty, *count));
                    all_captures.extend(res.captures);
                }
                let tail_res = tail.flow(ctx, tail.location(), errors);
                all_captures.extend(tail_res.captures);
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::Cons { head: new_heads, tail: Box::new(tail_res.ty) },
                        loc,
                    ),
                    all_captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Generalize(types) => {
                let mut new_types = Vec::new();
                let mut all_captures = HashMap::new();
                for ty in types {
                    let res = ty.flow(ctx, ty.location(), errors);
                    new_types.push(res.ty);
                    all_captures.extend(res.captures);
                }
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::Generalize(new_types), loc),
                    all_captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Specialize(types) => {
                let mut new_types = Vec::new();
                let mut all_captures = HashMap::new();
                for ty in types {
                    let res = ty.flow(ctx, ty.location(), errors);
                    new_types.push(res.ty);
                    all_captures.extend(res.captures);
                }
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::Specialize(new_types), loc),
                    all_captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Invoke { func, arg, continuation, perform_handler } => {
                let func_res = func.flow(ctx, func.location(), errors);
                let arg_res = arg.flow(ctx, arg.location(), errors);
                let cont_res = continuation
                    .as_ref()
                    .map(|continuation| continuation.flow(ctx, continuation.location(), errors));
                let perform_handler_res = perform_handler.as_ref().map(|perform_handler| {
                    perform_handler.flow(ctx, perform_handler.location(), errors)
                });
                let mut all_captures = func_res.captures;
                all_captures.extend(arg_res.captures);
                if let Some(cont_res) = &cont_res {
                    all_captures.extend(cont_res.captures.clone());
                }
                if let Some(perform_handler_res) = &perform_handler_res {
                    all_captures.extend(perform_handler_res.captures.clone());
                }

                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::Invoke {
                            func: Box::new(func_res.ty),
                            arg: Box::new(arg_res.ty),
                            continuation: cont_res.map(|r| Box::new(r.ty)),
                            perform_handler: perform_handler_res.map(|r| Box::new(r.ty)),
                        },
                        loc,
                    ),
                    all_captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::AtomicOpcode(atomic_opcode) => FlowResult::simple(
                WithLocation::new(LinearTypeAst::AtomicOpcode(atomic_opcode.clone()), loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::Namespace { tag, expr } => {
                let expr_res = expr.flow(ctx, expr.location(), errors);
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::Namespace { tag: tag.clone(), expr: Box::new(expr_res.ty) },
                        loc,
                    ),
                    expr_res.captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Generic { generic_vars, expr, constraint } => {
                ctx.enter_generic_scope();
                for name in generic_vars {
                    ctx.declare_variable(name.value().clone(), name.location())
                        .unwrap_or_else(|e| match e {
                            ContextError::EmptyContext => {
                                panic!(
                                    "Internal error: Context should not be empty when declaring a variable"
                                );
                            }
                            ContextError::NotDeclared(_) => unreachable!(),
                            ContextError::NotUsed(v) => {
                                errors.push(WithLocation::new(
                                    ParseError::UnusedVariable(
                                        WithLocation::new(self.clone(), name.location()),
                                        v,
                                    ),
                                    loc,
                                ));
                            }
                        });
                }
                let mut captures = HashMap::new();

                let (f, g) = constraint.as_ref();

                let mut expr_res = expr.flow(ctx, expr.location(), errors);
                let mut f_res = f.flow(ctx, f.location(), errors);
                let mut g_res = g.flow(ctx, g.location(), errors);
                for name in generic_vars {
                    expr_res.captures.remove(name.value()); // 移除掉泛型变量，因为它们不是自由变量
                    f_res.captures.remove(name.value());
                    g_res.captures.remove(name.value());
                }
                captures.extend(expr_res.captures);
                captures.extend(f_res.captures);
                captures.extend(g_res.captures);

                match ctx.exit_scope() {
                    Ok(_) => {}
                    Err(ContextError::EmptyContext) => {
                        panic!("Internal error: Context should not be empty when exiting a scope");
                    }
                    Err(ContextError::NotDeclared(_)) => unreachable!(),
                    Err(ContextError::NotUsed(v)) => {
                        errors.push(WithLocation::new(
                            ParseError::UnusedVariable(WithLocation::new(self.clone(), loc), v),
                            loc,
                        ));
                    }
                }
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::Generic {
                            generic_vars: generic_vars.clone(),
                            expr: Box::new(expr_res.ty),
                            constraint: Box::new((f_res.ty, g_res.ty)),
                        },
                        loc,
                    ),
                    captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::StaticFixPoint { param_name, expr } => {
                // 静态不动点类型的处理与普通不动点类型类似，但需要在ctx中声明param_name
                ctx.enter_fixpoint_scope(param_name.clone(), loc);
                let expr_res = expr.flow(ctx, expr.location(), errors);
                match ctx.exit_scope() {
                    Ok(_) => {}
                    Err(ContextError::EmptyContext) => {
                        panic!("Internal error: Context should not be empty when exiting a scope");
                    }
                    Err(ContextError::NotDeclared(_)) => unreachable!(),
                    Err(ContextError::NotUsed(v)) => {
                        errors.push(WithLocation::new(
                            ParseError::UnusedVariable(WithLocation::new(self.clone(), loc), v),
                            loc,
                        ));
                    }
                }
                let mut captures = expr_res.captures;
                captures.remove(param_name); // 移除掉不动点参数，因为它是递归定义的参数，不应当被视为捕获的自由变量
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::StaticFixPoint {
                            param_name: param_name.clone(),
                            expr: Box::new(expr_res.ty),
                        },
                        loc,
                    ),
                    captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Match { auto_captures, branches } => {
                let mut new_branches = Vec::new();
                let mut all_captures = HashMap::new();
                let mut all_body_captures = HashMap::new();
                for (params, p, (f, g), body) in branches {
                    // 处理模式
                    ctx.enter_generic_scope();
                    for name in params {
                        match ctx.declare_variable(name.value().clone(), name.location()) {
                            Ok(_) => {}
                            Err(ContextError::EmptyContext) => {
                                panic!(
                                    "Internal error: Context should not be empty when declaring a variable"
                                );
                            }
                            Err(ContextError::NotDeclared(_)) => unreachable!(),
                            Err(ContextError::NotUsed(v)) => {
                                errors.push(WithLocation::new(
                                    ParseError::UnusedVariable(
                                        WithLocation::new(self.clone(), name.location()),
                                        v,
                                    ),
                                    loc,
                                ));
                            }
                        }
                    }
                    let mut pattern_res = p.flow(ctx, p.location(), errors);
                    let mut f_res = f.flow(ctx, f.location(), errors);
                    let mut g_res = g.flow(ctx, g.location(), errors);
                    for name in params {
                        pattern_res.captures.remove(name.value()); // 移除掉模式变量，因为它们不是自由变量
                        f_res.captures.remove(name.value());
                        g_res.captures.remove(name.value());
                    }
                    match ctx.exit_scope() {
                        Ok(_) => {}
                        Err(ContextError::EmptyContext) => {
                            panic!(
                                "Internal error: Context should not be empty when exiting a scope"
                            );
                        }
                        Err(ContextError::NotDeclared(_)) => unreachable!(),
                        Err(ContextError::NotUsed(v)) => {
                            errors.push(WithLocation::new(
                                ParseError::UnusedVariable(WithLocation::new(self.clone(), loc), v),
                                loc,
                            ));
                        }
                    }
                    ctx.enter_scope();
                    for name in params {
                        match ctx.declare_variable(name.value().clone(), name.location()) {
                            Ok(_) => {}
                            Err(ContextError::EmptyContext) => {
                                panic!(
                                    "Internal error: Context should not be empty when declaring a variable"
                                );
                            }
                            Err(ContextError::NotDeclared(_)) => unreachable!(),
                            Err(ContextError::NotUsed(v)) => {
                                errors.push(WithLocation::new(
                                    ParseError::UnusedVariable(
                                        WithLocation::new(self.clone(), name.location()),
                                        v,
                                    ),
                                    loc,
                                ));
                            }
                        }
                    }
                    for (var, var_loc) in auto_captures {
                        match ctx.declare_variable(var.clone(), var_loc.location()) {
                            Ok(_) => {}
                            Err(ContextError::EmptyContext) => {
                                panic!(
                                    "Internal error: Context should not be empty when declaring a variable"
                                );
                            }
                            Err(ContextError::NotDeclared(_)) => unreachable!(),
                            Err(ContextError::NotUsed(v)) => {
                                errors.push(WithLocation::new(
                                    ParseError::UnusedVariable(
                                        WithLocation::new(self.clone(), var_loc.location()),
                                        v,
                                    ),
                                    loc,
                                ));
                            }
                        }
                    }
                    let mut body_res = body.flow(ctx, body.location(), errors); // 分支体不允许出现模式变量
                    for name in params {
                        body_res.captures.remove(name.value()); // 移除掉模式变量，因为它们不是自由变量
                    }
                    match ctx.exit_scope() {
                        Ok(_) => {}
                        Err(ContextError::EmptyContext) => {
                            panic!(
                                "Internal error: Context should not be empty when exiting a scope"
                            );
                        }
                        Err(ContextError::NotDeclared(_)) => unreachable!(),
                        Err(ContextError::NotUsed(v)) => {
                            errors.push(WithLocation::new(
                                ParseError::UnusedVariable(WithLocation::new(self.clone(), loc), v),
                                loc,
                            ));
                        }
                    }
                    new_branches.push((
                        params.clone(),
                        pattern_res.ty,
                        (f_res.ty, g_res.ty),
                        body_res.ty,
                    ));

                    all_captures.extend(pattern_res.captures);
                    all_captures.extend(f_res.captures);
                    all_captures.extend(g_res.captures);
                    all_captures.extend(body_res.captures.clone());
                    all_body_captures.extend(body_res.captures);
                }
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::Match {
                            auto_captures: all_body_captures,
                            branches: new_branches,
                        },
                        loc,
                    ),
                    all_captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Literal(inner) => {
                let inner_res = inner.flow(ctx, inner.location(), errors);
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::Literal(Box::new(inner_res.ty)), loc),
                    inner_res.captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::EqOf { value } => {
                let value_res = value.flow(ctx, value.location(), errors);
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::EqOf { value: Box::new(value_res.ty) }, loc),
                    value_res.captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::SubOf { value } => {
                let value_res = value.flow(ctx, value.location(), errors);
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::SubOf { value: Box::new(value_res.ty) }, loc),
                    value_res.captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
        }
    }
}

pub struct BuildResult<T: GcAllocObject<T, Inner = Type<T>>> {
    ty: Type<T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for BuildResult<T> {
    fn clone(&self) -> Self {
        BuildResult { ty: self.ty.clone() }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> BuildResult<T> {
    pub fn simple(ty: Type<T>) -> Self {
        BuildResult { ty }
    }

    pub fn fold(results: Vec<Self>) -> Vec<Type<T>> {
        let mut types = Vec::new();
        for res in results {
            types.push(res.ty);
        }
        types
    }

    pub fn ty(&self) -> &Type<T> {
        &self.ty
    }
}

#[allow(clippy::type_complexity)]
impl<'ast> LinearTypeAst<'ast> {
    #[stacksafe::stacksafe]
    pub fn to_type<'roots, T: GcAllocObject<T, Inner = Type<T>>>(
        &self,
        ctx: &mut BuildContext<T>,
        gc: &mut GC<T>,
        roots: &'roots mut RootStack<Type<T>, T>,
        loc: Option<&SourceLocation>,
    ) -> Result<BuildResult<T>, Result<TypeError<Type<T>, T>, ParseError<'ast>>> {
        match self {
            LinearTypeAst::Range { ty, min, delta } => {
                let ty_result = ty.to_type(ctx, gc, roots, ty.location())?;
                let repeat_count = delta.unwrap_or(1);
                Ok(BuildResult::simple(if *min == 0 && delta.is_none() {
                    // 空序列
                    Sequence::unit(loc.cloned().map(Arc::new))
                } else if *min == 0 {
                    // 重复序列，从0开始
                    Sequence::new_repeat(
                        vec![(ty_result.ty.clone(), NonZero::new(repeat_count).unwrap())],
                        Sequence::unit(loc.cloned().map(Arc::new)),
                        loc.cloned().map(Arc::new),
                    )
                } else {
                    // min > 0, 使用nature_number
                    Sequence::nature_number(*min, ty_result.ty.clone(), loc.cloned().map(Arc::new))
                }))
            }
            LinearTypeAst::Float => Ok(BuildResult::simple(Float::new(loc.cloned().map(Arc::new)))),
            LinearTypeAst::Char => {
                Ok(BuildResult::simple(Character::new(loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::OrderedType(v) => {
                Ok(BuildResult::simple(OrderedType::new(*v, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::Top => {
                Ok(BuildResult::simple(TypeBound::top(loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::Bottom => {
                Ok(BuildResult::simple(TypeBound::bottom(loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::FloatLiteral(v) => {
                Ok(BuildResult::simple(FloatValue::new(*v, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::CharLiteral(v) => {
                Ok(BuildResult::simple(CharacterValue::new(*v, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::Variable(var) => {
                if let Some(ty) = ctx.lookup(var) {
                    Ok(BuildResult::simple(ty))
                } else {
                    Err(Err(ParseError::UseBeforeDeclaration(
                        WithLocation::new(self.clone(), loc),
                        var.clone(),
                    )))
                }
            }
            LinearTypeAst::Tuple(basic_type_asts) => {
                let mut types = Vec::new();
                for (bta, _count) in basic_type_asts {
                    types.push(bta.to_type(ctx, gc, roots, bta.location())?);
                }
                let types = BuildResult::fold(types);
                let types = types
                    .into_iter()
                    .zip(basic_type_asts.iter())
                    .map(|(t, (_bta, count))| (t, *count))
                    .collect::<Vec<_>>();
                Ok(BuildResult::simple(Sequence::new_simple(types, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::List { head, tail } => {
                let mut results = Vec::new();
                for (h, _count) in head {
                    results.push(h.to_type(ctx, gc, roots, h.location())?);
                }
                let tail_res = tail.to_type(ctx, gc, roots, tail.location())?;
                results.push(tail_res);
                let types = BuildResult::fold(results);
                // last element is the tail
                let tail_ty = types.last().unwrap().clone();
                let prefix_types = types[..types.len() - 1].to_vec();
                let prefix = prefix_types
                    .into_iter()
                    .zip(head.iter())
                    .map(|(t, (_h, count))| (t, *count))
                    .collect::<Vec<_>>();
                Ok(BuildResult::simple(Sequence::new_repeat(
                    prefix,
                    tail_ty,
                    loc.cloned().map(Arc::new),
                )))
            }
            LinearTypeAst::Cons { head, tail } => {
                let mut results = Vec::new();
                for (h, _count) in head {
                    results.push(h.to_type(ctx, gc, roots, h.location())?);
                }
                let tail_res = tail.to_type(ctx, gc, roots, tail.location())?;
                results.push(tail_res);
                let types = BuildResult::fold(results);
                // last element is the tail
                let tail_ty = types.last().unwrap().clone();
                let prefix_types = types[..types.len() - 1].to_vec();
                let prefix = prefix_types
                    .into_iter()
                    .zip(head.iter())
                    .map(|(t, (_h, count))| (t, *count))
                    .collect::<Vec<_>>();
                Ok(BuildResult::simple(Sequence::new_cons(
                    prefix,
                    tail_ty,
                    loc.cloned().map(Arc::new),
                )))
            }
            LinearTypeAst::Generalize(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(ctx, gc, roots, bta.location())?);
                }
                let types = BuildResult::fold(types);
                Ok(BuildResult::simple(AnyOf::new(types, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::Specialize(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(ctx, gc, roots, bta.location())?);
                }
                let types = BuildResult::fold(types);
                Ok(BuildResult::simple(AllOf::new(types, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::Invoke { func, arg, continuation, perform_handler } => {
                let func_type = func.to_type(ctx, gc, roots, func.location())?;
                let arg_type = arg.to_type(ctx, gc, roots, arg.location())?;
                let continuation_type = match continuation {
                    Some(continuation) => {
                        Some(continuation.to_type(ctx, gc, roots, continuation.location())?)
                    }
                    None => None,
                };
                let perform_handler_type = match perform_handler {
                    Some(perform_handler) => {
                        Some(perform_handler.to_type(ctx, gc, roots, perform_handler.location())?)
                    }
                    None => None,
                };
                let mut fold_vec = vec![func_type, arg_type];
                if let Some(ct) = &continuation_type {
                    fold_vec.push(ct.clone());
                }
                if let Some(rht) = &perform_handler_type {
                    fold_vec.push(rht.clone());
                }
                let types = BuildResult::fold(fold_vec);
                Ok(BuildResult::simple(Invoke::new(
                    &types[0],
                    &types[1],
                    continuation_type.as_ref().map(|t| &t.ty),
                    perform_handler_type.as_ref().map(|t| &t.ty),
                    loc.cloned().map(Arc::new),
                )))
            }
            LinearTypeAst::Match { auto_captures, branches } => {
                let auto_captures = auto_captures
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<Vec<(String, WithLocation<()>)>>();
                let mut closure_env: Vec<(Arc<str>, EnvironmentVarState<Type<T>, T>)> = Vec::new();
                for (var, capture_loc) in &auto_captures {
                    if let Some(from) = ctx.lookup_function_env(var) {
                        closure_env.push((Arc::from(var.as_str()), from)) // 这里似乎是有点问题的
                    } else {
                        return Err(Err(ParseError::UseBeforeDeclaration(
                            WithLocation::new(self.clone(), capture_loc.location()),
                            var.clone(),
                        )));
                    }
                }
                let mut new_branches = Vec::new();
                for (params, p, (f, g), body) in branches {
                    let patterns = params
                        .iter()
                        .map(|name| (name.value().clone(), name.as_ref().map(|_| ())))
                        .collect::<HashMap<_, _>>();
                    ctx.enter_layer(BuildContextLayer::GenericBinding(patterns.clone()));
                    let pattern_type: BuildResult<T> = p.to_type(ctx, gc, roots, p.location())?; // 模式现在允许捕获环境变量
                    let f_type = f.to_type(ctx, gc, roots, f.location())?;
                    let g_type = g.to_type(ctx, gc, roots, g.location())?;
                    ctx.exit_layer();

                    ctx.enter_layer(BuildContextLayer::Function {
                        patterns: patterns.clone(),
                        captures: auto_captures.iter().cloned().collect(),
                    });
                    let body_type = body.to_type(ctx, gc, roots, body.location())?;
                    ctx.exit_layer();
                    new_branches.push((
                        closure_env.clone(),
                        Constraint::new_constraint(
                            params.iter().map(|s| s.value()).cloned(),
                            pattern_type.ty,
                            (f_type.ty, g_type.ty),
                            loc.cloned().map(Arc::new),
                        ),
                        body_type.ty,
                    ));
                }
                Ok(BuildResult::simple(Closure::new(new_branches, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::StaticFixPoint { param_name, expr } => {
                let placeholder = FixPoint::new_placeholder(gc, roots);
                ctx.enter_layer(BuildContextLayer::FixPoint(
                    param_name.clone(),
                    placeholder.clone(),
                ));
                let expr_type = expr.to_type(ctx, gc, roots, expr.location())?;
                ctx.exit_layer();
                as_type!(&placeholder, Type::FixPoint).set(expr_type.ty()).map_err(Ok)?;
                Ok(BuildResult::simple(placeholder))
            }
            LinearTypeAst::AtomicOpcode(atomic_opcode) => Ok(BuildResult::simple(Opcode::new(
                match atomic_opcode {
                    AtomicOpcode::Opcode => OpcodeKind::Opcode,
                    AtomicOpcode::Add => OpcodeKind::Add,
                    AtomicOpcode::Sub => OpcodeKind::Sub,
                    AtomicOpcode::Mul => OpcodeKind::Mul,
                    AtomicOpcode::Div => OpcodeKind::Div,
                    AtomicOpcode::Mod => OpcodeKind::Mod,
                    AtomicOpcode::Less => OpcodeKind::Less,
                    AtomicOpcode::Greater => OpcodeKind::Greater,
                    AtomicOpcode::Neg => OpcodeKind::Neg,
                    AtomicOpcode::Is => OpcodeKind::Is,
                    AtomicOpcode::Set => OpcodeKind::Set,
                    AtomicOpcode::BuildFixPoint => OpcodeKind::BuildFixPoint,
                    AtomicOpcode::IO(v) => OpcodeKind::IO(v.clone().into()),
                },
                loc.cloned().map(Arc::new),
            ))),
            LinearTypeAst::Namespace { tag, expr } => {
                let expr_type = expr.to_type(ctx, gc, roots, expr.location())?;
                Ok(BuildResult::simple(Namespace::new(
                    tag.clone(),
                    &expr_type.ty,
                    loc.cloned().map(Arc::new),
                )))
            }
            LinearTypeAst::Generic { generic_vars, expr, constraint } => {
                let bindings = generic_vars
                    .iter()
                    .map(|name| (name.value().clone(), name.as_ref().map(|_| ())))
                    .collect::<HashMap<_, _>>();

                ctx.enter_layer(BuildContextLayer::GenericBinding(bindings.clone()));
                let expr_type = expr.to_type(ctx, gc, roots, expr.location())?;
                let (f, g) = constraint.as_ref();
                let f_type = f.to_type(ctx, gc, roots, f.location())?;
                let g_type = g.to_type(ctx, gc, roots, g.location())?;
                ctx.exit_layer();

                Ok(BuildResult::simple(Constraint::new(
                    generic_vars.iter().map(|s| s.value()).cloned(),
                    expr_type.ty,
                    (f_type.ty, g_type.ty),
                    loc.cloned().map(Arc::new),
                )))
            }
            LinearTypeAst::Literal(inner) => {
                let inner_type = inner.to_type(ctx, gc, roots, inner.location())?;
                Ok(BuildResult::simple(Lazy::new(&inner_type.ty, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::EqOf { value } => {
                let value_type = value.to_type(ctx, gc, roots, value.location())?;
                Ok(BuildResult::simple(EqOf::new(&value_type.ty, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::SubOf { value } => {
                let value_type = value.to_type(ctx, gc, roots, value.location())?;
                Ok(BuildResult::simple(SubOf::new(&value_type.ty, loc.cloned().map(Arc::new))))
            }
        }
    }
}
