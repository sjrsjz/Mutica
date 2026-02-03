use crate::parser::lexer::{LexerToken, LexicalError};
use crate::parser::{
    BuildContext, BuildContextLayer, ContextError, MultiFileBuilder, MultiFileBuilderError,
    ParseContext, ParseError, SourceLocation, WithLocation, with_loc, with_no_loc,
};
use core::panic;
use lalrpop_util::ErrorRecovery;
use mutica_core::arc_gc::gc::GC;
use mutica_core::as_type;
use mutica_core::smallvec::SmallVec;
use mutica_core::types::allof::AllOf;
use mutica_core::types::anyof::AnyOf;
use mutica_core::types::character::Character;
use mutica_core::types::character_value::CharacterValue;
use mutica_core::types::closure::Closure;
use mutica_core::types::constraint::Constraint;
use mutica_core::types::fixpoint::FixPoint;
use mutica_core::types::float::Float;
use mutica_core::types::float_value::FloatValue;
use mutica_core::types::invoke::Invoke;
use mutica_core::types::lambda::Lambda;
use mutica_core::types::lazy::Lazy;
use mutica_core::types::mutable::Mutable;
use mutica_core::types::namespace::Namespace;
use mutica_core::types::natural_number::NaturalNumber;
use mutica_core::types::natural_number_set::NaturalNumberSet;
use mutica_core::types::opcode::{Opcode, OpcodeKind};
use mutica_core::types::sequence::Sequence;
use mutica_core::types::subof::SubOf;
use mutica_core::types::unify::capture_env::{CaptureEnv, CaptureEnvList, CaptureOrigin};
use mutica_core::types::{GcAllocObject, Type, TypeError};
use mutica_core::util::rootstack::RootStack;
use std::collections::HashMap;
use std::num::NonZero;
use std::ops::{Deref, Range};
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
    Assign,
    SetFixPoint,
    BuildFixPoint,
    IO(String),
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub enum GenericPattern {
    Standard {
        pattern: WithLocation<TypeAst>,
        constraint: Vec<(WithLocation<String>, WithLocation<TypeAst>)>,
    },
    AutoBind {
        pattern: WithLocation<TypeAst>,
    },
}

impl Clone for GenericPattern {
    #[stacksafe::stacksafe]
    fn clone(&self) -> Self {
        match self {
            GenericPattern::Standard { pattern, constraint } => GenericPattern::Standard {
                pattern: pattern.clone(),
                constraint: constraint.clone(),
            },
            GenericPattern::AutoBind { pattern } => {
                GenericPattern::AutoBind { pattern: pattern.clone() }
            }
        }
    }
}

pub enum LineKind {
    SideEffect(Range<usize>, WithLocation<TypeAst>), // expr;
    Let(Range<usize>, GenericPattern, WithLocation<TypeAst>), // let pattern = expr;
    Declare(Range<usize>, GenericPattern),           // let pattern;
    CustomLet(Range<usize>, WithLocation<String>, GenericPattern, WithLocation<TypeAst>), // @func pattern = expr;
    CustomSideEffect(Range<usize>, WithLocation<String>, WithLocation<TypeAst>), // @func expr;
    Extend(Range<usize>, WithLocation<String>, WithLocation<TypeAst>), // extend label: value;
    Loop(Range<usize>, WithLocation<String>, GenericPattern, WithLocation<TypeAst>), // loop label: pattern = value;
    HandleWithAndLet(Range<usize>, GenericPattern, WithLocation<TypeAst>, WithLocation<TypeAst>), // handle pattern = expr with handler;
    HandleWith(Range<usize>, WithLocation<TypeAst>), // handle with handler;
}

impl LineKind {
    /// Desugar a sequence of lines into a single TypeAst expression
    /// Processes statements from right to left (last to first), building nested expressions
    pub fn desugar_lines(
        lines: Vec<LineKind>,
        final_expr: WithLocation<TypeAst>,
        source: &Arc<mutica_core::util::source_info::SourceFile>,
    ) -> WithLocation<TypeAst> {
        lines.into_iter().rfold(final_expr, |acc, line| {
            match line {
                // expr; rest  =>  (() => rest)(expr)
                LineKind::SideEffect(span, expr) => {
                    let branch = GenericPattern::Standard {
                        pattern: with_no_loc(TypeAst::DiscardPattern),
                        constraint: vec![],
                    };
                    with_loc(
                        TypeAst::Apply {
                            func: Box::new(with_no_loc(TypeAst::Match {
                                branches: vec![(branch, acc)],
                            })),
                            arg: Box::new(expr),
                            auto_cps: true,
                        },
                        source,
                        span,
                    )
                }

                // let pattern = value; rest  =>  (pattern => rest)(value)
                LineKind::Let(span, pattern, value) => with_loc(
                    TypeAst::Apply {
                        func: Box::new(with_no_loc(TypeAst::Match {
                            branches: vec![(pattern, acc)],
                        })),
                        arg: Box::new(value),
                        auto_cps: true,
                    },
                    source,
                    span,
                ),

                // let pattern; rest  =>  pattern => rest
                LineKind::Declare(span, pattern) => {
                    with_loc(TypeAst::Match { branches: vec![(pattern, acc)] }, source, span)
                }

                // @func pattern = value; rest  =>  func(pattern => rest)(value)
                LineKind::CustomLet(span, func, pattern, value) => with_loc(
                    TypeAst::Apply {
                        func: Box::new(with_no_loc(TypeAst::Apply {
                            func: Box::new(with_no_loc(TypeAst::Variable(func.clone()))),
                            arg: Box::new(with_no_loc(TypeAst::Match {
                                branches: vec![(pattern, acc)],
                            })),
                            auto_cps: true,
                        })),
                        arg: Box::new(value),
                        auto_cps: true,
                    },
                    source,
                    span,
                ),

                // @func expr; rest  =>  func(() => rest)(expr)
                LineKind::CustomSideEffect(span, func, expr) => {
                    let branch = GenericPattern::Standard {
                        pattern: with_no_loc(TypeAst::DiscardPattern),
                        constraint: vec![],
                    };
                    with_loc(
                        TypeAst::Apply {
                            func: Box::new(with_no_loc(TypeAst::Apply {
                                func: Box::new(with_no_loc(TypeAst::Variable(func.clone()))),
                                arg: Box::new(with_no_loc(TypeAst::Match {
                                    branches: vec![(branch, acc)],
                                })),
                                auto_cps: true,
                            })),
                            arg: Box::new(expr),
                            auto_cps: true,
                        },
                        source,
                        span,
                    )
                }

                // extend label: value; rest  =>  let label: any = label + value; rest
                LineKind::Extend(span, label, value) => {
                    let branch = GenericPattern::Standard {
                        pattern: label.clone().map(|_| TypeAst::Variable(label.clone())),
                        constraint: vec![(label.clone(), with_no_loc(TypeAst::Wildcard))],
                    };
                    let binding_value = with_no_loc(TypeAst::Apply {
                        func: Box::new(with_no_loc(TypeAst::Variable(with_no_loc(
                            "op#add".to_string(),
                        )))),
                        arg: Box::new(with_no_loc(TypeAst::Tuple(vec![
                            (value, NonZero::new(1).unwrap()),
                            (
                                label.clone().map(|_| TypeAst::Variable(label.clone())),
                                NonZero::new(1).unwrap(),
                            ),
                        ]))),
                        auto_cps: true,
                    });
                    with_loc(
                        TypeAst::Apply {
                            func: Box::new(with_no_loc(TypeAst::Match {
                                branches: vec![(branch, acc)],
                            })),
                            arg: Box::new(binding_value),
                            auto_cps: true,
                        },
                        source,
                        span,
                    )
                }

                // loop label: pattern = value; rest  =>  fixpoint(label, pattern => rest)(value)
                LineKind::Loop(span, label, pattern, value) => with_loc(
                    TypeAst::Apply {
                        func: Box::new(with_no_loc(TypeAst::FixPoint {
                            param_name: label.clone(),
                            expr: Box::new(with_no_loc(TypeAst::Match {
                                branches: vec![(pattern, acc)],
                            })),
                        })),
                        arg: Box::new(value),
                        auto_cps: true,
                    },
                    source,
                    span,
                ),

                // handle pattern = value with handler; rest
                LineKind::HandleWithAndLet(span, pattern, value, handler) => with_loc(
                    TypeAst::HandleWith {
                        closure: Box::new(with_no_loc(TypeAst::Match {
                            branches: vec![(pattern, acc)],
                        })),
                        init_val: Box::new(value),
                        handler: Box::new(handler),
                    },
                    source,
                    span,
                ),

                // handle with handler; rest
                LineKind::HandleWith(span, handler) => with_loc(
                    TypeAst::HandleWith {
                        closure: Box::new(with_no_loc(TypeAst::Match {
                            branches: vec![(
                                GenericPattern::Standard {
                                    pattern: with_no_loc(TypeAst::DiscardPattern),
                                    constraint: vec![],
                                },
                                acc,
                            )],
                        })),
                        init_val: Box::new(with_no_loc(TypeAst::DiscardPattern)),
                        handler: Box::new(handler),
                    },
                    source,
                    span,
                ),
            }
        })
    }
}

#[derive(Debug)]
pub enum TypeAst {
    ParseError(ErrorRecovery<usize, LexerToken, LexicalError>),
    Import(String),
    Range {
        ty: Box<WithLocation<TypeAst>>,
        min: usize,
        delta: Option<usize>,
    },
    NaturalNumberSet,
    Float,
    Char,
    Lambda {
        patterns: Vec<GenericPattern>, // 参数模式
    },
    Wildcard,
    DiscardPattern,
    NaturalNumberLiteral(usize),
    FloatLiteral(f64),
    CharLiteral(char),
    Variable(WithLocation<String>),
    Tuple(Vec<(WithLocation<TypeAst>, NonZero<usize>)>),
    Cons {
        head: Vec<(WithLocation<TypeAst>, NonZero<usize>)>,
        tail: Box<WithLocation<TypeAst>>,
    },
    List {
        head: Vec<(WithLocation<TypeAst>, NonZero<usize>)>,
        tail: Box<WithLocation<TypeAst>>,
    },
    AnyOf(Vec<WithLocation<TypeAst>>),
    AllOf(Vec<WithLocation<TypeAst>>),
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
    Match {
        branches: Vec<(GenericPattern, WithLocation<TypeAst>)>, // pattern, expr
    },
    Bind {
        var: WithLocation<String>,
        expr: Box<WithLocation<TypeAst>>,
    },
    Apply {
        func: Box<WithLocation<TypeAst>>,
        arg: Box<WithLocation<TypeAst>>,
        auto_cps: bool,
    },
    AtomicOpcode(AtomicOpcode),
    FixPoint {
        param_name: WithLocation<String>,
        expr: Box<WithLocation<TypeAst>>,
    },
    StaticFixPoint {
        param_name: WithLocation<String>,
        expr: Box<WithLocation<TypeAst>>,
    },
    Namespace {
        tag: WithLocation<String>,
        expr: Box<WithLocation<TypeAst>>,
    },
    Generic(Box<GenericPattern>),
    Lazy(Box<WithLocation<TypeAst>>),
    Mutable {
        value: Box<WithLocation<TypeAst>>,
    },
    SubOf {
        value: Box<WithLocation<TypeAst>>,
    },
}

impl Clone for TypeAst {
    #[stacksafe::stacksafe]
    fn clone(&self) -> Self {
        match self {
            TypeAst::ParseError(e) => TypeAst::ParseError(e.clone()),
            TypeAst::Import(s) => TypeAst::Import(s.clone()),
            TypeAst::Range { ty, min, delta } => {
                TypeAst::Range { ty: ty.clone(), min: *min, delta: *delta }
            }
            TypeAst::NaturalNumberSet => TypeAst::NaturalNumberSet,
            TypeAst::Float => TypeAst::Float,
            TypeAst::Char => TypeAst::Char,
            TypeAst::Lambda { patterns } => TypeAst::Lambda { patterns: patterns.clone() },
            TypeAst::Wildcard => TypeAst::Wildcard,
            TypeAst::DiscardPattern => TypeAst::DiscardPattern,
            TypeAst::NaturalNumberLiteral(n) => TypeAst::NaturalNumberLiteral(*n),
            TypeAst::FloatLiteral(f) => TypeAst::FloatLiteral(*f),
            TypeAst::CharLiteral(c) => TypeAst::CharLiteral(*c),
            TypeAst::Variable(s) => TypeAst::Variable(s.clone()),
            TypeAst::Tuple(v) => TypeAst::Tuple(v.clone()),
            TypeAst::Cons { head, tail } => {
                TypeAst::Cons { head: head.clone(), tail: tail.clone() }
            }
            TypeAst::List { head, tail } => {
                TypeAst::List { head: head.clone(), tail: tail.clone() }
            }
            TypeAst::AnyOf(v) => TypeAst::AnyOf(v.clone()),
            TypeAst::AllOf(v) => TypeAst::AllOf(v.clone()),
            TypeAst::Invoke { func, arg, continuation, perform_handler } => TypeAst::Invoke {
                func: func.clone(),
                arg: arg.clone(),
                continuation: continuation.clone(),
                perform_handler: perform_handler.clone(),
            },
            TypeAst::HandleWith { closure, init_val, handler } => TypeAst::HandleWith {
                closure: closure.clone(),
                init_val: init_val.clone(),
                handler: handler.clone(),
            },
            TypeAst::Match { branches } => TypeAst::Match { branches: branches.clone() },
            TypeAst::Bind { var, expr } => TypeAst::Bind { var: var.clone(), expr: expr.clone() },
            TypeAst::Apply { func, arg, auto_cps } => {
                TypeAst::Apply { func: func.clone(), arg: arg.clone(), auto_cps: *auto_cps }
            }
            TypeAst::AtomicOpcode(op) => TypeAst::AtomicOpcode(op.clone()),
            TypeAst::FixPoint { param_name, expr } => {
                TypeAst::FixPoint { param_name: param_name.clone(), expr: expr.clone() }
            }
            TypeAst::StaticFixPoint { param_name, expr } => {
                TypeAst::StaticFixPoint { param_name: param_name.clone(), expr: expr.clone() }
            }
            TypeAst::Namespace { tag, expr } => {
                TypeAst::Namespace { tag: tag.clone(), expr: expr.clone() }
            }
            TypeAst::Generic(g) => TypeAst::Generic(g.clone()),
            TypeAst::Lazy(l) => TypeAst::Lazy(l.clone()),
            TypeAst::Mutable { value } => TypeAst::Mutable { value: value.clone() },
            TypeAst::SubOf { value } => TypeAst::SubOf { value: value.clone() },
        }
    }
}

#[derive(Debug, Clone)]
pub enum BasicGenericPattern {
    Standard {
        pattern: WithLocation<BasicTypeAst>,
        constraint: Vec<(WithLocation<String>, WithLocation<BasicTypeAst>)>,
    },
    AutoBind {
        pattern: WithLocation<BasicTypeAst>,
    },
}

#[derive(Debug)]
pub enum BasicTypeAst {
    Range {
        ty: Box<WithLocation<BasicTypeAst>>,
        min: usize,
        delta: Option<usize>,
    },
    NaturalNumberSet,
    Float,
    Char,
    Lambda {
        patterns: Vec<BasicGenericPattern>, // 参数模式
    },
    NaturalNumberLiteral(usize),
    FloatLiteral(f64),
    CharLiteral(char),
    Variable(WithLocation<String>),
    Tuple(Vec<(WithLocation<BasicTypeAst>, NonZero<usize>)>),
    List {
        head: Vec<(WithLocation<BasicTypeAst>, NonZero<usize>)>,
        tail: Box<WithLocation<BasicTypeAst>>,
    },
    Cons {
        head: Vec<(WithLocation<BasicTypeAst>, NonZero<usize>)>,
        tail: Box<WithLocation<BasicTypeAst>>,
    },
    AnyOf(Vec<WithLocation<BasicTypeAst>>),
    AllOf(Vec<WithLocation<BasicTypeAst>>),
    Invoke {
        func: Box<WithLocation<BasicTypeAst>>,
        arg: Box<WithLocation<BasicTypeAst>>,
        continuation: Option<Box<WithLocation<BasicTypeAst>>>,
        perform_handler: Option<Box<WithLocation<BasicTypeAst>>>,
    },
    Match {
        branches: Vec<(BasicGenericPattern, WithLocation<BasicTypeAst>)>,
    },
    Bind {
        var: WithLocation<String>,
        expr: Box<WithLocation<BasicTypeAst>>,
    },
    Apply {
        func: Box<WithLocation<BasicTypeAst>>,
        arg: Box<WithLocation<BasicTypeAst>>,
        handler: Option<Box<WithLocation<BasicTypeAst>>>,
        auto_cps: bool,
    },
    AtomicOpcode(AtomicOpcode),
    Namespace {
        tag: WithLocation<String>,
        expr: Box<WithLocation<BasicTypeAst>>,
    },
    Generic(Box<BasicGenericPattern>),
    Lazy(Box<WithLocation<BasicTypeAst>>),
    Mutable {
        value: Box<WithLocation<BasicTypeAst>>,
    },
    SubOf {
        value: Box<WithLocation<BasicTypeAst>>,
    },
    StaticFixPoint {
        param_name: WithLocation<String>,
        expr: Box<WithLocation<BasicTypeAst>>,
    },
}

impl Clone for BasicTypeAst {
    #[stacksafe::stacksafe]
    fn clone(&self) -> Self {
        match self {
            BasicTypeAst::Range { ty, min, delta } => {
                BasicTypeAst::Range { ty: ty.clone(), min: *min, delta: *delta }
            }
            BasicTypeAst::NaturalNumberSet => BasicTypeAst::NaturalNumberSet,
            BasicTypeAst::Float => BasicTypeAst::Float,
            BasicTypeAst::Char => BasicTypeAst::Char,
            BasicTypeAst::Lambda { patterns } => {
                BasicTypeAst::Lambda { patterns: patterns.clone() }
            }
            BasicTypeAst::NaturalNumberLiteral(n) => BasicTypeAst::NaturalNumberLiteral(*n),
            BasicTypeAst::FloatLiteral(f) => BasicTypeAst::FloatLiteral(*f),
            BasicTypeAst::CharLiteral(c) => BasicTypeAst::CharLiteral(*c),
            BasicTypeAst::Variable(s) => BasicTypeAst::Variable(s.clone()),
            BasicTypeAst::Tuple(v) => BasicTypeAst::Tuple(v.clone()),
            BasicTypeAst::List { head, tail } => {
                BasicTypeAst::List { head: head.clone(), tail: tail.clone() }
            }
            BasicTypeAst::Cons { head, tail } => {
                BasicTypeAst::Cons { head: head.clone(), tail: tail.clone() }
            }
            BasicTypeAst::AnyOf(v) => BasicTypeAst::AnyOf(v.clone()),
            BasicTypeAst::AllOf(v) => BasicTypeAst::AllOf(v.clone()),
            BasicTypeAst::Invoke { func, arg, continuation, perform_handler } => {
                BasicTypeAst::Invoke {
                    func: func.clone(),
                    arg: arg.clone(),
                    continuation: continuation.clone(),
                    perform_handler: perform_handler.clone(),
                }
            }
            BasicTypeAst::Match { branches } => BasicTypeAst::Match { branches: branches.clone() },
            BasicTypeAst::Bind { var, expr } => {
                BasicTypeAst::Bind { var: var.clone(), expr: expr.clone() }
            }
            BasicTypeAst::Apply { func, arg, handler, auto_cps } => BasicTypeAst::Apply {
                func: func.clone(),
                arg: arg.clone(),
                handler: handler.clone(),
                auto_cps: *auto_cps,
            },
            BasicTypeAst::AtomicOpcode(op) => BasicTypeAst::AtomicOpcode(op.clone()),
            BasicTypeAst::Namespace { tag, expr } => {
                BasicTypeAst::Namespace { tag: tag.clone(), expr: expr.clone() }
            }
            BasicTypeAst::Generic(g) => BasicTypeAst::Generic(g.clone()),
            BasicTypeAst::Lazy(l) => BasicTypeAst::Lazy(l.clone()),
            BasicTypeAst::Mutable { value } => BasicTypeAst::Mutable { value: value.clone() },
            BasicTypeAst::SubOf { value } => BasicTypeAst::SubOf { value: value.clone() },
            BasicTypeAst::StaticFixPoint { param_name, expr } => {
                BasicTypeAst::StaticFixPoint { param_name: param_name.clone(), expr: expr.clone() }
            }
        }
    }
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

    pub fn allocate_tmpvar_name(&mut self) -> WithLocation<String> {
        let index = self.allocate_tmpvar();
        WithLocation::new(format!("invoke#tmp#{}", index), None::<&SourceLocation>)
    }
}

#[derive(Debug)]
pub struct LinearizeResult {
    #[allow(clippy::type_complexity)]
    bindings: Vec<(
        WithLocation<LinearTypeAst, FlowedMetaData>,
        WithLocation<LinearTypeAst, FlowedMetaData>,
        Option<WithLocation<LinearTypeAst, FlowedMetaData>>,
        WithLocation<String>,
        bool,
    )>, // (func, arg, handler, tmpvar_name, auto_cps)
    tail_type: WithLocation<LinearTypeAst, FlowedMetaData>,
}

impl LinearizeResult {
    pub fn new_simple(ty: WithLocation<LinearTypeAst, FlowedMetaData>) -> Self {
        Self { bindings: Vec::new(), tail_type: ty }
    }

    #[allow(clippy::type_complexity)]
    pub fn new_with_binding(
        bindings: Vec<(
            WithLocation<LinearTypeAst, FlowedMetaData>,
            WithLocation<LinearTypeAst, FlowedMetaData>,
            Option<WithLocation<LinearTypeAst, FlowedMetaData>>,
            WithLocation<String>,
            bool,
        )>,
        ty: WithLocation<LinearTypeAst, FlowedMetaData>,
    ) -> Self {
        Self { bindings, tail_type: ty }
    }

    pub fn new_apply(
        func: LinearizeResult,
        arg: LinearizeResult,
        handler: Option<LinearizeResult>,
        allocated_tmpvar_name: WithLocation<String>,
        auto_cps: bool,
    ) -> Self {
        let mut bindings = func.bindings;
        bindings.extend(arg.bindings);
        if let Some(h) = handler {
            bindings.extend(h.bindings);
            bindings.push((
                func.tail_type,
                arg.tail_type,
                Some(h.tail_type),
                allocated_tmpvar_name.clone(),
                auto_cps,
            ));
        } else {
            bindings.push((
                func.tail_type,
                arg.tail_type,
                None,
                allocated_tmpvar_name.clone(),
                auto_cps,
            ));
        }
        Self { bindings, tail_type: LinearTypeAst::Variable(allocated_tmpvar_name).into() }
    }

    #[allow(clippy::type_complexity)]
    pub fn bindings(
        &self,
    ) -> &Vec<(
        WithLocation<LinearTypeAst, FlowedMetaData>,
        WithLocation<LinearTypeAst, FlowedMetaData>,
        Option<WithLocation<LinearTypeAst, FlowedMetaData>>,
        WithLocation<String>,
        bool,
    )> {
        &self.bindings
    }

    pub fn tail_type(&self) -> &WithLocation<LinearTypeAst, FlowedMetaData> {
        &self.tail_type
    }

    pub fn finalize(self) -> WithLocation<LinearTypeAst, FlowedMetaData> {
        let mut ty = self.tail_type;
        for (f, a, handler, tmpvar, auto_cps) in self.bindings.into_iter().rev() {
            let continuation = if let LinearTypeAst::Variable(v) = ty.value()
                && v.eq(tmpvar.value())
            {
                None // TCO（尾调用优化）
            } else {
                Some(WithLocation::new(
                    LinearTypeAst::Match {
                        auto_captures: HashMap::new(),
                        branches: vec![(
                            WithLocation::new(
                                LinearTypeAst::Variable(tmpvar.clone()),
                                None::<&SourceLocation>,
                            ),
                            vec![(
                                tmpvar.clone(),
                                WithLocation::new(
                                    LinearTypeAst::AllOf(vec![]),
                                    None::<&SourceLocation>,
                                ),
                            )],
                            ty.clone(),
                        )],
                    },
                    ty.location(),
                ))
            };
            if auto_cps {
                ty = WithLocation::new(
                    LinearTypeAst::Invoke {
                        func: Box::new(f),
                        arg: Box::new(a),
                        continuation: continuation.map(Box::new),
                        perform_handler: handler.map(Box::new),
                    },
                    None::<&SourceLocation>,
                )
            } else {
                if handler.is_some() {
                    panic!("Non-auto-cps Apply should not have a handler");
                };

                let continuation = match continuation {
                    Some(c) => c,
                    None => {
                        // identity
                        WithLocation::new(
                            LinearTypeAst::Match {
                                auto_captures: HashMap::new(),
                                branches: vec![(
                                    WithLocation::new(
                                        LinearTypeAst::Variable(tmpvar.clone()),
                                        None::<&SourceLocation>,
                                    ),
                                    vec![(
                                        tmpvar.clone(),
                                        WithLocation::new(
                                            LinearTypeAst::AllOf(vec![]),
                                            None::<&SourceLocation>,
                                        ),
                                    )],
                                    LinearTypeAst::Variable(tmpvar.clone()).into(),
                                )],
                            },
                            ty.location(),
                        )
                    }
                };

                // f(continuation)(a)

                // 1. Construct k1: \res -> res(a)
                let res_name =
                    WithLocation::new("invoke#manual_cps_res".to_string(), None::<&SourceLocation>);
                let res_var = WithLocation::new(
                    LinearTypeAst::Variable(res_name.clone()),
                    None::<&SourceLocation>,
                );

                let body_k1 = WithLocation::new(
                    LinearTypeAst::Invoke {
                        func: Box::new(res_var.clone()),
                        arg: Box::new(a),
                        continuation: None,
                        perform_handler: None,
                    },
                    None::<&SourceLocation>,
                );

                let k1 = WithLocation::new(
                    LinearTypeAst::Match {
                        auto_captures: HashMap::new(),
                        branches: vec![(
                            res_var, // pattern
                            vec![(
                                res_name.clone(),
                                WithLocation::new(
                                    LinearTypeAst::AllOf(vec![]),
                                    None::<&SourceLocation>,
                                ),
                            )],
                            body_k1,
                        )],
                    },
                    None::<&SourceLocation>,
                );

                // 2. Main call: f(continuation) with continuation k1
                ty = WithLocation::new(
                    LinearTypeAst::Invoke {
                        func: Box::new(f),
                        arg: Box::new(continuation),
                        continuation: Some(Box::new(k1)),
                        perform_handler: None,
                    },
                    None::<&SourceLocation>,
                );
            }
        }
        ty
    }
}

impl BasicTypeAst {
    /// 将 AutoBind 模式转换为 Standard 模式
    /// 返回转换后的 Standard 模式
    fn convert_auto_bind_to_standard(pattern: WithLocation<BasicTypeAst>) -> BasicGenericPattern {
        let (new_pattern, binds) = pattern.auto_bind();
        BasicGenericPattern::Standard { pattern: new_pattern, constraint: binds }
    }

    #[stacksafe::stacksafe]
    #[allow(clippy::type_complexity)]
    pub fn auto_bind(
        self,
        loc: Option<&SourceLocation>,
    ) -> (WithLocation<Self>, Vec<(WithLocation<String>, WithLocation<BasicTypeAst>)>) {
        let result = match self {
            BasicTypeAst::Bind { var, expr } => {
                let (new_expr, mut binds) = expr.auto_bind();
                binds.push((var.clone(), new_expr));
                (WithLocation::new(BasicTypeAst::Variable(var.clone()), var.location()), binds)
            }
            BasicTypeAst::Range { ty, min, delta } => {
                let (new_ty, binds) = ty.auto_bind();
                (
                    WithLocation::new(
                        BasicTypeAst::Range { ty: Box::new(new_ty), min, delta },
                        loc,
                    ),
                    binds,
                )
            }
            BasicTypeAst::Tuple(elements) => {
                let mut new_elements = Vec::new();
                let mut all_binds = Vec::new();
                for (e, n) in elements {
                    let (new_e, binds) = e.auto_bind();
                    new_elements.push((new_e, n));
                    all_binds.extend(binds);
                }
                (WithLocation::new(BasicTypeAst::Tuple(new_elements), loc), all_binds)
            }
            BasicTypeAst::List { head, tail } => {
                let mut new_head = Vec::new();
                let mut all_binds = Vec::new();
                for (e, n) in head {
                    let (new_e, binds) = e.auto_bind();
                    new_head.push((new_e, n));
                    all_binds.extend(binds);
                }
                let (new_tail, tail_binds) = tail.auto_bind();
                all_binds.extend(tail_binds);
                (
                    WithLocation::new(
                        BasicTypeAst::List { head: new_head, tail: Box::new(new_tail) },
                        loc,
                    ),
                    all_binds,
                )
            }
            BasicTypeAst::Cons { head, tail } => {
                let mut new_head = Vec::new();
                let mut all_binds = Vec::new();
                for (e, n) in head {
                    let (new_e, binds) = e.auto_bind();
                    new_head.push((new_e, n));
                    all_binds.extend(binds);
                }
                let (new_tail, tail_binds) = tail.auto_bind();
                all_binds.extend(tail_binds);
                (
                    WithLocation::new(
                        BasicTypeAst::Cons { head: new_head, tail: Box::new(new_tail) },
                        loc,
                    ),
                    all_binds,
                )
            }
            BasicTypeAst::AnyOf(elements) => {
                let mut new_elements = Vec::new();
                let mut all_binds = Vec::new();
                for e in elements {
                    let (new_e, binds) = e.auto_bind();
                    new_elements.push(new_e);
                    all_binds.extend(binds);
                }
                (WithLocation::new(BasicTypeAst::AnyOf(new_elements), loc), all_binds)
            }
            BasicTypeAst::AllOf(elements) => {
                let mut new_elements = Vec::new();
                let mut all_binds = Vec::new();
                for e in elements {
                    let (new_e, binds) = e.auto_bind();
                    new_elements.push(new_e);
                    all_binds.extend(binds);
                }
                (WithLocation::new(BasicTypeAst::AllOf(new_elements), loc), all_binds)
            }
            BasicTypeAst::Invoke { func, arg, continuation, perform_handler } => {
                let (new_func, mut binds) = func.auto_bind();
                let (new_arg, arg_binds) = arg.auto_bind();
                binds.extend(arg_binds);
                let new_cont = if let Some(c) = continuation {
                    let (nc, cb) = c.auto_bind();
                    binds.extend(cb);
                    Some(Box::new(nc))
                } else {
                    None
                };
                let new_ph = if let Some(ph) = perform_handler {
                    let (nph, phb) = ph.auto_bind();
                    binds.extend(phb);
                    Some(Box::new(nph))
                } else {
                    None
                };
                (
                    WithLocation::new(
                        BasicTypeAst::Invoke {
                            func: Box::new(new_func),
                            arg: Box::new(new_arg),
                            continuation: new_cont,
                            perform_handler: new_ph,
                        },
                        loc,
                    ),
                    binds,
                )
            }
            BasicTypeAst::Match { branches } => {
                let mut binds = Vec::new();
                let new_branches = branches
                    .into_iter()
                    .map(|(b, expr)| match b {
                        BasicGenericPattern::Standard { pattern, constraint } => {
                            let (new_pattern, a) = pattern.auto_bind();
                            binds.extend(a);
                            let mut new_constraints = Vec::new();
                            for (name, ctype) in constraint {
                                let (new_ctype, ac) = ctype.auto_bind();
                                binds.extend(ac);
                                new_constraints.push((name, new_ctype));
                            }
                            let (new_expr, a) = expr.auto_bind();
                            binds.extend(a);
                            (
                                BasicGenericPattern::Standard {
                                    pattern: new_pattern,
                                    constraint: new_constraints,
                                },
                                new_expr,
                            )
                        }
                        BasicGenericPattern::AutoBind { pattern } => {
                            let standard_pattern = Self::convert_auto_bind_to_standard(pattern);
                            let (new_expr, a) = expr.auto_bind();
                            binds.extend(a);
                            (standard_pattern, new_expr)
                        }
                    })
                    .collect();
                (WithLocation::new(BasicTypeAst::Match { branches: new_branches }, loc), binds)
            }
            BasicTypeAst::Lambda { patterns } => {
                let mut binds = Vec::new();
                let new_patterns = patterns
                    .into_iter()
                    .map(|b| match b {
                        BasicGenericPattern::Standard { pattern, constraint } => {
                            let (new_pattern, a) = pattern.auto_bind();
                            binds.extend(a);
                            let mut new_constraints = Vec::new();
                            for (name, ctype) in constraint {
                                let (new_ctype, ac) = ctype.auto_bind();
                                binds.extend(ac);
                                new_constraints.push((name, new_ctype));
                            }

                            BasicGenericPattern::Standard {
                                pattern: new_pattern,
                                constraint: new_constraints,
                            }
                        }
                        BasicGenericPattern::AutoBind { pattern } => {
                            Self::convert_auto_bind_to_standard(pattern)
                        }
                    })
                    .collect();
                (WithLocation::new(BasicTypeAst::Lambda { patterns: new_patterns }, loc), binds)
            }
            BasicTypeAst::Apply { func, arg, handler, auto_cps } => {
                let (new_func, mut binds) = func.auto_bind();
                let (new_arg, arg_binds) = arg.auto_bind();
                binds.extend(arg_binds);
                let new_handler = if let Some(h) = handler {
                    let (nh, hb) = h.auto_bind();
                    binds.extend(hb);
                    Some(Box::new(nh))
                } else {
                    None
                };
                (
                    WithLocation::new(
                        BasicTypeAst::Apply {
                            func: Box::new(new_func),
                            arg: Box::new(new_arg),
                            handler: new_handler,
                            auto_cps,
                        },
                        loc,
                    ),
                    binds,
                )
            }
            BasicTypeAst::Namespace { tag, expr } => {
                let (new_expr, binds) = expr.auto_bind();
                (
                    WithLocation::new(
                        BasicTypeAst::Namespace { tag, expr: Box::new(new_expr) },
                        loc,
                    ),
                    binds,
                )
            }
            BasicTypeAst::Generic(inner) => match *inner {
                BasicGenericPattern::Standard { pattern, constraint } => {
                    let (new_pattern, _) = pattern.auto_bind();
                    let mut new_constraints = Vec::new();
                    for (name, ctype) in constraint {
                        let (new_ctype, _) = ctype.auto_bind();
                        new_constraints.push((name, new_ctype));
                    }
                    (
                        WithLocation::new(
                            BasicTypeAst::Generic(Box::new(BasicGenericPattern::Standard {
                                pattern: new_pattern,
                                constraint: new_constraints,
                            })),
                            loc,
                        ),
                        Vec::new(),
                    )
                }
                BasicGenericPattern::AutoBind { pattern } => {
                    let standard_pattern = Self::convert_auto_bind_to_standard(pattern);
                    (
                        WithLocation::new(BasicTypeAst::Generic(Box::new(standard_pattern)), loc),
                        Vec::new(),
                    )
                }
            },
            BasicTypeAst::Lazy(inner) => {
                let (new_inner, binds) = inner.auto_bind();
                (WithLocation::new(BasicTypeAst::Lazy(Box::new(new_inner)), loc), binds)
            }
            BasicTypeAst::Mutable { value } => {
                let (new_value, binds) = value.auto_bind();
                (
                    WithLocation::new(BasicTypeAst::Mutable { value: Box::new(new_value) }, loc),
                    binds,
                )
            }
            BasicTypeAst::SubOf { value } => {
                let (new_value, binds) = value.auto_bind();
                (WithLocation::new(BasicTypeAst::SubOf { value: Box::new(new_value) }, loc), binds)
            }
            BasicTypeAst::StaticFixPoint { param_name, expr } => {
                let (new_expr, binds) = expr.auto_bind();
                (
                    WithLocation::new(
                        BasicTypeAst::StaticFixPoint { param_name, expr: Box::new(new_expr) },
                        loc,
                    ),
                    binds,
                )
            }
            BasicTypeAst::NaturalNumberSet
            | BasicTypeAst::Float
            | BasicTypeAst::Char
            | BasicTypeAst::NaturalNumberLiteral(_)
            | BasicTypeAst::FloatLiteral(_)
            | BasicTypeAst::CharLiteral(_)
            | BasicTypeAst::Variable(_)
            | BasicTypeAst::AtomicOpcode(_) => (WithLocation::new(self, loc), Vec::new()),
        };
        (result.0, result.1)
    }

    #[stacksafe::stacksafe]
    pub fn linearize(
        &self,
        ctx: &mut LinearizeContext,
        errors: &mut Vec<WithLocation<ParseError>>,
        loc: Option<&SourceLocation>,
    ) -> LinearizeResult {
        match self {
            BasicTypeAst::Range { ty, min, delta } => {
                let ty_result = ty.linearize(ctx, errors, ty.location());
                let tail_ty = ty_result.tail_type().clone();
                LinearizeResult::new_with_binding(
                    ty_result.bindings,
                    WithLocation::new(
                        LinearTypeAst::Range { ty: Box::new(tail_ty), min: *min, delta: *delta },
                        loc,
                    ),
                )
            }
            BasicTypeAst::NaturalNumberSet => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::NaturalNumberSet, loc))
            }
            BasicTypeAst::Float => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::Float, loc))
            }
            BasicTypeAst::Char => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::Char, loc))
            }
            BasicTypeAst::Lambda { patterns } => {
                let mut linearized_patterns = Vec::new();
                let mut bindings = Vec::new();
                for branch in patterns {
                    let BasicGenericPattern::Standard { pattern: p, constraint } = branch else {
                        errors.push(WithLocation::new(
                            ParseError::AstNotDesugared(WithLocation::new(self.clone(), loc)),
                            loc,
                        ));
                        return LinearizeResult::new_simple(WithLocation::new(
                            LinearTypeAst::AllOf(vec![]),
                            loc,
                        ));
                    };
                    let pat = p.linearize(ctx, errors, p.location());
                    let mut new_constraint = Vec::new();
                    for (name, ctype) in constraint {
                        let ctype = ctype.linearize(ctx, errors, ctype.location());
                        new_constraint.push((name.clone(), ctype.tail_type().clone()));
                        bindings.extend(ctype.bindings);
                    }
                    bindings.extend(pat.bindings.clone());
                    linearized_patterns.push((pat.tail_type().clone(), new_constraint));
                }
                let ty = LinearTypeAst::Lambda { patterns: linearized_patterns };
                LinearizeResult::new_with_binding(bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::NaturalNumberLiteral(v) => LinearizeResult::new_simple(
                WithLocation::new(LinearTypeAst::NaturalNumberLiteral(*v), loc),
            ),
            BasicTypeAst::FloatLiteral(v) => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::FloatLiteral(*v), loc))
            }
            BasicTypeAst::CharLiteral(v) => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::CharLiteral(*v), loc))
            }
            BasicTypeAst::Variable(v) => LinearizeResult::new_simple(WithLocation::new(
                LinearTypeAst::Variable(v.clone()),
                loc,
            )),
            BasicTypeAst::Tuple(v) => {
                let elements = v
                    .iter()
                    .map(|(e, n)| (e.linearize(ctx, errors, e.location()), *n))
                    .collect::<Vec<_>>();
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
                    .map(|(e, n)| (e.linearize(ctx, errors, e.location()), *n))
                    .collect::<Vec<_>>();
                let tail_result = tail.linearize(ctx, errors, tail.location());
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
                    .map(|(e, n)| (e.linearize(ctx, errors, e.location()), *n))
                    .collect::<Vec<_>>();
                let tail_result = tail.linearize(ctx, errors, tail.location());
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
            BasicTypeAst::AnyOf(v) => {
                let elements =
                    v.iter().map(|e| e.linearize(ctx, errors, e.location())).collect::<Vec<_>>();
                let ty =
                    LinearTypeAst::AnyOf(elements.iter().map(|e| e.tail_type().clone()).collect());
                LinearizeResult::new_with_binding(
                    elements.into_iter().flat_map(|e| e.bindings.into_iter()).collect(),
                    WithLocation::new(ty, loc),
                )
            }
            BasicTypeAst::AllOf(v) => {
                let elements =
                    v.iter().map(|e| e.linearize(ctx, errors, e.location())).collect::<Vec<_>>();
                let ty =
                    LinearTypeAst::AllOf(elements.iter().map(|e| e.tail_type().clone()).collect());
                LinearizeResult::new_with_binding(
                    elements.into_iter().flat_map(|e| e.bindings.into_iter()).collect(),
                    WithLocation::new(ty, loc),
                )
            }
            BasicTypeAst::Invoke { func, arg, continuation, perform_handler } => {
                let func = func.linearize(ctx, errors, func.location());
                let arg = arg.linearize(ctx, errors, arg.location());
                let continuation = continuation.as_ref().map(|continuation| {
                    Box::new(continuation.linearize(ctx, errors, continuation.location()))
                });
                let perform_handler = perform_handler.as_ref().map(|perform_handler| {
                    Box::new(perform_handler.linearize(ctx, errors, perform_handler.location()))
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
                for branch in branches {
                    let (BasicGenericPattern::Standard { pattern: p, constraint }, expr) = branch
                    else {
                        errors.push(WithLocation::new(
                            ParseError::AstNotDesugared(WithLocation::new(self.clone(), loc)),
                            loc,
                        ));
                        return LinearizeResult::new_simple(WithLocation::new(
                            LinearTypeAst::AllOf(vec![]),
                            loc,
                        ));
                    };
                    let pat = p.linearize(ctx, errors, p.location());
                    let mut new_constraint = Vec::new();
                    for (name, ctype) in constraint {
                        let ctype = ctype.linearize(ctx, errors, ctype.location());
                        new_constraint.push((name.clone(), ctype.tail_type().clone()));
                        bindings.extend(ctype.bindings);
                    }
                    let expr = expr.linearize(ctx, errors, expr.location()).finalize(); // expr 是严格独立上下文的，因此直接线性化不参与CPS
                    bindings.extend(pat.bindings.clone());
                    linearized_branches.push((pat.tail_type().clone(), new_constraint, expr));
                }
                let ty = LinearTypeAst::Match {
                    auto_captures: HashMap::new(),
                    branches: linearized_branches,
                };
                LinearizeResult::new_with_binding(bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Apply { func, arg, handler, auto_cps } => {
                let func = func.linearize(ctx, errors, func.location());
                let arg = arg.linearize(ctx, errors, arg.location());
                LinearizeResult::new_apply(
                    func,
                    arg,
                    handler.as_ref().map(|h| h.linearize(ctx, errors, h.location())),
                    ctx.allocate_tmpvar_name(),
                    *auto_cps,
                )
            }
            BasicTypeAst::AtomicOpcode(atomic_opcode) => LinearizeResult::new_simple(
                WithLocation::new(LinearTypeAst::AtomicOpcode(atomic_opcode.clone()), loc),
            ),
            BasicTypeAst::Namespace { tag, expr } => {
                let expr = expr.linearize(ctx, errors, expr.location());
                let ty = LinearTypeAst::Namespace {
                    tag: tag.clone(),
                    expr: Box::new(expr.tail_type().clone()),
                };
                LinearizeResult::new_with_binding(expr.bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Generic(inner) => {
                let BasicGenericPattern::Standard { pattern: p, constraint } = &**inner else {
                    errors.push(WithLocation::new(
                        ParseError::AstNotDesugared(WithLocation::new(self.clone(), loc)),
                        loc,
                    ));
                    return LinearizeResult::new_simple(WithLocation::new(
                        LinearTypeAst::AllOf(vec![]),
                        loc,
                    ));
                };
                let pat = p.linearize(ctx, errors, p.location());
                let mut bindings = pat.bindings.clone();
                let mut new_constraints = Vec::new();
                for (name, ctype) in constraint {
                    let ctype = ctype.linearize(ctx, errors, ctype.location());
                    new_constraints.push((name.clone(), ctype.tail_type().clone()));
                    bindings.extend(ctype.bindings);
                }
                let ty = LinearTypeAst::Generic {
                    expr: Box::new(pat.tail_type().clone()),
                    constraint: new_constraints,
                };
                LinearizeResult::new_with_binding(bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Lazy(inner) => LinearizeResult::new_simple(WithLocation::new(
                LinearTypeAst::Lazy(Box::new(
                    inner.linearize(ctx, errors, inner.location()).finalize(),
                )),
                loc,
            )),
            BasicTypeAst::Mutable { value } => {
                let value = value.linearize(ctx, errors, value.location());
                let ty = LinearTypeAst::Mutable { value: Box::new(value.tail_type().clone()) };
                LinearizeResult::new_with_binding(value.bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::SubOf { value } => {
                let value = value.linearize(ctx, errors, value.location());
                let ty = LinearTypeAst::SubOf { value: Box::new(value.tail_type().clone()) };
                LinearizeResult::new_with_binding(value.bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::StaticFixPoint { param_name, expr } => {
                let expr = expr.linearize(ctx, errors, expr.location());
                let ty = LinearTypeAst::StaticFixPoint {
                    param_name: param_name.clone(),
                    expr: Box::new(expr.tail_type().clone()),
                };
                LinearizeResult::new_with_binding(expr.bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Bind { .. } => {
                errors.push(WithLocation::new(
                    ParseError::AstNotDesugared(WithLocation::new(self.clone(), loc)),
                    loc,
                ));
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::Tuple(vec![]), loc))
            }
        }
    }
}

impl WithLocation<BasicTypeAst> {
    #[stacksafe::stacksafe]
    #[allow(clippy::type_complexity)]
    pub fn auto_bind(self) -> (Self, Vec<(WithLocation<String>, WithLocation<BasicTypeAst>)>) {
        let loc = self.location().cloned();
        self.value.auto_bind(loc.as_ref())
    }
}

#[derive(Debug, Clone, Default)]
pub struct FlowedMetaData {
    reference: Option<WithLocation<()>>,
    variable_context: Vec<WithLocation<String>>,
}

impl FlowedMetaData {
    pub fn reference(&self) -> Option<&WithLocation<()>> {
        self.reference.as_ref()
    }

    pub fn variable_context(&self) -> &Vec<WithLocation<String>> {
        &self.variable_context
    }

    pub fn with_reference(self, reference: Option<WithLocation<()>>) -> Self {
        Self { reference, ..self }
    }

    pub fn with_variable_context(self, variable_context: Vec<WithLocation<String>>) -> Self {
        Self { variable_context, ..self }
    }
}

#[derive(Debug)]
pub enum LinearTypeAst {
    Range {
        ty: Box<WithLocation<LinearTypeAst, FlowedMetaData>>,
        min: usize,
        delta: Option<usize>,
    },
    NaturalNumberSet,
    Char,
    Float,
    #[allow(clippy::type_complexity)]
    Lambda {
        patterns: Vec<(
            WithLocation<LinearTypeAst, FlowedMetaData>,
            Vec<(WithLocation<String>, WithLocation<LinearTypeAst, FlowedMetaData>)>,
        )>, // pattern
    },
    NaturalNumberLiteral(usize),
    FloatLiteral(f64),
    CharLiteral(char),
    Variable(WithLocation<String>), // None 表示续体
    Tuple(Vec<(WithLocation<LinearTypeAst, FlowedMetaData>, NonZero<usize>)>),
    List {
        head: Vec<(WithLocation<LinearTypeAst, FlowedMetaData>, NonZero<usize>)>,
        tail: Box<WithLocation<LinearTypeAst, FlowedMetaData>>,
    },
    Cons {
        head: Vec<(WithLocation<LinearTypeAst, FlowedMetaData>, NonZero<usize>)>,
        tail: Box<WithLocation<LinearTypeAst, FlowedMetaData>>,
    },
    AnyOf(Vec<WithLocation<LinearTypeAst, FlowedMetaData>>),
    AllOf(Vec<WithLocation<LinearTypeAst, FlowedMetaData>>),
    #[allow(clippy::type_complexity)]
    Match {
        auto_captures: HashMap<String, WithLocation<()>>,
        branches: Vec<(
            WithLocation<LinearTypeAst, FlowedMetaData>,
            Vec<(WithLocation<String>, WithLocation<LinearTypeAst, FlowedMetaData>)>,
            WithLocation<LinearTypeAst, FlowedMetaData>,
        )>, // pattern, expr
    },
    Invoke {
        func: Box<WithLocation<LinearTypeAst, FlowedMetaData>>,
        arg: Box<WithLocation<LinearTypeAst, FlowedMetaData>>,
        continuation: Option<Box<WithLocation<LinearTypeAst, FlowedMetaData>>>,
        perform_handler: Option<Box<WithLocation<LinearTypeAst, FlowedMetaData>>>,
    },
    AtomicOpcode(AtomicOpcode),
    Namespace {
        tag: WithLocation<String>,
        expr: Box<WithLocation<LinearTypeAst, FlowedMetaData>>,
    },
    Generic {
        expr: Box<WithLocation<LinearTypeAst, FlowedMetaData>>,
        constraint: Vec<(WithLocation<String>, WithLocation<LinearTypeAst, FlowedMetaData>)>,
    },
    Lazy(Box<WithLocation<LinearTypeAst, FlowedMetaData>>),
    Mutable {
        value: Box<WithLocation<LinearTypeAst, FlowedMetaData>>,
    },
    SubOf {
        value: Box<WithLocation<LinearTypeAst, FlowedMetaData>>,
    },
    StaticFixPoint {
        param_name: WithLocation<String>,
        expr: Box<WithLocation<LinearTypeAst, FlowedMetaData>>,
    },
}

impl Clone for LinearTypeAst {
    #[stacksafe::stacksafe]
    fn clone(&self) -> Self {
        match self {
            LinearTypeAst::Range { ty, min, delta } => {
                LinearTypeAst::Range { ty: ty.clone(), min: *min, delta: *delta }
            }
            LinearTypeAst::NaturalNumberSet => LinearTypeAst::NaturalNumberSet,
            LinearTypeAst::Char => LinearTypeAst::Char,
            LinearTypeAst::Float => LinearTypeAst::Float,
            LinearTypeAst::Lambda { patterns } => {
                LinearTypeAst::Lambda { patterns: patterns.clone() }
            }
            LinearTypeAst::NaturalNumberLiteral(n) => LinearTypeAst::NaturalNumberLiteral(*n),
            LinearTypeAst::FloatLiteral(f) => LinearTypeAst::FloatLiteral(*f),
            LinearTypeAst::CharLiteral(c) => LinearTypeAst::CharLiteral(*c),
            LinearTypeAst::Variable(s) => LinearTypeAst::Variable(s.clone()),
            LinearTypeAst::Tuple(v) => LinearTypeAst::Tuple(v.clone()),
            LinearTypeAst::List { head, tail } => {
                LinearTypeAst::List { head: head.clone(), tail: tail.clone() }
            }
            LinearTypeAst::Cons { head, tail } => {
                LinearTypeAst::Cons { head: head.clone(), tail: tail.clone() }
            }
            LinearTypeAst::AnyOf(v) => LinearTypeAst::AnyOf(v.clone()),
            LinearTypeAst::AllOf(v) => LinearTypeAst::AllOf(v.clone()),
            LinearTypeAst::Match { auto_captures, branches } => LinearTypeAst::Match {
                auto_captures: auto_captures.clone(),
                branches: branches.clone(),
            },
            LinearTypeAst::Invoke { func, arg, continuation, perform_handler } => {
                LinearTypeAst::Invoke {
                    func: func.clone(),
                    arg: arg.clone(),
                    continuation: continuation.clone(),
                    perform_handler: perform_handler.clone(),
                }
            }
            LinearTypeAst::AtomicOpcode(op) => LinearTypeAst::AtomicOpcode(op.clone()),
            LinearTypeAst::Namespace { tag, expr } => {
                LinearTypeAst::Namespace { tag: tag.clone(), expr: expr.clone() }
            }
            LinearTypeAst::Generic { expr, constraint } => {
                LinearTypeAst::Generic { expr: expr.clone(), constraint: constraint.clone() }
            }
            LinearTypeAst::Lazy(l) => LinearTypeAst::Lazy(l.clone()),
            LinearTypeAst::Mutable { value } => LinearTypeAst::Mutable { value: value.clone() },
            LinearTypeAst::SubOf { value } => LinearTypeAst::SubOf { value: value.clone() },
            LinearTypeAst::StaticFixPoint { param_name, expr } => {
                LinearTypeAst::StaticFixPoint { param_name: param_name.clone(), expr: expr.clone() }
            }
        }
    }
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
            TypeAst::NaturalNumberSet => WithLocation::new(BasicTypeAst::NaturalNumberSet, loc),
            TypeAst::Float => WithLocation::new(BasicTypeAst::Float, loc),
            TypeAst::Char => WithLocation::new(BasicTypeAst::Char, loc),
            TypeAst::Lambda { patterns } => WithLocation::new(
                BasicTypeAst::Lambda {
                    patterns: patterns
                        .iter()
                        .map(|branch| match branch {
                            GenericPattern::Standard { pattern, constraint } => {
                                BasicGenericPattern::Standard {
                                    pattern: pattern
                                        .into_basic(multifile_builder, pattern.location()),
                                    constraint: constraint
                                        .iter()
                                        .map(|(var, expr)| {
                                            (
                                                var.clone(),
                                                expr.into_basic(multifile_builder, expr.location()),
                                            )
                                        })
                                        .collect(),
                                }
                            }
                            GenericPattern::AutoBind { pattern } => BasicGenericPattern::AutoBind {
                                pattern: pattern.into_basic(multifile_builder, pattern.location()),
                            },
                        })
                        .collect(),
                },
                loc,
            ),
            TypeAst::Wildcard => WithLocation::new(BasicTypeAst::AllOf(vec![]), loc),
            TypeAst::DiscardPattern => WithLocation::new(BasicTypeAst::Tuple(vec![]), loc), // 副作用只允许丢弃unit
            TypeAst::NaturalNumberLiteral(v) => {
                WithLocation::new(BasicTypeAst::NaturalNumberLiteral(*v), loc)
            }
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
            TypeAst::AnyOf(elements) => WithLocation::new(
                BasicTypeAst::AnyOf(
                    elements
                        .iter()
                        .map(|e| e.into_basic(multifile_builder, e.location()))
                        .collect(),
                ),
                loc,
            ),
            TypeAst::AllOf(elements) => WithLocation::new(
                BasicTypeAst::AllOf(
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
                    auto_cps: true,
                },
                loc,
            ),
            TypeAst::Bind { var, expr } => WithLocation::new(
                BasicTypeAst::Bind {
                    var: var.clone(),
                    expr: Box::new(expr.into_basic(multifile_builder, expr.location())),
                },
                loc,
            ),
            TypeAst::Match { branches } => WithLocation::new(
                BasicTypeAst::Match {
                    branches: branches
                        .iter()
                        .map(|(branch, expr)| {
                            (
                                match branch {
                                    GenericPattern::Standard { pattern, constraint } => {
                                        BasicGenericPattern::Standard {
                                            pattern: pattern
                                                .into_basic(multifile_builder, pattern.location()),
                                            constraint: constraint
                                                .iter()
                                                .map(|(var, expr)| {
                                                    (
                                                        var.clone(),
                                                        expr.into_basic(
                                                            multifile_builder,
                                                            expr.location(),
                                                        ),
                                                    )
                                                })
                                                .collect(),
                                        }
                                    }
                                    GenericPattern::AutoBind { pattern } => {
                                        BasicGenericPattern::AutoBind {
                                            pattern: pattern
                                                .into_basic(multifile_builder, pattern.location()),
                                        }
                                    }
                                },
                                expr.into_basic(multifile_builder, expr.location()),
                            )
                        })
                        .collect(),
                },
                loc,
            ),
            TypeAst::Apply { func, arg, auto_cps } => WithLocation::new(
                BasicTypeAst::Apply {
                    func: Box::new(func.into_basic(multifile_builder, func.location())),
                    arg: Box::new(arg.into_basic(multifile_builder, arg.location())),
                    handler: None,
                    auto_cps: *auto_cps,
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
                            BasicGenericPattern::Standard {
                                pattern: WithLocation::new(
                                    BasicTypeAst::Variable(param_name.clone()),
                                    loc,
                                ),
                                constraint: vec![(
                                    param_name.clone(),
                                    WithLocation::new(BasicTypeAst::AllOf(vec![]), loc),
                                )],
                            },
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
                        auto_cps: true,
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
            TypeAst::Generic(bind) => match &**bind {
                GenericPattern::Standard { pattern, constraint } => WithLocation::new(
                    BasicTypeAst::Generic(Box::new(BasicGenericPattern::Standard {
                        pattern: pattern.into_basic(multifile_builder, pattern.location()),
                        constraint: constraint
                            .iter()
                            .map(|(var, expr)| {
                                (var.clone(), expr.into_basic(multifile_builder, expr.location()))
                            })
                            .collect(),
                    })),
                    loc,
                ),
                GenericPattern::AutoBind { pattern } => WithLocation::new(
                    BasicTypeAst::Generic(Box::new(BasicGenericPattern::AutoBind {
                        pattern: pattern.into_basic(multifile_builder, pattern.location()),
                    })),
                    loc,
                ),
            },
            TypeAst::Lazy(inner) => WithLocation::new(
                BasicTypeAst::Lazy(Box::new(inner.into_basic(multifile_builder, inner.location()))),
                loc,
            ),
            TypeAst::Mutable { value } => WithLocation::new(
                BasicTypeAst::Mutable {
                    value: Box::new(value.into_basic(multifile_builder, value.location())),
                },
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
                        .unwrap_or(WithLocation::new(BasicTypeAst::Tuple(vec![]), loc)),
                    Err(e) => {
                        multifile_builder
                            .errors
                            .push(WithLocation::new(MultiFileBuilderError::IOError(e), loc));
                        WithLocation::new(BasicTypeAst::Tuple(vec![]), loc)
                    }
                }
            }
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

    #[stacksafe::stacksafe]
    pub fn collect_errors(&self, errors: &mut Vec<ErrorRecovery<usize, LexerToken, LexicalError>>) {
        match self {
            TypeAst::ParseError(span) => {
                errors.push(span.clone());
            }
            TypeAst::Float
            | TypeAst::NaturalNumberSet
            | TypeAst::Char
            | TypeAst::Wildcard
            | TypeAst::DiscardPattern
            | TypeAst::NaturalNumberLiteral(_)
            | TypeAst::FloatLiteral(_)
            | TypeAst::CharLiteral(_)
            | TypeAst::Variable(_)
            | TypeAst::Import(_) => {}
            TypeAst::Range { ty, .. } => {
                ty.collect_errors(errors);
            }
            TypeAst::Bind { expr, .. } => {
                expr.collect_errors(errors);
            }
            TypeAst::Tuple(elements) => {
                for (elem, _) in elements {
                    elem.collect_errors(errors);
                }
            }
            TypeAst::AnyOf(elements) | TypeAst::AllOf(elements) => {
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
            TypeAst::Match { branches } => {
                for (branch, expr) in branches {
                    match branch {
                        GenericPattern::Standard { pattern, constraint } => {
                            pattern.collect_errors(errors);
                            expr.collect_errors(errors);
                            for (_, c_expr) in constraint {
                                c_expr.collect_errors(errors);
                            }
                        }
                        GenericPattern::AutoBind { pattern } => {
                            pattern.collect_errors(errors);
                            expr.collect_errors(errors);
                        }
                    }
                }
            }
            TypeAst::Lambda { patterns } => {
                for branch in patterns {
                    match branch {
                        GenericPattern::Standard { pattern, constraint } => {
                            pattern.collect_errors(errors);
                            for (_, c_expr) in constraint {
                                c_expr.collect_errors(errors);
                            }
                        }
                        GenericPattern::AutoBind { pattern } => {
                            pattern.collect_errors(errors);
                        }
                    }
                }
            }
            TypeAst::Apply { func, arg, .. } => {
                func.collect_errors(errors);
                arg.collect_errors(errors);
            }
            TypeAst::AtomicOpcode(_) => {}
            TypeAst::FixPoint { expr, .. } => {
                expr.collect_errors(errors);
            }
            TypeAst::Namespace { expr, .. } => {
                expr.collect_errors(errors);
            }
            TypeAst::Generic(branch) => match &**branch {
                GenericPattern::Standard { pattern, constraint } => {
                    pattern.collect_errors(errors);
                    for (_, c_expr) in constraint {
                        c_expr.collect_errors(errors);
                    }
                }
                GenericPattern::AutoBind { pattern } => {
                    pattern.collect_errors(errors);
                }
            },
            TypeAst::Lazy(inner) => {
                inner.collect_errors(errors);
            }
            TypeAst::Mutable { value } => {
                value.collect_errors(errors);
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
            TypeAst::SubOf { value } => {
                value.collect_errors(errors);
            }
            TypeAst::StaticFixPoint { expr, .. } => {
                expr.collect_errors(errors);
            }
        }
    }

    #[stacksafe::stacksafe]
    pub fn sanitize(ast: WithLocation<Self>) -> WithLocation<Self> {
        ast.map(|ast| match ast {
            TypeAst::ParseError(_) => TypeAst::Wildcard,
            TypeAst::Float
            | TypeAst::NaturalNumberSet
            | TypeAst::Char
            | TypeAst::Wildcard
            | TypeAst::DiscardPattern
            | TypeAst::NaturalNumberLiteral(_)
            | TypeAst::FloatLiteral(_)
            | TypeAst::CharLiteral(_)
            | TypeAst::Variable(_)
            | TypeAst::Import(_) => ast,
            TypeAst::Range { ty, min, delta } => {
                TypeAst::Range { ty: Box::new(Self::sanitize(*ty)), min, delta }
            }
            TypeAst::Bind { var, expr } => {
                TypeAst::Bind { var, expr: Box::new(Self::sanitize(*expr)) }
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
            TypeAst::AnyOf(elements) => {
                TypeAst::AnyOf(elements.into_iter().map(Self::sanitize).collect())
            }
            TypeAst::AllOf(elements) => {
                TypeAst::AllOf(elements.into_iter().map(Self::sanitize).collect())
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
            TypeAst::Match { branches } => TypeAst::Match {
                branches: branches
                    .into_iter()
                    .map(|(branch, expr)| {
                        let sanitized_branch = match branch {
                            GenericPattern::Standard { pattern, constraint } => {
                                GenericPattern::Standard {
                                    pattern: Self::sanitize(pattern),
                                    constraint: constraint
                                        .into_iter()
                                        .map(|(var, c_expr)| (var, Self::sanitize(c_expr)))
                                        .collect(),
                                }
                            }
                            GenericPattern::AutoBind { pattern } => {
                                GenericPattern::AutoBind { pattern: Self::sanitize(pattern) }
                            }
                        };
                        (sanitized_branch, Self::sanitize(expr))
                    })
                    .collect(),
            },
            TypeAst::Lambda { patterns } => TypeAst::Lambda {
                patterns: patterns
                    .into_iter()
                    .map(|branch| match branch {
                        GenericPattern::Standard { pattern, constraint } => {
                            GenericPattern::Standard {
                                pattern: Self::sanitize(pattern),
                                constraint: constraint
                                    .into_iter()
                                    .map(|(var, c_expr)| (var, Self::sanitize(c_expr)))
                                    .collect(),
                            }
                        }
                        GenericPattern::AutoBind { pattern } => {
                            GenericPattern::AutoBind { pattern: Self::sanitize(pattern) }
                        }
                    })
                    .collect(),
            },
            TypeAst::Apply { func, arg, auto_cps } => TypeAst::Apply {
                func: Box::new(Self::sanitize(*func)),
                arg: Box::new(Self::sanitize(*arg)),
                auto_cps,
            },
            TypeAst::AtomicOpcode(op) => TypeAst::AtomicOpcode(op),
            TypeAst::FixPoint { param_name, expr } => {
                TypeAst::FixPoint { param_name, expr: Box::new(Self::sanitize(*expr)) }
            }
            TypeAst::Namespace { tag, expr } => {
                TypeAst::Namespace { tag, expr: Box::new(Self::sanitize(*expr)) }
            }
            TypeAst::Generic(bind) => TypeAst::Generic(match &*bind {
                GenericPattern::Standard { pattern, constraint } => {
                    Box::new(GenericPattern::Standard {
                        pattern: Self::sanitize(pattern.clone()),
                        constraint: constraint
                            .iter()
                            .map(|(var, c_expr)| (var.clone(), Self::sanitize(c_expr.clone())))
                            .collect(),
                    })
                }
                GenericPattern::AutoBind { pattern } => {
                    Box::new(GenericPattern::AutoBind { pattern: Self::sanitize(pattern.clone()) })
                }
            }),
            TypeAst::Lazy(inner) => TypeAst::Lazy(Box::new(Self::sanitize(*inner))),
            TypeAst::Mutable { value } => {
                TypeAst::Mutable { value: Box::new(Self::sanitize(*value)) }
            }
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

pub struct FlowResult {
    ty: WithLocation<LinearTypeAst, FlowedMetaData>, // flow后的类型
    captures: HashMap<String, WithLocation<()>>,     // 该类型所捕获的自由变量
}

impl FlowResult {
    pub fn simple(ty: WithLocation<LinearTypeAst, FlowedMetaData>) -> Self {
        FlowResult { ty: ty.with_payload(FlowedMetaData::default()), captures: HashMap::new() }
    }

    pub fn complex(
        ty: WithLocation<LinearTypeAst, FlowedMetaData>,
        captures: HashMap<String, WithLocation<()>>,
    ) -> Self {
        FlowResult { ty: ty.with_payload(FlowedMetaData::default()), captures }
    }

    pub fn ty(&self) -> &WithLocation<LinearTypeAst, FlowedMetaData> {
        &self.ty
    }

    pub fn captures(&self) -> &HashMap<String, WithLocation<()>> {
        &self.captures
    }

    pub fn with_payload(self, payload: FlowedMetaData) -> Self {
        FlowResult { ty: self.ty.with_payload(payload), captures: self.captures }
    }
}

impl LinearTypeAst {
    #[stacksafe::stacksafe]
    pub fn flow(
        &self,
        ctx: &mut ParseContext,
        loc: Option<&SourceLocation>,
        errors: &mut Vec<WithLocation<ParseError>>,
    ) -> FlowResult {
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
            LinearTypeAst::NaturalNumberSet => FlowResult::simple(
                WithLocation::new(LinearTypeAst::NaturalNumberSet, loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::Float => FlowResult::simple(
                WithLocation::new(LinearTypeAst::Float, loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::Char => FlowResult::simple(
                WithLocation::new(LinearTypeAst::Char, loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::NaturalNumberLiteral(v) => FlowResult::simple(
                WithLocation::new(LinearTypeAst::NaturalNumberLiteral(*v), loc)
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
                Ok((var_loc, outgoing)) => {
                    let mut captures = HashMap::new();
                    match outgoing {
                        None => {
                            captures.insert(name.value().clone(), var_loc.clone());
                        }
                        Some(layer) => {
                            if layer > 0 {
                                errors.push(WithLocation::new(
                                    ParseError::OutgoingFixPointReference(
                                        WithLocation::new(self.clone(), loc),
                                        name.clone(),
                                        layer,
                                    ),
                                    loc,
                                ));
                            }
                        }
                    }
                    FlowResult::complex(
                        WithLocation::new(LinearTypeAst::Variable(name.clone()), loc),
                        captures,
                    )
                    .with_payload(
                        FlowedMetaData::default()
                            .with_reference(Some(var_loc.clone()))
                            .with_variable_context(ctx.capture()),
                    )
                }
                Err(context_error) => match context_error {
                    ContextError::NotDeclared(_) => {
                        errors.push(WithLocation::new(
                            ParseError::UseBeforeDeclaration(
                                WithLocation::new(self.clone(), loc),
                                name.clone(),
                            ),
                            loc,
                        ));
                        FlowResult::simple(
                            WithLocation::new(LinearTypeAst::Tuple(vec![]), loc).with_payload(
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
            LinearTypeAst::AnyOf(types) => {
                let mut new_types = Vec::new();
                let mut all_captures = HashMap::new();
                for ty in types {
                    let res = ty.flow(ctx, ty.location(), errors);
                    new_types.push(res.ty);
                    all_captures.extend(res.captures);
                }
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::AnyOf(new_types), loc),
                    all_captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::AllOf(types) => {
                let mut new_types = Vec::new();
                let mut all_captures = HashMap::new();
                for ty in types {
                    let res = ty.flow(ctx, ty.location(), errors);
                    new_types.push(res.ty);
                    all_captures.extend(res.captures);
                }
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::AllOf(new_types), loc),
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
            LinearTypeAst::Generic { expr, constraint } => {
                ctx.enter_generic_scope();
                for (name, _) in constraint {
                    ctx.declare_variable(name.clone())
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
                let mut expr_res = expr.flow(ctx, expr.location(), errors);
                for (name, _) in constraint {
                    expr_res.captures.remove(name.as_str()); // 移除掉泛型变量，因为它们不是自由变量
                }
                captures.extend(expr_res.captures);
                let mut flowed_constraints = Vec::new();
                for (name, constraint_ty) in constraint {
                    let mut constraint_res =
                        constraint_ty.flow(ctx, constraint_ty.location(), errors);
                    flowed_constraints.push((name.clone(), constraint_res.ty));
                    for (name, _) in constraint {
                        constraint_res.captures.remove(name.as_str()); // 移除掉泛型变量，因为它们不是自由变量
                    }
                    captures.extend(constraint_res.captures);
                }

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
                            expr: Box::new(expr_res.ty),
                            constraint: flowed_constraints,
                        },
                        loc,
                    ),
                    captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::StaticFixPoint { param_name, expr } => {
                // 静态不动点类型的处理与普通不动点类型类似，但需要在ctx中声明param_name
                ctx.enter_fixpoint_scope(param_name.clone());
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
                captures.remove(param_name.value()); // 移除掉不动点参数，因为它是递归定义的参数，不应当被视为捕获的自由变量
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
                for (pattern, constraints, body) in branches {
                    // 第一个作用域：处理模式和约束类型
                    ctx.enter_generic_scope();
                    for (name, _) in constraints {
                        match ctx.declare_variable(name.clone()) {
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

                    let mut pattern_res = pattern.flow(ctx, pattern.location(), errors);
                    let mut flowed_constraints = Vec::new();
                    for (name, ctype) in constraints {
                        let mut ctype_res = ctype.flow(ctx, ctype.location(), errors);
                        for (name, _) in constraints {
                            ctype_res.captures.remove(name.value());
                        }
                        flowed_constraints.push((name.clone(), ctype_res.ty));
                        all_captures.extend(ctype_res.captures.clone());
                    }

                    for (name, _) in constraints {
                        pattern_res.captures.remove(name.value());
                    }
                    all_captures.extend(pattern_res.captures);

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

                    // 第二个作用域：处理主体（重新声明变量 + auto_captures）
                    ctx.enter_scope();
                    for (name, _) in constraints {
                        match ctx.declare_variable(name.clone()) {
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
                        match ctx.declare_variable(var_loc.clone().map(|_| var.clone())) {
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

                    let mut body_res = body.flow(ctx, body.location(), errors);
                    for (name, _) in constraints {
                        body_res.captures.remove(name.value());
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

                    new_branches.push((pattern_res.ty, flowed_constraints, body_res.ty));
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
            LinearTypeAst::Lambda { patterns } => {
                let mut new_patterns = Vec::new();
                let mut all_captures = HashMap::new();
                for (pattern, constraints) in patterns {
                    // 第一个作用域：处理模式和约束类型
                    ctx.enter_generic_scope();
                    for (name, _) in constraints {
                        match ctx.declare_variable(name.clone()) {
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

                    let mut pattern_res = pattern.flow(ctx, pattern.location(), errors);
                    let mut flowed_constraints = Vec::new();
                    for (name, ctype) in constraints {
                        let mut ctype_res = ctype.flow(ctx, ctype.location(), errors);
                        for (name, _) in constraints {
                            ctype_res.captures.remove(name.value());
                        }
                        flowed_constraints.push((name.clone(), ctype_res.ty));
                        all_captures.extend(ctype_res.captures.clone());
                    }

                    for (name, _) in constraints {
                        pattern_res.captures.remove(name.value());
                    }
                    all_captures.extend(pattern_res.captures);

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
                    new_patterns.push((pattern_res.ty, flowed_constraints));
                }
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::Lambda { patterns: new_patterns }, loc),
                    all_captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Lazy(inner) => {
                let inner_res = inner.flow(ctx, inner.location(), errors);
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::Lazy(Box::new(inner_res.ty)), loc),
                    inner_res.captures,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Mutable { value } => {
                let value_res = value.flow(ctx, value.location(), errors);
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::Mutable { value: Box::new(value_res.ty) },
                        loc,
                    ),
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
impl LinearTypeAst {
    #[stacksafe::stacksafe]
    pub fn to_type<'roots, T: GcAllocObject<T, Inner = Type<T>>>(
        &self,
        ctx: &mut BuildContext<T>,
        gc: &mut GC<T>,
        roots: &'roots mut RootStack<Type<T>, T>,
        loc: Option<&SourceLocation>,
    ) -> Result<BuildResult<T>, Result<TypeError<Type<T>, T>, ParseError>> {
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
            LinearTypeAst::NaturalNumberSet => {
                Ok(BuildResult::simple(NaturalNumberSet::new(loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::Float => Ok(BuildResult::simple(Float::new(loc.cloned().map(Arc::new)))),
            LinearTypeAst::Char => {
                Ok(BuildResult::simple(Character::new(loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::NaturalNumberLiteral(v) => {
                Ok(BuildResult::simple(NaturalNumber::new(*v, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::FloatLiteral(v) => {
                Ok(BuildResult::simple(FloatValue::new(*v, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::CharLiteral(v) => {
                Ok(BuildResult::simple(CharacterValue::new(*v, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::Variable(var) => {
                if let Some((ty, outgoing)) = ctx.lookup(var.value(), var.location().cloned()) {
                    if let Some(outgoing) = outgoing
                        && outgoing != 0
                    {
                        return Err(Err(ParseError::OutgoingFixPointReference(
                            WithLocation::new(self.clone(), loc),
                            var.clone(),
                            outgoing,
                        )));
                    }
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
            LinearTypeAst::AnyOf(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(ctx, gc, roots, bta.location())?);
                }
                let types = BuildResult::fold(types);
                let empty_env = CaptureEnv::Solved(SmallVec::new());
                Ok(BuildResult::simple(
                    AnyOf::new(types, loc.cloned().map(Arc::new), CaptureEnvList::new(&empty_env))
                        .map_err(Ok)?,
                ))
            }
            LinearTypeAst::AllOf(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(ctx, gc, roots, bta.location())?);
                }
                let types = BuildResult::fold(types);
                let empty_env = CaptureEnv::Solved(SmallVec::new());
                Ok(BuildResult::simple(
                    AllOf::new(types, loc.cloned().map(Arc::new), CaptureEnvList::new(&empty_env))
                        .map_err(Ok)?,
                ))
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
                let mut closure_env: Vec<(Arc<str>, CaptureOrigin)> = Vec::new();
                for (var, capture_loc) in &auto_captures {
                    if let Some(from) = ctx.lookup_function_env(var) {
                        closure_env.push((Arc::from(var.as_str()), from))
                    } else {
                        return Err(Err(ParseError::UseBeforeDeclaration(
                            WithLocation::new(self.clone(), capture_loc.location()),
                            capture_loc.clone().map(|_| var.clone()),
                        )));
                    }
                }
                let mut new_branches = Vec::new();
                for (pattern, constraints, body) in branches {
                    let patterns = constraints
                        .iter()
                        .map(|(name, _)| (name.value().clone(), name.as_ref().map(|_| ())))
                        .collect::<HashMap<_, _>>();
                    ctx.enter_layer(BuildContextLayer::GenericBinding(patterns.clone(), true));
                    let pattern_type: BuildResult<T> =
                        pattern.to_type(ctx, gc, roots, pattern.location())?;
                    ctx.exit_layer();
                    ctx.enter_layer(BuildContextLayer::GenericBinding(patterns.clone(), false));
                    let mut constraint_types = Vec::new();
                    for (name, ctype) in constraints {
                        let ctype_result = ctype.to_type(ctx, gc, roots, ctype.location())?;
                        constraint_types.push((Arc::from(name.value().as_str()), ctype_result.ty));
                    }
                    ctx.exit_layer();

                    ctx.enter_layer(BuildContextLayer::Function {
                        patterns: patterns.clone(),
                        captures: auto_captures.iter().cloned().collect(),
                    });
                    let body_type = body.to_type(ctx, gc, roots, body.location())?;
                    ctx.exit_layer();

                    let empty_env = CaptureEnv::Solved(SmallVec::new());
                    new_branches.push((
                        closure_env.clone(),
                        Constraint::new_constraint(
                            pattern_type.ty,
                            constraint_types,
                            CaptureEnvList::new(&empty_env),
                            loc.cloned().map(Arc::new),
                        )
                        .map_err(Ok)?,
                        body_type.ty,
                    ));
                }
                Ok(BuildResult::simple(Closure::new(new_branches, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::Lambda { patterns } => {
                let mut new_patterns = Vec::new();
                for (pattern, constraints) in patterns {
                    let patterns = constraints
                        .iter()
                        .map(|(name, _)| (name.value().clone(), name.as_ref().map(|_| ())))
                        .collect::<HashMap<_, _>>();
                    ctx.enter_layer(BuildContextLayer::GenericBinding(patterns.clone(), true));
                    let pattern_type: BuildResult<T> =
                        pattern.to_type(ctx, gc, roots, pattern.location())?;
                    ctx.exit_layer();
                    ctx.enter_layer(BuildContextLayer::GenericBinding(patterns.clone(), false));
                    let mut constraint_types = Vec::new();
                    for (name, ctype) in constraints {
                        let ctype_result = ctype.to_type(ctx, gc, roots, ctype.location())?;
                        constraint_types.push((Arc::from(name.value().as_str()), ctype_result.ty));
                    }
                    ctx.exit_layer();

                    let empty_env = CaptureEnv::Solved(SmallVec::new());
                    new_patterns.push(
                        Constraint::new_constraint(
                            pattern_type.ty,
                            constraint_types,
                            CaptureEnvList::new(&empty_env),
                            loc.cloned().map(Arc::new),
                        )
                        .map_err(Ok)?,
                    );
                }
                Ok(BuildResult::simple(Lambda::new(new_patterns, loc.cloned().map(Arc::new))))
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
                    AtomicOpcode::Assign => OpcodeKind::Assign,
                    AtomicOpcode::SetFixPoint => OpcodeKind::SetFixPoint,
                    AtomicOpcode::BuildFixPoint => OpcodeKind::BuildFixPoint,
                    AtomicOpcode::IO(v) => OpcodeKind::IO(v.clone().into()),
                },
                loc.cloned().map(Arc::new),
            ))),
            LinearTypeAst::Namespace { tag, expr } => {
                let expr_type = expr.to_type(ctx, gc, roots, expr.location())?;
                Ok(BuildResult::simple(Namespace::new(
                    tag.value().clone(),
                    &expr_type.ty,
                    loc.cloned().map(Arc::new),
                )))
            }
            LinearTypeAst::Generic { expr, constraint } => {
                let bindings = constraint
                    .iter()
                    .map(|(name, _)| (name.value().clone(), name.as_ref().map(|_| ())))
                    .collect::<HashMap<_, _>>();

                ctx.enter_layer(BuildContextLayer::GenericBinding(bindings.clone(), true));
                let expr_type = expr.to_type(ctx, gc, roots, expr.location())?;
                ctx.exit_layer();
                ctx.enter_layer(BuildContextLayer::GenericBinding(bindings.clone(), false));
                let mut constraint_types = Vec::new();
                for (name, ctype) in constraint {
                    let ctype_result = ctype.to_type(ctx, gc, roots, ctype.location())?;
                    constraint_types.push((Arc::from(name.value().as_str()), ctype_result.ty));
                }
                ctx.exit_layer();

                let empty_env = CaptureEnv::Solved(SmallVec::new());
                Ok(BuildResult::simple(
                    Constraint::new(
                        expr_type.ty,
                        constraint_types,
                        CaptureEnvList::new(&empty_env),
                        loc.cloned().map(Arc::new),
                    )
                    .map_err(Ok)?,
                ))
            }
            LinearTypeAst::Lazy(inner) => {
                let inner_type = inner.to_type(ctx, gc, roots, inner.location())?;
                Ok(BuildResult::simple(Lazy::new(&inner_type.ty, loc.cloned().map(Arc::new))))
            }
            LinearTypeAst::Mutable { value } => {
                let value_type = value.to_type(ctx, gc, roots, value.location())?;
                Ok(BuildResult::simple(Mutable::new(
                    &value_type.ty,
                    loc.cloned().map(Arc::new),
                    gc,
                    roots,
                )))
            }
            LinearTypeAst::SubOf { value } => {
                let value_type = value.to_type(ctx, gc, roots, value.location())?;
                Ok(BuildResult::simple(SubOf::new(&value_type.ty, loc.cloned().map(Arc::new))))
            }
        }
    }
}
