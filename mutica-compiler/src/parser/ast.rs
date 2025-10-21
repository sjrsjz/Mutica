use crate::parser::lexer::{LexerToken, LexicalError};
use crate::parser::{
    BuildContext, ContextError, MultiFileBuilder, MultiFileBuilderError, ParseContext, ParseError,
    PatternCounter, SourceLocation, WithLocation,
};
use lalrpop_util::ErrorRecovery;
use mutica_core::arc_gc::gc::GC;
use mutica_core::as_type;
use mutica_core::types::character::Character;
use mutica_core::types::character_value::CharacterValue;
use mutica_core::types::closure::{Closure, ClosureEnv};
use mutica_core::types::fixpoint::FixPoint;
use mutica_core::types::float::Float;
use mutica_core::types::float_value::FloatValue;
use mutica_core::types::generalize::Generalize;
use mutica_core::types::integer::Integer;
use mutica_core::types::integer_value::IntegerValue;
use mutica_core::types::invoke::Invoke;
use mutica_core::types::lazy::Lazy;
use mutica_core::types::list::List;
use mutica_core::types::namespace::Namespace;
use mutica_core::types::opcode::Opcode;
use mutica_core::types::pattern::Pattern;
use mutica_core::types::rot::Rotate;
use mutica_core::types::specialize::Specialize;
use mutica_core::types::tuple::Tuple;
use mutica_core::types::type_bound::TypeBound;
use mutica_core::types::variable::Variable;
use mutica_core::types::{GcAllocObject, Type, TypeError};
use mutica_core::util::rootstack::RootStack;
use std::collections::HashMap;
use std::ops::Deref;
use std::path::PathBuf;

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
    IO(String),
    Neg,
}

#[derive(Debug, Clone)]
pub enum TypeAst {
    ParseError(ErrorRecovery<usize, LexerToken, LexicalError>),
    Import(String), // 用于import语句
    Int,
    Float, // Added for float type
    Char,
    Top,
    Bottom,
    DiscardPattern, // 用于表示_模式
    IntLiteral(i64),
    FloatLiteral(f64), // Added for float literal
    CharLiteral(char),
    Variable(String),
    Tuple(Vec<WithLocation<TypeAst>>),
    List(Vec<WithLocation<TypeAst>>),
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
    Expression {
        binding_patterns: Vec<WithLocation<TypeAst>>,
        binding_types: Vec<WithLocation<TypeAst>>,
        body: Box<WithLocation<TypeAst>>,
    },
    Match {
        branches: Vec<(WithLocation<TypeAst>, WithLocation<TypeAst>)>, // pattern, expr
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
    Namespace {
        tag: String,
        expr: Box<WithLocation<TypeAst>>,
    },
    Pattern {
        name: String,
        expr: Box<WithLocation<TypeAst>>,
    },
    Literal(Box<WithLocation<TypeAst>>),
    Rot {
        value: Box<WithLocation<TypeAst>>,
    },
}

#[derive(Debug, Clone)]
pub enum BasicTypeAst {
    Int,
    Float,
    Char,
    Top,
    Bottom,
    IntLiteral(i64),
    FloatLiteral(f64),
    CharLiteral(char),
    Variable(String),
    Tuple(Vec<WithLocation<BasicTypeAst>>),
    List(Vec<WithLocation<BasicTypeAst>>),
    Generalize(Vec<WithLocation<BasicTypeAst>>),
    Specialize(Vec<WithLocation<BasicTypeAst>>),
    Invoke {
        func: Box<WithLocation<BasicTypeAst>>,
        arg: Box<WithLocation<BasicTypeAst>>,
        continuation: Option<Box<WithLocation<BasicTypeAst>>>,
        perform_handler: Option<Box<WithLocation<BasicTypeAst>>>,
    },
    Match {
        branches: Vec<(WithLocation<BasicTypeAst>, WithLocation<BasicTypeAst>)>, // pattern, expr
    },
    Apply {
        func: Box<WithLocation<BasicTypeAst>>,
        arg: Box<WithLocation<BasicTypeAst>>,
        handler: Option<Box<WithLocation<BasicTypeAst>>>,
    },
    AtomicOpcode(AtomicOpcode),
    FixPoint {
        param_name: String,
        expr: Box<WithLocation<BasicTypeAst>>,
    },
    Namespace {
        tag: String,
        expr: Box<WithLocation<BasicTypeAst>>,
    },
    Pattern {
        name: String,
        expr: Box<WithLocation<BasicTypeAst>>,
    },
    Literal(Box<WithLocation<BasicTypeAst>>),
    Rot {
        value: Box<WithLocation<BasicTypeAst>>,
    },
}

pub struct LinearizeContext {
    invoke_tmpvar_counter: usize, // 用于生成唯一的
}

impl LinearizeContext {
    pub fn new() -> Self {
        Self {
            invoke_tmpvar_counter: 0,
        }
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
        Self {
            bindings: Vec::new(),
            tail_type: ty,
        }
    }

    pub fn new_with_binding(
        bindings: Vec<(
            WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
            WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
            Option<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
            String,
        )>,
        ty: WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
    ) -> Self {
        Self {
            bindings,
            tail_type: ty,
        }
    }

    pub fn new_apply(
        func: LinearizeResult<'ast>,
        arg: LinearizeResult<'ast>,
        handler: Option<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
        allocated_tmpvar_name: String,
    ) -> Self {
        let mut bindings = func.bindings;
        bindings.extend(arg.bindings);
        bindings.push((
            func.tail_type,
            arg.tail_type,
            handler,
            allocated_tmpvar_name.clone(),
        ));
        Self {
            bindings,
            tail_type: LinearTypeAst::Variable(allocated_tmpvar_name).into(),
        }
    }

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
            ty = WithLocation::from(LinearTypeAst::Invoke {
                func: Box::new(f),
                arg: Box::new(a),
                continuation: {
                    if let LinearTypeAst::Variable(v) = ty.value()
                        && v.eq(&tmpvar)
                    {
                        None // TCO（尾调用优化）
                    } else {
                        Some(
                            WithLocation::from(LinearTypeAst::Match {
                                auto_captures: HashMap::new(),
                                branches: vec![(
                                    WithLocation::from(LinearTypeAst::Pattern {
                                        name: tmpvar,
                                        expr: Box::new(WithLocation::from(LinearTypeAst::Top)),
                                    }),
                                    ty.clone(),
                                )],
                            })
                            .into(),
                        )
                    }
                },
                perform_handler: handler.map(|h| Box::new(h)),
            })
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
            BasicTypeAst::Int => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::Int, loc))
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
            BasicTypeAst::IntLiteral(v) => {
                LinearizeResult::new_simple(WithLocation::new(LinearTypeAst::IntLiteral(*v), loc))
            }
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
                    .into_iter()
                    .map(|e| e.linearize(ctx, e.location()))
                    .collect::<Vec<_>>();
                let ty =
                    LinearTypeAst::Tuple(elements.iter().map(|e| e.tail_type().clone()).collect());

                LinearizeResult::new_with_binding(
                    elements
                        .into_iter()
                        .flat_map(|e| e.bindings.into_iter())
                        .collect(),
                    WithLocation::new(ty, loc),
                )
            }
            BasicTypeAst::List(v) => {
                let elements = v
                    .into_iter()
                    .map(|e| e.linearize(ctx, e.location()))
                    .collect::<Vec<_>>();
                let ty =
                    LinearTypeAst::List(elements.iter().map(|e| e.tail_type().clone()).collect());
                LinearizeResult::new_with_binding(
                    elements
                        .into_iter()
                        .flat_map(|e| e.bindings.into_iter())
                        .collect(),
                    WithLocation::new(ty, loc),
                )
            }
            BasicTypeAst::Generalize(v) => {
                let elements = v
                    .into_iter()
                    .map(|e| e.linearize(ctx, e.location()))
                    .collect::<Vec<_>>();
                let ty = LinearTypeAst::Generalize(
                    elements.iter().map(|e| e.tail_type().clone()).collect(),
                );
                LinearizeResult::new_with_binding(
                    elements
                        .into_iter()
                        .flat_map(|e| e.bindings.into_iter())
                        .collect(),
                    WithLocation::new(ty, loc),
                )
            }
            BasicTypeAst::Specialize(v) => {
                let elements = v
                    .into_iter()
                    .map(|e| e.linearize(ctx, e.location()))
                    .collect::<Vec<_>>();
                let ty = LinearTypeAst::Specialize(
                    elements.iter().map(|e| e.tail_type().clone()).collect(),
                );
                LinearizeResult::new_with_binding(
                    elements
                        .into_iter()
                        .flat_map(|e| e.bindings.into_iter())
                        .collect(),
                    WithLocation::new(ty, loc),
                )
            }
            BasicTypeAst::Invoke {
                func,
                arg,
                continuation,
                perform_handler,
            } => {
                let func = func.linearize(ctx, func.location());
                let arg = arg.linearize(ctx, arg.location());
                let continuation = match continuation {
                    Some(continuation) => Some(Box::new(
                        continuation.linearize(ctx, continuation.location()),
                    )),
                    None => None,
                };
                let perform_handler = match perform_handler {
                    Some(perform_handler) => Some(Box::new(
                        perform_handler.linearize(ctx, perform_handler.location()),
                    )),
                    None => None,
                };
                let ty = LinearTypeAst::Invoke {
                    func: func.tail_type().clone().into(),
                    arg: arg.tail_type().clone().into(),
                    continuation: continuation.as_ref().map(|c| c.tail_type().clone().into()),
                    perform_handler: perform_handler
                        .as_ref()
                        .map(|r| r.tail_type().clone().into()),
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
                // let mut bindings = Vec::new();
                // let pattern = pattern.linearize(ctx, pattern.location());
                // bindings.extend(pattern.bindings.clone());
                // // fail_branch 是严格独立上下文的，因此直接线性化不参与CPS
                // let fail_branch = match fail_branch {
                //     Some(fail_branch) => Some(Box::new(
                //         fail_branch
                //             .linearize(ctx, fail_branch.location())
                //             .finalize(),
                //     )),
                //     None => None,
                // };
                // let ty = LinearTypeAst::Closure {
                //     pattern: Box::new(pattern.tail_type().clone()),
                //     auto_captures: HashMap::new(),
                //     // body 也是严格独立上下文的，因此直接线性化不参与CPS
                //     body: Box::new(body.linearize(ctx, body.location()).finalize()),
                //     fail_branch,
                // };
                // LinearizeResult::new_with_binding(bindings, WithLocation::new(ty, loc))
                let mut linearized_branches = Vec::new();
                let mut bindings = Vec::new();
                for (pat, expr) in branches {
                    let pat = pat.linearize(ctx, pat.location());
                    bindings.extend(pat.bindings.clone());
                    let expr = expr.linearize(ctx, expr.location()).finalize(); // expr 是严格独立上下文的，因此直接线性化不参与CPS
                    linearized_branches.push((pat.tail_type().clone(), expr));
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
            BasicTypeAst::FixPoint { param_name, expr } => {
                let expr = expr.linearize(ctx, expr.location());
                let ty = LinearTypeAst::FixPoint {
                    param_name: param_name.clone(),
                    expr: Box::new(expr.tail_type().clone()),
                };
                LinearizeResult::new_with_binding(expr.bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Namespace { tag, expr } => {
                let expr = expr.linearize(ctx, expr.location());
                let ty = LinearTypeAst::Namespace {
                    tag: tag.clone(),
                    expr: Box::new(expr.tail_type().clone()),
                };
                LinearizeResult::new_with_binding(expr.bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Pattern { name, expr } => {
                let expr = expr.linearize(ctx, expr.location());
                let ty = LinearTypeAst::Pattern {
                    name: name.clone(),
                    expr: Box::new(expr.tail_type().clone()),
                };
                LinearizeResult::new_with_binding(expr.bindings, WithLocation::new(ty, loc))
            }
            BasicTypeAst::Literal(inner) => LinearizeResult::new_simple(WithLocation::new(
                LinearTypeAst::Literal(Box::new(inner.linearize(ctx, inner.location()).finalize())),
                loc,
            )),
            BasicTypeAst::Rot { value } => {
                let value = value.linearize(ctx, value.location());
                let ty = LinearTypeAst::Rot {
                    value: Box::new(value.tail_type().clone()),
                };
                LinearizeResult::new_with_binding(value.bindings, WithLocation::new(ty, loc))
            }
        }
    }
}

#[derive(Debug, Clone)]
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
        Self {
            variable_context,
            ..self
        }
    }
}

impl Default for FlowedMetaData<'_> {
    fn default() -> Self {
        Self {
            reference: None,
            variable_context: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LinearTypeAst<'ast> {
    Int,
    Char,
    Float,
    Top,
    Bottom,
    IntLiteral(i64),
    FloatLiteral(f64),
    CharLiteral(char),
    Variable(String), // None 表示续体
    Tuple(Vec<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>),
    List(Vec<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>),
    Generalize(Vec<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>),
    Specialize(Vec<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>),
    Match {
        auto_captures: HashMap<String, WithLocation<()>>,
        branches: Vec<(
            WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
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
    FixPoint {
        param_name: String,
        expr: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
    },
    Namespace {
        tag: String,
        expr: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
    },
    Pattern {
        name: String,
        expr: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
    },
    Literal(Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>),
    Rot {
        value: Box<WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>,
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
                panic!(
                    "Cannot convert TypeAst::ParseError to BasicTypeAst: {:?}",
                    span
                )
            }
            TypeAst::Int => WithLocation::new(BasicTypeAst::Int, loc),
            TypeAst::Float => WithLocation::new(BasicTypeAst::Float, loc),
            TypeAst::Char => WithLocation::new(BasicTypeAst::Char, loc),
            TypeAst::Top => WithLocation::new(BasicTypeAst::Top, loc),
            TypeAst::Bottom => WithLocation::new(BasicTypeAst::Bottom, loc),
            TypeAst::DiscardPattern => WithLocation::new(BasicTypeAst::Tuple(vec![]), loc), // discard 只允许丢弃unit
            TypeAst::IntLiteral(v) => WithLocation::new(BasicTypeAst::IntLiteral(*v), loc),
            TypeAst::FloatLiteral(v) => WithLocation::new(BasicTypeAst::FloatLiteral(*v), loc),
            TypeAst::CharLiteral(v) => WithLocation::new(BasicTypeAst::CharLiteral(*v), loc),
            TypeAst::Variable(name) => WithLocation::new(BasicTypeAst::Variable(name.clone()), loc),
            TypeAst::Tuple(elements) => WithLocation::new(
                BasicTypeAst::Tuple(
                    elements
                        .into_iter()
                        .map(|e| e.into_basic(multifile_builder, e.location()))
                        .collect(),
                ),
                loc,
            ),
            TypeAst::List(elements) => WithLocation::new(
                BasicTypeAst::List(
                    elements
                        .into_iter()
                        .map(|e| e.into_basic(multifile_builder, e.location()))
                        .collect(),
                ),
                loc,
            ),
            TypeAst::Generalize(elements) => WithLocation::new(
                BasicTypeAst::Generalize(
                    elements
                        .into_iter()
                        .map(|e| e.into_basic(multifile_builder, e.location()))
                        .collect(),
                ),
                loc,
            ),
            TypeAst::Specialize(elements) => WithLocation::new(
                BasicTypeAst::Specialize(
                    elements
                        .into_iter()
                        .map(|e| e.into_basic(multifile_builder, e.location()))
                        .collect(),
                ),
                loc,
            ),
            TypeAst::Invoke {
                func,
                arg,
                continuation,
                perform_handler,
            } => WithLocation::new(
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
            TypeAst::HandleWith {
                closure,
                init_val,
                handler,
            } => WithLocation::new(
                BasicTypeAst::Apply {
                    func: Box::new(closure.into_basic(multifile_builder, closure.location())),
                    arg: Box::new(init_val.into_basic(multifile_builder, init_val.location())),
                    handler: Some(Box::new(
                        handler.into_basic(multifile_builder, handler.location()),
                    )),
                },
                loc,
            ),
            TypeAst::Expression {
                binding_patterns,
                binding_types,
                body,
            } => {
                // 转换为嵌套的闭包和应用
                let mut expr = body.into_basic(multifile_builder, body.location());
                for (pat, ty) in binding_patterns
                    .into_iter()
                    .rev()
                    .zip(binding_types.into_iter().rev())
                {
                    expr = WithLocation::new(
                        BasicTypeAst::Apply {
                            func: Box::new(WithLocation::new(
                                BasicTypeAst::Match {
                                    branches: vec![(
                                        pat.into_basic(multifile_builder, pat.location()),
                                        expr,
                                    )],
                                },
                                pat.location(),
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
                        .into_iter()
                        .map(|(pat, expr)| {
                            (
                                pat.into_basic(multifile_builder, pat.location()),
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
                            branches: vec![(
                                WithLocation::new(
                                    BasicTypeAst::Tuple(vec![
                                        WithLocation::new(
                                            BasicTypeAst::Pattern {
                                                name: "eq#left".to_string(),
                                                expr: Box::new(WithLocation::new(
                                                    BasicTypeAst::Top,
                                                    loc,
                                                )),
                                            },
                                            loc,
                                        ),
                                        WithLocation::new(
                                            BasicTypeAst::Pattern {
                                                name: "eq#right".to_string(),
                                                expr: Box::new(WithLocation::new(
                                                    BasicTypeAst::Top,
                                                    loc,
                                                )),
                                            },
                                            loc,
                                        ),
                                    ]),
                                    loc,
                                ),
                                WithLocation::new(
                                    BasicTypeAst::Specialize(vec![
                                        WithLocation::new(
                                            BasicTypeAst::Apply {
                                                func: Box::new(WithLocation::new(
                                                    BasicTypeAst::AtomicOpcode(AtomicOpcode::Is),
                                                    loc,
                                                )),
                                                arg: Box::new(WithLocation::new(
                                                    BasicTypeAst::Tuple(vec![
                                                        WithLocation::new(
                                                            BasicTypeAst::Variable(
                                                                "eq#left".to_string(),
                                                            ),
                                                            loc,
                                                        ),
                                                        WithLocation::new(
                                                            BasicTypeAst::Variable(
                                                                "eq#right".to_string(),
                                                            ),
                                                            loc,
                                                        ),
                                                    ]),
                                                    loc,
                                                )),
                                                handler: None,
                                            },
                                            loc,
                                        ),
                                        WithLocation::new(
                                            BasicTypeAst::Apply {
                                                func: Box::new(WithLocation::new(
                                                    BasicTypeAst::AtomicOpcode(AtomicOpcode::Is),
                                                    loc,
                                                )),
                                                arg: Box::new(WithLocation::new(
                                                    BasicTypeAst::Tuple(vec![
                                                        WithLocation::new(
                                                            BasicTypeAst::Variable(
                                                                "eq#right".to_string(),
                                                            ),
                                                            loc,
                                                        ),
                                                        WithLocation::new(
                                                            BasicTypeAst::Variable(
                                                                "eq#left".to_string(),
                                                            ),
                                                            loc,
                                                        ),
                                                    ]),
                                                    loc,
                                                )),
                                                handler: None,
                                            },
                                            loc,
                                        ),
                                    ]),
                                    loc,
                                ),
                            )],
                        },
                        loc,
                    )),
                    arg: Box::new(WithLocation::new(
                        BasicTypeAst::Tuple(vec![
                            left.into_basic(multifile_builder, left.location()),
                            right.into_basic(multifile_builder, right.location()),
                        ]),
                        loc,
                    )),
                    handler: None,
                },
                loc,
            ),
            TypeAst::Neq { left, right } => {
                // a != b  ===  !(a == b)
                WithLocation::<_, ()>::new(
                    TypeAst::Not {
                        value: WithLocation::new(
                            TypeAst::Eq {
                                left: left.clone(),
                                right: right.clone(),
                            },
                            loc,
                        )
                        .into(),
                    },
                    loc,
                )
                .into_basic(multifile_builder, loc)
            }
            TypeAst::Not { value } => WithLocation::new(
                BasicTypeAst::Apply {
                    func: WithLocation::new(
                        BasicTypeAst::Match {
                            branches: vec![
                                (
                                    WithLocation::new(BasicTypeAst::Bottom, loc),
                                    WithLocation::new(BasicTypeAst::Top, loc),
                                ),
                                (
                                    WithLocation::new(BasicTypeAst::Top, loc),
                                    WithLocation::new(BasicTypeAst::Bottom, loc),
                                ),
                            ],
                        },
                        loc,
                    )
                    .into(),
                    arg: value.into_basic(multifile_builder, value.location()).into(),
                    handler: None,
                },
                loc,
            ),
            TypeAst::AtomicOpcode(binary_op) => {
                WithLocation::new(BasicTypeAst::AtomicOpcode(binary_op.clone()), loc)
            }
            TypeAst::FixPoint { param_name, expr } => WithLocation::new(
                BasicTypeAst::FixPoint {
                    param_name: param_name.clone(),
                    expr: Box::new(expr.into_basic(multifile_builder, expr.location())),
                },
                loc,
            ),
            TypeAst::Namespace { tag, expr } => WithLocation::new(
                BasicTypeAst::Namespace {
                    tag: tag.clone(),
                    expr: Box::new(expr.into_basic(multifile_builder, expr.location())),
                },
                loc,
            ),
            TypeAst::Pattern { name, expr } => WithLocation::new(
                BasicTypeAst::Pattern {
                    name: name.clone(),
                    expr: Box::new(expr.into_basic(multifile_builder, expr.location())),
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
            TypeAst::Rot { value } => WithLocation::new(
                BasicTypeAst::Rot {
                    value: Box::new(value.into_basic(multifile_builder, value.location())),
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
            TypeAst::Int
            | TypeAst::Float
            | TypeAst::Char
            | TypeAst::Top
            | TypeAst::Bottom
            | TypeAst::DiscardPattern
            | TypeAst::IntLiteral(_)
            | TypeAst::FloatLiteral(_)
            | TypeAst::CharLiteral(_)
            | TypeAst::Variable(_)
            | TypeAst::Import(_) => {}
            TypeAst::Tuple(elements)
            | TypeAst::List(elements)
            | TypeAst::Generalize(elements)
            | TypeAst::Specialize(elements) => {
                for elem in elements {
                    elem.collect_errors(errors);
                }
            }
            TypeAst::Invoke {
                func,
                arg,
                continuation,
                perform_handler,
            } => {
                func.collect_errors(errors);
                arg.collect_errors(errors);
                if let Some(continuation) = continuation {
                    continuation.collect_errors(errors);
                }
                if let Some(perform_handler) = perform_handler {
                    perform_handler.collect_errors(errors);
                }
            }
            TypeAst::HandleWith {
                closure,
                init_val,
                handler: catch,
            } => {
                closure.collect_errors(errors);
                init_val.collect_errors(errors);
                catch.collect_errors(errors);
            }
            TypeAst::Expression {
                binding_patterns,
                binding_types,
                body,
            } => {
                for pat in binding_patterns {
                    pat.collect_errors(errors);
                }
                for ty in binding_types {
                    ty.collect_errors(errors);
                }
                body.collect_errors(errors);
            }
            TypeAst::Match { branches } => {
                for (pat, expr) in branches {
                    pat.collect_errors(errors);
                    expr.collect_errors(errors);
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
            TypeAst::Pattern { expr, .. } => {
                expr.collect_errors(errors);
            }
            TypeAst::Literal(inner) => {
                inner.collect_errors(errors);
            }
            TypeAst::Rot { value } => {
                value.collect_errors(errors);
            }
        }
    }

    pub fn sanitize(ast: WithLocation<Self>) -> WithLocation<Self> {
        ast.map(|ast| match ast {
            TypeAst::ParseError(_) => TypeAst::Bottom,
            TypeAst::Int
            | TypeAst::Float
            | TypeAst::Char
            | TypeAst::Top
            | TypeAst::Bottom
            | TypeAst::DiscardPattern
            | TypeAst::IntLiteral(_)
            | TypeAst::FloatLiteral(_)
            | TypeAst::CharLiteral(_)
            | TypeAst::Variable(_)
            | TypeAst::Import(_) => ast,
            TypeAst::Tuple(elements) => {
                TypeAst::Tuple(elements.into_iter().map(Self::sanitize).collect())
            }
            TypeAst::List(elements) => {
                TypeAst::List(elements.into_iter().map(Self::sanitize).collect())
            }
            TypeAst::Generalize(elements) => {
                TypeAst::Generalize(elements.into_iter().map(Self::sanitize).collect())
            }
            TypeAst::Specialize(elements) => {
                TypeAst::Specialize(elements.into_iter().map(Self::sanitize).collect())
            }
            TypeAst::Invoke {
                func,
                arg,
                continuation,
                perform_handler,
            } => TypeAst::Invoke {
                func: Box::new(Self::sanitize(*func)),
                arg: Box::new(Self::sanitize(*arg)),
                continuation: continuation.map(|c| Box::new(Self::sanitize(*c))),
                perform_handler: perform_handler.map(|h| Box::new(Self::sanitize(*h))),
            },
            TypeAst::HandleWith {
                closure,
                init_val,
                handler: catch,
            } => TypeAst::HandleWith {
                closure: Box::new(Self::sanitize(*closure)),
                init_val: Box::new(Self::sanitize(*init_val)),
                handler: Box::new(Self::sanitize(*catch)),
            },
            TypeAst::Expression {
                binding_patterns,
                binding_types,
                body,
            } => TypeAst::Expression {
                binding_patterns: binding_patterns.into_iter().map(Self::sanitize).collect(),
                binding_types: binding_types.into_iter().map(Self::sanitize).collect(),
                body: Box::new(Self::sanitize(*body)),
            },
            TypeAst::Match { branches } => TypeAst::Match {
                branches: branches
                    .into_iter()
                    .map(|(p, e)| (Self::sanitize(p), Self::sanitize(e)))
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
            TypeAst::Not { value } => TypeAst::Not {
                value: Box::new(Self::sanitize(*value)),
            },
            TypeAst::AtomicOpcode(op) => TypeAst::AtomicOpcode(op),
            TypeAst::FixPoint { param_name, expr } => TypeAst::FixPoint {
                param_name,
                expr: Box::new(Self::sanitize(*expr)),
            },
            TypeAst::Namespace { tag, expr } => TypeAst::Namespace {
                tag,
                expr: Box::new(Self::sanitize(*expr)),
            },
            TypeAst::Pattern { name, expr } => TypeAst::Pattern {
                name,
                expr: Box::new(Self::sanitize(*expr)),
            },
            TypeAst::Literal(inner) => TypeAst::Literal(Box::new(Self::sanitize(*inner))),
            TypeAst::Rot { value } => TypeAst::Rot {
                value: Box::new(Self::sanitize(*value)),
            },
        })
    }
}

pub struct PatternEnv {
    declared: HashMap<String, WithLocation<()>>, // 已声明的模式变量
}

impl Deref for PatternEnv {
    type Target = HashMap<String, WithLocation<()>>;
    fn deref(&self) -> &Self::Target {
        &self.declared
    }
}

impl PatternEnv {
    pub fn new() -> Self {
        PatternEnv {
            declared: HashMap::new(),
        }
    }

    pub fn extend(&mut self, names: impl IntoIterator<Item = WithLocation<String>>) {
        for name in names {
            if self.declared.contains_key(name.value()) {
                return; // 重复声明，忽略
            }
            self.declared
                .insert(name.value().clone(), WithLocation::new((), name.location()));
        }
    }
}

impl IntoIterator for PatternEnv {
    type Item = (String, WithLocation<()>);
    type IntoIter = std::collections::hash_map::IntoIter<String, WithLocation<()>>;

    fn into_iter(self) -> Self::IntoIter {
        self.declared.into_iter()
    }
}

pub struct FlowResult<'ast> {
    ty: WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>, // flow后的类型
    captures: HashMap<String, WithLocation<()>>,                 // 该类型所捕获的自由变量
    patterns: PatternEnv,                                        // 该类型中出现的所有模式变量
}

impl<'ast> FlowResult<'ast> {
    pub fn simple(ty: WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>) -> Self {
        FlowResult {
            ty: ty.with_payload(FlowedMetaData::default()),
            captures: HashMap::new(),
            patterns: PatternEnv::new(),
        }
    }

    pub fn complex(
        ty: WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
        captures: HashMap<String, WithLocation<()>>,
        patterns: PatternEnv,
    ) -> Self {
        FlowResult {
            ty: ty.with_payload(FlowedMetaData::default()),
            captures,
            patterns,
        }
    }

    pub fn ty(&self) -> &WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>> {
        &self.ty
    }

    pub fn captures(&self) -> &HashMap<String, WithLocation<()>> {
        &self.captures
    }

    pub fn patterns(&self) -> &PatternEnv {
        &self.patterns
    }

    pub fn with_payload(self, payload: FlowedMetaData<'ast>) -> Self {
        FlowResult {
            ty: self.ty.with_payload(payload),
            captures: self.captures,
            patterns: self.patterns,
        }
    }
}

impl<'ast> LinearTypeAst<'ast> {
    #[stacksafe::stacksafe]
    pub fn flow(
        &self,
        ctx: &mut ParseContext,
        pattern_mode: bool,
        loc: Option<&SourceLocation>,
        errors: &mut Vec<WithLocation<ParseError<'ast>>>,
    ) -> FlowResult<'ast> {
        match self {
            LinearTypeAst::Int => FlowResult::simple(
                WithLocation::new(LinearTypeAst::Int, loc)
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
            LinearTypeAst::Top => FlowResult::simple(
                WithLocation::new(LinearTypeAst::Top, loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::Bottom => FlowResult::simple(
                WithLocation::new(LinearTypeAst::Bottom, loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::IntLiteral(v) => FlowResult::simple(
                WithLocation::new(LinearTypeAst::IntLiteral(*v), loc)
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
                        PatternEnv::new(),
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
                let mut all_patterns = PatternEnv::new();
                for elem in elements {
                    let res = elem.flow(ctx, pattern_mode, elem.location(), errors);
                    new_elements.push(res.ty);
                    all_captures.extend(res.captures);
                    all_patterns.extend(
                        res.patterns
                            .into_iter()
                            .map(|(name, loc)| WithLocation::new(name, loc.location())),
                    );
                }
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::Tuple(new_elements), loc),
                    all_captures,
                    all_patterns,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::List(elements) => {
                let mut new_elements = Vec::new();
                let mut all_captures = HashMap::new();
                let mut all_patterns = PatternEnv::new();
                for elem in elements {
                    let res = elem.flow(ctx, pattern_mode, elem.location(), errors);
                    new_elements.push(res.ty);
                    all_captures.extend(res.captures);
                    all_patterns.extend(
                        res.patterns
                            .into_iter()
                            .map(|(name, loc)| WithLocation::new(name, loc.location())),
                    );
                }
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::List(new_elements), loc),
                    all_captures,
                    all_patterns,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Generalize(types) => {
                let mut new_types = Vec::new();
                let mut all_captures = HashMap::new();
                let mut all_patterns = PatternEnv::new();
                for ty in types {
                    let res = ty.flow(ctx, pattern_mode, ty.location(), errors);
                    new_types.push(res.ty);
                    all_captures.extend(res.captures);
                    all_patterns.extend(
                        res.patterns
                            .into_iter()
                            .map(|(name, loc)| WithLocation::new(name, loc.location())),
                    );
                }
                if !all_patterns.is_empty() {
                    // 泛化类型中不允许出现模式变量，因为泛化类型是乱序的
                    errors.push(WithLocation::new(
                        ParseError::AmbiguousPattern(WithLocation::new(self.clone(), loc)),
                        loc,
                    ));
                    all_patterns = PatternEnv::new(); // 清空模式变量
                }
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::Generalize(new_types), loc),
                    all_captures,
                    all_patterns,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Specialize(types) => {
                let mut new_types = Vec::new();
                let mut all_captures = HashMap::new();
                let mut all_patterns = PatternEnv::new();
                for ty in types {
                    let res = ty.flow(ctx, pattern_mode, ty.location(), errors);
                    new_types.push(res.ty);
                    all_captures.extend(res.captures);
                    all_patterns.extend(
                        res.patterns
                            .into_iter()
                            .map(|(name, loc)| WithLocation::new(name, loc.location())),
                    );
                }
                if !all_patterns.is_empty() {
                    // 专化类型中不允许出现模式变量，因为专化类型是乱序的
                    errors.push(WithLocation::new(
                        ParseError::AmbiguousPattern(WithLocation::new(self.clone(), loc)),
                        loc,
                    ));
                    all_patterns = PatternEnv::new(); // 清空模式变量
                }
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::Specialize(new_types), loc),
                    all_captures,
                    all_patterns,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Invoke {
                func,
                arg,
                continuation,
                perform_handler,
            } => {
                let func_res = func.flow(ctx, pattern_mode, func.location(), errors);
                let arg_res = arg.flow(ctx, pattern_mode, arg.location(), errors);
                let cont_res = continuation.as_ref().map(|continuation| {
                    continuation.flow(ctx, pattern_mode, continuation.location(), errors)
                });
                let perform_handler_res = perform_handler.as_ref().map(|perform_handler| {
                    perform_handler.flow(ctx, pattern_mode, perform_handler.location(), errors)
                });
                let mut all_captures = func_res.captures;
                all_captures.extend(arg_res.captures);
                if let Some(cont_res) = &cont_res {
                    all_captures.extend(cont_res.captures.clone());
                }
                if let Some(perform_handler_res) = &perform_handler_res {
                    all_captures.extend(perform_handler_res.captures.clone());
                }

                let mut all_patterns = func_res.patterns;
                all_patterns.extend(
                    arg_res
                        .patterns
                        .into_iter()
                        .map(|(name, loc)| WithLocation::new(name, loc.location())),
                );
                if let Some(cont_res) = &cont_res {
                    all_patterns.extend(
                        cont_res
                            .patterns
                            .clone()
                            .into_iter()
                            .map(|(name, loc)| WithLocation::new(name, loc.location())),
                    );
                }
                if let Some(perform_handler_res) = &perform_handler_res {
                    all_patterns.extend(
                        perform_handler_res
                            .patterns
                            .clone()
                            .into_iter()
                            .map(|(name, loc)| WithLocation::new(name, loc.location())),
                    );
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
                    all_patterns,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::AtomicOpcode(atomic_opcode) => FlowResult::simple(
                WithLocation::new(LinearTypeAst::AtomicOpcode(atomic_opcode.clone()), loc)
                    .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture())),
            ),
            LinearTypeAst::FixPoint { param_name, expr } => {
                ctx.enter_scope();
                match ctx.declare_variable(param_name.clone(), loc) {
                    Ok(_) => {}
                    Err(ContextError::EmptyContext) => {
                        panic!(
                            "Internal error: Context should not be empty when declaring a variable"
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
                // 递归函数的表达式中不允许出现模式变量
                let mut expr_res = expr.flow(ctx, false, expr.location(), errors);
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
                // 移除掉param_name，因为它是递归函数的参数，不应当被视为捕获的自由变量
                expr_res.captures.remove(param_name);
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::FixPoint {
                            param_name: param_name.clone(),
                            expr: Box::new(expr_res.ty),
                        },
                        loc,
                    ),
                    expr_res.captures,
                    PatternEnv::new(), // fixpoint类型本身不应当把模式变量泄露出去
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Namespace { tag, expr } => {
                let expr_res = expr.flow(ctx, pattern_mode, expr.location(), errors);
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::Namespace {
                            tag: tag.clone(),
                            expr: Box::new(expr_res.ty),
                        },
                        loc,
                    ),
                    expr_res.captures,
                    expr_res.patterns,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Pattern { name, expr } => {
                if !pattern_mode {
                    errors.push(WithLocation::new(
                        ParseError::PatternOutOfParameterDefinition(WithLocation::new(
                            self.clone(),
                            loc,
                        )),
                        loc,
                    ));
                    // 继续处理，但返回简单的结果
                    return FlowResult::simple(
                        WithLocation::new(LinearTypeAst::Bottom, loc).with_payload(
                            FlowedMetaData::default().with_variable_context(ctx.capture()),
                        ),
                    );
                }
                let expr_res = expr.flow(ctx, pattern_mode, expr.location(), errors);
                let mut patterns = PatternEnv::new();
                patterns.extend(vec![WithLocation::new(name.clone(), loc)]);
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::Pattern {
                            name: name.clone(),
                            expr: Box::new(expr_res.ty),
                        },
                        loc,
                    ),
                    expr_res.captures,
                    patterns,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Match {
                auto_captures,
                branches,
            } => {
                let mut new_branches = Vec::new();
                let mut all_captures = HashMap::new();
                let mut all_body_captures = HashMap::new();
                for (pattern, body) in branches {
                    // 处理模式
                    ctx.enter_scope();
                    let pattern_res = pattern.flow(ctx, true, pattern.location(), errors);
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

                    // 处理分支体
                    ctx.enter_scope();
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
                    for (var, var_loc) in pattern_res.patterns.iter() {
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
                                        WithLocation::new(self.clone(), loc),
                                        v,
                                    ),
                                    loc,
                                ));
                            }
                        }
                    }
                    let body_res = body.flow(ctx, false, body.location(), errors); // 分支体不允许出现模式变量
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
                    let mut body_captures = body_res.captures;
                    // 移除掉模式变量，因为它们是分支的参数，不应当被视为捕获的自由变量
                    for (var, _) in pattern_res.patterns.iter() {
                        body_captures.remove(var);
                    }

                    new_branches.push((pattern_res.ty, body_res.ty));
                    all_captures.extend(pattern_res.captures);
                    all_captures.extend(body_captures.clone());
                    all_body_captures.extend(body_captures);
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
                    PatternEnv::new(), // match类型本身不应当把模式变量泄露出去
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Literal(inner) => {
                let inner_res = inner.flow(ctx, pattern_mode, inner.location(), errors);
                FlowResult::complex(
                    WithLocation::new(LinearTypeAst::Literal(Box::new(inner_res.ty)), loc),
                    inner_res.captures,
                    inner_res.patterns,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
            LinearTypeAst::Rot { value } => {
                // Rot类型内不允许出现模式变量，因为Rot检查是反向的
                let value_res = value.flow(ctx, false, value.location(), errors);
                FlowResult::complex(
                    WithLocation::new(
                        LinearTypeAst::Rot {
                            value: Box::new(value_res.ty),
                        },
                        loc,
                    ),
                    value_res.captures,
                    value_res.patterns,
                )
                .with_payload(FlowedMetaData::default().with_variable_context(ctx.capture()))
            }
        }
    }
}

pub struct BuildResult<T: GcAllocObject<T, Inner = Type<T>>> {
    ty: Type<T>,
    patterns: Vec<WithLocation<String>>, // 按照de Bruijn索引顺序排列的模式变量
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for BuildResult<T> {
    fn clone(&self) -> Self {
        BuildResult {
            ty: self.ty.clone(),
            patterns: self.patterns.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> BuildResult<T> {
    pub fn simple(ty: Type<T>) -> Self {
        BuildResult {
            ty,
            patterns: Vec::new(),
        }
    }

    pub fn complex(ty: Type<T>, patterns: Vec<WithLocation<String>>) -> Self {
        BuildResult { ty, patterns }
    }

    pub fn fold(results: Vec<Self>) -> (Vec<Type<T>>, Vec<WithLocation<String>>) {
        let mut types = Vec::new();
        let mut patterns = Vec::new();
        for res in results {
            types.push(res.ty);
            patterns.extend(res.patterns);
        }
        (types, patterns)
    }

    pub fn ty(&self) -> &Type<T> {
        &self.ty
    }

    pub fn patterns(&self) -> &Vec<WithLocation<String>> {
        &self.patterns
    }
}

impl<'ast> LinearTypeAst<'ast> {
    #[stacksafe::stacksafe]
    pub fn to_type<'roots, T: GcAllocObject<T, Inner = Type<T>>>(
        &self,
        ctx: &mut BuildContext<T>,
        pattern_counter: &mut PatternCounter,
        pattern_mode: bool,
        gc: &mut GC<T>,
        roots: &'roots mut RootStack<Type<T>, T>,
        loc: Option<&SourceLocation>,
    ) -> Result<BuildResult<T>, Result<TypeError<Type<T>, T>, ParseError<'ast>>> {
        match self {
            LinearTypeAst::Int => Ok(BuildResult::simple(Integer::new())),
            LinearTypeAst::Float => Ok(BuildResult::simple(Float::new())),
            LinearTypeAst::Char => Ok(BuildResult::simple(Character::new())),
            LinearTypeAst::Top => Ok(BuildResult::simple(TypeBound::top())),
            LinearTypeAst::Bottom => Ok(BuildResult::simple(TypeBound::bottom())),
            LinearTypeAst::IntLiteral(v) => Ok(BuildResult::simple(IntegerValue::new(*v))),
            LinearTypeAst::FloatLiteral(v) => Ok(BuildResult::simple(FloatValue::new(*v))),
            LinearTypeAst::CharLiteral(v) => Ok(BuildResult::simple(CharacterValue::new(*v))),
            LinearTypeAst::Variable(var) => {
                if let Some(ty) = ctx.current_layer().get(var) {
                    match ty {
                        Ok(t) => Ok(BuildResult::simple(t.clone())), // fixpoint类型
                        Err(index) => Ok(BuildResult::simple(Variable::new_debruijn(index))),
                    }
                } else {
                    Err(Err(ParseError::UseBeforeDeclaration(
                        WithLocation::new(self.clone(), loc),
                        var.clone(),
                    )))
                }
            }
            LinearTypeAst::Tuple(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(
                        ctx,
                        pattern_counter,
                        pattern_mode,
                        gc,
                        roots,
                        bta.location(),
                    )?);
                }
                let (types, patterns) = BuildResult::fold(types);
                Ok(BuildResult::complex(Tuple::new(&types), patterns))
            }
            LinearTypeAst::List(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(
                        ctx,
                        pattern_counter,
                        pattern_mode,
                        gc,
                        roots,
                        bta.location(),
                    )?);
                }
                let (types, patterns) = BuildResult::fold(types);
                Ok(BuildResult::complex(List::new(&types), patterns))
            }
            LinearTypeAst::Generalize(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(
                        ctx,
                        &mut PatternCounter::new(), // 泛化类型内不允许出现模式变量，安全起见传入一个新的计数器
                        false,
                        gc,
                        roots,
                        bta.location(),
                    )?);
                }
                // 我们无法计算出泛化类型的闭包环境，因此传入一个空的闭包环境
                let (types, patterns) = BuildResult::fold(types);
                Ok(BuildResult::complex(Generalize::new_raw(&types), patterns))
            }
            LinearTypeAst::Specialize(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(
                        ctx,
                        &mut PatternCounter::new(), // 专化类型内不允许出现模式变量，安全起见传入一个新的计数器
                        false,
                        gc,
                        roots,
                        bta.location(),
                    )?);
                }
                // 我们无法计算出专化类型的闭包环境，因此传入一个空的闭包环境
                let (types, patterns) = BuildResult::fold(types);
                if !patterns.is_empty() {
                    return Err(Err(ParseError::AmbiguousPattern(WithLocation::new(
                        self.clone(),
                        loc,
                    ))));
                }
                Ok(BuildResult::complex(Specialize::new_raw(&types), patterns))
            }
            LinearTypeAst::Invoke {
                func,
                arg,
                continuation,
                perform_handler,
            } => {
                let func_type =
                    func.to_type(ctx, pattern_counter, false, gc, roots, func.location())?;
                let arg_type = arg.to_type(
                    ctx,
                    pattern_counter,
                    pattern_mode,
                    gc,
                    roots,
                    arg.location(),
                )?;
                let continuation_type = match continuation {
                    Some(continuation) => Some(continuation.to_type(
                        ctx,
                        pattern_counter,
                        false,
                        gc,
                        roots,
                        continuation.location(),
                    )?),
                    None => None,
                };
                let perform_handler_type = match perform_handler {
                    Some(perform_handler) => Some(perform_handler.to_type(
                        ctx,
                        pattern_counter,
                        false,
                        gc,
                        roots,
                        perform_handler.location(),
                    )?),
                    None => None,
                };
                let mut fold_vec = vec![func_type, arg_type];
                if let Some(ct) = &continuation_type {
                    fold_vec.push(ct.clone());
                }
                if let Some(rht) = &perform_handler_type {
                    fold_vec.push(rht.clone());
                }
                let (types, patterns) = BuildResult::fold(fold_vec);
                Ok(BuildResult::complex(
                    Invoke::new(
                        &types[0],
                        &types[1],
                        continuation_type.as_ref().map(|t| &t.ty),
                        perform_handler_type.as_ref().map(|t| &t.ty),
                    ),
                    patterns,
                ))
            }
            LinearTypeAst::Match {
                auto_captures,
                branches,
            } => {
                let auto_captures = auto_captures
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<Vec<(String, WithLocation<()>)>>();
                let mut closure_env = Vec::new();
                for (var, loc) in &auto_captures {
                    if let Some(ty) = ctx.current_layer().get(var) {
                        match ty {
                            Ok(t) => closure_env.push(t.clone()), // fixpoint类型
                            Err(index) => closure_env.push(Variable::new_debruijn(index)),
                        }
                    } else {
                        return Err(Err(ParseError::UseBeforeDeclaration(
                            WithLocation::new(self.clone(), loc.location()),
                            var.clone(),
                        )));
                    }
                }
                let closure_env = ClosureEnv::new(closure_env);
                let mut new_branches = Vec::new();
                for (pattern, body) in branches {
                    let pattern_type: BuildResult<T> = pattern.to_type(
                        ctx,
                        &mut PatternCounter::new(), // 进入模式定义，重新计数
                        true,
                        gc,
                        roots,
                        pattern.location(),
                    )?; // 模式现在允许捕获环境变量

                    ctx.enter_layer();
                    for (var, var_loc) in &auto_captures {
                        if ctx.current_layer_mut().push_captured(var.clone()).is_none() {
                            return Err(Err(ParseError::RedeclaredCaptureValue(
                                WithLocation::new(self.clone(), loc),
                                var_loc.clone().map(|_| var.clone()),
                            )));
                        }
                    }
                    // 把模式变量加入到当前作用域
                    // 我们仅仅按照顺序加入模式变量，如果有重名的模式变量，直接跳过即可
                    for var in pattern_type.patterns.iter() {
                        let _ = ctx.current_layer_mut().push_pattern(var.value().clone());
                    }
                    let body_type = body.to_type(
                        ctx,
                        &mut PatternCounter::new(), // 进入闭包体，闭包体不允许出现模式变量，安全起见传入一个新的计数器
                        false,
                        gc,
                        roots,
                        body.location(),
                    )?;
                    ctx.exit_layer();
                    new_branches.push((pattern_type.ty, body_type.ty));
                }
                Ok(BuildResult::simple(
                    Closure::new::<Type<T>, Type<T>, Type<T>>(new_branches, closure_env),
                ))
            }
            LinearTypeAst::AtomicOpcode(atomic_opcode) => {
                Ok(BuildResult::simple(Opcode::new(match atomic_opcode {
                    AtomicOpcode::Opcode => Opcode::Opcode,
                    AtomicOpcode::Add => Opcode::Add,
                    AtomicOpcode::Sub => Opcode::Sub,
                    AtomicOpcode::Mul => Opcode::Mul,
                    AtomicOpcode::Div => Opcode::Div,
                    AtomicOpcode::Mod => Opcode::Mod,
                    AtomicOpcode::Less => Opcode::Less,
                    AtomicOpcode::Greater => Opcode::Greater,
                    AtomicOpcode::Neg => Opcode::Neg,
                    AtomicOpcode::Is => Opcode::Is,
                    AtomicOpcode::IO(v) => Opcode::IO(v.clone().into()),
                })))
            }
            LinearTypeAst::FixPoint { param_name, expr } => {
                let placeholder = FixPoint::new_placeholder(gc, roots);
                ctx.current_layer_mut()
                    .enter_fixpoint(param_name.clone(), placeholder.clone());
                let expr_type = expr.to_type(
                    ctx,
                    &mut PatternCounter::new(),
                    false,
                    gc,
                    roots,
                    expr.location(),
                )?; // 递归函数的表达式中不允许出现模式变量，安全起见传入一个新的计数器
                ctx.current_layer_mut().exit_fixpoint();
                as_type!(&placeholder, Type::FixPoint)
                    .set(expr_type.ty)
                    .map_err(Ok)?;
                Ok(BuildResult::simple(placeholder))
            }
            LinearTypeAst::Namespace { tag, expr } => {
                let expr_type = expr.to_type(
                    ctx,
                    pattern_counter,
                    pattern_mode,
                    gc,
                    roots,
                    expr.location(),
                )?;
                Ok(BuildResult::complex(
                    Namespace::new(tag.clone(), &expr_type.ty),
                    expr_type.patterns,
                ))
            }
            LinearTypeAst::Pattern { name, expr } => {
                if !pattern_mode {
                    return Err(Err(ParseError::PatternOutOfParameterDefinition(
                        WithLocation::new(self.clone(), loc),
                    )));
                }
                let expr_type = expr.to_type(
                    ctx,
                    pattern_counter,
                    pattern_mode,
                    gc,
                    roots,
                    expr.location(),
                )?;
                let mut patterns = expr_type.patterns;
                let debruijn_index = pattern_counter.alloc(name.clone());
                patterns.extend(vec![WithLocation::new(name.clone(), loc)]);
                Ok(BuildResult::complex(
                    Pattern::new(debruijn_index, &expr_type.ty),
                    patterns,
                ))
            }
            LinearTypeAst::Literal(inner) => {
                let inner_type = inner.to_type(
                    ctx,
                    pattern_counter,
                    pattern_mode,
                    gc,
                    roots,
                    inner.location(),
                )?;
                Ok(BuildResult::complex(
                    Lazy::new(&inner_type.ty),
                    inner_type.patterns,
                ))
            }
            LinearTypeAst::Rot { value } => {
                let value_type = value.to_type(
                    ctx,
                    pattern_counter,
                    false, // Rot类型内不允许出现模式变量
                    gc,
                    roots,
                    value.location(),
                )?;
                Ok(BuildResult::complex(
                    Rotate::new(&value_type.ty),
                    value_type.patterns,
                ))
            }
        }
    }
}
