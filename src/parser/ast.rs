use arc_gc::gc::GC;

use crate::as_type;
use crate::parser::{BuildContext, ParseContext, ParseError};
use crate::types::character::Character;
use crate::types::character_value::CharacterValue;
use crate::types::closure::{Closure, ClosureEnv};
use crate::types::fixpoint::{FixPoint, FixPointInner};
use crate::types::generalize::Generalize;
use crate::types::integer::Integer;
use crate::types::integer_value::IntegerValue;
use crate::types::invoke::Invoke;
use crate::types::list::List;
use crate::types::namespace::Namespace;
use crate::types::opcode::Opcode;
use crate::types::pattern::Pattern;
use crate::types::specialize::Specialize;
use crate::types::tuple::Tuple;
use crate::types::type_bound::TypeBound;
use crate::types::variable::Variable;
use crate::types::{Stabilized, StabilizedType, Type, TypeError};
use std::collections::HashSet;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    Greater,
    Is,
}

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
}

#[derive(Debug, Clone)]
pub enum TypeAst {
    Int,
    Char,
    Top,
    Bottom,
    Opcode,
    DiscardPattern, // 用于表示_模式
    IntLiteral(isize),
    CharLiteral(char),
    Variable(Option<String>),
    Tuple(Vec<TypeAst>),
    List(Vec<TypeAst>),
    Generalize(Vec<TypeAst>),
    Specialize(Vec<TypeAst>),
    Invoke {
        func: Box<TypeAst>,
        arg: Box<TypeAst>,
        continuation: Box<TypeAst>,
    },
    Expression {
        binding_patterns: Vec<TypeAst>,
        binding_types: Vec<TypeAst>,
        body: Box<TypeAst>,
    },
    Match {
        value: Box<TypeAst>,
        match_branch: Vec<(TypeAst, TypeAst)>,
        else_branch: Option<Box<TypeAst>>,
    },
    Closure {
        pattern: Box<TypeAst>,
        auto_captures: HashSet<String>,
        body: Box<TypeAst>,
        fail_branch: Option<Box<TypeAst>>,
    },
    Apply {
        func: Box<TypeAst>,
        arg: Box<TypeAst>,
    },
    Eq {
        left: Box<TypeAst>,
        right: Box<TypeAst>,
    },
    Neq {
        left: Box<TypeAst>,
        right: Box<TypeAst>,
    },
    Not {
        value: Box<TypeAst>,
    },
    BinaryOp(BinaryOp),
    FixPoint {
        param_name: String,
        expr: Box<TypeAst>,
    },
    Namespace {
        tag: String,
        expr: Box<TypeAst>,
    },
    Pattern {
        name: String,
        expr: Box<TypeAst>,
    },
}

#[derive(Debug, Clone)]
pub enum BasicTypeAst {
    Int,
    Char,
    Top,
    Bottom,
    IntLiteral(isize),
    CharLiteral(char),
    Variable(Option<String>), // None 表示续体
    Tuple(Vec<BasicTypeAst>),
    List(Vec<BasicTypeAst>),
    Generalize(Vec<BasicTypeAst>),
    Specialize(Vec<BasicTypeAst>),
    Invoke {
        func: Box<BasicTypeAst>,
        arg: Box<BasicTypeAst>,
        continuation: Box<BasicTypeAst>,
    },
    Closure {
        pattern: Box<BasicTypeAst>,
        auto_captures: HashSet<String>,
        body: Box<BasicTypeAst>,
        fail_branch: Option<Box<BasicTypeAst>>,
    },
    Apply {
        func: Box<BasicTypeAst>,
        arg: Box<BasicTypeAst>,
    },
    AtomicOpcode(AtomicOpcode),
    FixPoint {
        param_name: String,
        expr: Box<BasicTypeAst>,
    },
    Namespace {
        tag: String,
        expr: Box<BasicTypeAst>,
    },
    Pattern {
        name: String,
        expr: Box<BasicTypeAst>,
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
pub struct LinearizeResult {
    bindings: Vec<(LinearTypeAst, LinearTypeAst, String)>, // (func, arg, tmpvar_name)
    final_type: LinearTypeAst,
}

impl LinearizeResult {
    pub fn new_simple(ty: LinearTypeAst) -> Self {
        Self {
            bindings: Vec::new(),
            final_type: ty,
        }
    }

    pub fn new_with_binding(
        bindings: Vec<(LinearTypeAst, LinearTypeAst, String)>,
        ty: LinearTypeAst,
    ) -> Self {
        Self {
            bindings,
            final_type: ty,
        }
    }

    pub fn new_apply(
        func: LinearizeResult,
        arg: LinearizeResult,
        allocated_tmpvar_name: String,
    ) -> Self {
        let mut bindings = func.bindings;
        bindings.extend(arg.bindings);
        bindings.push((
            func.final_type,
            arg.final_type,
            allocated_tmpvar_name.clone(),
        ));
        Self {
            bindings,
            final_type: LinearTypeAst::Variable(Some(allocated_tmpvar_name)).into(),
        }
    }

    pub fn bindings(&self) -> &Vec<(LinearTypeAst, LinearTypeAst, String)> {
        &self.bindings
    }

    pub fn final_type(&self) -> &LinearTypeAst {
        &self.final_type
    }

    pub fn linearize(self) -> LinearTypeAst {
        let mut ty = self.final_type;
        for (f, a, tmpvar) in self.bindings.into_iter().rev() {
            ty = LinearTypeAst::Invoke {
                func: Box::new(f),
                arg: Box::new(a),
                continuation: LinearTypeAst::Closure {
                    pattern: LinearTypeAst::Pattern {
                        name: tmpvar,
                        expr: Box::new(LinearTypeAst::Top),
                    }
                    .into(),
                    auto_captures: HashSet::new(),
                    body: Box::new(ty),
                    fail_branch: None,
                }
                .into(),
            }
        }
        ty
    }
}

impl BasicTypeAst {
    pub fn linearize(self, ctx: &mut LinearizeContext) -> LinearizeResult {
        match self {
            BasicTypeAst::Int => LinearizeResult::new_simple(LinearTypeAst::Int),
            BasicTypeAst::Char => LinearizeResult::new_simple(LinearTypeAst::Char),
            BasicTypeAst::Top => LinearizeResult::new_simple(LinearTypeAst::Top),
            BasicTypeAst::Bottom => LinearizeResult::new_simple(LinearTypeAst::Bottom),
            BasicTypeAst::IntLiteral(v) => {
                LinearizeResult::new_simple(LinearTypeAst::IntLiteral(v))
            }
            BasicTypeAst::CharLiteral(v) => {
                LinearizeResult::new_simple(LinearTypeAst::CharLiteral(v))
            }
            BasicTypeAst::Variable(v) => LinearizeResult::new_simple(LinearTypeAst::Variable(v)),
            BasicTypeAst::Tuple(v) => {
                let elements = v.into_iter().map(|e| e.linearize(ctx)).collect::<Vec<_>>();
                let ty =
                    LinearTypeAst::Tuple(elements.iter().map(|e| e.final_type().clone()).collect());

                LinearizeResult::new_with_binding(
                    elements
                        .into_iter()
                        .flat_map(|e| e.bindings.into_iter())
                        .collect(),
                    ty,
                )
            }
            BasicTypeAst::List(v) => {
                let elements = v.into_iter().map(|e| e.linearize(ctx)).collect::<Vec<_>>();
                let ty =
                    LinearTypeAst::List(elements.iter().map(|e| e.final_type().clone()).collect());
                LinearizeResult::new_with_binding(
                    elements
                        .into_iter()
                        .flat_map(|e| e.bindings.into_iter())
                        .collect(),
                    ty,
                )
            }
            BasicTypeAst::Generalize(v) => {
                let elements = v.into_iter().map(|e| e.linearize(ctx)).collect::<Vec<_>>();
                let ty = LinearTypeAst::Generalize(
                    elements.iter().map(|e| e.final_type().clone()).collect(),
                );
                LinearizeResult::new_with_binding(
                    elements
                        .into_iter()
                        .flat_map(|e| e.bindings.into_iter())
                        .collect(),
                    ty,
                )
            }
            BasicTypeAst::Specialize(v) => {
                let elements = v.into_iter().map(|e| e.linearize(ctx)).collect::<Vec<_>>();
                let ty = LinearTypeAst::Specialize(
                    elements.iter().map(|e| e.final_type().clone()).collect(),
                );
                LinearizeResult::new_with_binding(
                    elements
                        .into_iter()
                        .flat_map(|e| e.bindings.into_iter())
                        .collect(),
                    ty,
                )
            }
            BasicTypeAst::Invoke {
                func,
                arg,
                continuation,
            } => {
                let func = func.linearize(ctx);
                let arg = arg.linearize(ctx);
                let continuation = continuation.linearize(ctx);
                let ty = LinearTypeAst::Invoke {
                    func: func.final_type().clone().into(),
                    arg: arg.final_type().clone().into(),
                    continuation: continuation.final_type().clone().into(),
                };
                let mut bindings = func.bindings;
                bindings.extend(arg.bindings);
                bindings.extend(continuation.bindings);
                LinearizeResult::new_with_binding(bindings, ty)
            }
            BasicTypeAst::Closure {
                pattern,
                auto_captures,
                body,
                fail_branch,
            } => {
                let fail_branch = fail_branch.map(|b| b.linearize(ctx).linearize().into());
                let ty = LinearTypeAst::Closure {
                    pattern: Box::new(pattern.linearize(ctx).linearize()), // pattern 直接完整线性化
                    auto_captures,
                    body: Box::new(body.linearize(ctx).linearize()),
                    fail_branch,
                };
                LinearizeResult::new_simple(ty)
            }
            BasicTypeAst::Apply { func, arg } => {
                let func = func.linearize(ctx);
                let arg = arg.linearize(ctx);
                let allocated_tmpvar_name = ctx.allocate_tmpvar_name();
                LinearizeResult::new_apply(func, arg, allocated_tmpvar_name)
            }
            BasicTypeAst::AtomicOpcode(atomic_opcode) => {
                LinearizeResult::new_simple(LinearTypeAst::AtomicOpcode(atomic_opcode))
            }
            BasicTypeAst::FixPoint { param_name, expr } => {
                let expr = expr.linearize(ctx);
                let ty = LinearTypeAst::FixPoint {
                    param_name,
                    expr: Box::new(expr.final_type().clone()),
                };
                LinearizeResult::new_with_binding(expr.bindings, ty)
            }
            BasicTypeAst::Namespace { tag, expr } => {
                let expr = expr.linearize(ctx);
                let ty = LinearTypeAst::Namespace {
                    tag,
                    expr: Box::new(expr.final_type().clone()),
                };
                LinearizeResult::new_with_binding(expr.bindings, ty)
            }
            BasicTypeAst::Pattern { name, expr } => {
                let expr = expr.linearize(ctx);
                let ty = LinearTypeAst::Pattern {
                    name,
                    expr: Box::new(expr.final_type().clone()),
                };
                LinearizeResult::new_with_binding(expr.bindings, ty)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum LinearTypeAst {
    Int,
    Char,
    Top,
    Bottom,
    IntLiteral(isize),
    CharLiteral(char),
    Variable(Option<String>), // None 表示续体
    Tuple(Vec<LinearTypeAst>),
    List(Vec<LinearTypeAst>),
    Generalize(Vec<LinearTypeAst>),
    Specialize(Vec<LinearTypeAst>),
    Closure {
        pattern: Box<LinearTypeAst>,
        auto_captures: HashSet<String>,
        body: Box<LinearTypeAst>,
        fail_branch: Option<Box<LinearTypeAst>>,
    },
    Invoke {
        func: Box<LinearTypeAst>,
        arg: Box<LinearTypeAst>,
        continuation: Box<LinearTypeAst>,
    },
    AtomicOpcode(AtomicOpcode),
    FixPoint {
        param_name: String,
        expr: Box<LinearTypeAst>,
    },
    Namespace {
        tag: String,
        expr: Box<LinearTypeAst>,
    },
    Pattern {
        name: String,
        expr: Box<LinearTypeAst>,
    },
}

impl TypeAst {
    // 把高级抽象语法转换为基础抽象语法
    pub fn into_basic(self) -> BasicTypeAst {
        match self {
            TypeAst::Int => BasicTypeAst::Int,
            TypeAst::Char => BasicTypeAst::Char,
            TypeAst::Top => BasicTypeAst::Top,
            TypeAst::Bottom => BasicTypeAst::Bottom,
            TypeAst::Opcode => BasicTypeAst::AtomicOpcode(AtomicOpcode::Opcode),
            TypeAst::DiscardPattern => BasicTypeAst::Pattern {
                name: "discard#value".to_string(),
                expr: Box::new(BasicTypeAst::Top),
            },
            TypeAst::IntLiteral(v) => BasicTypeAst::IntLiteral(v),
            TypeAst::CharLiteral(v) => BasicTypeAst::CharLiteral(v),
            TypeAst::Variable(name) => BasicTypeAst::Variable(name),
            TypeAst::Tuple(elements) => {
                BasicTypeAst::Tuple(elements.into_iter().map(|e| e.into_basic()).collect())
            }
            TypeAst::List(elements) => {
                BasicTypeAst::List(elements.into_iter().map(|e| e.into_basic()).collect())
            }
            TypeAst::Generalize(elements) => {
                BasicTypeAst::Generalize(elements.into_iter().map(|e| e.into_basic()).collect())
            }
            TypeAst::Specialize(elements) => {
                BasicTypeAst::Specialize(elements.into_iter().map(|e| e.into_basic()).collect())
            }
            TypeAst::Invoke {
                func,
                arg,
                continuation,
            } => BasicTypeAst::Invoke {
                func: Box::new(func.into_basic()),
                arg: Box::new(arg.into_basic()),
                continuation: Box::new(continuation.into_basic()),
            },
            TypeAst::Expression {
                binding_patterns,
                binding_types,
                body,
            } => {
                // 转换为嵌套的闭包和应用
                let mut expr = body.into_basic();
                for (pat, ty) in binding_patterns
                    .into_iter()
                    .rev()
                    .zip(binding_types.into_iter().rev())
                {
                    expr = BasicTypeAst::Apply {
                        func: Box::new(BasicTypeAst::Closure {
                            pattern: Box::new(pat.into_basic()),
                            auto_captures: HashSet::new(),
                            body: Box::new(expr),
                            fail_branch: None,
                        }),
                        arg: Box::new(ty.into_basic()),
                    };
                }
                expr
            }
            TypeAst::Match {
                value,
                match_branch,
                else_branch,
            } => {
                let value = value.into_basic();
                let mut branches = Vec::new();
                for (pat, expr) in match_branch {
                    branches.push((pat.into_basic(), expr.into_basic()));
                }
                let else_branch = else_branch.map(|b| Box::new(b.into_basic()));
                // 把match转换为一系列的Apply和Closure
                let mut expr = else_branch;
                for (pat, branch_expr) in branches.into_iter().rev() {
                    expr = Some(Box::new(BasicTypeAst::Apply {
                        func: Box::new(BasicTypeAst::Closure {
                            pattern: Box::new(pat),
                            auto_captures: HashSet::new(),
                            body: branch_expr.into(),
                            fail_branch: expr,
                        }),
                        arg: Box::new(BasicTypeAst::Variable(Some("match#value".to_string()))),
                    }));
                }
                BasicTypeAst::Apply {
                    func: Box::new(BasicTypeAst::Closure {
                        pattern: Box::new(BasicTypeAst::Pattern {
                            name: "match#value".to_string(),
                            expr: Box::new(BasicTypeAst::Top),
                        }),
                        auto_captures: HashSet::new(),
                        body: expr.expect("There should be at least one branch").into(),
                        fail_branch: None,
                    }),
                    arg: Box::new(value),
                }
            }
            TypeAst::Closure {
                pattern,
                auto_captures,
                body,
                fail_branch,
            } => BasicTypeAst::Closure {
                pattern: Box::new(pattern.into_basic()),
                auto_captures,
                body: Box::new(body.into_basic()),
                fail_branch: fail_branch.map(|b| Box::new(b.into_basic())),
            },
            TypeAst::Apply { func, arg } => BasicTypeAst::Apply {
                func: Box::new(func.into_basic()),
                arg: Box::new(arg.into_basic()),
            },
            TypeAst::Eq { left, right } => BasicTypeAst::Apply {
                func: BasicTypeAst::Closure {
                    pattern: BasicTypeAst::Tuple(vec![
                        BasicTypeAst::Pattern {
                            name: "eq#left".to_string(),
                            expr: Box::new(BasicTypeAst::Top),
                        },
                        BasicTypeAst::Pattern {
                            name: "eq#right".to_string(),
                            expr: Box::new(BasicTypeAst::Top),
                        },
                    ])
                    .into(),
                    auto_captures: HashSet::new(),
                    body: Box::new(BasicTypeAst::Specialize(vec![
                        BasicTypeAst::Apply {
                            func: Box::new(BasicTypeAst::AtomicOpcode(AtomicOpcode::Is)),
                            arg: Box::new(BasicTypeAst::Tuple(vec![
                                BasicTypeAst::Variable(Some("eq#left".to_string())),
                                BasicTypeAst::Variable(Some("eq#right".to_string())),
                            ])),
                        }
                        .into(),
                        BasicTypeAst::Apply {
                            func: Box::new(BasicTypeAst::AtomicOpcode(AtomicOpcode::Is)),
                            arg: Box::new(BasicTypeAst::Tuple(vec![
                                BasicTypeAst::Variable(Some("eq#right".to_string())),
                                BasicTypeAst::Variable(Some("eq#left".to_string())),
                            ])),
                        }
                        .into(),
                    ]))
                    .into(),
                    fail_branch: None,
                }
                .into(),
                arg: BasicTypeAst::Tuple(vec![left.into_basic(), right.into_basic()]).into(),
            },
            TypeAst::Neq { left, right } => {
                // a != b  ===  !(a == b)
                TypeAst::Not {
                    value: TypeAst::Eq { left, right }.into(),
                }
                .into_basic()
            }
            TypeAst::Not { value } => BasicTypeAst::Apply {
                func: BasicTypeAst::Closure {
                    pattern: BasicTypeAst::Bottom.into(),
                    auto_captures: HashSet::new(),
                    body: BasicTypeAst::Top.into(),
                    fail_branch: Some(BasicTypeAst::Bottom.into()),
                }
                .into(),
                arg: value.into_basic().into(),
            },
            TypeAst::BinaryOp(binary_op) => match binary_op {
                BinaryOp::Add => BasicTypeAst::AtomicOpcode(AtomicOpcode::Add),
                BinaryOp::Sub => BasicTypeAst::AtomicOpcode(AtomicOpcode::Sub),
                BinaryOp::Mul => BasicTypeAst::AtomicOpcode(AtomicOpcode::Mul),
                BinaryOp::Div => BasicTypeAst::AtomicOpcode(AtomicOpcode::Div),
                BinaryOp::Mod => BasicTypeAst::AtomicOpcode(AtomicOpcode::Mod),
                BinaryOp::Less => BasicTypeAst::AtomicOpcode(AtomicOpcode::Less),
                BinaryOp::Greater => BasicTypeAst::AtomicOpcode(AtomicOpcode::Greater),
                BinaryOp::Is => BasicTypeAst::AtomicOpcode(AtomicOpcode::Is),
            },
            TypeAst::FixPoint { param_name, expr } => BasicTypeAst::FixPoint {
                param_name,
                expr: Box::new(expr.into_basic()),
            },
            TypeAst::Namespace { tag, expr } => BasicTypeAst::Namespace {
                tag,
                expr: Box::new(expr.into_basic()),
            },
            TypeAst::Pattern { name, expr } => BasicTypeAst::Pattern {
                name,
                expr: Box::new(expr.into_basic()),
            },
        }
    }
}

pub struct PatternEnv {
    declared: HashSet<String>, // 已声明的模式变量
}

impl Deref for PatternEnv {
    type Target = HashSet<String>;

    fn deref(&self) -> &Self::Target {
        &self.declared
    }
}

impl PatternEnv {
    pub fn new() -> Self {
        PatternEnv {
            declared: HashSet::new(),
        }
    }

    pub fn extend(&mut self, names: impl IntoIterator<Item = String>) -> Result<(), String> {
        for name in names {
            if self.declared.contains(&name) {
                return Err(name);
            }
            self.declared.insert(name);
        }
        Ok(())
    }
}

impl IntoIterator for PatternEnv {
    type Item = String;
    type IntoIter = std::collections::hash_set::IntoIter<String>;

    fn into_iter(self) -> Self::IntoIter {
        self.declared.into_iter()
    }
}

pub struct FlowResult {
    ty: LinearTypeAst,         // flow后的类型
    captures: HashSet<String>, // 该类型所捕获的自由变量
    patterns: PatternEnv,      // 该类型中出现的所有模式变量
}

impl FlowResult {
    pub fn simple(ty: LinearTypeAst) -> Self {
        FlowResult {
            ty,
            captures: HashSet::new(),
            patterns: PatternEnv::new(),
        }
    }

    pub fn complex(ty: LinearTypeAst, captures: HashSet<String>, patterns: PatternEnv) -> Self {
        FlowResult {
            ty,
            captures,
            patterns,
        }
    }

    pub fn ty(&self) -> &LinearTypeAst {
        &self.ty
    }

    pub fn captures(&self) -> &HashSet<String> {
        &self.captures
    }

    pub fn patterns(&self) -> &PatternEnv {
        &self.patterns
    }
}

impl LinearTypeAst {
    pub fn flow(
        &self,
        ctx: &mut ParseContext,
        pattern_mode: bool,
    ) -> Result<FlowResult, ParseError> {
        match self {
            LinearTypeAst::Int => Ok(FlowResult::simple(LinearTypeAst::Int)),
            LinearTypeAst::Char => Ok(FlowResult::simple(LinearTypeAst::Char)),
            LinearTypeAst::Top => Ok(FlowResult::simple(LinearTypeAst::Top)),
            LinearTypeAst::Bottom => Ok(FlowResult::simple(LinearTypeAst::Bottom)),
            LinearTypeAst::IntLiteral(v) => Ok(FlowResult::simple(LinearTypeAst::IntLiteral(*v))),
            LinearTypeAst::CharLiteral(v) => Ok(FlowResult::simple(LinearTypeAst::CharLiteral(*v))),
            LinearTypeAst::Variable(Some(name)) => {
                if ctx.is_declared(name) {
                    let mut captures = HashSet::new();
                    captures.insert(name.clone());
                    Ok(FlowResult::complex(
                        LinearTypeAst::Variable(Some(name.clone())),
                        captures,
                        PatternEnv::new(),
                    ))
                } else {
                    Err(ParseError::UseBeforeDeclaration(self.clone(), name.clone()))
                }
            }
            LinearTypeAst::Variable(None) => Ok(FlowResult::simple(LinearTypeAst::Variable(None))), // None 表示续体，不会捕获任何变量（因为续体对于任何函数都是存在的）
            LinearTypeAst::Tuple(elements) => {
                let mut new_elements = Vec::new();
                let mut all_captures = HashSet::new();
                let mut all_patterns = PatternEnv::new();
                for elem in elements {
                    let res = elem.flow(ctx, pattern_mode)?;
                    new_elements.push(res.ty);
                    all_captures.extend(res.captures);
                    all_patterns
                        .extend(res.patterns)
                        .map_err(|name| ParseError::RedeclaredPattern(self.clone(), name))?;
                }
                Ok(FlowResult::complex(
                    LinearTypeAst::Tuple(new_elements),
                    all_captures,
                    all_patterns,
                ))
            }
            LinearTypeAst::List(elements) => {
                let mut new_elements = Vec::new();
                let mut all_captures = HashSet::new();
                let mut all_patterns = PatternEnv::new();
                for elem in elements {
                    let res = elem.flow(ctx, pattern_mode)?;
                    new_elements.push(res.ty);
                    all_captures.extend(res.captures);
                    all_patterns
                        .extend(res.patterns)
                        .map_err(|name| ParseError::RedeclaredPattern(self.clone(), name))?;
                }
                Ok(FlowResult::complex(
                    LinearTypeAst::List(new_elements),
                    all_captures,
                    all_patterns,
                ))
            }
            LinearTypeAst::Generalize(types) => {
                let mut new_types = Vec::new();
                let mut all_captures = HashSet::new();
                let mut all_patterns = PatternEnv::new();
                for ty in types {
                    let res = ty.flow(ctx, pattern_mode)?;
                    new_types.push(res.ty);
                    all_captures.extend(res.captures);
                    all_patterns
                        .extend(res.patterns)
                        .map_err(|name| ParseError::RedeclaredPattern(self.clone(), name))?;
                }
                if !all_patterns.is_empty() {
                    // 泛化类型中不允许出现模式变量，因为泛化类型是乱序的
                    return Err(ParseError::AmbiguousPattern(self.clone()));
                }
                Ok(FlowResult::complex(
                    LinearTypeAst::Generalize(new_types),
                    all_captures,
                    all_patterns,
                ))
            }
            LinearTypeAst::Specialize(types) => {
                let mut new_types = Vec::new();
                let mut all_captures = HashSet::new();
                let mut all_patterns = PatternEnv::new();
                for ty in types {
                    let res = ty.flow(ctx, pattern_mode)?;
                    new_types.push(res.ty);
                    all_captures.extend(res.captures);
                    all_patterns
                        .extend(res.patterns)
                        .map_err(|name| ParseError::RedeclaredPattern(self.clone(), name))?;
                }
                if !all_patterns.is_empty() {
                    // 专化类型中不允许出现模式变量，因为专化类型是乱序的
                    return Err(ParseError::AmbiguousPattern(self.clone()));
                }
                Ok(FlowResult::complex(
                    LinearTypeAst::Specialize(new_types),
                    all_captures,
                    all_patterns,
                ))
            }
            LinearTypeAst::Invoke {
                func,
                arg,
                continuation,
            } => {
                let func_res = func.flow(ctx, pattern_mode)?;
                let arg_res = arg.flow(ctx, pattern_mode)?;
                let cont_res = continuation.flow(ctx, pattern_mode)?;
                let mut all_captures = func_res.captures;
                all_captures.extend(arg_res.captures);
                all_captures.extend(cont_res.captures.clone());

                let mut all_patterns = func_res.patterns;
                all_patterns
                    .extend(arg_res.patterns)
                    .map_err(|name| ParseError::RedeclaredPattern(self.clone(), name))?;
                all_patterns
                    .extend(cont_res.patterns.clone())
                    .map_err(|name| ParseError::RedeclaredPattern(self.clone(), name))?;
                Ok(FlowResult::complex(
                    LinearTypeAst::Invoke {
                        func: Box::new(func_res.ty),
                        arg: Box::new(arg_res.ty),
                        continuation: Box::new(cont_res.ty),
                    },
                    all_captures,
                    all_patterns,
                ))
            }
            LinearTypeAst::AtomicOpcode(atomic_opcode) => Ok(FlowResult::simple(
                LinearTypeAst::AtomicOpcode(atomic_opcode.clone()),
            )),
            LinearTypeAst::FixPoint { param_name, expr } => {
                ctx.enter_scope();
                ctx.declare_variable(param_name.clone());
                // 递归函数的表达式中不允许出现模式变量
                let mut expr_res = expr.flow(ctx, false)?;
                ctx.exit_scope();
                // 移除掉param_name，因为它是递归函数的参数，不应当被视为捕获的自由变量
                expr_res.captures.remove(param_name);
                Ok(FlowResult::complex(
                    LinearTypeAst::FixPoint {
                        param_name: param_name.clone(),
                        expr: Box::new(expr_res.ty),
                    },
                    expr_res.captures,
                    PatternEnv::new(), // fixpoint类型本身不应当把模式变量泄露出去
                ))
            }
            LinearTypeAst::Namespace { tag, expr } => {
                let expr_res = expr.flow(ctx, pattern_mode)?;
                Ok(FlowResult::complex(
                    LinearTypeAst::Namespace {
                        tag: tag.clone(),
                        expr: Box::new(expr_res.ty),
                    },
                    expr_res.captures,
                    expr_res.patterns,
                ))
            }
            LinearTypeAst::Pattern { name, expr } => {
                if !pattern_mode {
                    return Err(ParseError::PatternOutOfParameterDefinition(self.clone()));
                }
                let expr_res = expr.flow(ctx, pattern_mode)?;
                let mut patterns = PatternEnv::new();
                patterns.extend(vec![name.clone()]).unwrap();
                Ok(FlowResult::complex(
                    LinearTypeAst::Pattern {
                        name: name.clone(),
                        expr: Box::new(expr_res.ty),
                    },
                    expr_res.captures,
                    patterns,
                ))
            }
            LinearTypeAst::Closure {
                pattern,
                auto_captures,
                body,
                fail_branch,
            } => {
                // 模式不应当捕获环境变量，因此直接传入空的环境
                let pattern_res = pattern.flow(&mut ParseContext::new(), true)?;
                ctx.enter_scope();
                for var in auto_captures {
                    ctx.declare_variable(var.clone());
                }
                for var in pattern_res.patterns.iter() {
                    ctx.declare_variable(var.clone());
                }
                let body_res = body.flow(ctx, false)?; // 闭包体不允许出现模式变量
                ctx.exit_scope();
                let mut body_captures = body_res.captures;
                // 移除掉模式变量，因为它们是闭包的参数，不应当被视为捕获的自由变量
                for var in pattern_res.patterns.iter() {
                    body_captures.remove(var);
                }
                let fail_branch = if let Some(fb) = fail_branch {
                    let fb_res = fb.flow(ctx, false)?; // 失败分支不允许出现模式变量
                    body_captures.extend(fb_res.captures);
                    Some(fb_res.ty)
                } else {
                    None
                };
                Ok(FlowResult::complex(
                    LinearTypeAst::Closure {
                        pattern: Box::new(pattern_res.ty),
                        auto_captures: body_captures.clone(),
                        body: Box::new(body_res.ty),
                        fail_branch: fail_branch.map(Box::new),
                    },
                    body_captures,
                    PatternEnv::new(), // 闭包类型本身不应当把模式变量泄露出去
                ))
            }
        }
    }
}

pub struct BuildResult {
    ty: StabilizedType,
    patterns: Vec<String>, // 按照de Bruijn索引顺序排列的模式变量
}

impl BuildResult {
    pub fn simple(ty: StabilizedType) -> Self {
        BuildResult {
            ty,
            patterns: Vec::new(),
        }
    }

    pub fn complex(ty: StabilizedType, patterns: Vec<String>) -> Self {
        BuildResult { ty, patterns }
    }

    pub fn fold(results: Vec<Self>) -> (Vec<StabilizedType>, Vec<String>) {
        let mut types = Vec::new();
        let mut patterns = Vec::new();
        for res in results {
            types.push(res.ty);
            patterns.extend(res.patterns);
        }
        (types, patterns)
    }

    pub fn ty(&self) -> &StabilizedType {
        &self.ty
    }

    pub fn patterns(&self) -> &Vec<String> {
        &self.patterns
    }
}

impl LinearTypeAst {
    pub fn to_type(
        &self,
        ctx: &mut BuildContext,
        pattern_mode: bool,
        gc: &mut GC<FixPointInner>,
    ) -> Result<BuildResult, Result<TypeError, ParseError>> {
        match self {
            LinearTypeAst::Int => Ok(BuildResult::simple(Integer::new())),
            LinearTypeAst::Char => Ok(BuildResult::simple(Character::new())),
            LinearTypeAst::Top => Ok(BuildResult::simple(TypeBound::top())),
            LinearTypeAst::Bottom => Ok(BuildResult::simple(TypeBound::bottom())),
            LinearTypeAst::IntLiteral(v) => Ok(BuildResult::simple(IntegerValue::new(*v))),
            LinearTypeAst::CharLiteral(v) => Ok(BuildResult::simple(CharacterValue::new(*v))),
            LinearTypeAst::Variable(Some(var)) => {
                if let Some(ty) = ctx.current_layer().get(var) {
                    match ty {
                        Ok(t) => Ok(BuildResult::simple(t.clone().stabilize())), // fixpoint类型
                        Err(index) => Ok(BuildResult::simple(Variable::new_deburijn(index))),
                    }
                } else {
                    Err(Err(ParseError::UseBeforeDeclaration(
                        self.clone(),
                        var.clone(),
                    )))
                }
            }
            LinearTypeAst::Variable(None) => Ok(BuildResult::simple(Variable::new_continuation())),
            LinearTypeAst::Tuple(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(ctx, pattern_mode, gc)?);
                }
                let (types, patterns) = BuildResult::fold(types);
                Ok(BuildResult::complex(Tuple::new(&types), patterns))
            }
            LinearTypeAst::List(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(ctx, pattern_mode, gc)?);
                }
                let (types, patterns) = BuildResult::fold(types);
                Ok(BuildResult::complex(List::new(&types), patterns))
            }
            LinearTypeAst::Generalize(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(ctx, pattern_mode, gc)?);
                }
                // 我们无法计算出泛化类型的闭包环境，因此传入一个空的闭包环境
                let (types, patterns) = BuildResult::fold(types);
                Ok(BuildResult::complex(Generalize::new_raw(&types), patterns))
            }
            LinearTypeAst::Specialize(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(ctx, false, gc)?);
                }
                // 我们无法计算出专化类型的闭包环境，因此传入一个空的闭包环境
                let (types, patterns) = BuildResult::fold(types);
                if !patterns.is_empty() {
                    return Err(Err(ParseError::AmbiguousPattern(self.clone())));
                }
                Ok(BuildResult::complex(Specialize::new_raw(&types), patterns))
            }
            LinearTypeAst::Invoke {
                func,
                arg,
                continuation,
            } => {
                let func_type = func.to_type(ctx, false, gc)?;
                let arg_type = arg.to_type(ctx, pattern_mode, gc)?;
                let continuation_type = continuation.to_type(ctx, false, gc)?;
                let (types, patterns) =
                    BuildResult::fold(vec![func_type, arg_type, continuation_type]);
                Ok(BuildResult::complex(
                    Invoke::new(&types[0], &types[1], &types[2]),
                    patterns,
                ))
            }
            LinearTypeAst::Closure {
                pattern,
                auto_captures,
                body,
                fail_branch,
            } => {
                let auto_captures = auto_captures.iter().cloned().collect::<Vec<String>>();
                let mut closure_env = Vec::new();
                for var in &auto_captures {
                    if let Some(ty) = ctx.current_layer().get(var) {
                        match ty {
                            Ok(t) => closure_env.push(t.clone().stabilize()), // fixpoint类型
                            Err(index) => closure_env.push(Variable::new_deburijn(index)),
                        }
                    } else {
                        return Err(Err(ParseError::UseBeforeDeclaration(
                            self.clone(),
                            var.clone(),
                        )));
                    }
                }
                let closure_env = ClosureEnv::new(closure_env);

                let pattern_type = pattern.to_type(&mut BuildContext::new(), true, gc)?; // 模式不应当捕获环境变量，因此传入空的BuildContext

                ctx.enter_layer();
                for var in auto_captures {
                    if ctx.current_layer_mut().push_captured(var.clone()).is_none() {
                        return Err(Err(ParseError::RedeclaredPattern(
                            self.clone(),
                            var.clone(),
                        )));
                    }
                }
                // 把模式变量加入到当前作用域
                for var in pattern_type.patterns.iter() {
                    if ctx.current_layer_mut().push_pattern(var.clone()).is_none() {
                        return Err(Err(ParseError::RedeclaredPattern(
                            self.clone(),
                            var.clone(),
                        )));
                    }
                }
                let body_type = body.to_type(ctx, pattern_mode, gc)?;
                ctx.exit_layer();
                let fail_branch_type = if let Some(fb) = fail_branch {
                    Some(fb.to_type(ctx, pattern_mode, gc)?)
                } else {
                    None
                };
                Ok(BuildResult::simple(Closure::new(
                    pattern_type.patterns.len(),
                    pattern_type.ty,
                    body_type.ty,
                    fail_branch_type.map(|fbt| fbt.ty),
                    closure_env,
                )))
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
                    AtomicOpcode::Is => Opcode::Is,
                })))
            }
            LinearTypeAst::FixPoint { param_name, expr } => {
                let placeholder = FixPoint::new_placeholder(gc);
                ctx.current_layer_mut()
                    .enter_fixpoint(param_name.clone(), placeholder.weak().clone());
                let expr_type = expr.to_type(ctx, false, gc)?;
                ctx.current_layer_mut().exit_fixpoint();
                as_type!(placeholder.weak(), Type::FixPoint)
                    .set(expr_type.ty)
                    .map_err(Ok)?;
                Ok(BuildResult::simple(placeholder))
            }
            LinearTypeAst::Namespace { tag, expr } => {
                let expr_type = expr.to_type(ctx, pattern_mode, gc)?;
                Ok(BuildResult::complex(
                    Namespace::new(tag.clone(), &expr_type.ty),
                    expr_type.patterns,
                ))
            }
            LinearTypeAst::Pattern { name, expr } => {
                if !pattern_mode {
                    return Err(Err(ParseError::PatternOutOfParameterDefinition(
                        self.clone(),
                    )));
                }
                let expr_type = expr.to_type(ctx, pattern_mode, gc)?;
                let mut patterns = expr_type.patterns;
                if patterns.contains(name) {
                    return Err(Err(ParseError::RedeclaredPattern(
                        self.clone(),
                        name.clone(),
                    )));
                }
                let debruijn_index = ctx.current_layer_mut().inc_pattern_count();
                patterns.extend(vec![name.clone()]);
                Ok(BuildResult::complex(
                    Pattern::new(debruijn_index, &expr_type.ty),
                    patterns,
                ))
            }
        }
    }
}
