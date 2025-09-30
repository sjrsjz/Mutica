use arc_gc::gc::GC;

use crate::as_type;
use crate::parser::{BuildContext, ParseContext, ParseError};
use crate::types::apply::Apply;
use crate::types::character::Character;
use crate::types::character_value::CharacterValue;
use crate::types::closure::{Closure, ClosureEnv};
use crate::types::effect::Effect;
use crate::types::fixpoint::{FixPoint, FixPointInner};
use crate::types::generalize::Generalize;
use crate::types::integer::Integer;
use crate::types::integer_value::IntegerValue;
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
    Variable(String),
    Tuple(Vec<TypeAst>),
    List(Vec<TypeAst>),
    Generalize(Vec<TypeAst>),
    Specialize(Vec<TypeAst>),
    Effect {
        payload: Box<TypeAst>,
        then: Box<TypeAst>,
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
    Variable(String),
    Tuple(Vec<BasicTypeAst>),
    List(Vec<BasicTypeAst>),
    Generalize(Vec<BasicTypeAst>),
    Specialize(Vec<BasicTypeAst>),
    Effect {
        payload: Box<BasicTypeAst>,
        then: Box<BasicTypeAst>,
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
            TypeAst::Effect { payload, then } => BasicTypeAst::Effect {
                payload: Box::new(payload.into_basic()),
                then: Box::new(then.into_basic()),
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
                        arg: Box::new(BasicTypeAst::Variable("match#value".to_string())),
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
                                BasicTypeAst::Variable("eq#left".to_string()),
                                BasicTypeAst::Variable("eq#right".to_string()),
                            ])),
                        }
                        .into(),
                        BasicTypeAst::Apply {
                            func: Box::new(BasicTypeAst::AtomicOpcode(AtomicOpcode::Is)),
                            arg: Box::new(BasicTypeAst::Tuple(vec![
                                BasicTypeAst::Variable("eq#right".to_string()),
                                BasicTypeAst::Variable("eq#left".to_string()),
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
    ty: BasicTypeAst,          // flow后的类型
    captures: HashSet<String>, // 该类型所捕获的自由变量
    patterns: PatternEnv,      // 该类型中出现的所有模式变量
}

impl FlowResult {
    pub fn simple(ty: BasicTypeAst) -> Self {
        FlowResult {
            ty,
            captures: HashSet::new(),
            patterns: PatternEnv::new(),
        }
    }

    pub fn complex(ty: BasicTypeAst, captures: HashSet<String>, patterns: PatternEnv) -> Self {
        FlowResult {
            ty,
            captures,
            patterns,
        }
    }

    pub fn ty(&self) -> &BasicTypeAst {
        &self.ty
    }

    pub fn captures(&self) -> &HashSet<String> {
        &self.captures
    }

    pub fn patterns(&self) -> &PatternEnv {
        &self.patterns
    }
}

impl BasicTypeAst {
    pub fn flow(
        &self,
        ctx: &mut ParseContext,
        pattern_mode: bool,
    ) -> Result<FlowResult, ParseError> {
        match self {
            BasicTypeAst::Int => Ok(FlowResult::simple(BasicTypeAst::Int)),
            BasicTypeAst::Char => Ok(FlowResult::simple(BasicTypeAst::Char)),
            BasicTypeAst::Top => Ok(FlowResult::simple(BasicTypeAst::Top)),
            BasicTypeAst::Bottom => Ok(FlowResult::simple(BasicTypeAst::Bottom)),
            BasicTypeAst::IntLiteral(v) => Ok(FlowResult::simple(BasicTypeAst::IntLiteral(*v))),
            BasicTypeAst::CharLiteral(v) => Ok(FlowResult::simple(BasicTypeAst::CharLiteral(*v))),
            BasicTypeAst::Variable(name) => {
                if ctx.is_declared(name) {
                    let mut captures = HashSet::new();
                    captures.insert(name.clone());
                    Ok(FlowResult::complex(
                        BasicTypeAst::Variable(name.clone()),
                        captures,
                        PatternEnv::new(),
                    ))
                } else {
                    Err(ParseError::UseBeforeDeclaration(self.clone(), name.clone()))
                }
            }
            BasicTypeAst::Tuple(elements) => {
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
                    BasicTypeAst::Tuple(new_elements),
                    all_captures,
                    all_patterns,
                ))
            }
            BasicTypeAst::List(elements) => {
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
                    BasicTypeAst::List(new_elements),
                    all_captures,
                    all_patterns,
                ))
            }
            BasicTypeAst::Generalize(types) => {
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
                    BasicTypeAst::Generalize(new_types),
                    all_captures,
                    all_patterns,
                ))
            }
            BasicTypeAst::Specialize(types) => {
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
                    BasicTypeAst::Specialize(new_types),
                    all_captures,
                    all_patterns,
                ))
            }
            BasicTypeAst::Effect { payload, then } => {
                let payload_res = payload.flow(ctx, pattern_mode)?;
                let then_res = then.flow(ctx, pattern_mode)?;
                let mut all_captures = payload_res.captures;
                all_captures.extend(then_res.captures);
                let mut all_patterns = payload_res.patterns;
                all_patterns
                    .extend(then_res.patterns)
                    .map_err(|name| ParseError::RedeclaredPattern(self.clone(), name))?;
                Ok(FlowResult::complex(
                    BasicTypeAst::Effect {
                        payload: Box::new(payload_res.ty),
                        then: Box::new(then_res.ty),
                    },
                    all_captures,
                    all_patterns,
                ))
            }

            BasicTypeAst::Apply { func, arg } => {
                let func_res = func.flow(ctx, pattern_mode)?;
                let arg_res = arg.flow(ctx, pattern_mode)?;
                let mut all_captures = func_res.captures;
                all_captures.extend(arg_res.captures);
                let mut all_patterns = func_res.patterns;
                all_patterns
                    .extend(arg_res.patterns)
                    .map_err(|name| ParseError::RedeclaredPattern(self.clone(), name))?;
                Ok(FlowResult::complex(
                    BasicTypeAst::Apply {
                        func: Box::new(func_res.ty),
                        arg: Box::new(arg_res.ty),
                    },
                    all_captures,
                    all_patterns,
                ))
            }
            BasicTypeAst::AtomicOpcode(atomic_opcode) => Ok(FlowResult::simple(
                BasicTypeAst::AtomicOpcode(atomic_opcode.clone()),
            )),
            BasicTypeAst::FixPoint { param_name, expr } => {
                ctx.enter_scope();
                ctx.declare_variable(param_name.clone());
                // 递归函数的表达式中不允许出现模式变量
                let mut expr_res = expr.flow(ctx, false)?;
                ctx.exit_scope();
                // 移除掉param_name，因为它是递归函数的参数，不应当被视为捕获的自由变量
                expr_res.captures.remove(param_name);
                Ok(FlowResult::complex(
                    BasicTypeAst::FixPoint {
                        param_name: param_name.clone(),
                        expr: Box::new(expr_res.ty),
                    },
                    expr_res.captures,
                    PatternEnv::new(), // fixpoint类型本身不应当把模式变量泄露出去
                ))
            }
            BasicTypeAst::Namespace { tag, expr } => {
                let expr_res = expr.flow(ctx, pattern_mode)?;
                Ok(FlowResult::complex(
                    BasicTypeAst::Namespace {
                        tag: tag.clone(),
                        expr: Box::new(expr_res.ty),
                    },
                    expr_res.captures,
                    expr_res.patterns,
                ))
            }
            BasicTypeAst::Pattern { name, expr } => {
                if !pattern_mode {
                    return Err(ParseError::PatternOutOfParameterDefinition(self.clone()));
                }
                let expr_res = expr.flow(ctx, pattern_mode)?;
                let mut patterns = PatternEnv::new();
                patterns.extend(vec![name.clone()]).unwrap();
                Ok(FlowResult::complex(
                    BasicTypeAst::Pattern {
                        name: name.clone(),
                        expr: Box::new(expr_res.ty),
                    },
                    expr_res.captures,
                    patterns,
                ))
            }
            BasicTypeAst::Closure {
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
                    BasicTypeAst::Closure {
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

impl BasicTypeAst {
    pub fn to_type(
        &self,
        ctx: &mut BuildContext,
        pattern_mode: bool,
        gc: &mut GC<FixPointInner>,
    ) -> Result<BuildResult, Result<TypeError, ParseError>> {
        match self {
            BasicTypeAst::Int => Ok(BuildResult::simple(Integer::new())),
            BasicTypeAst::Char => Ok(BuildResult::simple(Character::new())),
            BasicTypeAst::Top => Ok(BuildResult::simple(TypeBound::top())),
            BasicTypeAst::Bottom => Ok(BuildResult::simple(TypeBound::bottom())),
            BasicTypeAst::IntLiteral(v) => Ok(BuildResult::simple(IntegerValue::new(*v))),
            BasicTypeAst::CharLiteral(v) => Ok(BuildResult::simple(CharacterValue::new(*v))),
            BasicTypeAst::Variable(var) => {
                if let Some(ty) = ctx.current_layer().get(var) {
                    match ty {
                        Ok(t) => Ok(BuildResult::simple(t.clone().stabilize())), // fixpoint类型
                        Err(index) => Ok(BuildResult::simple(Variable::new(index))),
                    }
                } else {
                    Err(Err(ParseError::UseBeforeDeclaration(
                        self.clone(),
                        var.clone(),
                    )))
                }
            }
            BasicTypeAst::Tuple(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(ctx, pattern_mode, gc)?);
                }
                let (types, patterns) = BuildResult::fold(types);
                Ok(BuildResult::complex(Tuple::new(&types), patterns))
            }
            BasicTypeAst::List(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(ctx, pattern_mode, gc)?);
                }
                let (types, patterns) = BuildResult::fold(types);
                Ok(BuildResult::complex(List::new(&types), patterns))
            }
            BasicTypeAst::Generalize(basic_type_asts) => {
                let mut types = Vec::new();
                for bta in basic_type_asts {
                    types.push(bta.to_type(ctx, pattern_mode, gc)?);
                }
                // 我们无法计算出泛化类型的闭包环境，因此传入一个空的闭包环境
                let (types, patterns) = BuildResult::fold(types);
                Ok(BuildResult::complex(Generalize::new_raw(&types), patterns))
            }
            BasicTypeAst::Specialize(basic_type_asts) => {
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
            BasicTypeAst::Effect { payload, then } => {
                let payload_type = payload.to_type(ctx, pattern_mode, gc)?;
                let then_type = then.to_type(ctx, pattern_mode, gc)?;
                let (types, patterns) = BuildResult::fold(vec![payload_type, then_type]);
                Ok(BuildResult::complex(
                    Effect::new(&types[0], &types[1]),
                    patterns,
                ))
            }
            BasicTypeAst::Closure {
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
                            Err(index) => closure_env.push(Variable::new(index)),
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
            BasicTypeAst::Apply { func, arg } => {
                let func_type = func.to_type(ctx, pattern_mode, gc)?;
                let arg_type = arg.to_type(ctx, pattern_mode, gc)?;
                let (types, patterns) = BuildResult::fold(vec![func_type, arg_type]);
                Ok(BuildResult::complex(
                    Apply::new(&types[0], &types[1]),
                    patterns,
                ))
            }
            BasicTypeAst::AtomicOpcode(atomic_opcode) => {
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
            BasicTypeAst::FixPoint { param_name, expr } => {
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
            BasicTypeAst::Namespace { tag, expr } => {
                let expr_type = expr.to_type(ctx, pattern_mode, gc)?;
                Ok(BuildResult::complex(
                    Namespace::new(tag.clone(), &expr_type.ty),
                    expr_type.patterns,
                ))
            }
            BasicTypeAst::Pattern { name, expr } => {
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
