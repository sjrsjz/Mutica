//! Mutica 类型系统模块

pub mod character;
pub mod character_value;
pub mod closure;
pub mod fixpoint;
pub mod generalize;
pub mod integer;
pub mod integer_value;
pub mod invoke;
pub mod lazy;
pub mod list;
pub mod namespace;
pub mod opcode;
pub mod pattern;
pub mod specialize;
pub mod tuple;
pub mod type_bound;
pub mod variable;

use std::{error::Error, fmt::Debug, sync::Arc};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};
use smallvec::SmallVec;

use crate::{
    types::{
        character::Character,
        character_value::CharacterValue,
        closure::{Closure, ClosureEnv, ParamEnv},
        fixpoint::{FixPoint, FixPointInner},
        generalize::Generalize,
        integer::Integer,
        integer_value::IntegerValue,
        invoke::Invoke,
        lazy::Lazy,
        list::List,
        namespace::Namespace,
        opcode::Opcode,
        pattern::Pattern,
        specialize::Specialize,
        tuple::Tuple,
        type_bound::TypeBound,
        variable::Variable,
    },
    util::{
        collector::Collector,
        cycle_detector::FastCycleDetector,
        rootstack::{RootStack, Rootable},
    },
};

#[derive(Clone)]
pub struct Type {
    ty: TypeEnum,
    is_nf: bool,
}

impl Type {
    /// 指定一个未知是否为范式的类型
    pub fn new(ty: TypeEnum) -> Self {
        Self { ty, is_nf: false }
    }

    /// 指定一个已经是范式的类型（即 reduce 后不变的类型）
    pub fn new_nf(ty: TypeEnum) -> Self {
        Self { ty, is_nf: true }
    }

    /// 判断类型是否为范式
    #[inline(always)]
    pub fn is_nf(&self) -> bool {
        self.is_nf
    }

    #[inline(always)]
    pub fn ty(&self) -> &TypeEnum {
        &self.ty
    }
}

#[derive(Clone)]
pub enum TypeEnum {
    // 类型边界
    Bound(TypeBound),
    // 整数类型
    Integer(Integer),
    // 整数值类型
    IntegerValue(IntegerValue),
    // 字符类型
    Char(Character),
    // 字符值类型
    CharValue(CharacterValue),
    // 元组类型
    Tuple(Tuple),
    // 列表类型（嵌套元组的优化表示）
    List(List),
    // 泛化类型
    Generalize(Generalize),
    // 专化类型
    Specialize(Specialize),
    // 不动点类型
    FixPoint(FixPoint),
    // 类型应用
    Invoke(Invoke),
    // 类型变量
    Variable(Variable),
    // 闭包类型
    Closure(Closure),
    // 操作码类型
    Opcode(Opcode),
    // 命名空间类型
    Namespace(Namespace),
    // 模式类型
    Pattern(Pattern),
    // 惰性包装器
    Lazy(Lazy),
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.represent(&mut FastCycleDetector::new()))
    }
}

use thiserror::Error;

#[derive(Clone, Error)]
pub enum TypeError {
    #[error("Unresolvable type (e.g. fixpoint reference lost)")]
    UnresolvableType,
    #[error("Infinite recursion")]
    InfiniteRecursion,
    #[error("Type redeclared")]
    RedeclaredType,
    #[error("Non-applicable type: {0:?}")]
    NonApplicableType(Box<Type>),
    #[error("Tuple index out of bounds: index {0:?}")]
    TupleIndexOutOfBounds(Box<(Type, Type)>),
    #[error("Type mismatch: {0:?}")]
    TypeMismatch(Box<(Type, String)>),
    #[error("Unbound variable: id={0}")]
    UnboundVariable(isize),
    #[error("Assert failed: L </: R {0:?}")]
    AssertFailed(Box<(Type, Type)>),
    #[error("Missing continuation")]
    MissingContinuation,
    #[error("Runtime error: {0}")]
    RuntimeError(Arc<dyn Error + Send + Sync>),
}

impl Debug for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

macro_rules! type_dispatch {
    ($self:expr, $method:ident $(, $args:expr)*) => {
        match $self {
            TypeEnum::Bound(v) => v.$method($($args),*),
            TypeEnum::Integer(v) => v.$method($($args),*),
            TypeEnum::IntegerValue(v) => v.$method($($args),*),
            TypeEnum::Tuple(v) => v.$method($($args),*),
            TypeEnum::Generalize(v) => v.$method($($args),*),
            TypeEnum::Specialize(v) => v.$method($($args),*),
            TypeEnum::FixPoint(v) => v.$method($($args),*),
            TypeEnum::Invoke(v) => v.$method($($args),*),
            TypeEnum::Variable(v) => v.$method($($args),*),
            TypeEnum::Closure(v) => v.$method($($args),*),
            TypeEnum::Opcode(v) => v.$method($($args),*),
            TypeEnum::List(v) => v.$method($($args),*),
            TypeEnum::Char(v) => v.$method($($args),*),
            TypeEnum::CharValue(v) => v.$method($($args),*),
            TypeEnum::Namespace(v) => v.$method($($args),*),
            TypeEnum::Pattern(v) => v.$method($($args),*),
            TypeEnum::Lazy(v) => v.$method($($args),*),
        }
    };
}

impl GCTraceable<FixPointInner> for Type {
    #[stacksafe::stacksafe]
    fn collect(&self, queue: &mut std::collections::VecDeque<GCArcWeak<FixPointInner>>) {
        type_dispatch!(&self.ty, collect, queue)
    }
}

impl Rootable for Type {
    #[stacksafe::stacksafe]
    fn upgrade<'roots>(&self, collected: &'roots mut Vec<GCArc<FixPointInner>>) {
        type_dispatch!(&self.ty, upgrade, collected)
    }
}

impl CoinductiveType<Type> for Type {
    #[stacksafe::stacksafe]
    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        type_dispatch!(&self.ty, is, other, ctx)
    }

    fn dispatch(self) -> Type {
        self
    }

    #[stacksafe::stacksafe]
    fn reduce(self, ctx: &mut ReductionContext) -> Result<Type, TypeError> {
        // 如果已经是范式类型则直接返回
        if self.is_nf {
            return Ok(self);
        }
        type_dispatch!(self.ty, reduce, ctx)
    }

    #[stacksafe::stacksafe]
    fn invoke(&self, ctx: &mut InvokeContext) -> Result<Type, TypeError> {
        type_dispatch!(&self.ty, invoke, ctx)
    }
}

impl Representable for Type {
    #[stacksafe::stacksafe]
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        type_dispatch!(&self.ty, represent, path)
    }

    #[stacksafe::stacksafe]
    fn display(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        type_dispatch!(&self.ty, display, path)
    }
}

// 分派到指定类型，如果与指定类型不同则立即panic
#[macro_export]
macro_rules! as_type {
    ($self:expr, $type:path) => {
        match $self {
            $type(v) => v,
            _ => panic!("Expected type {}", stringify!($type)),
        }
    };
}

impl Type {
    pub fn map<F, R>(&self, path: &mut FastCycleDetector<*const ()>, f: F) -> Result<R, TypeError>
    where
        F: FnOnce(&mut FastCycleDetector<*const ()>, &Type) -> R,
    {
        match &self.ty {
            TypeEnum::FixPoint(v) => v.map(path, f),
            _ => Ok(f(path, self)),
        }
    }
    pub fn equivalent<T: AsType>(&self, other: T) -> Result<bool, TypeError> {
        let mut assumptions = SmallVec::new();
        let empty_env = ClosureEnv::new(Vec::<Type>::new());
        let mut pattern_env = Collector::new();
        let type_check_ctx = &mut TypeCheckContext::new(
            &mut assumptions,
            (&empty_env, &empty_env),
            &mut pattern_env,
            false,
        );
        Ok(self.is(other.as_type_ref(), type_check_ctx)?.is_some()
            && other.as_type_ref().is(self, type_check_ctx)?.is_some())
    }
}

/// Trait to extract Type reference from different input types
pub trait AsType {
    fn as_type_ref(&self) -> &Type;
    fn into_type(self) -> Type
    where
        Self: Sized;
}

// Implement AsTypeRef for different types
impl AsType for Type {
    fn as_type_ref(&self) -> &Type {
        self
    }
    fn into_type(self) -> Type
    where
        Self: Sized,
    {
        self
    }
}

impl AsType for &Type {
    fn as_type_ref(&self) -> &Type {
        self
    }
    fn into_type(self) -> Type
    where
        Self: Sized,
    {
        self.clone()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct TaggedPtr<T> {
    ptr: *const T,
    tag: usize,
}

impl<T> TaggedPtr<T> {
    pub fn new(ptr: *const T, tag: usize) -> Self {
        Self { ptr, tag }
    }

    pub fn new_unique(ptr: *const T) -> Self {
        Self { ptr, tag: 0 }
    }

    pub fn ptr(&self) -> *const T {
        self.ptr
    }

    pub fn tag(&self) -> usize {
        self.tag
    }
}

/// 类型检查上下文，用于 `is` 和 `has` 方法
pub struct TypeCheckContext<'a> {
    pub assumptions: &'a mut SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
    pub closure_env: (&'a ClosureEnv, &'a ClosureEnv),
    pub pattern_env: &'a mut Collector<(usize, Type)>,
    pub pattern_mode: bool,
}

impl<'a> TypeCheckContext<'a> {
    pub fn new(
        assumptions: &'a mut SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&'a ClosureEnv, &'a ClosureEnv),
        pattern_env: &'a mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Self {
        Self {
            assumptions,
            closure_env,
            pattern_env,
            pattern_mode,
        }
    }
}

/// 归约上下文，用于 `reduce` 方法
pub struct ReductionContext<'a, 'roots> {
    pub closure_env: &'a ClosureEnv,
    pub param_env: &'a ParamEnv,
    pub continuation: Option<&'a Type>,
    pub rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
    pub gc: &'a mut GC<FixPointInner>,
    pub roots: &'roots mut RootStack,
}

impl<'a, 'roots> ReductionContext<'a, 'roots> {
    pub fn new(
        closure_env: &'a ClosureEnv,
        param_env: &'a ParamEnv,
        continuation: Option<&'a Type>,
        rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &'a mut GC<FixPointInner>,
        roots: &'roots mut RootStack,
    ) -> Self {
        Self {
            closure_env,
            param_env,
            continuation,
            rec_assumptions,
            gc,
            roots,
        }
    }
}

/// 类型应用上下文，用于 `invoke` 方法
pub struct InvokeContext<'a, 'roots> {
    pub arg: &'a Type,
    pub closure_env: &'a ClosureEnv,
    pub param_env: &'a ParamEnv,
    pub continuation: Option<&'a Type>,
    pub rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
    pub gc: &'a mut GC<FixPointInner>,
    pub roots: &'roots mut RootStack,
}

impl<'a, 'roots> InvokeContext<'a, 'roots> {
    pub fn new(
        arg: &'a Type,
        closure_env: &'a ClosureEnv,
        param_env: &'a ParamEnv,
        continuation: Option<&'a Type>,
        rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &'a mut GC<FixPointInner>,
        roots: &'roots mut RootStack,
    ) -> Self {
        Self {
            arg,
            closure_env,
            param_env,
            continuation,
            rec_assumptions,
            gc,
            roots,
        }
    }
}

pub trait CoinductiveType<T: CoinductiveType<T>>: Clone {
    fn is(&self, other: &T, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError>;

    fn dispatch(self) -> T;

    // 归约变换
    fn reduce(self, ctx: &mut ReductionContext) -> Result<T, TypeError>;

    // 类型应用
    fn invoke(&self, ctx: &mut InvokeContext) -> Result<T, TypeError>;

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        TaggedPtr::new_unique(self as *const _ as *const ())
    }
}

pub trait CoinductiveTypeWithAny<T: CoinductiveType<T>> {
    fn has<V: CoinductiveType<T>>(
        &self,
        other: &V,
        ctx: &mut TypeCheckContext,
    ) -> Result<Option<()>, TypeError>;
}

pub trait Representable {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String;
    fn display(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        self.represent(path)
    }
}
