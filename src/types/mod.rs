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

use std::{error::Error, fmt::Debug, ops::Deref, sync::Arc};

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
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

#[derive(Clone)]
pub enum Type {
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
    // 字面量类型
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
    NonApplicableType(Box<StabilizedType>),
    #[error("Tuple index out of bounds: index {0:?}")]
    TupleIndexOutOfBounds(Box<(StabilizedType, StabilizedType)>),
    #[error("Type mismatch: {0:?}, expected {1}")]
    TypeMismatch(Box<StabilizedType>, String),
    #[error("Unbound variable: id={0}")]
    UnboundVariable(isize),
    #[error("Assert failed: L </: R {0:?}")]
    AssertFailed(Box<(StabilizedType, StabilizedType)>),
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
            Type::Bound(v) => v.$method($($args),*),
            Type::Integer(v) => v.$method($($args),*),
            Type::IntegerValue(v) => v.$method($($args),*),
            Type::Tuple(v) => v.$method($($args),*),
            Type::Generalize(v) => v.$method($($args),*),
            Type::Specialize(v) => v.$method($($args),*),
            Type::FixPoint(v) => v.$method($($args),*),
            Type::Invoke(v) => v.$method($($args),*),
            Type::Variable(v) => v.$method($($args),*),
            Type::Closure(v) => v.$method($($args),*),
            Type::Opcode(v) => v.$method($($args),*),
            Type::List(v) => v.$method($($args),*),
            Type::Char(v) => v.$method($($args),*),
            Type::CharValue(v) => v.$method($($args),*),
            Type::Namespace(v) => v.$method($($args),*),
            Type::Pattern(v) => v.$method($($args),*),
            Type::Lazy(v) => v.$method($($args),*),
        }
    };
}

impl GCTraceable<FixPointInner> for Type {
    #[stacksafe::stacksafe]
    fn collect(&self, queue: &mut std::collections::VecDeque<GCArcWeak<FixPointInner>>) {
        type_dispatch!(self, collect, queue)
    }
}

impl Rootable for Type {
    #[stacksafe::stacksafe]
    fn upgrade(&self, collected: &mut SmallVec<[GCArc<FixPointInner>; 8]>) {
        type_dispatch!(self, upgrade, collected)
    }
}

impl CoinductiveType<Type, StabilizedType> for Type {
    #[stacksafe::stacksafe]
    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        type_dispatch!(self, is, other, ctx)
    }

    fn dispatch(self) -> Type {
        self
    }

    #[stacksafe::stacksafe]
    fn reduce(&self, ctx: &mut ReductionContext) -> Result<StabilizedType, TypeError> {
        type_dispatch!(self, reduce, ctx)
    }

    #[stacksafe::stacksafe]
    fn invoke(&self, ctx: &mut InvokeContext) -> Result<StabilizedType, TypeError> {
        type_dispatch!(self, invoke, ctx)
    }
}

impl Representable for Type {
    #[stacksafe::stacksafe]
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        type_dispatch!(self, represent, path)
    }

    #[stacksafe::stacksafe]
    fn display(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        type_dispatch!(self, display, path)
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
        match self {
            Type::FixPoint(v) => v.map(path, f),
            _ => Ok(f(path, self)),
        }
    }

    pub fn stabilize(self) -> StabilizedType {
        StabilizedType::new(self)
    }
}

/// Trait to extract Type reference from different input types
pub trait AsTypeRef {
    fn as_type_ref(&self) -> &Type;
    fn into_type(self) -> Type
    where
        Self: Sized;
}

// Implement AsTypeRef for different types
impl AsTypeRef for Type {
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

impl AsTypeRef for &Type {
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

impl AsTypeRef for StabilizedType {
    fn as_type_ref(&self) -> &Type {
        self.weak()
    }
    fn into_type(self) -> Type
    where
        Self: Sized,
    {
        self.weak().clone()
    }
}

impl AsTypeRef for &StabilizedType {
    fn as_type_ref(&self) -> &Type {
        self.weak()
    }
    fn into_type(self) -> Type
    where
        Self: Sized,
    {
        self.weak().clone()
    }
}

#[derive(Clone)]
/// GC 强引用存储策略枚举。
///
/// 根据对象的复杂性选择不同的引用存储策略，优化内存使用和性能。
///
/// # 变体说明
/// - `None`: 无需额外引用（基础类型如 Integer, String）
/// - `Single`: 单个强引用（如 FixPoint 对象）
/// - `Multiple`: 多个强引用（复杂对象如 Pair）
///
/// # 设计考虑
/// 这种分层设计避免了为简单对象分配不必要的 Vec，
/// 同时为复杂对象提供了灵活的引用管理。
pub enum GCArcStorage {
    None,
    Single(GCArc<FixPointInner>),
    Multiple(Arc<[GCArc<FixPointInner>]>),
}

impl<T: Rootable> From<&T> for GCArcStorage {
    fn from(value: &T) -> Self {
        let mut collected = SmallVec::<[GCArc<FixPointInner>; 8]>::new();
        value.upgrade(&mut collected);
        match collected.len() {
            0 => GCArcStorage::None,
            1 => GCArcStorage::Single(collected.pop().unwrap()),
            _ => GCArcStorage::Multiple(collected.into_boxed_slice().into()),
        }
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

pub trait Rootable {
    #[allow(unused_variables)]
    fn upgrade(&self, collected: &mut SmallVec<[GCArc<FixPointInner>; 8]>) {}
}

pub trait Stabilized<T: CoinductiveType<T, U>, U: Stabilized<T, U>> {
    fn weak(&self) -> &T;
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
pub struct ReductionContext<'a> {
    pub closure_env: &'a ClosureEnv,
    pub param_env: &'a ParamEnv,
    pub continuation: Option<&'a Type>,
    pub rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
    pub gc: &'a mut GC<FixPointInner>,
}

impl<'a> ReductionContext<'a> {
    pub fn new(
        closure_env: &'a ClosureEnv,
        param_env: &'a ParamEnv,
        continuation: Option<&'a Type>,
        rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &'a mut GC<FixPointInner>,
    ) -> Self {
        Self {
            closure_env,
            param_env,
            continuation,
            rec_assumptions,
            gc,
        }
    }
}

/// 类型应用上下文，用于 `invoke` 方法
pub struct InvokeContext<'a> {
    pub arg: &'a Type,
    pub closure_env: &'a ClosureEnv,
    pub param_env: &'a ParamEnv,
    pub continuation: Option<&'a Type>,
    pub rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
    pub gc: &'a mut GC<FixPointInner>,
}

impl<'a> InvokeContext<'a> {
    pub fn new(
        arg: &'a Type,
        closure_env: &'a ClosureEnv,
        param_env: &'a ParamEnv,
        continuation: Option<&'a Type>,
        rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &'a mut GC<FixPointInner>,
    ) -> Self {
        Self {
            arg,
            closure_env,
            param_env,
            continuation,
            rec_assumptions,
            gc,
        }
    }
}

pub trait CoinductiveType<T: CoinductiveType<T, U>, U: Stabilized<T, U>>: Clone {
    fn is(&self, other: &T, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError>;

    fn dispatch(self) -> T;

    // 归约变换
    fn reduce(&self, ctx: &mut ReductionContext) -> Result<U, TypeError>;

    // 类型应用
    fn invoke(&self, ctx: &mut InvokeContext) -> Result<U, TypeError>;

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        TaggedPtr::new_unique(self as *const _ as *const ())
    }
}

pub trait CoinductiveTypeWithAny<T: CoinductiveType<T, U>, U: Stabilized<T, U>> {
    fn has<V: CoinductiveType<T, U>>(
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

#[derive(Clone)]
pub struct StabilizedType {
    inner: Type,
    #[allow(dead_code)]
    ref_holder: GCArcStorage,
}

impl Debug for StabilizedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.represent(&mut FastCycleDetector::new()))
    }
}

impl Deref for StabilizedType {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl Stabilized<Type, StabilizedType> for StabilizedType {
    fn weak(&self) -> &Type {
        &self.inner
    }
}

impl Representable for StabilizedType {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        self.inner.represent(path)
    }
}

impl StabilizedType {
    pub fn new(inner: Type) -> Self {
        let ref_holder = GCArcStorage::from(&inner);
        StabilizedType { inner, ref_holder }
    }

    pub fn new_with_ref(inner: Type, ref_holder: GCArcStorage) -> Self {
        StabilizedType { inner, ref_holder }
    }

    pub fn map<F, R>(&self, path: &mut FastCycleDetector<*const ()>, f: F) -> Result<R, TypeError>
    where
        F: FnOnce(&mut FastCycleDetector<*const ()>, &Type) -> R,
    {
        self.inner.map(path, f)
    }
}
