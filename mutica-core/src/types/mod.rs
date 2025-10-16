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
pub mod type_range;
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
        fixpoint::FixPoint,
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
        type_range::TypeRange,
        variable::Variable,
    },
    util::{
        collector::Collector,
        cycle_detector::FastCycleDetector,
        rootstack::{RootStack, Rootable},
    },
};

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Type<T> {
    fn clone(&self) -> Self {
        match self {
            Type::Bound(v) => Type::<T>::Bound(v.clone()),
            Type::Integer(v) => Type::<T>::Integer(v.clone()),
            Type::IntegerValue(v) => Type::<T>::IntegerValue(v.clone()),
            Type::Char(v) => Type::<T>::Char(v.clone()),
            Type::CharValue(v) => Type::<T>::CharValue(v.clone()),
            Type::Tuple(v) => Type::<T>::Tuple(v.clone()),
            Type::List(v) => Type::<T>::List(v.clone()),
            Type::Generalize(v) => Type::<T>::Generalize(v.clone()),
            Type::Specialize(v) => Type::<T>::Specialize(v.clone()),
            Type::FixPoint(v) => Type::<T>::FixPoint(v.clone()),
            Type::Invoke(v) => Type::<T>::Invoke(v.clone()),
            Type::Variable(v) => Type::<T>::Variable(v.clone()),
            Type::Closure(v) => Type::<T>::Closure(v.clone()),
            Type::Opcode(v) => Type::<T>::Opcode(v.clone()),
            Type::Namespace(v) => Type::<T>::Namespace(v.clone()),
            Type::Pattern(v) => Type::<T>::Pattern(v.clone()),
            Type::Lazy(v) => Type::<T>::Lazy(v.clone()),
            Type::Range(v) => Type::<T>::Range(v.clone()),
        }
    }
}

pub enum Type<T: GcAllocObject<T, Inner = Type<T>>> {
    // 类型边界
    Bound(TypeBound<T>),
    // 整数类型
    Integer(Integer<T>),
    // 整数值类型
    IntegerValue(IntegerValue<T>),
    // 字符类型
    Char(Character<T>),
    // 字符值类型
    CharValue(CharacterValue<T>),
    // 元组类型
    Tuple(Tuple<T>),
    // 列表类型（嵌套元组的优化表示）
    List(List<T>),
    // 泛化类型
    Generalize(Generalize<T>),
    // 专化类型
    Specialize(Specialize<T>),
    // 不动点类型
    FixPoint(FixPoint<T>),
    // 类型应用
    Invoke(Invoke<T>),
    // 类型变量
    Variable(Variable<T>),
    // 闭包类型
    Closure(Closure<T>),
    // 操作码类型
    Opcode(Opcode<T>),
    // 命名空间类型
    Namespace(Namespace<T>),
    // 模式类型
    Pattern(Pattern<T>),
    // 惰性包装器
    Lazy(Lazy<T>),
    // 类型范围
    Range(TypeRange<T>),
}

pub enum TypeRef<'a, T: GcAllocObject<T, Inner = Type<T>>> {
    Bound(&'a TypeBound<T>),
    Integer(&'a Integer<T>),
    IntegerValue(&'a IntegerValue<T>),
    Char(&'a Character<T>),
    CharValue(&'a CharacterValue<T>),
    Tuple(&'a Tuple<T>),
    List(&'a List<T>),
    Generalize(&'a Generalize<T>),
    Specialize(&'a Specialize<T>),
    FixPoint(&'a FixPoint<T>),
    Invoke(&'a Invoke<T>),
    Variable(&'a Variable<T>),
    Closure(&'a Closure<T>),
    Opcode(&'a Opcode<T>),
    Namespace(&'a Namespace<T>),
    Pattern(&'a Pattern<T>),
    Lazy(&'a Lazy<T>),
    Range(&'a TypeRange<T>),
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for TypeRef<'_, T> {
    fn clone(&self) -> Self {
        match self {
            TypeRef::Bound(v) => TypeRef::Bound(v),
            TypeRef::Integer(v) => TypeRef::Integer(v),
            TypeRef::IntegerValue(v) => TypeRef::IntegerValue(v),
            TypeRef::Char(v) => TypeRef::Char(v),
            TypeRef::CharValue(v) => TypeRef::CharValue(v),
            TypeRef::Tuple(v) => TypeRef::Tuple(v),
            TypeRef::List(v) => TypeRef::List(v),
            TypeRef::Generalize(v) => TypeRef::Generalize(v),
            TypeRef::Specialize(v) => TypeRef::Specialize(v),
            TypeRef::FixPoint(v) => TypeRef::FixPoint(v),
            TypeRef::Invoke(v) => TypeRef::Invoke(v),
            TypeRef::Variable(v) => TypeRef::Variable(v),
            TypeRef::Closure(v) => TypeRef::Closure(v),
            TypeRef::Opcode(v) => TypeRef::Opcode(v),
            TypeRef::Namespace(v) => TypeRef::Namespace(v),
            TypeRef::Pattern(v) => TypeRef::Pattern(v),
            TypeRef::Lazy(v) => TypeRef::Lazy(v),
            TypeRef::Range(v) => TypeRef::Range(v),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Copy for TypeRef<'_, T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> TypeRef<'_, T> {
    fn clone_data(self) -> Type<T> {
        match self {
            TypeRef::Bound(v) => Type::<T>::Bound(v.clone()),
            TypeRef::Integer(v) => Type::<T>::Integer(v.clone()),
            TypeRef::IntegerValue(v) => Type::<T>::IntegerValue(v.clone()),
            TypeRef::Char(v) => Type::<T>::Char(v.clone()),
            TypeRef::CharValue(v) => Type::<T>::CharValue(v.clone()),
            TypeRef::Tuple(v) => Type::<T>::Tuple(v.clone()),
            TypeRef::List(v) => Type::<T>::List(v.clone()),
            TypeRef::Generalize(v) => Type::<T>::Generalize(v.clone()),
            TypeRef::Specialize(v) => Type::<T>::Specialize(v.clone()),
            TypeRef::FixPoint(v) => Type::<T>::FixPoint(v.clone()),
            TypeRef::Invoke(v) => Type::<T>::Invoke(v.clone()),
            TypeRef::Variable(v) => Type::<T>::Variable(v.clone()),
            TypeRef::Closure(v) => Type::<T>::Closure(v.clone()),
            TypeRef::Opcode(v) => Type::<T>::Opcode(v.clone()),
            TypeRef::Namespace(v) => Type::<T>::Namespace(v.clone()),
            TypeRef::Pattern(v) => Type::<T>::Pattern(v.clone()),
            TypeRef::Lazy(v) => Type::<T>::Lazy(v.clone()),
            TypeRef::Range(v) => Type::<T>::Range(v.clone()),
        }
    }
}

impl<'a, T: GcAllocObject<T, Inner = Type<T>>> TypeRef<'a, T> {
    pub fn tagged_ptr(self) -> TaggedPtr<()> {
        match self {
            TypeRef::Bound(v) => v.tagged_ptr(),
            TypeRef::Integer(v) => v.tagged_ptr(),
            TypeRef::IntegerValue(v) => v.tagged_ptr(),
            TypeRef::Char(v) => v.tagged_ptr(),
            TypeRef::CharValue(v) => v.tagged_ptr(),
            TypeRef::Tuple(v) => v.tagged_ptr(),
            TypeRef::List(v) => v.tagged_ptr(),
            TypeRef::Generalize(v) => v.tagged_ptr(),
            TypeRef::Specialize(v) => v.tagged_ptr(),
            TypeRef::FixPoint(v) => v.tagged_ptr(),
            TypeRef::Invoke(v) => v.tagged_ptr(),
            TypeRef::Variable(v) => v.tagged_ptr(),
            TypeRef::Closure(v) => v.tagged_ptr(),
            TypeRef::Opcode(v) => v.tagged_ptr(),
            TypeRef::Namespace(v) => v.tagged_ptr(),
            TypeRef::Pattern(v) => v.tagged_ptr(),
            TypeRef::Lazy(v) => v.tagged_ptr(),
            TypeRef::Range(v) => v.tagged_ptr(),
        }
    }

    fn map_inner<F, R>(
        self,
        path: &mut FastCycleDetector<*const ()>,
        f: F,
    ) -> Result<R, TypeError<Type<T>, T>>
    where
        F: FnOnce(&mut FastCycleDetector<*const ()>, TypeRef<T>) -> R,
        T: GcAllocObject<T, Inner = Type<T>>,
    {
        match self {
            TypeRef::FixPoint(v) => v.map(path, f),
            _ => Ok(f(path, self)),
        }
    }

    fn is(
        self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        match self {
            TypeRef::Bound(v) => v.is(other, ctx),
            TypeRef::Integer(v) => v.is(other, ctx),
            TypeRef::IntegerValue(v) => v.is(other, ctx),
            TypeRef::Char(v) => v.is(other, ctx),
            TypeRef::CharValue(v) => v.is(other, ctx),
            TypeRef::Tuple(v) => v.is(other, ctx),
            TypeRef::List(v) => v.is(other, ctx),
            TypeRef::Generalize(v) => v.is(other, ctx),
            TypeRef::Specialize(v) => v.is(other, ctx),
            TypeRef::FixPoint(v) => v.is(other, ctx),
            TypeRef::Invoke(v) => v.is(other, ctx),
            TypeRef::Variable(v) => v.is(other, ctx),
            TypeRef::Closure(v) => v.is(other, ctx),
            TypeRef::Opcode(v) => v.is(other, ctx),
            TypeRef::Namespace(v) => v.is(other, ctx),
            TypeRef::Pattern(v) => v.is(other, ctx),
            TypeRef::Lazy(v) => v.is(other, ctx),
            TypeRef::Range(v) => v.is(other, ctx),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Debug for Type<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.represent(&mut FastCycleDetector::new()))
    }
}

use thiserror::Error;

#[derive(Clone, Error)]
pub enum TypeError<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    #[error("Unresolvable type (e.g. fixpoint reference lost)")]
    UnresolvableType,
    #[error("Infinite recursion")]
    InfiniteRecursion,
    #[error("Type redeclared")]
    RedeclaredType,
    #[error("Non-applicable type: {0:?}")]
    NonApplicableType(Box<U>),
    #[error("Tuple index out of bounds")]
    TupleIndexOutOfBounds(Box<(U, U)>),
    #[error("Type mismatch")]
    TypeMismatch(Box<(U, String)>),
    #[error("Unbound variable: id={0}")]
    UnboundVariable(isize),
    #[error("Assert failed: L </: R")]
    AssertFailed(Box<(U, U)>),
    #[error("Missing continuation")]
    MissingContinuation,
    #[error("Runtime error: {0}")]
    RuntimeError(Arc<dyn Error + Send + Sync>),
    #[error("Other error: {0}")]
    OtherError(String),
    #[error("Pandom")]
    #[doc(hidden)]
    Pandom(std::marker::PhantomData<V>),
}

impl<U: CoinductiveType<U, V> + Debug, V: GcAllocObject<V>> Debug for TypeError<U, V> {
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
            Type::Range(v) => v.$method($($args),*),
        }
    };
}

pub trait GcAllocObject<T: GCTraceable<T> + 'static + Sized>:
    GCTraceable<T> + 'static + Sized
{
    type Inner: CoinductiveType<Self::Inner, T>
    where
        T: GcAllocObject<T>;
    fn new_placeholder() -> Self {
        unimplemented!()
    }

    fn get_inner(&self) -> Option<&Self::Inner>
    where
        T: GcAllocObject<T>,
    {
        unimplemented!()
    }

    fn set_inner(&self, _value: Self::Inner) -> Result<(), TypeError<Self::Inner, T>>
    where
        T: GcAllocObject<T>,
    {
        unimplemented!()
    }

    // pub fn map<F, R>(
    //     &self,
    //     path: &mut FastCycleDetector<*const ()>,
    //     f: F,
    // ) -> Result<R, TypeError<Type<T>, T>>
    // where
    //     F: FnOnce(&mut FastCycleDetector<*const ()>, &Type<T>) -> R,
    // {
    //     match self {
    //         Type::FixPoint(v) => v.map(path, f),
    //         _ => Ok(f(path, self)),
    //     }
    // }

    fn map_inner<F, R>(
        &self,
        path: &mut FastCycleDetector<*const ()>,
        f: F,
    ) -> Result<R, TypeError<Self::Inner, T>>
    where
        F: FnOnce(
            &mut FastCycleDetector<*const ()>,
            <Self::Inner as AsDispatcher<Self::Inner, T>>::RefDispatcher<'_>,
        ) -> R,
        T: GcAllocObject<T>,
    {
        if let Some(inner) = self.get_inner() {
            Ok(f(path, inner.as_ref_dispatcher()))
        } else {
            Err(TypeError::UnresolvableType)
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Type<T> {
    #[stacksafe::stacksafe]
    fn collect(&self, queue: &mut std::collections::VecDeque<GCArcWeak<T>>) {
        type_dispatch!(self, collect, queue)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for Type<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Type<T> {
    #[stacksafe::stacksafe]
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        type_dispatch!(self, upgrade, collected)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Type<T> {
    #[stacksafe::stacksafe]
    fn is(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        type_dispatch!(self, is, other, ctx)
    }

    #[stacksafe::stacksafe]
    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        // 如果已经是范式类型则直接返回
        if self.is_normal_form() {
            return Ok(self);
        }
        type_dispatch!(self, reduce, ctx)
    }

    #[stacksafe::stacksafe]
    fn invoke(
        &self,
        ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        type_dispatch!(self, invoke, ctx)
    }

    fn is_normal_form(&self) -> bool {
        match self {
            Type::Bound(v) => v.is_normal_form(),
            Type::Integer(v) => v.is_normal_form(),
            Type::IntegerValue(v) => v.is_normal_form(),
            Type::Char(v) => v.is_normal_form(),
            Type::CharValue(v) => v.is_normal_form(),
            Type::Tuple(v) => v.is_normal_form(),
            Type::List(v) => v.is_normal_form(),
            Type::Generalize(v) => v.is_normal_form(),
            Type::Specialize(v) => v.is_normal_form(),
            Type::FixPoint(v) => v.is_normal_form(),
            Type::Invoke(v) => v.is_normal_form(),
            Type::Variable(v) => v.is_normal_form(),
            Type::Closure(v) => v.is_normal_form(),
            Type::Opcode(v) => v.is_normal_form(),
            Type::Namespace(v) => v.is_normal_form(),
            Type::Pattern(v) => v.is_normal_form(),
            Type::Lazy(v) => v.is_normal_form(),
            Type::Range(v) => v.is_normal_form(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Type<T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Type<T> {
    pub fn map<F, R>(
        &self,
        path: &mut FastCycleDetector<*const ()>,
        f: F,
    ) -> Result<R, TypeError<Type<T>, T>>
    where
        F: FnOnce(&mut FastCycleDetector<*const ()>, TypeRef<T>) -> R,
    {
        match self {
            Type::FixPoint(v) => v.map(path, f),
            _ => Ok(f(path, self.as_ref_dispatcher())),
        }
    }
}

/// Trait to extract Type reference from different input types
pub trait AsDispatcher<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    type RefDispatcher<'a>
    where
        Self: 'a;

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a>;
    fn into_dispatcher(self) -> U
    where
        Self: Sized;
}

// Implement AsTypeRef for different types
impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Type<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        match self {
            Type::Bound(v) => TypeRef::Bound(v),
            Type::Integer(v) => TypeRef::Integer(v),
            Type::IntegerValue(v) => TypeRef::IntegerValue(v),
            Type::Char(v) => TypeRef::Char(v),
            Type::CharValue(v) => TypeRef::CharValue(v),
            Type::Tuple(v) => TypeRef::Tuple(v),
            Type::List(v) => TypeRef::List(v),
            Type::Generalize(v) => TypeRef::Generalize(v),
            Type::Specialize(v) => TypeRef::Specialize(v),
            Type::FixPoint(v) => TypeRef::FixPoint(v),
            Type::Invoke(v) => TypeRef::Invoke(v),
            Type::Variable(v) => TypeRef::Variable(v),
            Type::Closure(v) => TypeRef::Closure(v),
            Type::Opcode(v) => TypeRef::Opcode(v),
            Type::Namespace(v) => TypeRef::Namespace(v),
            Type::Pattern(v) => TypeRef::Pattern(v),
            Type::Lazy(v) => TypeRef::Lazy(v),
            Type::Range(v) => TypeRef::Range(v),
        }
    }
    fn into_dispatcher(self) -> Type<T>
    where
        Self: Sized,
    {
        self
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for &Type<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        match self {
            Type::Bound(v) => TypeRef::Bound(v),
            Type::Integer(v) => TypeRef::Integer(v),
            Type::IntegerValue(v) => TypeRef::IntegerValue(v),
            Type::Char(v) => TypeRef::Char(v),
            Type::CharValue(v) => TypeRef::CharValue(v),
            Type::Tuple(v) => TypeRef::Tuple(v),
            Type::List(v) => TypeRef::List(v),
            Type::Generalize(v) => TypeRef::Generalize(v),
            Type::Specialize(v) => TypeRef::Specialize(v),
            Type::FixPoint(v) => TypeRef::FixPoint(v),
            Type::Invoke(v) => TypeRef::Invoke(v),
            Type::Variable(v) => TypeRef::Variable(v),
            Type::Closure(v) => TypeRef::Closure(v),
            Type::Opcode(v) => TypeRef::Opcode(v),
            Type::Namespace(v) => TypeRef::Namespace(v),
            Type::Pattern(v) => TypeRef::Pattern(v),
            Type::Lazy(v) => TypeRef::Lazy(v),
            Type::Range(v) => TypeRef::Range(v),
        }
    }
    fn into_dispatcher(self) -> Type<T>
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
pub struct TypeCheckContext<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub assumptions: &'a mut SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
    pub closure_env: (&'a ClosureEnv<U, V>, &'a ClosureEnv<U, V>),
    pub pattern_env: &'a mut Collector<(usize, U)>,
    pandom: std::marker::PhantomData<V>,
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> TypeCheckContext<'a, U, V> {
    pub fn new(
        assumptions: &'a mut SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&'a ClosureEnv<U, V>, &'a ClosureEnv<U, V>),
        pattern_env: &'a mut Collector<(usize, U)>,
    ) -> Self {
        Self {
            assumptions,
            closure_env,
            pattern_env,
            pandom: std::marker::PhantomData,
        }
    }
}

/// 归约上下文，用于 `reduce` 方法
pub struct ReductionContext<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub closure_env: &'a ClosureEnv<U, V>,
    pub param_env: &'a ParamEnv<U, V>,
    pub continuation: Option<&'a U>,
    pub rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
    pub gc: &'a mut GC<V>,
    pub roots: &'roots mut RootStack<V>,
}

impl<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> ReductionContext<'a, 'roots, U, V> {
    pub fn new(
        closure_env: &'a ClosureEnv<U, V>,
        param_env: &'a ParamEnv<U, V>,
        continuation: Option<&'a U>,
        rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
        gc: &'a mut GC<V>,
        roots: &'roots mut RootStack<V>,
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
pub struct InvokeContext<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub arg: &'a U,
    pub closure_env: &'a ClosureEnv<U, V>,
    pub param_env: &'a ParamEnv<U, V>,
    pub continuation: Option<&'a U>,
    pub rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
    pub gc: &'a mut GC<V>,
    pub roots: &'roots mut RootStack<V>,
}

impl<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> InvokeContext<'a, 'roots, U, V> {
    pub fn new(
        arg: &'a U,
        closure_env: &'a ClosureEnv<U, V>,
        param_env: &'a ParamEnv<U, V>,
        continuation: Option<&'a U>,
        rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
        gc: &'a mut GC<V>,
        roots: &'roots mut RootStack<V>,
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
pub trait CoinductiveType<U: CoinductiveType<U, V>, V: GcAllocObject<V>>:
    GcAllocObject<V> + Clone + Rootable<V> + Representable + AsDispatcher<U, V>
{
    fn is(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<Option<()>, TypeError<U, V>>;

    // 归约变换
    fn reduce(self, ctx: &mut ReductionContext<U, V>) -> Result<U, TypeError<U, V>>;

    // 类型应用
    fn invoke(&self, ctx: &mut InvokeContext<U, V>) -> Result<U, TypeError<U, V>>;

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        TaggedPtr::new_unique(self as *const _ as *const ())
    }

    fn is_normal_form(&self) -> bool;

    fn equivalent(&self, other: &Self) -> Result<bool, TypeError<U, V>> {
        let mut assumptions = SmallVec::new();
        let empty_env = ClosureEnv::<U, V>::new(Vec::<U>::new());
        let mut pattern_env = Collector::new_disabled();
        let type_check_ctx = &mut TypeCheckContext::new(
            &mut assumptions,
            (&empty_env, &empty_env),
            &mut pattern_env,
        );
        Ok(self
            .is(other.as_ref_dispatcher(), type_check_ctx)?
            .is_some()
            && other
                .is(self.as_ref_dispatcher(), type_check_ctx)?
                .is_some())
    }

    fn dispatch(self) -> U {
        <Self as AsDispatcher<U, V>>::into_dispatcher(self)
    }

    fn dispatch_ref<'a>(&'a self) -> Self::RefDispatcher<'a>
    where
        Self: 'a,
    {
        <Self as AsDispatcher<U, V>>::as_ref_dispatcher(self)
    }
}

pub trait CoinductiveTypeWithAny<U: CoinductiveType<U, V>, V: GcAllocObject<V>>:
    AsDispatcher<U, V>
{
    fn has(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<Option<()>, TypeError<U, V>>;
}

pub trait Representable {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String;
    fn display(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        self.represent(path)
    }
}
