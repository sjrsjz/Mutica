//! Mutica 类型系统模块

pub mod allof;
pub mod anyof;
pub mod character;
pub mod character_value;
pub mod closure;
pub mod constraint;
pub mod fixpoint;
pub mod float;
pub mod float_value;
pub mod invoke;
pub mod lazy;
pub mod mutable;
pub mod namespace;
pub mod natural_number;
pub mod natural_number_set;
pub mod opaque;
pub mod opcode;
pub mod pattern;
pub mod sequence;
pub mod subof;
pub mod unify;
pub mod variable;

macro_rules! type_dispatch {
    ($self:expr, $method:ident $(, $args:expr)*) => {
        match $self {
            Type::Sequence(v) => v.$method($($args),*),
            Type::Float(v) => v.$method($($args),*),
            Type::FloatValue(v) => v.$method($($args),*),
            Type::Char(v) => v.$method($($args),*),
            Type::CharValue(v) => v.$method($($args),*),
            Type::Any(v) => v.$method($($args),*),
            Type::All(v) => v.$method($($args),*),
            Type::FixPoint(v) => v.$method($($args),*),
            Type::Invoke(v) => v.$method($($args),*),
            Type::Variable(v) => v.$method($($args),*),
            Type::Closure(v) => v.$method($($args),*),
            Type::Opcode(v) => v.$method($($args),*),
            Type::Namespace(v) => v.$method($($args),*),
            Type::Constraint(v) => v.$method($($args),*),
            Type::Pattern(v) => v.$method($($args),*),
            Type::Lazy(v) => v.$method($($args),*),
            Type::SubOf(v) => v.$method($($args),*),
            Type::Mutable(v) => v.$method($($args),*),
            Type::NaturalNumber(v) => v.$method($($args),*),
            Type::NaturalNumberSet (v) => v.$method($($args),*),
            Type::OpaqueObject (v) => v.$method($($args),*),
        }
    };
}

macro_rules! typeref_dispatch {
    ($self:expr, $method:ident $(, $args:expr)*) => {
        match $self {
            TypeRef::Sequence(v) => v.$method($($args),*),
            TypeRef::Float(v) => v.$method($($args),*),
            TypeRef::FloatValue(v) => v.$method($($args),*),
            TypeRef::Char(v) => v.$method($($args),*),
            TypeRef::CharValue(v) => v.$method($($args),*),
            TypeRef::Any(v) => v.$method($($args),*),
            TypeRef::All(v) => v.$method($($args),*),
            TypeRef::FixPoint(v) => v.$method($($args),*),
            TypeRef::Invoke(v) => v.$method($($args),*),
            TypeRef::Variable(v) => v.$method($($args),*),
            TypeRef::Closure(v) => v.$method($($args),*),
            TypeRef::Opcode(v) => v.$method($($args),*),
            TypeRef::Namespace(v) => v.$method($($args),*),
            TypeRef::Constraint(v) => v.$method($($args),*),
            TypeRef::Pattern(v) => v.$method($($args),*),
            TypeRef::Lazy(v) => v.$method($($args),*),
            TypeRef::SubOf(v) => v.$method($($args),*),
            TypeRef::Mutable(v) => v.$method($($args),*),
            TypeRef::NaturalNumber(v) => v.$method($($args),*),
            TypeRef::NaturalNumberSet (v) => v.$method($($args),*),
            TypeRef::OpaqueObject (v) => v.$method($($args),*),
        }
    };
}

use std::{error::Error, fmt::Debug, sync::Arc};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};
use smallvec::SmallVec;

use crate::{
    types::{
        allof::AllOf,
        anyof::AnyOf,
        character::Character,
        character_value::CharacterValue,
        closure::Closure,
        constraint::Constraint,
        fixpoint::FixPoint,
        float::Float,
        float_value::FloatValue,
        invoke::Invoke,
        lazy::Lazy,
        mutable::Mutable,
        namespace::Namespace,
        natural_number::NaturalNumber,
        natural_number_set::NaturalNumberSet,
        opaque::OpaqueObject,
        opcode::Opcode,
        pattern::Pattern,
        sequence::Sequence,
        subof::SubOf,
        unify::{
            ArgumentBinding, GenericBinding, capture_env::CaptureEnvList, collector::Collector,
            path_collector::PathCollector,
        },
        variable::Variable,
    },
    util::{
        cycle_detector::FastCycleDetector,
        rootstack::{RootStack, Rootable},
        source_info::{SourceLocation, byte_offset_to_char_offset},
        three_valued_logic::ThreeValuedLogic,
    },
};

pub type TypeReport = ariadne::Report<'static, (String, std::ops::Range<usize>)>;

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Type<T> {
    fn clone(&self) -> Self {
        match self {
            Type::Sequence(v) => Type::Sequence(v.clone()),
            Type::Float(v) => Type::Float(v.clone()),
            Type::FloatValue(v) => Type::FloatValue(v.clone()),
            Type::Char(v) => Type::Char(v.clone()),
            Type::CharValue(v) => Type::CharValue(v.clone()),
            Type::Any(v) => Type::Any(v.clone()),
            Type::All(v) => Type::All(v.clone()),
            Type::FixPoint(v) => Type::FixPoint(v.clone()),
            Type::Invoke(v) => Type::Invoke(v.clone()),
            Type::Variable(v) => Type::Variable(v.clone()),
            Type::Closure(v) => Type::Closure(v.clone()),
            Type::Opcode(v) => Type::Opcode(v.clone()),
            Type::Namespace(v) => Type::Namespace(v.clone()),
            Type::Constraint(v) => Type::Constraint(v.clone()),
            Type::Pattern(v) => Type::Pattern(v.clone()),
            Type::Lazy(v) => Type::Lazy(v.clone()),
            Type::SubOf(v) => Type::SubOf(v.clone()),
            Type::Mutable(v) => Type::Mutable(v.clone()),
            Type::NaturalNumber(v) => Type::NaturalNumber(v.clone()),
            Type::NaturalNumberSet(v) => Type::NaturalNumberSet(v.clone()),
            Type::OpaqueObject(v) => Type::OpaqueObject(v.clone()),
        }
    }
}

pub enum Type<T: GcAllocObject<T, Inner = Type<T>>> {
    // 序列类型
    Sequence(Sequence<Type<T>, T>),
    // 浮点类型
    Float(Float<Type<T>, T>),
    // 浮点值类型
    FloatValue(FloatValue<Type<T>, T>),
    // 字符类型
    Char(Character<Type<T>, T>),
    // 字符值类型
    CharValue(CharacterValue<Type<T>, T>),
    // 泛化类型（无序Union，不允许分配律）
    Any(AnyOf<Type<T>, T>),
    // 专化类型（无序Intersection，不允许分配律）
    All(AllOf<Type<T>, T>),
    // 不动点类型
    FixPoint(FixPoint<Type<T>, T>),
    // 类型应用
    Invoke(Invoke<Type<T>, T>),
    // 类型变量
    Variable(Variable<Type<T>, T>),
    // 闭包类型（Pi类型）
    Closure(Closure<Type<T>, T>),
    // 操作码类型
    Opcode(Opcode<Type<T>, T>),
    // 命名空间类型
    Namespace(Namespace<Type<T>, T>),
    // 约束类型（Sigma类型）
    Constraint(Constraint<Type<T>, T>),
    // 模式类型（泛型参数绑定）
    Pattern(Pattern<Type<T>, T>),
    // 惰性包装器
    Lazy(Lazy<Type<T>, T>),
    // 子类型
    SubOf(SubOf<Type<T>, T>),
    // 可变包装器
    Mutable(Mutable<Type<T>, T>),
    // 自然数类型
    NaturalNumber(NaturalNumber<Type<T>, T>),
    // 自然数集合类型
    NaturalNumberSet(NaturalNumberSet<Type<T>, T>),
    // 不透明对象类型（用于封装宿主语言对象）
    OpaqueObject(OpaqueObject<Type<T>, T>),
}

pub enum TypeRef<'a, T: GcAllocObject<T, Inner = Type<T>>> {
    Sequence(&'a Sequence<Type<T>, T>),
    Float(&'a Float<Type<T>, T>),
    FloatValue(&'a FloatValue<Type<T>, T>),
    Char(&'a Character<Type<T>, T>),
    CharValue(&'a CharacterValue<Type<T>, T>),
    Any(&'a AnyOf<Type<T>, T>),
    All(&'a AllOf<Type<T>, T>),
    FixPoint(&'a FixPoint<Type<T>, T>),
    Invoke(&'a Invoke<Type<T>, T>),
    Variable(&'a Variable<Type<T>, T>),
    Closure(&'a Closure<Type<T>, T>),
    Opcode(&'a Opcode<Type<T>, T>),
    Namespace(&'a Namespace<Type<T>, T>),
    Constraint(&'a Constraint<Type<T>, T>),
    Pattern(&'a Pattern<Type<T>, T>),
    Lazy(&'a Lazy<Type<T>, T>),
    SubOf(&'a SubOf<Type<T>, T>),
    Mutable(&'a Mutable<Type<T>, T>),
    NaturalNumber(&'a NaturalNumber<Type<T>, T>),
    NaturalNumberSet(&'a NaturalNumberSet<Type<T>, T>),
    OpaqueObject(&'a OpaqueObject<Type<T>, T>),
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for TypeRef<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Copy for TypeRef<'_, T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for TypeRef<'_, T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        *self
    }

    fn into_dispatcher(self) -> Type<T> {
        self.clone_data()
    }
}

impl<'a, T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeRef<'a, Type<T>, T, Self>
    for TypeRef<'a, T>
{
    fn check(
        &self,
        other: Self,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        typeref_dispatch!(self, check, other, ctx)
    }

    fn subof(
        &self,
        other: Self,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        typeref_dispatch!(self, subof, other, ctx)
    }

    fn type_of(
        &self,
        ctx: &mut TypeOfContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        typeref_dispatch!(self, type_of, ctx)
    }

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        typeref_dispatch!(self, tagged_ptr)
    }

    fn as_ref_dispatcher(&self) -> Self {
        *self
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        typeref_dispatch!(self, source_info)
    }

    fn report_source_info(&self) -> TypeReport {
        typeref_dispatch!(self, report_source_info)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> TypeRef<'_, T> {
    pub fn clone_data(self) -> Type<T> {
        match self {
            TypeRef::Sequence(v) => Type::Sequence(v.clone()),
            TypeRef::Float(v) => Type::Float(v.clone()),
            TypeRef::FloatValue(v) => Type::FloatValue(v.clone()),
            TypeRef::Char(v) => Type::Char(v.clone()),
            TypeRef::CharValue(v) => Type::CharValue(v.clone()),
            TypeRef::Any(v) => Type::Any(v.clone()),
            TypeRef::All(v) => Type::All(v.clone()),
            TypeRef::FixPoint(v) => Type::FixPoint(v.clone()),
            TypeRef::Invoke(v) => Type::Invoke(v.clone()),
            TypeRef::Variable(v) => Type::Variable(v.clone()),
            TypeRef::Closure(v) => Type::Closure(v.clone()),
            TypeRef::Opcode(v) => Type::Opcode(v.clone()),
            TypeRef::Namespace(v) => Type::Namespace(v.clone()),
            TypeRef::Constraint(v) => Type::Constraint(v.clone()),
            TypeRef::Pattern(v) => Type::Pattern(v.clone()),
            TypeRef::Lazy(v) => Type::Lazy(v.clone()),
            TypeRef::SubOf(v) => Type::SubOf(v.clone()),
            TypeRef::Mutable(v) => Type::Mutable(v.clone()),
            TypeRef::NaturalNumber(v) => Type::NaturalNumber(v.clone()),
            TypeRef::NaturalNumberSet(v) => Type::NaturalNumberSet(v.clone()),
            TypeRef::OpaqueObject(v) => Type::OpaqueObject(v.clone()),
        }
    }
}

impl<'a, T: GcAllocObject<T, Inner = Type<T>>> TypeRef<'a, T> {
    pub fn map<F, R>(
        self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        f: F,
    ) -> Result<Option<R>, TypeError<Type<T>, T>>
    where
        F: FnOnce(&mut FastCycleDetector<TaggedPtr<()>>, TypeRef<T>) -> R,
        T: GcAllocObject<T, Inner = Type<T>>,
    {
        match self {
            TypeRef::FixPoint(v) => v.map(path, f),
            _ => Ok(Some(f(path, self))),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Debug for Type<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.represent(&mut FastCycleDetector::new(), 0, 2))
    }
}

use thiserror::Error;

/// A report bundle that includes both the ariadne report and source files needed to display it
pub struct TypeErrorReport {
    pub report: TypeReport,
    pub sources: Vec<(String, String)>, // (filepath, content) pairs
}

impl TypeErrorReport {
    pub fn new(report: TypeReport, sources: Vec<(String, String)>) -> Self {
        Self { report, sources }
    }

    /// Print the report with all required sources
    pub fn eprint(&self) -> std::io::Result<()> {
        let cache = ariadne::sources(
            self.sources
                .iter()
                .map(|(path, content)| (path.clone(), content.clone()))
                .collect::<std::collections::HashMap<_, _>>(),
        );
        self.report.eprint(cache)
    }
}

#[derive(Clone, Error)]
pub enum TypeError<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    UnresolvableType(Box<U>),
    InfiniteRecursion,
    RedeclaredType,
    NonApplicableType(Box<U>),
    TupleIndexOutOfBounds(Box<(U, U)>),
    TypeMismatch(Box<(U, String)>),
    MissingVariable(Box<str>),
    UnboundArgument(Box<str>),
    GenericLayerOverflow(Box<U>),
    AssertFailed(Box<(U, U)>),
    MissingContinuation(Box<U>),
    MissingPerformHandler(Box<U>),
    ClosureNotReduced(Box<U>),
    RuntimeError(Arc<dyn Error + Send + Sync>),
    Panic(Box<U>),
    Perform(Box<U>),
    TypeMayCauseCircularReasoning(Box<U>),
    #[doc(hidden)]
    Pandom(std::marker::PhantomData<V>),
}

impl<U: CoinductiveType<U, V> + Debug, V: GcAllocObject<V>> std::fmt::Display for TypeError<U, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UnresolvableType(ty) => {
                write!(f, "Unresolvable type: {:?}", ty)
            }
            TypeError::InfiniteRecursion => write!(f, "Infinite recursion"),
            TypeError::RedeclaredType => write!(f, "Type redeclared"),
            TypeError::NonApplicableType(ty) => {
                write!(f, "Non-applicable type: {:?}", ty)
            }
            TypeError::TupleIndexOutOfBounds(types) => {
                write!(f, "Tuple index out of bounds for types: {:?} and {:?}", types.0, types.1)
            }
            TypeError::TypeMismatch(info) => {
                write!(f, "Type mismatch: expected {}, found {:?}", info.1, info.0)
            }
            TypeError::MissingVariable(name) => {
                write!(f, "Missing variable: {}", name)
            }
            TypeError::UnboundArgument(name) => {
                write!(f, "Unbound environment variable: {}", name)
            }
            TypeError::GenericLayerOverflow(ty) => {
                write!(f, "Generic layer overflow for type: {:?}", ty)
            }
            TypeError::AssertFailed(types) => {
                write!(f, "Assert failed: {:?} doesn't accept {:?}", types.0, types.1)
            }
            TypeError::MissingContinuation(ty) => {
                write!(f, "Missing continuation: {:?}", ty)
            }
            TypeError::MissingPerformHandler(ty) => {
                write!(f, "Missing perform handler: {:?}", ty)
            }
            TypeError::TypeMayCauseCircularReasoning(ty) => {
                write!(f, "Type may cause circular reasoning: {:?}", ty)
            }
            TypeError::Panic(ty) => {
                write!(f, "Panic raised: {:?}", ty)
            }
            TypeError::ClosureNotReduced(ty) => write!(f, "Closure not reduced: {:?}", ty),
            TypeError::RuntimeError(err) => write!(f, "Runtime error: {}", err),
            TypeError::Perform(ty) => write!(f, "Perform raised: {:?}", ty),
            TypeError::Pandom(_) => write!(f, "Pandom error (hidden)"),
        }
    }
}

impl<U: CoinductiveType<U, V> + Debug, V: GcAllocObject<V>> Debug for TypeError<U, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> TypeError<U, V> {
    /// Generate an ariadne Report with source location information for the error
    pub fn to_report(&self) -> TypeErrorReport {
        let mut sources = Vec::new();

        let byte_offset_span_to_char_span =
            |content: &str, byte_span: std::ops::Range<usize>| -> std::ops::Range<usize> {
                let start = byte_offset_to_char_offset(content, byte_span.start);
                let end = byte_offset_to_char_offset(content, byte_span.end);
                start..end
            };

        let report = match self {
            TypeError::NonApplicableType(ty) => {
                let ty_repr = ty.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = ty.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Non-applicable type: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new((filepath, span))
                                .with_message("This type cannot be applied as a function"),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Non-applicable type: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Type cannot be applied"),
                        )
                        .finish()
                }
            }
            TypeError::TypeMismatch(info) => {
                let ty_repr = info.0.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = info.0.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Type mismatch: expected {}", info.1))
                        .with_label(
                            ariadne::Label::new((filepath, span))
                                .with_message(format!("Found {}", ty_repr)),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Type mismatch: expected {}", info.1))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Type mismatch (no source location)"),
                        )
                        .finish()
                }
            }
            TypeError::AssertFailed(types) => {
                let repr_a = types.0.represent(&mut FastCycleDetector::new(), 0, usize::MAX);
                let repr_b = types.1.represent(&mut FastCycleDetector::new(), 0, 3);
                let loc_a = types.0.source_info();
                let loc_b = types.1.source_info();

                if let Some(loc) = loc_a {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    let mut builder = ariadne::Report::build(
                        ariadne::ReportKind::Error,
                        filepath.clone(),
                        span.start,
                    )
                    .with_message(format!(
                        "Type assertion failed: {} doesn't accept {}",
                        repr_a, repr_b
                    ))
                    .with_label(
                        ariadne::Label::new((filepath.clone(), span))
                            .with_message(format!("Expected type: {}", repr_a)),
                    );

                    if let Some(rhs) = loc_b {
                        let rhs_span = byte_offset_span_to_char_span(
                            rhs.source().content(),
                            rhs.span().clone(),
                        );
                        let rhs_filepath = rhs.source().filepath().to_string();
                        let rhs_content = rhs.source().content().to_string();
                        // Only add source if it's different from lhs
                        if rhs_filepath != filepath {
                            sources.push((rhs_filepath.clone(), rhs_content));
                        }
                        builder = builder.with_label(
                            ariadne::Label::new((rhs_filepath, rhs_span))
                                .with_message(format!("Provided type: {}", repr_b)),
                        );
                    }

                    builder.finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!(
                            "Type assertion failed: {} doesn't accept {}",
                            repr_a, repr_b
                        ))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Assertion failed"),
                        )
                        .finish()
                }
            }
            TypeError::TupleIndexOutOfBounds(types) => {
                let tuple_repr = types.0.represent(&mut FastCycleDetector::new(), 0, 3);
                let index_repr = types.1.represent(&mut FastCycleDetector::new(), 0, 3);
                let tuple_loc = types.0.source_info();
                let index_loc = types.1.source_info();

                if let Some(loc) = index_loc {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    let mut builder = ariadne::Report::build(
                        ariadne::ReportKind::Error,
                        filepath.clone(),
                        span.start,
                    )
                    .with_message(format!(
                        "Tuple index out of bounds: index {} for tuple {}",
                        index_repr, tuple_repr
                    ))
                    .with_label(
                        ariadne::Label::new((filepath.clone(), span))
                            .with_message(format!("Index: {}", index_repr)),
                    );

                    if let Some(tuple) = tuple_loc {
                        let tuple_span = byte_offset_span_to_char_span(
                            tuple.source().content(),
                            tuple.span().clone(),
                        );
                        let tuple_filepath = tuple.source().filepath().to_string();
                        let tuple_content = tuple.source().content().to_string();
                        // Only add source if it's different from index's source
                        if tuple_filepath != filepath {
                            sources.push((tuple_filepath.clone(), tuple_content));
                        }
                        builder = builder.with_label(
                            ariadne::Label::new((tuple_filepath, tuple_span))
                                .with_message(format!("Tuple type: {}", tuple_repr)),
                        );
                    }

                    builder.finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!(
                            "Tuple index out of bounds: index {} for tuple {}",
                            index_repr, tuple_repr
                        ))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Index out of bounds"),
                        )
                        .finish()
                }
            }
            TypeError::MissingContinuation(ty) => {
                let ty_repr = ty.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = ty.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Missing continuation for type: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new((filepath, span))
                                .with_message("Continuation expected here"),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Missing continuation for type: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Continuation missing"),
                        )
                        .finish()
                }
            }
            TypeError::MissingPerformHandler(ty) => {
                let ty_repr = ty.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = ty.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Missing perform handler for type: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new((filepath, span))
                                .with_message("Perform handler expected here"),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Missing perform handler for type: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Handler missing"),
                        )
                        .finish()
                }
            }
            TypeError::ClosureNotReduced(ty) => {
                let ty_repr = ty.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = ty.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Closure not reduced: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new((filepath, span))
                                .with_message("Closure should have been reduced here, otherwise the captured variables may not be valid"),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Closure not reduced: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Closure not reduced"),
                        )
                        .finish()
                }
            }
            TypeError::Perform(ty) => {
                let ty_repr = ty.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = ty.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Perform raised: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new((filepath, span))
                                .with_message("Perform effect raised here"),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Perform raised: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Effect raised"),
                        )
                        .finish()
                }
            }
            TypeError::UnresolvableType(ty) => {
                let ty_repr = ty.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = ty.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Unresolvable type: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new((filepath, span))
                                .with_message("Type could not be resolved here"),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Unresolvable type: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Type unresolvable"),
                        )
                        .finish()
                }
            }
            TypeError::TypeMayCauseCircularReasoning(ty) => {
                let ty_repr = ty.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = ty.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Type may cause circular reasoning: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new((filepath, span))
                                .with_message("This type may lead to circular reasoning here"),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Type may cause circular reasoning: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Potential circular reasoning"),
                        )
                        .finish()
                }
            }
            TypeError::Panic(payload_value) => {
                let payload_repr = payload_value.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = payload_value.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Panic raised with payload: {}", payload_repr))
                        .with_label(
                            ariadne::Label::new((filepath, span)).with_message("Panic raised here"),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Panic raised with payload: {}", payload_repr))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Panic raised"),
                        )
                        .finish()
                }
            }
            // For errors without type information, create a generic report
            TypeError::InfiniteRecursion => {
                ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                    .with_message("Infinite recursion detected")
                    .with_label(
                        ariadne::Label::new(("<unknown>".to_string(), 0..0))
                            .with_message("Recursion limit exceeded"),
                    )
                    .finish()
            }
            TypeError::RedeclaredType => {
                ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                    .with_message("Type redeclared")
                    .with_label(
                        ariadne::Label::new(("<unknown>".to_string(), 0..0))
                            .with_message("Type was already declared"),
                    )
                    .finish()
            }
            TypeError::MissingVariable(id) => {
                ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                    .with_message(format!("Unbound variable: {}", id))
                    .with_label(
                        ariadne::Label::new(("<unknown>".to_string(), 0..0))
                            .with_message(format!("Variable {} not found", id)),
                    )
                    .finish()
            }
            TypeError::UnboundArgument(id) => {
                ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                    .with_message(format!("Unbound argument {}", id))
                    .with_label(
                        ariadne::Label::new(("<unknown>".to_string(), 0..0))
                            .with_message(format!("Argument {} not defined", id)),
                    )
                    .finish()
            }
            TypeError::GenericLayerOverflow(ty) => {
                let ty_repr = ty.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = ty.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Generic layer overflow for type: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new((filepath, span))
                                .with_message("Generic layer limit exceeded here"),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Generic layer overflow for type: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Layer overflow"),
                        )
                        .finish()
                }
            }
            TypeError::RuntimeError(err) => {
                ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                    .with_message(format!("Runtime error: {}", err))
                    .with_label(
                        ariadne::Label::new(("<unknown>".to_string(), 0..0))
                            .with_message("Runtime error occurred"),
                    )
                    .finish()
            }
            TypeError::Pandom(_) => {
                ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                    .with_message("Phantom error")
                    .with_label(
                        ariadne::Label::new(("<unknown>".to_string(), 0..0))
                            .with_message("Internal phantom error"),
                    )
                    .finish()
            }
        };

        TypeErrorReport::new(report, sources)
    }
}

pub trait GcAllocObject<T: GCTraceable<T> + 'static + Sized + Send + Sync>:
    GCTraceable<T> + 'static + Sized + Send + Sync
{
    type Inner: CoinductiveType<Self::Inner, T>
    where
        T: GcAllocObject<T>;
    fn new_fixpoint_placeholder() -> Self;
    fn new_mutable_slot(value: Self::Inner) -> Self
    where
        T: GcAllocObject<T>;
    fn is_mutable_slot(&self) -> bool
    where
        T: GcAllocObject<T>;

    fn get_fixpoint_value(&self) -> Option<&Self::Inner>
    where
        T: GcAllocObject<T>;
    fn get_mutable_value(&self) -> Option<Self::Inner>
    where
        T: GcAllocObject<T>;

    // fn get_value(&self) -> Option<&Self::Inner>
    // where
    //     T: GcAllocObject<T>,
    // {
    //     if self.is_mutable_slot() { self.get_mutable_value() } else { self.get_fixpoint_value() }
    // }

    fn set_fixpoint_value(&self, value: Self::Inner) -> Result<(), TypeError<Self::Inner, T>>
    where
        T: GcAllocObject<T>;

    fn set_mutable_value(&self, value: Self::Inner) -> Result<(), TypeError<Self::Inner, T>>
    where
        T: GcAllocObject<T>;

    // fn set_value(&self, value: Self::Inner) -> Result<(), TypeError<Self::Inner, T>>
    // where
    //     T: GcAllocObject<T>,
    // {
    //     if self.is_mutable_slot() {
    //         self.set_mutable_value(value)
    //     } else {
    //         self.set_fixpoint_value(value)
    //     }
    // }

    fn map_fixpoint_value<F, R>(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        f: F,
    ) -> Option<R>
    where
        F: FnOnce(
            &mut FastCycleDetector<TaggedPtr<()>>,
            <Self::Inner as AsDispatcher<Self::Inner, T>>::RefDispatcher<'_>,
        ) -> R,
        T: GcAllocObject<T>;

    fn map_mutable_value<F, R>(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        f: F,
    ) -> Option<R>
    where
        F: FnOnce(
            &mut FastCycleDetector<TaggedPtr<()>>,
            <Self::Inner as AsDispatcher<Self::Inner, T>>::RefDispatcher<'_>,
        ) -> R,
        T: GcAllocObject<T>;

    // fn map_value<F, R>(&self, path: &mut FastCycleDetector<TaggedPtr<()>>, f: F) -> Option<R>
    // where
    //     F: FnOnce(
    //         &mut FastCycleDetector<TaggedPtr<()>>,
    //         <Self::Inner as AsDispatcher<Self::Inner, T>>::RefDispatcher<'_>,
    //     ) -> R,
    //     T: GcAllocObject<T>,
    // {
    //     if self.is_mutable_slot() {
    //         self.map_mutable_value(path, f)
    //     } else {
    //         self.map_fixpoint_value(path, f)
    //     }
    // }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Type<T> {
    #[stacksafe::stacksafe]
    fn collect(&self, queue: &mut std::collections::VecDeque<GCArcWeak<T>>) {
        type_dispatch!(self, collect, queue)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Type<T> {
    #[stacksafe::stacksafe]
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        type_dispatch!(self, upgrade, collected)
    }

    #[stacksafe::stacksafe]
    fn rootless(&self) -> bool {
        type_dispatch!(self, rootless)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Type<T> {
    #[stacksafe::stacksafe]
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        type_dispatch!(self, check, other, ctx)
    }

    #[stacksafe::stacksafe]
    fn reduce(
        &self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        type_dispatch!(self, reduce, ctx)
    }

    #[stacksafe::stacksafe]
    fn invoke(&self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        type_dispatch!(self, invoke, ctx)
    }

    #[stacksafe::stacksafe]
    fn type_of(
        &self,
        ctx: &mut TypeOfContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        type_dispatch!(self, type_of, ctx)
    }

    #[stacksafe::stacksafe]
    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        type_dispatch!(self, subof, other, ctx)
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        type_dispatch!(self, source_info)
    }

    fn report_source_info(&self) -> TypeReport {
        type_dispatch!(self, report_source_info)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Type<T> {
    #[stacksafe::stacksafe]
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        type_dispatch!(self, represent, path, depth, max_depth)
    }

    #[stacksafe::stacksafe]
    fn display(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        type_dispatch!(self, display, path, depth, max_depth)
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
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        f: F,
    ) -> Result<Option<R>, TypeError<Type<T>, T>>
    where
        F: FnOnce(&mut FastCycleDetector<TaggedPtr<()>>, TypeRef<T>) -> R,
    {
        match self {
            Type::FixPoint(v) => v.map(path, f),
            _ => Ok(Some(f(path, self.as_ref_dispatcher()))),
        }
    }

    pub fn take<F, R>(
        self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        f: F,
    ) -> Result<Option<R>, TypeError<Type<T>, T>>
    where
        F: FnOnce(&mut FastCycleDetector<TaggedPtr<()>>, Type<T>) -> R,
    {
        match self {
            Type::FixPoint(v) => v.take(path, f),
            _ => Ok(Some(f(path, self))),
        }
    }
}

/// Trait to extract Type reference from different input types
pub trait AsDispatcher<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    type RefDispatcher<'b>: CoinductiveTypeRef<'b, U, V, Self::RefDispatcher<'b>>
        + AsDispatcher<U, V>
    where
        Self: 'b;

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
            Type::Sequence(v) => TypeRef::Sequence(v),
            Type::Float(v) => TypeRef::Float(v),
            Type::FloatValue(v) => TypeRef::FloatValue(v),
            Type::Char(v) => TypeRef::Char(v),
            Type::CharValue(v) => TypeRef::CharValue(v),
            Type::Any(v) => TypeRef::Any(v),
            Type::All(v) => TypeRef::All(v),
            Type::FixPoint(v) => TypeRef::FixPoint(v),
            Type::Invoke(v) => TypeRef::Invoke(v),
            Type::Variable(v) => TypeRef::Variable(v),
            Type::Closure(v) => TypeRef::Closure(v),
            Type::Opcode(v) => TypeRef::Opcode(v),
            Type::Namespace(v) => TypeRef::Namespace(v),
            Type::Constraint(v) => TypeRef::Constraint(v),
            Type::Pattern(v) => TypeRef::Pattern(v),
            Type::Lazy(v) => TypeRef::Lazy(v),
            Type::SubOf(v) => TypeRef::SubOf(v),
            Type::Mutable(v) => TypeRef::Mutable(v),
            Type::NaturalNumber(v) => TypeRef::NaturalNumber(v),
            Type::NaturalNumberSet(v) => TypeRef::NaturalNumberSet(v),
            Type::OpaqueObject(v) => TypeRef::OpaqueObject(v),
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
            Type::Sequence(v) => TypeRef::Sequence(v),
            Type::Float(v) => TypeRef::Float(v),
            Type::FloatValue(v) => TypeRef::FloatValue(v),
            Type::Char(v) => TypeRef::Char(v),
            Type::CharValue(v) => TypeRef::CharValue(v),
            Type::Any(v) => TypeRef::Any(v),
            Type::All(v) => TypeRef::All(v),
            Type::FixPoint(v) => TypeRef::FixPoint(v),
            Type::Invoke(v) => TypeRef::Invoke(v),
            Type::Variable(v) => TypeRef::Variable(v),
            Type::Closure(v) => TypeRef::Closure(v),
            Type::Opcode(v) => TypeRef::Opcode(v),
            Type::Namespace(v) => TypeRef::Namespace(v),
            Type::Constraint(v) => TypeRef::Constraint(v),
            Type::Pattern(v) => TypeRef::Pattern(v),
            Type::Lazy(v) => TypeRef::Lazy(v),
            Type::SubOf(v) => TypeRef::SubOf(v),
            Type::Mutable(v) => TypeRef::Mutable(v),
            Type::NaturalNumber(v) => TypeRef::NaturalNumber(v),
            Type::NaturalNumberSet(v) => TypeRef::NaturalNumberSet(v),
            Type::OpaqueObject(v) => TypeRef::OpaqueObject(v),
        }
    }
    fn into_dispatcher(self) -> Type<T>
    where
        Self: Sized,
    {
        self.clone()
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TaggedPtr<T> {
    ptr: *const T,
    tag: usize,
    length: Option<usize>,
}

impl<T> TaggedPtr<T> {
    pub fn unit() -> Self {
        Self { ptr: std::ptr::null(), tag: 0, length: None }
    }

    pub fn new(ptr: *const T, tag: usize) -> Self {
        Self { ptr, tag, length: None }
    }

    pub fn new_unique(ptr: *const T) -> Self {
        Self { ptr, tag: 0, length: None }
    }

    pub fn with_length(mut self, length: usize) -> Self {
        self.length = Some(length);
        self
    }

    pub fn ptr(&self) -> *const T {
        self.ptr
    }

    pub fn tag(&self) -> usize {
        self.tag
    }

    pub fn length(&self) -> Option<usize> {
        self.length
    }
}

pub enum PatternCollector<'a, 'b, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    None,
    Deconstruct(&'a mut Collector<(Arc<str>, U)>),
    Subtyping(&'a mut PathCollector<'b, (Arc<str>, Arc<str>)>),
    Pandom(std::marker::PhantomData<V>),
}

pub trait CollectorExt<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    fn collect<F, E>(&mut self, f: F) -> Result<ThreeValuedLogic, E>
    where
        F: FnOnce(PatternCollector<'_, '_, U, V>) -> Result<ThreeValuedLogic, E>;
}

impl<'a, 'b, U: CoinductiveType<U, V>, V: GcAllocObject<V>> CollectorExt<U, V>
    for PatternCollector<'a, 'b, U, V>
{
    fn collect<F, E>(&mut self, f: F) -> Result<ThreeValuedLogic, E>
    where
        F: for<'c, 'd> FnOnce(PatternCollector<'c, 'd, U, V>) -> Result<ThreeValuedLogic, E>,
    {
        // if let Some(collector) = self { collector.collect(|c| f(Some(c))) } else { f(None) }
        match self {
            PatternCollector::None => f(PatternCollector::None),
            PatternCollector::Deconstruct(c) => {
                c.collect(|collected: &mut Collector<(Arc<str>, U)>| {
                    f(PatternCollector::Deconstruct(collected))
                })
            }
            PatternCollector::Subtyping(c) => {
                c.collect(|collected| f(PatternCollector::Subtyping(collected)))
            }
            PatternCollector::Pandom(_) => f(PatternCollector::Pandom(std::marker::PhantomData)),
        }
    }
}

/// 类型检查上下文，用于 `check` 方法
#[allow(clippy::type_complexity)]
pub struct TypeCheckContext<'a, 'b, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub coinductive_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
    pub pattern_collector: PatternCollector<'a, 'b, U, V>,
    pub lhs_env: CaptureEnvList<'a, U, V>,
    pub rhs_env: CaptureEnvList<'a, U, V>,
    pub bound_generic_variables: &'a GenericBinding<'a, U, V>,
}

impl<'a, 'b, U: CoinductiveType<U, V>, V: GcAllocObject<V>> TypeCheckContext<'a, 'b, U, V> {
    #[allow(clippy::type_complexity)]
    pub fn new(
        coinductive_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        pattern_collector: PatternCollector<'a, 'b, U, V>,
        lhs_env: CaptureEnvList<'a, U, V>,
        rhs_env: CaptureEnvList<'a, U, V>,
        bound_generic_variables: &'a GenericBinding<U, V>,
    ) -> Self {
        Self {
            coinductive_assumptions,
            pattern_collector,
            lhs_env,
            rhs_env,
            bound_generic_variables,
        }
    }
}

/// 归约上下文，用于 `reduce` 方法
pub struct ReductionContext<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub solved_argument: &'a [(Arc<str>, ArgumentBinding<U, V>)],
    pub capture_env: CaptureEnvList<'a, U, V>,
    pub rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
    pub gc: &'a mut GC<V>,
    pub roots: &'roots mut RootStack<U, V>,
}

impl<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> ReductionContext<'a, 'roots, U, V> {
    pub fn new(
        solved_argument: &'a [(Arc<str>, ArgumentBinding<U, V>)],
        capture_env: CaptureEnvList<'a, U, V>,
        rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
        gc: &'a mut GC<V>,
        roots: &'roots mut RootStack<U, V>,
    ) -> Self {
        Self { solved_argument, capture_env, rec_assumptions, gc, roots }
    }
}

/// 类型应用上下文，用于 `invoke` 方法
pub struct InvokeContext<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub arg: &'a U,
    pub environment: CaptureEnvList<'a, U, V>,
    pub rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
    pub gc: &'a mut GC<V>,
    pub roots: &'roots mut RootStack<U, V>,
    pub source_info: Option<&'a Arc<SourceLocation>>,
}

impl<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> InvokeContext<'a, 'roots, U, V> {
    pub fn new(
        arg: &'a U,
        environment: CaptureEnvList<'a, U, V>,
        rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
        gc: &'a mut GC<V>,
        roots: &'roots mut RootStack<U, V>,
        source_info: Option<&'a Arc<SourceLocation>>,
    ) -> Self {
        Self { arg, environment, rec_assumptions, gc, roots, source_info }
    }
}

pub struct TypeOfContext<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub capture_env: CaptureEnvList<'a, U, V>,
    pub rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
    pub gc: &'a mut GC<V>,
    pub roots: &'roots mut RootStack<U, V>,
}

impl<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> TypeOfContext<'a, 'roots, U, V> {
    pub fn new(
        capture_env: CaptureEnvList<'a, U, V>,
        rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
        gc: &'a mut GC<V>,
        roots: &'roots mut RootStack<U, V>,
    ) -> Self {
        Self { capture_env, rec_assumptions, gc, roots }
    }
}

pub trait CoinductiveType<U: CoinductiveType<U, V>, V: GcAllocObject<V>>:
    Clone + Rootable<V> + Representable + AsDispatcher<U, V> + GCTraceable<V> + 'static + Sized
{
    // A : B
    fn check<'a>(
        &'a self,
        other: Self::RefDispatcher<'a>,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<ThreeValuedLogic, TypeError<U, V>>;

    // A <: B，验证类型图A是图B的特例（关键处理的是AnyOf和AllOf）
    fn subof<'a>(
        &'a self,
        other: Self::RefDispatcher<'a>,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<ThreeValuedLogic, TypeError<U, V>>;

    // 归约变换 (beta-reduction)
    fn reduce(&self, ctx: &mut ReductionContext<U, V>) -> Result<U, TypeError<U, V>>;

    // 类型应用 (apply)
    fn invoke(&self, ctx: InvokeContext<U, V>) -> Result<U, TypeError<U, V>>;

    fn type_of(&self, ctx: &mut TypeOfContext<U, V>) -> Result<U, TypeError<U, V>>;

    fn source_info(&self) -> Option<&Arc<SourceLocation>>;

    fn report_source_info(&self) -> TypeReport;

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        TaggedPtr::new_unique(self as *const _ as *const ())
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

pub trait CoinductiveTypeRef<
    'a,
    U: CoinductiveType<U, V>,
    V: GcAllocObject<V>,
    W: AsDispatcher<U, V> + CoinductiveTypeRef<'a, U, V, W>,
>: Clone + Sized
{
    // A : B
    fn check(
        &self,
        other: W,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<ThreeValuedLogic, TypeError<U, V>>;

    // A <: B，验证类型图A是图B的特例（关键处理的是AnyOf和AllOf）
    fn subof(
        &self,
        other: W,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<ThreeValuedLogic, TypeError<U, V>>;

    fn type_of(&self, ctx: &mut TypeOfContext<U, V>) -> Result<U, TypeError<U, V>>;

    fn source_info(&self) -> Option<&Arc<SourceLocation>>;

    fn report_source_info(&self) -> TypeReport;

    fn tagged_ptr(&self) -> TaggedPtr<()>;

    fn as_ref_dispatcher(&self) -> W;
}

pub trait CoinductiveTypeWithAny<U: CoinductiveType<U, V>, V: GcAllocObject<V>>:
    AsDispatcher<U, V>
{
    /// 检查当前类型是否接受另一个类型
    fn accept<'a>(
        &'a self,
        other: Self::RefDispatcher<'a>,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<ThreeValuedLogic, TypeError<U, V>>;

    fn superof<'a>(
        &'a self,
        other: Self::RefDispatcher<'a>,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<ThreeValuedLogic, TypeError<U, V>>;
}

pub trait Representable {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String;
    fn display(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        self.represent(path, depth, max_depth)
    }
}

impl<T: Representable> Representable for Vec<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        let mut repr = String::from("[");
        for (i, item) in self.iter().enumerate() {
            if i != 0 {
                repr.push_str(", ");
            }
            repr.push_str(&item.represent(path, depth, max_depth));
        }
        repr.push(']');
        repr
    }
}
