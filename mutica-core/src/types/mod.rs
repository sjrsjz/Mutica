//! Mutica 类型系统模块

pub mod allof;
pub mod anyof;
pub mod character;
pub mod character_value;
pub mod closure;
pub mod eqof;
pub mod fixpoint;
pub mod float;
pub mod float_value;
pub mod invoke;
pub mod lazy;
pub mod namespace;
pub mod opcode;
pub mod ordered_type;
pub mod pattern;
pub mod rot;
pub mod sequence;
pub mod subof;
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
    test_true,
    types::{
        allof::AllOf,
        anyof::AnyOf,
        character::Character,
        character_value::CharacterValue,
        closure::{Closure, ClosureEnv, ParamEnv},
        eqof::EqOf,
        fixpoint::FixPoint,
        float::Float,
        float_value::FloatValue,
        invoke::Invoke,
        lazy::Lazy,
        namespace::Namespace,
        opcode::Opcode,
        ordered_type::OrderedType,
        pattern::Pattern,
        rot::Rotate,
        sequence::Sequence,
        subof::SubOf,
        tuple::Tuple,
        type_bound::TypeBound,
        variable::Variable,
    },
    util::{
        collector::Collector,
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
            Type::Bound(v) => Type::<T>::Bound(v.clone()),
            Type::Sequence(v) => Type::<T>::Sequence(v.clone()),
            Type::Float(v) => Type::<T>::Float(v.clone()),
            Type::FloatValue(v) => Type::<T>::FloatValue(v.clone()),
            Type::Char(v) => Type::<T>::Char(v.clone()),
            Type::CharValue(v) => Type::<T>::CharValue(v.clone()),
            Type::Tuple(v) => Type::<T>::Tuple(v.clone()),
            Type::Any(v) => Type::<T>::Any(v.clone()),
            Type::All(v) => Type::<T>::All(v.clone()),
            Type::FixPoint(v) => Type::<T>::FixPoint(v.clone()),
            Type::Invoke(v) => Type::<T>::Invoke(v.clone()),
            Type::Variable(v) => Type::<T>::Variable(v.clone()),
            Type::Closure(v) => Type::<T>::Closure(v.clone()),
            Type::Opcode(v) => Type::<T>::Opcode(v.clone()),
            Type::Namespace(v) => Type::<T>::Namespace(v.clone()),
            Type::Pattern(v) => Type::<T>::Pattern(v.clone()),
            Type::Lazy(v) => Type::<T>::Lazy(v.clone()),
            Type::Rot(v) => Type::<T>::Rot(v.clone()),
            Type::OrderedType(v) => Type::<T>::OrderedType(v.clone()),
            Type::EqOf(v) => Type::<T>::EqOf(v.clone()),
            Type::SubOf(v) => Type::<T>::SubOf(v.clone()),
        }
    }
}

pub enum Type<T: GcAllocObject<T, Inner = Type<T>>> {
    // 类型边界
    Bound(TypeBound<T>),
    // 整数类型
    Sequence(Sequence<T>),
    // 浮点类型
    Float(Float<T>),
    // 浮点值类型
    FloatValue(FloatValue<T>),
    // 字符类型
    Char(Character<T>),
    // 字符值类型
    CharValue(CharacterValue<T>),
    // 元组类型
    Tuple(Tuple<T>),
    // 泛化类型
    Any(AnyOf<T>),
    // 专化类型
    All(AllOf<T>),
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
    // Rot变换
    Rot(Rotate<T>),
    // 高阶类型
    OrderedType(OrderedType<T>),
    // 单例等价类型
    EqOf(EqOf<T>),
    // 子类型
    SubOf(SubOf<T>),
}

pub enum TypeRef<'a, T: GcAllocObject<T, Inner = Type<T>>> {
    Bound(&'a TypeBound<T>),
    Sequence(&'a Sequence<T>),
    Float(&'a Float<T>),
    FloatValue(&'a FloatValue<T>),
    Char(&'a Character<T>),
    CharValue(&'a CharacterValue<T>),
    Tuple(&'a Tuple<T>),
    Any(&'a AnyOf<T>),
    All(&'a AllOf<T>),
    FixPoint(&'a FixPoint<T>),
    Invoke(&'a Invoke<T>),
    Variable(&'a Variable<T>),
    Closure(&'a Closure<T>),
    Opcode(&'a Opcode<T>),
    Namespace(&'a Namespace<T>),
    Pattern(&'a Pattern<T>),
    Lazy(&'a Lazy<T>),
    Rot(&'a Rotate<T>),
    OrderedType(&'a OrderedType<T>),
    EqOf(&'a EqOf<T>),
    SubOf(&'a SubOf<T>),
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
        match self {
            TypeRef::Bound(v) => v.check(other, ctx),
            TypeRef::Sequence(v) => v.check(other, ctx),
            TypeRef::Float(v) => v.check(other, ctx),
            TypeRef::FloatValue(v) => v.check(other, ctx),
            TypeRef::Char(v) => v.check(other, ctx),
            TypeRef::CharValue(v) => v.check(other, ctx),
            TypeRef::Tuple(v) => v.check(other, ctx),
            TypeRef::Any(v) => v.check(other, ctx),
            TypeRef::All(v) => v.check(other, ctx),
            TypeRef::FixPoint(v) => v.check(other, ctx),
            TypeRef::Invoke(v) => v.check(other, ctx),
            TypeRef::Variable(v) => v.check(other, ctx),
            TypeRef::Closure(v) => v.check(other, ctx),
            TypeRef::Opcode(v) => v.check(other, ctx),
            TypeRef::Namespace(v) => v.check(other, ctx),
            TypeRef::Pattern(v) => v.check(other, ctx),
            TypeRef::Lazy(v) => v.check(other, ctx),
            TypeRef::Rot(v) => v.check(other, ctx),
            TypeRef::OrderedType(v) => v.check(other, ctx),
            TypeRef::EqOf(v) => v.check(other, ctx),
            TypeRef::SubOf(v) => v.check(other, ctx),
        }
    }

    fn subof(
        &self,
        other: Self,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        match self {
            TypeRef::Bound(v) => v.subof(other, ctx),
            TypeRef::Sequence(v) => v.subof(other, ctx),
            TypeRef::Float(v) => v.subof(other, ctx),
            TypeRef::FloatValue(v) => v.subof(other, ctx),
            TypeRef::Char(v) => v.subof(other, ctx),
            TypeRef::CharValue(v) => v.subof(other, ctx),
            TypeRef::Tuple(v) => v.subof(other, ctx),
            TypeRef::Any(v) => v.subof(other, ctx),
            TypeRef::All(v) => v.subof(other, ctx),
            TypeRef::FixPoint(v) => v.subof(other, ctx),
            TypeRef::Invoke(v) => v.subof(other, ctx),
            TypeRef::Variable(v) => v.subof(other, ctx),
            TypeRef::Closure(v) => v.subof(other, ctx),
            TypeRef::Opcode(v) => v.subof(other, ctx),
            TypeRef::Namespace(v) => v.subof(other, ctx),
            TypeRef::Pattern(v) => v.subof(other, ctx),
            TypeRef::Lazy(v) => v.subof(other, ctx),
            TypeRef::Rot(v) => v.subof(other, ctx),
            TypeRef::OrderedType(v) => v.subof(other, ctx),
            TypeRef::EqOf(v) => v.subof(other, ctx),
            TypeRef::SubOf(v) => v.subof(other, ctx),
        }
    }

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        match self {
            TypeRef::Bound(v) => v.tagged_ptr(),
            TypeRef::Sequence(v) => v.tagged_ptr(),
            TypeRef::Float(v) => v.tagged_ptr(),
            TypeRef::FloatValue(v) => v.tagged_ptr(),
            TypeRef::Char(v) => v.tagged_ptr(),
            TypeRef::CharValue(v) => v.tagged_ptr(),
            TypeRef::Tuple(v) => v.tagged_ptr(),
            TypeRef::Any(v) => v.tagged_ptr(),
            TypeRef::All(v) => v.tagged_ptr(),
            TypeRef::FixPoint(v) => v.tagged_ptr(),
            TypeRef::Invoke(v) => v.tagged_ptr(),
            TypeRef::Variable(v) => v.tagged_ptr(),
            TypeRef::Closure(v) => v.tagged_ptr(),
            TypeRef::Opcode(v) => v.tagged_ptr(),
            TypeRef::Namespace(v) => v.tagged_ptr(),
            TypeRef::Pattern(v) => v.tagged_ptr(),
            TypeRef::Lazy(v) => v.tagged_ptr(),
            TypeRef::Rot(v) => v.tagged_ptr(),
            TypeRef::OrderedType(v) => v.tagged_ptr(),
            TypeRef::EqOf(v) => v.tagged_ptr(),
            TypeRef::SubOf(v) => v.tagged_ptr(),
        }
    }

    fn as_ref_dispatcher(&self) -> Self {
        *self
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        match self {
            TypeRef::Bound(v) => v.source_info(),
            TypeRef::Sequence(v) => v.source_info(),
            TypeRef::Float(v) => v.source_info(),
            TypeRef::FloatValue(v) => v.source_info(),
            TypeRef::Char(v) => v.source_info(),
            TypeRef::CharValue(v) => v.source_info(),
            TypeRef::Tuple(v) => v.source_info(),
            TypeRef::Any(v) => v.source_info(),
            TypeRef::All(v) => v.source_info(),
            TypeRef::FixPoint(v) => v.source_info(),
            TypeRef::Invoke(v) => v.source_info(),
            TypeRef::Variable(v) => v.source_info(),
            TypeRef::Closure(v) => v.source_info(),
            TypeRef::Opcode(v) => v.source_info(),
            TypeRef::Namespace(v) => v.source_info(),
            TypeRef::Pattern(v) => v.source_info(),
            TypeRef::Lazy(v) => v.source_info(),
            TypeRef::Rot(v) => v.source_info(),
            TypeRef::OrderedType(v) => v.source_info(),
            TypeRef::EqOf(v) => v.source_info(),
            TypeRef::SubOf(v) => v.source_info(),
        }
    }

    fn report_source_info(&self) -> TypeReport {
        match self {
            TypeRef::Bound(v) => v.report_source_info(),
            TypeRef::Sequence(v) => v.report_source_info(),
            TypeRef::Float(v) => v.report_source_info(),
            TypeRef::FloatValue(v) => v.report_source_info(),
            TypeRef::Char(v) => v.report_source_info(),
            TypeRef::CharValue(v) => v.report_source_info(),
            TypeRef::Tuple(v) => v.report_source_info(),
            TypeRef::Any(v) => v.report_source_info(),
            TypeRef::All(v) => v.report_source_info(),
            TypeRef::FixPoint(v) => v.report_source_info(),
            TypeRef::Invoke(v) => v.report_source_info(),
            TypeRef::Variable(v) => v.report_source_info(),
            TypeRef::Closure(v) => v.report_source_info(),
            TypeRef::Opcode(v) => v.report_source_info(),
            TypeRef::Namespace(v) => v.report_source_info(),
            TypeRef::Pattern(v) => v.report_source_info(),
            TypeRef::Lazy(v) => v.report_source_info(),
            TypeRef::Rot(v) => v.report_source_info(),
            TypeRef::OrderedType(v) => v.report_source_info(),
            TypeRef::EqOf(v) => v.report_source_info(),
            TypeRef::SubOf(v) => v.report_source_info(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> TypeRef<'_, T> {
    pub fn clone_data(self) -> Type<T> {
        match self {
            TypeRef::Bound(v) => Type::<T>::Bound(v.clone()),
            TypeRef::Sequence(v) => Type::<T>::Sequence(v.clone()),
            TypeRef::Float(v) => Type::<T>::Float(v.clone()),
            TypeRef::FloatValue(v) => Type::<T>::FloatValue(v.clone()),
            TypeRef::Char(v) => Type::<T>::Char(v.clone()),
            TypeRef::CharValue(v) => Type::<T>::CharValue(v.clone()),
            TypeRef::Tuple(v) => Type::<T>::Tuple(v.clone()),
            TypeRef::Any(v) => Type::<T>::Any(v.clone()),
            TypeRef::All(v) => Type::<T>::All(v.clone()),
            TypeRef::FixPoint(v) => Type::<T>::FixPoint(v.clone()),
            TypeRef::Invoke(v) => Type::<T>::Invoke(v.clone()),
            TypeRef::Variable(v) => Type::<T>::Variable(v.clone()),
            TypeRef::Closure(v) => Type::<T>::Closure(v.clone()),
            TypeRef::Opcode(v) => Type::<T>::Opcode(v.clone()),
            TypeRef::Namespace(v) => Type::<T>::Namespace(v.clone()),
            TypeRef::Pattern(v) => Type::<T>::Pattern(v.clone()),
            TypeRef::Lazy(v) => Type::<T>::Lazy(v.clone()),
            TypeRef::Rot(v) => Type::<T>::Rot(v.clone()),
            TypeRef::OrderedType(v) => Type::<T>::OrderedType(v.clone()),
            TypeRef::EqOf(v) => Type::<T>::EqOf(v.clone()),
            TypeRef::SubOf(v) => Type::<T>::SubOf(v.clone()),
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
    UnresolvableType(Box<str>),
    InfiniteRecursion,
    RedeclaredType,
    NonApplicableType(Box<U>),
    TupleIndexOutOfBounds(Box<(U, U)>),
    TypeMismatch(Box<(U, String)>),
    UnboundVariable(isize),
    UndefinedPatternVariable(isize),
    AssertFailed(Box<(U, U)>),
    MissingContinuation(Box<U>),
    MissingPerformHandler(Box<U>),
    RuntimeError(Arc<dyn Error + Send + Sync>),
    OtherError(Box<str>),
    Perform(Box<U>),
    Break(Box<U>),
    Resume(Box<U>),
    #[doc(hidden)]
    Pandom(std::marker::PhantomData<V>),
}

impl<U: CoinductiveType<U, V> + Debug, V: GcAllocObject<V>> std::fmt::Display for TypeError<U, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UnresolvableType(msg) => {
                write!(f, "Unresolvable type: {}", msg)
            }
            TypeError::InfiniteRecursion => write!(f, "Infinite recursion"),
            TypeError::RedeclaredType => write!(f, "Type redeclared"),
            TypeError::NonApplicableType(ty) => write!(f, "Non-applicable type: {:?}", ty),
            TypeError::TupleIndexOutOfBounds(types) => write!(
                f,
                "Tuple index out of bounds for types: {:?} and {:?}",
                types.0, types.1
            ),
            TypeError::TypeMismatch(info) => {
                write!(f, "Type mismatch: expected {}, found {:?}", info.1, info.0)
            }
            TypeError::UnboundVariable(id) => write!(f, "Unbound variable: id = {}", id),
            TypeError::UndefinedPatternVariable(id) => {
                write!(f, "Undefined pattern variable: id = {}", id)
            }
            TypeError::AssertFailed(types) => {
                write!(
                    f,
                    "Assert failed: {:?} doesn't accept {:?}",
                    types.0, types.1
                )
            }
            TypeError::MissingContinuation(ty) => write!(f, "Missing continuation: {:?}", ty),
            TypeError::MissingPerformHandler(ty) => write!(f, "Missing perform handler: {:?}", ty),
            TypeError::RuntimeError(err) => write!(f, "Runtime error: {}", err),
            TypeError::Perform(ty) => write!(f, "Perform raised: {:?}", ty),
            TypeError::Break(ty) => write!(f, "Break raised: {:?}", ty),
            TypeError::Resume(ty) => write!(f, "Resume raised: {:?}", ty),
            TypeError::OtherError(msg) => write!(f, "Other error: {}", msg),
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
                let repr_a = types.0.represent(&mut FastCycleDetector::new(), 0, 3);
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
            TypeError::Break(ty) => {
                let ty_repr = ty.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = ty.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Break raised: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new((filepath, span)).with_message("Break raised here"),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Break raised: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Break raised"),
                        )
                        .finish()
                }
            }
            TypeError::Resume(ty) => {
                let ty_repr = ty.represent(&mut FastCycleDetector::new(), 0, 3);
                if let Some(loc) = ty.source_info() {
                    let span =
                        byte_offset_span_to_char_span(loc.source().content(), loc.span().clone());
                    let filepath = loc.source().filepath().to_string();
                    let content = loc.source().content().to_string();
                    sources.push((filepath.clone(), content));

                    ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                        .with_message(format!("Resume raised: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new((filepath, span))
                                .with_message("Resume raised here"),
                        )
                        .finish()
                } else {
                    ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                        .with_message(format!("Resume raised: {}", ty_repr))
                        .with_label(
                            ariadne::Label::new(("<unknown>".to_string(), 0..0))
                                .with_message("Resume raised"),
                        )
                        .finish()
                }
            }
            // For errors without type information, create a generic report
            TypeError::UnresolvableType(msg) => {
                ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                    .with_message(format!("Unresolvable type: {}", msg))
                    .with_label(
                        ariadne::Label::new(("<unknown>".to_string(), 0..0))
                            .with_message(msg.as_ref()),
                    )
                    .finish()
            }
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
            TypeError::UnboundVariable(id) => {
                ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                    .with_message(format!("Unbound variable: id = {}", id))
                    .with_label(
                        ariadne::Label::new(("<unknown>".to_string(), 0..0))
                            .with_message(format!("Variable {} not found", id)),
                    )
                    .finish()
            }
            TypeError::UndefinedPatternVariable(id) => {
                ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                    .with_message(format!("Undefined pattern variable: id = {}", id))
                    .with_label(
                        ariadne::Label::new(("<unknown>".to_string(), 0..0))
                            .with_message(format!("Pattern variable {} not defined", id)),
                    )
                    .finish()
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
            TypeError::OtherError(msg) => {
                ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                    .with_message(format!("Error: {}", msg))
                    .with_label(
                        ariadne::Label::new(("<unknown>".to_string(), 0..0))
                            .with_message(msg.as_ref()),
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

macro_rules! type_dispatch {
    ($self:expr, $method:ident $(, $args:expr)*) => {
        match $self {
            Type::Bound(v) => v.$method($($args),*),
            Type::Sequence(v) => v.$method($($args),*),
            Type::Float(v) => v.$method($($args),*),
            Type::FloatValue(v) => v.$method($($args),*),
            Type::Tuple(v) => v.$method($($args),*),
            Type::Any(v) => v.$method($($args),*),
            Type::All(v) => v.$method($($args),*),
            Type::FixPoint(v) => v.$method($($args),*),
            Type::Invoke(v) => v.$method($($args),*),
            Type::Variable(v) => v.$method($($args),*),
            Type::Closure(v) => v.$method($($args),*),
            Type::Opcode(v) => v.$method($($args),*),
            Type::Char(v) => v.$method($($args),*),
            Type::CharValue(v) => v.$method($($args),*),
            Type::Namespace(v) => v.$method($($args),*),
            Type::Pattern(v) => v.$method($($args),*),
            Type::Lazy(v) => v.$method($($args),*),
            Type::Rot(v) => v.$method($($args),*),
            Type::OrderedType(v) => v.$method($($args),*),
            Type::EqOf(v) => v.$method($($args),*),
            Type::SubOf(v) => v.$method($($args),*),
        }
    };
}

pub trait GcAllocObject<T: GCTraceable<T> + 'static + Sized>:
    GCTraceable<T> + 'static + Sized
{
    type Inner: CoinductiveType<Self::Inner, T>
    where
        T: GcAllocObject<T>;
    fn new_placeholder() -> Self;

    fn get_value(&self) -> Option<&Self::Inner>
    where
        T: GcAllocObject<T>;

    fn set_value(&self, _value: Self::Inner) -> Result<(), TypeError<Self::Inner, T>>
    where
        T: GcAllocObject<T>;

    fn map_value<F, R>(&self, path: &mut FastCycleDetector<TaggedPtr<()>>, f: F) -> Option<R>
    where
        F: FnOnce(
            &mut FastCycleDetector<TaggedPtr<()>>,
            <Self::Inner as AsDispatcher<Self::Inner, T>>::RefDispatcher<'_>,
        ) -> R,
        T: GcAllocObject<T>,
    {
        self.get_value()
            .map(|inner| f(path, inner.as_ref_dispatcher()))
    }

    fn take_value<F, R>(&self, path: &mut FastCycleDetector<TaggedPtr<()>>, f: F) -> Option<R>
    where
        F: FnOnce(&mut FastCycleDetector<TaggedPtr<()>>, Self::Inner) -> R,
        T: GcAllocObject<T>;
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
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        type_dispatch!(self, reduce, ctx)
    }

    #[stacksafe::stacksafe]
    fn invoke(self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        type_dispatch!(self, invoke, ctx)
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
            Type::Bound(v) => TypeRef::Bound(v),
            Type::Sequence(v) => TypeRef::Sequence(v),
            Type::Float(v) => TypeRef::Float(v),
            Type::FloatValue(v) => TypeRef::FloatValue(v),
            Type::Char(v) => TypeRef::Char(v),
            Type::CharValue(v) => TypeRef::CharValue(v),
            Type::Tuple(v) => TypeRef::Tuple(v),
            Type::Any(v) => TypeRef::Any(v),
            Type::All(v) => TypeRef::All(v),
            Type::FixPoint(v) => TypeRef::FixPoint(v),
            Type::Invoke(v) => TypeRef::Invoke(v),
            Type::Variable(v) => TypeRef::Variable(v),
            Type::Closure(v) => TypeRef::Closure(v),
            Type::Opcode(v) => TypeRef::Opcode(v),
            Type::Namespace(v) => TypeRef::Namespace(v),
            Type::Pattern(v) => TypeRef::Pattern(v),
            Type::Lazy(v) => TypeRef::Lazy(v),
            Type::Rot(v) => TypeRef::Rot(v),
            Type::OrderedType(v) => TypeRef::OrderedType(v),
            Type::EqOf(v) => TypeRef::EqOf(v),
            Type::SubOf(v) => TypeRef::SubOf(v),
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
            Type::Sequence(v) => TypeRef::Sequence(v),
            Type::Float(v) => TypeRef::Float(v),
            Type::FloatValue(v) => TypeRef::FloatValue(v),
            Type::Char(v) => TypeRef::Char(v),
            Type::CharValue(v) => TypeRef::CharValue(v),
            Type::Tuple(v) => TypeRef::Tuple(v),
            Type::Any(v) => TypeRef::Any(v),
            Type::All(v) => TypeRef::All(v),
            Type::FixPoint(v) => TypeRef::FixPoint(v),
            Type::Invoke(v) => TypeRef::Invoke(v),
            Type::Variable(v) => TypeRef::Variable(v),
            Type::Closure(v) => TypeRef::Closure(v),
            Type::Opcode(v) => TypeRef::Opcode(v),
            Type::Namespace(v) => TypeRef::Namespace(v),
            Type::Pattern(v) => TypeRef::Pattern(v),
            Type::Lazy(v) => TypeRef::Lazy(v),
            Type::Rot(v) => TypeRef::Rot(v),
            Type::OrderedType(v) => TypeRef::OrderedType(v),
            Type::EqOf(v) => TypeRef::EqOf(v),
            Type::SubOf(v) => TypeRef::SubOf(v),
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
    pub fn new(ptr: *const T, tag: usize) -> Self {
        Self {
            ptr,
            tag,
            length: None,
        }
    }

    pub fn new_unique(ptr: *const T) -> Self {
        Self {
            ptr,
            tag: 0,
            length: None,
        }
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

/// 类型检查上下文，用于 `check` 方法
pub struct TypeCheckContext<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub assumptions: &'a mut SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
    pub closure_env: (&'a ClosureEnv<U, V>, &'a ClosureEnv<U, V>),
    pub pattern_env: &'a mut Collector<(usize, U)>,
    pub rhs: bool,
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> TypeCheckContext<'a, U, V> {
    pub fn new(
        assumptions: &'a mut SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&'a ClosureEnv<U, V>, &'a ClosureEnv<U, V>),
        pattern_env: &'a mut Collector<(usize, U)>,
        rhs: bool,
    ) -> Self {
        Self {
            assumptions,
            closure_env,
            pattern_env,
            rhs,
        }
    }
}

/// 归约上下文，用于 `reduce` 方法
pub struct ReductionContext<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub closure_env: &'a ClosureEnv<U, V>,
    pub param_env: &'a ParamEnv<U, V>,
    pub rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
    pub gc: &'a mut GC<V>,
    pub roots: &'roots mut RootStack<U, V>,
}

impl<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> ReductionContext<'a, 'roots, U, V> {
    pub fn new(
        closure_env: &'a ClosureEnv<U, V>,
        param_env: &'a ParamEnv<U, V>,
        rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
        gc: &'a mut GC<V>,
        roots: &'roots mut RootStack<U, V>,
    ) -> Self {
        Self {
            closure_env,
            param_env,
            rec_assumptions,
            gc,
            roots,
        }
    }
}

/// 类型应用上下文，用于 `invoke` 方法
pub struct InvokeContext<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub arg: U,
    pub closure_env: &'a ClosureEnv<U, V>,
    pub param_env: &'a ParamEnv<U, V>,
    pub rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
    pub gc: &'a mut GC<V>,
    pub roots: &'roots mut RootStack<U, V>,
    pub source_info: Option<&'a Arc<SourceLocation>>,
}

impl<'a, 'roots, U: CoinductiveType<U, V>, V: GcAllocObject<V>> InvokeContext<'a, 'roots, U, V> {
    pub fn new(
        arg: U,
        closure_env: &'a ClosureEnv<U, V>,
        param_env: &'a ParamEnv<U, V>,
        rec_assumptions: &'a mut SmallVec<[(TaggedPtr<()>, U, bool); 8]>,
        gc: &'a mut GC<V>,
        roots: &'roots mut RootStack<U, V>,
        source_info: Option<&'a Arc<SourceLocation>>,
    ) -> Self {
        Self {
            arg,
            closure_env,
            param_env,
            rec_assumptions,
            gc,
            roots,
            source_info,
        }
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

    // A <: B，验证类型图A是图B的特例（关键处理的是Generalize和Specialize）
    fn subof<'a>(
        &'a self,
        other: Self::RefDispatcher<'a>,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<ThreeValuedLogic, TypeError<U, V>>;

    // A == B，验证类型图A与图B等价
    fn equals<'a>(
        &'a self,
        other: Self::RefDispatcher<'a>,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<ThreeValuedLogic, TypeError<U, V>> {
        let mut rev_ctx = TypeCheckContext {
            assumptions: ctx.assumptions,
            closure_env: (ctx.closure_env.1, ctx.closure_env.0),
            pattern_env: ctx.pattern_env,
            rhs: !ctx.rhs,
        };
        let sub_ba = test_true!(other.subof(self.as_ref_dispatcher(), &mut rev_ctx)?);
        let sub_ab = test_true!(self.subof(other, ctx)?);
        Ok(sub_ab & sub_ba)
    }

    fn pure_equals<'a>(&'a self, other: Self::RefDispatcher<'a>) -> bool {
        let closure_env = ClosureEnv::new(Vec::<U>::new());
        match self.equals(
            other,
            &mut TypeCheckContext {
                assumptions: &mut SmallVec::new(),
                closure_env: (&closure_env, &closure_env),
                pattern_env: &mut Collector::new(),
                rhs: false,
            },
        ) {
            Ok(result) => result == ThreeValuedLogic::True,
            Err(_) => false,
        }
    }

    // 归约变换
    fn reduce(self, ctx: &mut ReductionContext<U, V>) -> Result<U, TypeError<U, V>>;

    // 类型应用
    fn invoke(self, ctx: InvokeContext<U, V>) -> Result<U, TypeError<U, V>>;

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

    // A <: B，验证类型图A是图B的特例（关键处理的是Generalize和Specialize）
    fn subof(
        &self,
        other: W,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<ThreeValuedLogic, TypeError<U, V>>;

    fn equals(
        &self,
        other: W,
        ctx: &mut TypeCheckContext<U, V>,
    ) -> Result<ThreeValuedLogic, TypeError<U, V>> {
        let mut rev_ctx = TypeCheckContext {
            assumptions: ctx.assumptions,
            closure_env: (ctx.closure_env.1, ctx.closure_env.0),
            pattern_env: ctx.pattern_env,
            rhs: !ctx.rhs,
        };

        let sub_ba = test_true!(other.subof(self.as_ref_dispatcher(), &mut rev_ctx)?);
        let sub_ab = test_true!(self.subof(other, ctx)?);
        Ok(sub_ab & sub_ba)
    }

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
