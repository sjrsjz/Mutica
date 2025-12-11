use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef,
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub enum TypeBoundKind<T: GcAllocObject<T, Inner = Type<T>>> {
    Top,
    Bottom,
    PandomData(std::marker::PhantomData<T>),
}

pub struct TypeBound<T: GcAllocObject<T, Inner = Type<T>>> {
    pub kind: TypeBoundKind<T>,
    source_info: Option<Arc<SourceLocation>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for TypeBound<T> {
    fn clone(&self) -> Self {
        Self {
            kind: match &self.kind {
                TypeBoundKind::Top => TypeBoundKind::Top,
                TypeBoundKind::Bottom => TypeBoundKind::Bottom,
                TypeBoundKind::PandomData(_) => TypeBoundKind::PandomData(std::marker::PhantomData),
            },
            source_info: self.source_info.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for TypeBound<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for TypeBound<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for TypeBound<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Bound(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Bound(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for TypeBound<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match other {
                // 这些都是规则变换类型，他们必须被优先处理
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => match &self.kind {
                    TypeBoundKind::Top => match other {
                        TypeRef::Bound(v) => match &v.kind {
                            TypeBoundKind::Top => Ok(ThreeValuedLogic::True),
                            _ => Ok(ThreeValuedLogic::False),
                        },
                        _ => Ok(ThreeValuedLogic::False),
                    },
                    TypeBoundKind::Bottom => Ok(ThreeValuedLogic::True), // ⊥ 可以满足任何类型
                    TypeBoundKind::PandomData(_) => Ok(ThreeValuedLogic::False),
                },
            }
        })
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match other {
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                _ => match &self.kind {
                    TypeBoundKind::Bottom => Ok(ThreeValuedLogic::True), // ⊥ 是所有类型的子类型
                    TypeBoundKind::Top => match other {
                        TypeRef::Bound(v) => match &v.kind {
                            TypeBoundKind::Top => Ok(ThreeValuedLogic::True),
                            _ => Ok(ThreeValuedLogic::False),
                        },
                        _ => Ok(ThreeValuedLogic::False),
                    },
                    TypeBoundKind::PandomData(_) => Ok(ThreeValuedLogic::False),
                },
            }
        })
    }

    fn reduce(
        self,
        _ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(self.dispatch())
    }

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = &self.source_info {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Type bound at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Type bound defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Type bound has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for TypeBound<T> {
    fn represent(
        &self,
        _path: &mut FastCycleDetector<TaggedPtr<()>>,
        _depth: usize,
        _max_depth: usize,
    ) -> String {
        match &self.kind {
            TypeBoundKind::Top => "⊤".to_string(),
            TypeBoundKind::Bottom => "⊥".to_string(),
            TypeBoundKind::PandomData(_) => "<?>".to_string(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> TypeBound<T> {
    pub fn top(source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        Self { kind: TypeBoundKind::Top, source_info }.dispatch()
    }

    pub fn bottom(source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        Self { kind: TypeBoundKind::Bottom, source_info }.dispatch()
    }
}
