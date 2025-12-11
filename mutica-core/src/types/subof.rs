use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, Representable,
        Rootable, TaggedPtr, Type, TypeCheckContext, TypeError, TypeRef,
    },
    util::{arc_opt::ArcOpt, source_info::SourceLocation, three_valued_logic::ThreeValuedLogic},
};

use crate::types::CoinductiveTypeRef;

pub struct SubOf<T: GcAllocObject<T, Inner = Type<T>>> {
    #[allow(clippy::type_complexity)]
    inner: ArcOpt<(Type<T>, Option<Arc<SourceLocation>>)>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for SubOf<T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for SubOf<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        let (value, _) = self.inner.as_ref();
        value.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for SubOf<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        let (value, _) = self.inner.as_ref();
        value.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for SubOf<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        let (value, _) = self.inner.as_ref();
        format!("Sub<{}>", value.represent(path, depth + 1, max_depth))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for SubOf<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::SubOf(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::SubOf(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for SubOf<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
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
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
                // 我们实际上无法找到SubOf所归属的任何类型类，因此只能简单地拒绝其他类型。
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
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
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
                TypeRef::SubOf(v) => {
                    let (self_value, _) = self.inner.as_ref();
                    let (v_value, _) = v.inner.as_ref();
                    self_value.subof(v_value.as_ref_dispatcher(), &mut inner_ctx)
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.inner.modify(|(value, source_info)| {
            let new_value = value.reduce(ctx)?;
            Ok((new_value, source_info))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (value, source_info) = self.inner.as_ref();
                let new_value = value.clone().reduce(ctx)?;
                Ok(Self::new(new_value, source_info.clone()))
            }
        }
    }

    fn invoke(
        self,
        _ctx: super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.dispatch().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.inner.as_ref().1.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.inner.as_ref().1.as_ref() {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Subtype constraint at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Sub<...> defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Subtype constraint has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for SubOf<T> {
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        let (value, _) = self.inner.as_ref();
        other.subof(value.as_ref_dispatcher(), ctx)
    }

    fn superof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        match other {
            TypeRef::SubOf(v) => {
                let (self_value, _) = self.inner.as_ref();
                let (v_value, _) = v.inner.as_ref();
                self_value.subof(v_value.as_ref_dispatcher(), ctx)
            }
            _ => other.subof(self.as_ref_dispatcher(), ctx),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> SubOf<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<X: AsDispatcher<Type<T>, T>>(
        value: X,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        Self { inner: ArcOpt::new((value.into_dispatcher(), source_info)) }.dispatch()
    }

    pub fn value(&self) -> &Type<T> {
        &self.inner.as_ref().0
    }
}
