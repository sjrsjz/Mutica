use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef,
    },
    util::{
        arc_opt::ArcOpt, cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub struct Namespace<T: GcAllocObject<T, Inner = Type<T>>> {
    #[allow(clippy::type_complexity)]
    inner: ArcOpt<(Arc<str>, Type<T>, Option<Arc<SourceLocation>>)>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Namespace<T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Namespace<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        let (_, expr, _) = self.inner.as_ref();
        expr.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Namespace<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        let (_, expr, _) = self.inner.as_ref();
        expr.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Namespace<T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        let (tag, expr, _) = self.inner.as_ref();
        format!("{}::{}", tag, expr.represent(path, depth + 1, max_depth))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Namespace<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Namespace(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Namespace(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Namespace<T> {
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
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
                TypeRef::Namespace(v) => {
                    let (self_tag, self_expr, _) = self.inner.as_ref();
                    let (v_tag, v_expr, _) = v.inner.as_ref();
                    if self_tag == v_tag {
                        self_expr.check(v_expr.as_ref_dispatcher(), &mut inner_ctx)
                    } else {
                        Ok(ThreeValuedLogic::False)
                    }
                }
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
                TypeRef::Constraint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
                TypeRef::Namespace(v) => {
                    let (self_tag, self_expr, _) = self.inner.as_ref();
                    let (v_tag, v_expr, _) = v.inner.as_ref();
                    if self_tag == v_tag {
                        self_expr.subof(v_expr.as_ref_dispatcher(), &mut inner_ctx)
                    } else {
                        Ok(ThreeValuedLogic::False)
                    }
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.inner.modify(|(tag, expr, source_info)| {
            let new_expr = expr.reduce(ctx)?;
            Ok((tag, new_expr, source_info))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (tag, expr, source_info) = self.inner.as_ref();
                let new_expr = expr.clone().reduce(ctx)?;
                Ok(Self::new(tag.clone(), new_expr, source_info.clone()))
            }
        }
    }

    fn invoke(self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.inner.take() {
            Ok((_, expr, _)) => expr.invoke(ctx),
            Err(v) => {
                let (_, expr, _) = v.as_ref();
                expr.clone().invoke(ctx)
            }
        }
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.inner.as_ref().2.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.inner.as_ref().2.as_ref() {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            let ns_name = &self.inner.as_ref().0;
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Namespace '{}' at {}", ns_name, filepath))
                .with_label(
                    ariadne::Label::new((filepath, span))
                        .with_message(format!("Namespace '{}' defined here", ns_name)),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Namespace has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Namespace<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<I: AsDispatcher<Type<T>, T>, S: Into<Arc<str>>>(
        tag: S,
        expr: I,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        Self { inner: ArcOpt::new((tag.into(), expr.into_dispatcher(), source_info)) }.dispatch()
    }

    pub fn expr(&self) -> &Type<T> {
        &self.inner.as_ref().1
    }

    pub fn tag(&self) -> &str {
        &self.inner.as_ref().0
    }
}
