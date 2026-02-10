use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError, TypeOfContext,
        TypeRef,
    },
    util::{source_info::SourceLocation, three_valued_logic::ThreeValuedLogic},
};

use crate::types::CoinductiveTypeRef;

pub struct SubOf<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    expr: Arc<U>,
    source_info: Option<Arc<SourceLocation>>,
    rootless: bool,
    _phantom: std::marker::PhantomData<V>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for SubOf<U, V> {
    fn clone(&self) -> Self {
        Self {
            expr: self.expr.clone(),
            rootless: self.rootless,
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for SubOf<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        if self.rootless {
            return;
        }
        self.expr.collect(queue);
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for SubOf<U, V> {
    fn upgrade(&self, collected: &mut Vec<GCArc<V>>) {
        if self.rootless {
            return;
        }
        self.expr.upgrade(collected);
    }

    fn rootless(&self) -> bool {
        self.rootless
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for SubOf<Type<T>, T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        format!("Sub<{}>", self.expr.represent(path, depth + 1, max_depth))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for SubOf<Type<T>, T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for SubOf<Type<T>, T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
            );
            match other {
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

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
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::SubOf(v) => {
                    // SubOf<A> 是 SubOf<B> 的子类型当且仅当 A 是 B 的子类型
                    self.expr.subof(v.expr.as_ref_dispatcher(), &mut inner_ctx)
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        &self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let new_expr = self.expr.reduce(ctx)?;
        let rootless = new_expr.rootless();
        Ok(SubOf {
            expr: Arc::new(new_expr),
            rootless,
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
        .dispatch())
    }

    fn invoke(
        &self,
        _ctx: super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn type_of(
        &self,
        _ctx: &mut TypeOfContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(SubOf::new(self.clone(), self.source_info.clone()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.source_info() {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T>
    for SubOf<Type<T>, T>
{
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        other.subof(self.expr.as_ref_dispatcher(), ctx)
    }

    fn superof(
        &self,
        _other: Self::RefDispatcher<'_>,
        _ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        panic!("SubOf::superof should never be called");
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> SubOf<Type<T>, T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<X: AsDispatcher<Type<T>, T>>(
        value: X,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let expr = value.into_dispatcher();
        let rootless = expr.rootless();
        Self { expr: Arc::new(expr), rootless, source_info, _phantom: std::marker::PhantomData }
            .into_dispatcher()
    }

    pub fn value(&self) -> &Type<T> {
        &self.expr
    }
}
