use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use arena_arc::ArcSingle;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeRef, CoinductiveTypeWithAny, CollectorExt,
        GcAllocObject, InvokeContext, PatternCollector, ReductionContext, Representable, Rootable,
        TaggedPtr, Type, TypeCheckContext, TypeError, TypeRef, allocator::Allocators,
    },
    util::{source_info::SourceLocation, three_valued_logic::ThreeValuedLogic},
};

pub struct Pattern<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    bind_name: Arc<str>,
    expr: ArcSingle<U, usize>,
    rootless: bool,
    source_info: Option<Arc<SourceLocation>>,
    _phantom: std::marker::PhantomData<V>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for Pattern<U, V> {
    fn clone(&self) -> Self {
        Self {
            bind_name: self.bind_name.clone(),
            expr: self.expr.clone(),
            rootless: self.rootless,
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for Pattern<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        if self.rootless {
            return;
        }
        self.expr.collect(queue);
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for Pattern<U, V> {
    fn upgrade(&self, collected: &mut Vec<GCArc<V>>) {
        if self.rootless {
            return;
        }
        self.expr.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Pattern<Type<T>, T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        format!("T<{}>.{}", self.expr.represent(path, depth + 1, max_depth), self.bind_name)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Pattern<Type<T>, T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Pattern(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Pattern(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Pattern<Type<T>, T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
                ctx.allocators,
            );
            match other {
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                // Pattern 无法直接匹配其他类型
                _ => Ok(ThreeValuedLogic::False),
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
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
                ctx.allocators,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => {
                    if let PatternCollector::Subtyping(c) = &mut inner_ctx.pattern_collector {
                        c.push_single((self.bind_name.clone(), v.bind_name.clone())); // 记录绑定关系
                        self.expr.subof(v.expr.as_ref_dispatcher(), &mut inner_ctx)
                    } else {
                        Ok(ThreeValuedLogic::False)
                    }
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn invoke(&self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn reduce(
        &self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let new_expr = self.expr.reduce(ctx)?;
        let rootless = new_expr.rootless();
        Ok(Self {
            bind_name: self.bind_name.clone(),
            expr: ctx.allocators.v.alloc_value(new_expr),
            rootless,
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
        .dispatch())
    }

    fn tagged_ptr(&self) -> super::TaggedPtr<()> {
        super::TaggedPtr::new_unique(&self as *const _ as *const ())
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.source_info.as_ref() {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Pattern type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Pattern defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Pattern type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T>
    for Pattern<Type<T>, T>
{
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        if let PatternCollector::Deconstruct(pattern_env) = &mut ctx.pattern_collector {
            pattern_env.push((self.bind_name.clone(), other.clone_data()));
            other.check(self.expr.as_ref_dispatcher(), ctx)
        } else {
            Ok(ThreeValuedLogic::False)
        }
    }

    #[stacksafe::stacksafe]
    fn superof(
        &self,
        _other: Self::RefDispatcher<'_>,
        _ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        panic!("Pattern::superof should not be called directly")
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Pattern<Type<T>, T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<S: Into<Arc<str>>, X: AsDispatcher<Type<T>, T>>(
        bind_name: S,
        expr: X,
        allocators: &mut Allocators<Type<T>, T>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let expr = expr.into_dispatcher();
        let rootless = expr.rootless();
        Self {
            bind_name: bind_name.into(),
            expr: allocators.v.alloc_value(expr),
            rootless,
            source_info,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }

    pub fn bind_name(&self) -> &Arc<str> {
        &self.bind_name
    }
}
