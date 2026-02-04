use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeRef, CoinductiveTypeWithAny, CollectorExt,
        GcAllocObject, InvokeContext, PatternCollector, ReductionContext, Representable, Rootable,
        TaggedPtr, Type, TypeCheckContext, TypeError, TypeRef,
    },
    util::{arc_opt::ArcOpt, source_info::SourceLocation, three_valued_logic::ThreeValuedLogic},
};

pub struct Pattern<T: GcAllocObject<T, Inner = Type<T>>> {
    bind_name: Arc<str>,
    expr: ArcOpt<Type<T>>,
    source_info: Option<Arc<SourceLocation>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Pattern<T> {
    fn clone(&self) -> Self {
        Self {
            bind_name: self.bind_name.clone(),
            expr: self.expr.clone(),
            source_info: self.source_info.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Pattern<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.expr.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Pattern<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.expr.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Pattern<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        format!("T<{}>.{}", self.expr.represent(path, depth + 1, max_depth), self.bind_name)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Pattern<T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Pattern<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.instance_assumptions,
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
                ctx.instance_assumptions,
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

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.into_dispatcher().into()))
    }

    fn reduce(
        mut self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.expr.modify(|value| {
            let new_value = value.reduce(ctx)?;
            Ok(new_value)
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let value = self.expr.as_ref();
                let new_value = value.clone().reduce(ctx)?;
                Ok(Self::new(self.bind_name.clone(), new_value, self.source_info.clone()))
            }
        }
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for Pattern<T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Pattern<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<S: Into<Arc<str>>, X: AsDispatcher<Type<T>, T>>(
        bind_name: S,
        expr: X,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        Self { bind_name: bind_name.into(), expr: ArcOpt::new(expr.into_dispatcher()), source_info }
            .dispatch()
    }

    pub fn bind_name(&self) -> &Arc<str> {
        &self.bind_name
    }
}
