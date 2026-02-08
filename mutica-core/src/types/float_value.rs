use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        InvokeContext, ReductionContext, Representable, Rootable, TaggedPtr, Type,
        TypeCheckContext, TypeError, TypeRef,
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub struct FloatValue<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    value: f64,
    source_info: Option<Arc<SourceLocation>>,
    _phantom: std::marker::PhantomData<(U, V)>,
}
impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for FloatValue<U, V> {
    fn clone(&self) -> Self {
        Self {
            value: self.value,
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for FloatValue<U, V> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {}
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for FloatValue<U, V> {
    fn rootless(&self) -> bool {
        true
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for FloatValue<Type<T>, T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::FloatValue(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::FloatValue(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for FloatValue<Type<T>, T> {
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
                
            );
            match other {
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Float(_) => Ok(ThreeValuedLogic::True),
                TypeRef::FloatValue(v) => Ok((self.value == v.value).into()),
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

                // 由于FloatValue和Float是不同的类型类，它们没有子类型关系，因此这里不进行特判，直接返回False
                TypeRef::FloatValue(v) => Ok((self.value == v.value).into()),
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        &self,
        _ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(self.clone().dispatch())
    }

    fn invoke(&self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
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
                .with_message(format!("Float value {} at {}", self.value, filepath))
                .with_label(
                    ariadne::Label::new((filepath, span))
                        .with_message(format!("Float value {} defined here", self.value)),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message(format!("Float value {} has no source location", self.value))
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for FloatValue<Type<T>, T> {
    fn represent(
        &self,
        _path: &mut FastCycleDetector<TaggedPtr<()>>,
        _depth: usize,
        _max_depth: usize,
    ) -> String {
        format!("{}", self.value)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> FloatValue<Type<T>, T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(value: f64, source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        FloatValue { value, source_info, _phantom: std::marker::PhantomData }.dispatch()
    }

    pub fn value(&self) -> f64 {
        self.value
    }
}
