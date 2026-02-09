use std::{any::Any, sync::Arc};

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        InvokeContext, ReductionContext, Representable, Rootable, TaggedPtr, Type,
        TypeCheckContext, TypeError, TypeOfContext, TypeRef, subof::SubOf,
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub trait OpaqueValue<U: CoinductiveType<U, V>, V: GcAllocObject<V>>:
    Any + Send + Sync + GCTraceable<V> + Rootable<V> + Representable
{
    /// 尝试调用此不透明对象。
    ///
    /// 返回 Ok(result) 表示调用成功。
    /// 返回 Err(TypeError::NonApplicableType) 表示对象不可调用。
    /// 返回 Err(其他错误) 表示调用失败。
    fn invoke(&self, ctx: InvokeContext<U, V>) -> Result<U, TypeError<U, V>>;
}

pub struct OpaqueObject<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    object: Arc<dyn OpaqueValue<U, V>>,
    source_info: Option<Arc<SourceLocation>>,
}
impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for OpaqueObject<U, V> {
    fn clone(&self) -> Self {
        Self { object: self.object.clone(), source_info: self.source_info.clone() }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for OpaqueObject<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        self.object.collect(queue);
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for OpaqueObject<U, V> {
    fn upgrade(&self, collected: &mut Vec<arc_gc::arc::GCArc<V>>) {
        self.object.upgrade(collected);
    }

    fn rootless(&self) -> bool {
        false
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for OpaqueObject<Type<T>, T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::OpaqueObject(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::OpaqueObject(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T>
    for OpaqueObject<Type<T>, T>
{
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

                // 仅当两个OpaqueObject指向同一个对象时，才认为它们是子类型关系。
                TypeRef::OpaqueObject(v) => Ok(Arc::ptr_eq(&self.object, &v.object).into()),
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

    fn invoke(&self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        // 直接调用不透明对象
        self.object.invoke(ctx)
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
        if let Some(loc) = &self.source_info {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!(
                    "Opaque object {} at {}",
                    self.object.represent(&mut FastCycleDetector::new(), 0, 10),
                    filepath
                ))
                .with_label(ariadne::Label::new((filepath, span)).with_message(format!(
                    "Opaque object {} defined here",
                    self.object.represent(&mut FastCycleDetector::new(), 0, 10)
                )))
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message(format!(
                    "Opaque object {} has no source location",
                    self.object.represent(&mut FastCycleDetector::new(), 0, 10)
                ))
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for OpaqueObject<Type<T>, T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        format!("Opaque<{}>", self.object.represent(path, depth, max_depth))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> OpaqueObject<Type<T>, T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(
        object: impl OpaqueValue<Type<T>, T> + 'static,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        OpaqueObject { object: Arc::new(object), source_info }.dispatch()
    }

    pub fn value(&self) -> &dyn OpaqueValue<Type<T>, T> {
        self.object.as_ref()
    }
}
