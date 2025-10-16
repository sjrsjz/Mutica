use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::types::{
    AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, Representable, Rootable,
    Type, TypeCheckContext, TypeRef, type_bound::TypeBound,
};

pub struct Lazy<T: GcAllocObject<T>> {
    value: Arc<Type<T>>,
    is_nf: bool,
}

impl<T: GcAllocObject<T>> Clone for Lazy<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            is_nf: self.is_nf,
        }
    }
}

impl<T: GcAllocObject<T>> GCTraceable<T> for Lazy<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.value.collect(queue);
    }
}

impl<T: GcAllocObject<T>> GcAllocObject<T> for Lazy<T> {}

impl<T: GcAllocObject<T>> Rootable<T> for Lazy<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.value.upgrade(collected);
    }
}

impl<T: GcAllocObject<T>> Representable for Lazy<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<*const ()>,
    ) -> String {
        format!("Lazy<{}>", self.value.represent(path))
    }
}

impl<T: GcAllocObject<T>> AsDispatcher<Type<T>, T> for Lazy<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Lazy(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Lazy(self)
    }
}

impl<T: GcAllocObject<T>> CoinductiveType<Type<T>, T> for Lazy<T> {
    fn is(
        &self,
        other: &Type<T>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, super::TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                Type::Lazy(v) => self.value.is(&v.value, &mut inner_ctx),
                Type::Bound(TypeBound::Top) => Ok(Some(())),
                Type::Generalize(v) => v.has(self, &mut inner_ctx),
                Type::Specialize(v) => v.has(self, &mut inner_ctx),
                Type::FixPoint(v) => v.has(self, &mut inner_ctx),
                Type::Pattern(v) => v.has(self, &mut inner_ctx),
                Type::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        self.value.as_ref().clone().reduce(ctx).map(Self::new)
    }

    fn invoke(
        &self,
        _ctx: &mut super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        Err(super::TypeError::NonApplicableType(
            self.clone().dispatch().into(),
        ))
    }

    fn is_normal_form(&self) -> bool {
        self.is_nf
    }
}

impl<T: GcAllocObject<T>> Lazy<T> {
    pub fn new<X: AsDispatcher<Type<T>, T>>(value: X) -> Type<T> {
        let value = value.into_dispatcher();
        let is_nf = value.is_normal_form();
        Self {
            value: Arc::new(value),
            is_nf,
        }
        .dispatch()
    }
}
