use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::types::{
    AsTypeRef, CoinductiveType, CoinductiveTypeWithAny, Representable, Rootable, StabilizedType,
    Type, TypeCheckContext, fixpoint::FixPointInner, type_bound::TypeBound,
};

#[derive(Clone)]
pub struct Lazy {
    value: Arc<Type>,
}

impl GCTraceable<FixPointInner> for Lazy {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        self.value.collect(queue);
    }
}

impl Rootable for Lazy {
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
        self.value.upgrade(collected);
    }
}

impl Representable for Lazy {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<*const ()>,
    ) -> String {
        format!("Lazy<{}>", self.value.represent(path))
    }
}
impl CoinductiveType<Type, StabilizedType> for Lazy {
    fn dispatch(self) -> Type {
        Type::Lazy(self)
    }

    fn is(
        &self,
        other: &Type,
        ctx: &mut super::TypeCheckContext,
    ) -> Result<Option<()>, super::TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
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
        &self,
        ctx: &mut super::ReductionContext,
    ) -> Result<StabilizedType, super::TypeError> {
        self.value.reduce(ctx).map(Self::new)
    }

    fn invoke(&self, _ctx: &mut super::InvokeContext) -> Result<StabilizedType, super::TypeError> {
        Err(super::TypeError::NonApplicableType(
            self.clone().dispatch().stabilize().into(),
        ))
    }
}

impl Lazy {
    pub fn new<T: AsTypeRef>(value: T) -> StabilizedType {
        Self {
            value: Arc::new(value.into_type()),
        }
        .dispatch()
        .stabilize()
    }
}
