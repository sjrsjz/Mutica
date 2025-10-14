use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::types::{
    AsType, CoinductiveType, CoinductiveTypeWithAny, Representable, Rootable, Type,
    TypeCheckContext, TypeEnum, fixpoint::FixPointInner, type_bound::TypeBound,
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
    fn upgrade(&self, collected: &mut Vec<GCArc<FixPointInner>>) {
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
impl CoinductiveType<Type> for Lazy {
    fn dispatch(self) -> Type {
        Type::new(TypeEnum::Lazy(self))
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
            match &other.ty {
                TypeEnum::Lazy(v) => self.value.is(&v.value, &mut inner_ctx),
                TypeEnum::Bound(TypeBound::Top) => Ok(Some(())),
                TypeEnum::Generalize(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Specialize(v) => v.has(self, &mut inner_ctx),
                TypeEnum::FixPoint(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Pattern(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(self, ctx: &mut super::ReductionContext) -> Result<Type, super::TypeError> {
        self.value.as_ref().clone().reduce(ctx).map(Self::new)
    }

    fn invoke(&self, _ctx: &mut super::InvokeContext) -> Result<Type, super::TypeError> {
        Err(super::TypeError::NonApplicableType(
            self.clone().dispatch().into(),
        ))
    }
}

impl Lazy {
    pub fn new<T: AsType>(value: T) -> Type {
        let value = value.into_type();
        let ty = Self {
            value: Arc::new(value),
        };
        if ty.value.is_nf() {
            ty.dispatch_nf()
        } else {
            ty.dispatch()
        }
    }

    fn dispatch_nf(self) -> Type {
        Type::new_nf(TypeEnum::Lazy(self))
    }
}
