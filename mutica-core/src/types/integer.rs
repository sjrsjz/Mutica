use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext, Representable,
        Rootable, StabilizedType, Type, TypeCheckContext, TypeError, fixpoint::FixPointInner,
        type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

#[derive(Clone)]
pub struct Integer {}

impl GCTraceable<FixPointInner> for Integer {
    fn collect(
        &self,
        _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
    }
}

impl Rootable for Integer {}

impl CoinductiveType<Type, StabilizedType> for Integer {
    fn dispatch(self) -> Type {
        Type::Integer(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, super::TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
            match other {
                Type::Integer(_) => Ok(Some(())),
                Type::Bound(TypeBound::Top) => Ok(Some(())),
                Type::Specialize(v) => v.has(self, &mut inner_ctx),
                Type::Generalize(v) => v.has(self, &mut inner_ctx),
                Type::FixPoint(v) => v.has(self, &mut inner_ctx),
                Type::Pattern(v) => v.has(self, &mut inner_ctx),
                Type::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(&self, _ctx: &mut ReductionContext) -> Result<StabilizedType, TypeError> {
        Ok(self.clone().dispatch().stabilize())
    }

    fn invoke(&self, _ctx: &mut InvokeContext) -> Result<StabilizedType, TypeError> {
        Err(TypeError::NonApplicableType(
            self.clone().dispatch().stabilize().into(),
        ))
    }
}

impl Representable for Integer {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        "Integer".to_string()
    }
}

impl Integer {
    pub fn new() -> StabilizedType {
        Self {}.dispatch().stabilize()
    }
}
