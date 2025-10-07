use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, Representable, TypeCheckContext, ReductionContext, InvokeContext, Rootable, StabilizedType,
        Type, TypeError,
        fixpoint::FixPointInner,
        type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

#[derive(Clone)]
pub struct Character {}

impl GCTraceable<FixPointInner> for Character {
    fn collect(
        &self,
        _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
    }
}

impl Rootable for Character {}

impl CoinductiveType<Type, StabilizedType> for Character {
    fn dispatch(self) -> Type {
        Type::Char(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, super::TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.pattern_mode);
            match other {
                Type::Char(_) => Ok(Some(())),
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

impl Representable for Character {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        "Char".to_string()
    }
}

impl Character {
    pub fn new() -> StabilizedType {
        Self {}.dispatch().stabilize()
    }
}
