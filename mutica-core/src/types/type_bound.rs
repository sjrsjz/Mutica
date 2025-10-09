use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, Representable, TypeCheckContext, ReductionContext, InvokeContext, Rootable, StabilizedType,
        Type, TypeError,
        
        fixpoint::FixPointInner,
    },
    util::cycle_detector::FastCycleDetector,
};

#[derive(Clone)]
pub enum TypeBound {
    Top,
    Bottom,
}

impl GCTraceable<FixPointInner> for TypeBound {
    fn collect(
        &self,
        _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
    }
}

impl Rootable for TypeBound {}

impl CoinductiveType<Type, StabilizedType> for TypeBound {
    fn is(
        &self,
        other: &Type,
        ctx: &mut TypeCheckContext,
    ) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.pattern_mode);
            if let Type::Pattern(p) = other {
                return p.has(self, &mut inner_ctx);
            }
            match (self, other) {
                (TypeBound::Bottom, _) => Ok(Some(())),
                (TypeBound::Top, Type::Bound(TypeBound::Top)) => Ok(Some(())),
                _ => Ok(None),
            }
        })
    }

    fn dispatch(self) -> Type {
        Type::Bound(self)
    }

    fn reduce(
        &self,
        _ctx: &mut ReductionContext,
    ) -> Result<StabilizedType, TypeError> {
        Ok(self.clone().dispatch().stabilize())
    }

    fn invoke(
        &self,
        _ctx: &mut InvokeContext,
    ) -> Result<StabilizedType, TypeError> {
        Err(TypeError::NonApplicableType(
            self.clone().dispatch().stabilize().into(),
        ))
    }
}

impl Representable for TypeBound {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        match self {
            TypeBound::Top => "⊤".to_string(),
            TypeBound::Bottom => "⊥".to_string(),
        }
    }
    fn display(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        match self {
            TypeBound::Top => "true".to_string(),
            TypeBound::Bottom => "false".to_string(),
        }
    }
}

impl TypeBound {
    pub fn top() -> StabilizedType {
        Self::Top.dispatch().stabilize()
    }

    pub fn bottom() -> StabilizedType {
        Self::Bottom.dispatch().stabilize()
    }
}
