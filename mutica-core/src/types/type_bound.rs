use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, Representable, TypeCheckContext, ReductionContext, InvokeContext, Rootable,
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

impl CoinductiveType<Type> for TypeBound {
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
    ) -> Result<Type, TypeError> {
        Ok(self.clone().dispatch())
    }

    fn invoke(
        &self,
        _ctx: &mut InvokeContext,
    ) -> Result<Type, TypeError> {
        Err(TypeError::NonApplicableType(
            self.clone().dispatch().into(),
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
    pub fn top() -> Type {
        Self::Top.dispatch()
    }

    pub fn bottom() -> Type {
        Self::Bottom.dispatch()
    }
}
