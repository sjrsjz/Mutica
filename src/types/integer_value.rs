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
pub struct IntegerValue {
    value: isize,
}

impl GCTraceable<FixPointInner> for IntegerValue {
    fn collect(
        &self,
        _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
    }
}

impl Rootable for IntegerValue {}

impl CoinductiveType<Type, StabilizedType> for IntegerValue {
    fn dispatch(self) -> Type {
        Type::IntegerValue(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.pattern_mode);
            match other {
                Type::IntegerValue(v) => {
                    if self.value == v.value {
                        Ok(Some(()))
                    } else {
                        Ok(None)
                    }
                }
                Type::Bound(TypeBound::Top) => Ok(Some(())),
                Type::Integer(_) => Ok(Some(())),
                Type::Generalize(v) => v.has(self, &mut inner_ctx),
                Type::Specialize(v) => v.has(self, &mut inner_ctx),
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

impl Representable for IntegerValue {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{}", self.value)
    }
}

impl IntegerValue {
    pub fn new(value: isize) -> StabilizedType {
        Self { value }.dispatch().stabilize()
    }

    pub fn value(&self) -> isize {
        self.value
    }
}
