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
pub struct CharacterValue {
    value: char,
}

impl GCTraceable<FixPointInner> for CharacterValue {
    fn collect(
        &self,
        _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
    }
}

impl Rootable for CharacterValue {}

impl CoinductiveType<Type, StabilizedType> for CharacterValue {
    fn dispatch(self) -> Type {
        Type::CharValue(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.pattern_mode);
            match other {
                Type::CharValue(v) => {
                    if self.value == v.value {
                        Ok(Some(()))
                    } else {
                        Ok(None)
                    }
                }
                Type::Bound(TypeBound::Top) => Ok(Some(())),
                Type::Char(_) => Ok(Some(())),
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

impl Representable for CharacterValue {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{:?}", self.value)
    }

    fn display(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{}", self.value)
    }
}

impl CharacterValue {
    pub fn new(value: char) -> StabilizedType {
        Self { value }.dispatch().stabilize()
    }

    pub fn value(&self) -> char {
        self.value
    }
}
