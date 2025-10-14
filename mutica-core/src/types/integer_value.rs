use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext, Representable,
        Rootable, Type, TypeCheckContext, TypeError, TypeEnum, fixpoint::FixPointInner,
        type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

#[derive(Clone)]
pub struct IntegerValue {
    value: i64,
}

impl GCTraceable<FixPointInner> for IntegerValue {
    fn collect(
        &self,
        _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
    }
}

impl Rootable for IntegerValue {}

impl CoinductiveType<Type> for IntegerValue {
    fn dispatch(self) -> Type {
        Type::new_nf(TypeEnum::IntegerValue(self))
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
            match &other.ty {
                TypeEnum::IntegerValue(v) => {
                    if self.value == v.value {
                        Ok(Some(()))
                    } else {
                        Ok(None)
                    }
                }
                TypeEnum::Bound(TypeBound::Top) => Ok(Some(())),
                TypeEnum::Integer(_) => Ok(Some(())),
                TypeEnum::Generalize(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Specialize(v) => v.has(self, &mut inner_ctx),
                TypeEnum::FixPoint(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Pattern(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(self, _ctx: &mut ReductionContext) -> Result<Type, TypeError> {
        Ok(self.dispatch())
    }

    fn invoke(&self, _ctx: &mut InvokeContext) -> Result<Type, TypeError> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }
}

impl Representable for IntegerValue {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{}", self.value)
    }
}

impl IntegerValue {
    pub fn new(value: i64) -> Type {
        Self { value }.dispatch()
    }

    pub fn value(&self) -> i64 {
        self.value
    }
}
