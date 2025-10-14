use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext, Representable,
        Rootable, Type, TypeCheckContext, TypeError, TypeEnum, fixpoint::FixPointInner,
        integer_value::IntegerValue, type_bound::TypeBound,
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

impl CoinductiveType<Type> for Integer {
    fn dispatch(self) -> Type {
        Type::new_nf(TypeEnum::Integer(self))
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, super::TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
            match &other.ty {
                TypeEnum::Integer(_) => Ok(Some(())),
                TypeEnum::Bound(TypeBound::Top) => Ok(Some(())),
                TypeEnum::Specialize(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Generalize(v) => v.has(self, &mut inner_ctx),
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

    fn invoke(&self, ctx: &mut InvokeContext) -> Result<Type, TypeError> {
        ctx.arg
            .map(&mut FastCycleDetector::new(), |_, arg| match &arg.ty {
                TypeEnum::IntegerValue(_) => Ok(arg.clone()),
                TypeEnum::CharValue(c) => Ok(IntegerValue::new(c.value() as i64)),
                _ => Err(super::TypeError::TypeMismatch(
                    (ctx.arg.clone(), "IntegerValue or CharValue".into()).into(),
                )),
            })?
    }
}

impl Representable for Integer {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        "Integer".to_string()
    }
}

impl Integer {
    pub fn new() -> Type {
        Self {}.dispatch()
    }
}
