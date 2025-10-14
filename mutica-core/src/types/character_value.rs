use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext, Representable,
        Rootable, Type, TypeCheckContext, TypeEnum, TypeError, fixpoint::FixPointInner,
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

impl CoinductiveType<Type> for CharacterValue {
    fn dispatch(self) -> Type {
        Type::new_nf(TypeEnum::CharValue(self))
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
                TypeEnum::CharValue(v) => {
                    if self.value == v.value {
                        Ok(Some(()))
                    } else {
                        Ok(None)
                    }
                }
                TypeEnum::Bound(TypeBound::Top) => Ok(Some(())),
                TypeEnum::Char(_) => Ok(Some(())),
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

impl Representable for CharacterValue {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{:?}", self.value)
    }

    fn display(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{}", self.value)
    }
}

impl CharacterValue {
    pub fn new(value: char) -> Type {
        Self { value }.dispatch()
    }

    pub fn value(&self) -> char {
        self.value
    }
}
