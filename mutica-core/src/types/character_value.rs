use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext, Representable,
        Rootable, Type, TypeCheckContext, TypeError, fixpoint::FixPointInner,
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
        Type::CharValue(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
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

    fn reduce(self, _ctx: &mut ReductionContext) -> Result<Type, TypeError> {
        Ok(self.dispatch())
    }

    fn invoke(&self, _ctx: &mut InvokeContext) -> Result<Type, TypeError> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn is_normal_form(&self) -> bool {
        true
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
