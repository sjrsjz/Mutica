use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext, Representable,
        Rootable, StabilizedType, Type, TypeCheckContext, TypeError,
        character_value::CharacterValue, fixpoint::FixPointInner, type_bound::TypeBound,
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
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
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

    fn invoke(&self, ctx: &mut InvokeContext) -> Result<StabilizedType, TypeError> {
        ctx.arg
            .map(&mut FastCycleDetector::new(), |_, arg| match arg {
                Type::IntegerValue(iv) => {
                    let v = iv.value();
                    if v > std::char::MAX as i64 || v < 0 {
                        return Err(TypeError::TypeMismatch(
                            ctx.arg.clone().stabilize().into(),
                            "Expected a valid Unicode code point".to_string(),
                        ));
                    }
                    Ok(CharacterValue::new(std::char::from_u32(v as u32).unwrap()))
                }
                Type::CharValue(_) => Ok(arg.clone().stabilize()),
                _ => Err(TypeError::TypeMismatch(
                    ctx.arg.clone().stabilize().into(),
                    "IntegerValue or CharValue".to_string(),
                )),
            })?
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
