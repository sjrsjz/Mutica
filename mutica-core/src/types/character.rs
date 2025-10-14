use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext, Representable,
        Rootable, Type, TypeCheckContext, TypeEnum, TypeError, character_value::CharacterValue,
        fixpoint::FixPointInner, type_bound::TypeBound,
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

impl CoinductiveType<Type> for Character {
    fn dispatch(self) -> Type {
        Type::new_nf(TypeEnum::Char(self))
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
                TypeEnum::Char(_) => Ok(Some(())),
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
                TypeEnum::IntegerValue(iv) => {
                    let v = iv.value();
                    if v > std::char::MAX as i64 || v < 0 {
                        return Err(TypeError::TypeMismatch(
                            (
                                ctx.arg.clone(),
                                "Expected a valid Unicode code point".into(),
                            )
                                .into(),
                        ));
                    }
                    Ok(CharacterValue::new(std::char::from_u32(v as u32).unwrap()))
                }
                TypeEnum::CharValue(_) => Ok(arg.clone()),
                _ => Err(TypeError::TypeMismatch(
                    (ctx.arg.clone(), "IntegerValue or CharValue".into()).into(),
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
    pub fn new() -> Type {
        Self {}.dispatch()
    }
}
