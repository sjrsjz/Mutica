use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        character_value::CharacterValue, type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

#[derive(Clone)]
pub struct Character {}

impl<T: GcAllocObject<T>> GCTraceable<T> for Character {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T>> GcAllocObject<T> for Character {}

impl<T: GcAllocObject<T>> Rootable<T> for Character {}

impl<T: GcAllocObject<T>> AsDispatcher<Type<T>, T> for Character {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::Char(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::Char(self)
    }
}

impl<T: GcAllocObject<T>> CoinductiveType<Type<T>, T> for Character {
    fn is(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Char(_) => Ok(Some(())),
                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::Specialize(v) => v.has(self, &mut inner_ctx),
                TypeRef::Generalize(v) => v.has(self, &mut inner_ctx),
                TypeRef::FixPoint(v) => v.has(self, &mut inner_ctx),
                TypeRef::Pattern(v) => v.has(self, &mut inner_ctx),
                TypeRef::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(
        self,
        _ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(self.dispatch())
    }

    fn invoke(
        &self,
        ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        ctx.arg.map(
            &mut FastCycleDetector::new(),
            |_, arg: &Type<T>| match arg {
                Type::<T>::IntegerValue(iv) => {
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
                    Ok(CharacterValue::new::<T>(
                        std::char::from_u32(v as u32).unwrap(),
                    ))
                }
                Type::<T>::CharValue(_) => Ok(arg.clone()),
                _ => Err(TypeError::TypeMismatch(
                    (ctx.arg.clone(), "IntegerValue or CharValue".into()).into(),
                )),
            },
        )?
    }

    fn is_normal_form(&self) -> bool {
        true
    }
}

impl Representable for Character {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        "Char".to_string()
    }
}

impl Character {
    pub fn new<T: GcAllocObject<T>>() -> Type<T> {
        Self {}.dispatch()
    }
}
