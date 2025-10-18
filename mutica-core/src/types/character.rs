use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        character_value::CharacterValue, type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

pub struct Character<T: GcAllocObject<T, Inner = Type<T>>>(std::marker::PhantomData<T>);

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Character<T> {
    fn clone(&self) -> Self {
        Self(std::marker::PhantomData)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Character<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for Character<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Character<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Character<T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Character<T> {
    fn fulfill(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Specialize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::Char(_) => Ok(Some(())),
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
            |_, arg: TypeRef<'_, T>| match arg {
                TypeRef::IntegerValue(iv) => {
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
                TypeRef::CharValue(_) => Ok(arg.clone_data()),
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Character<T> {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        "Char".to_string()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Character<T> {
    pub fn new() -> Type<T> {
        Self(std::marker::PhantomData).dispatch()
    }
}
