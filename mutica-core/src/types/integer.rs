use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        integer_value::IntegerValue, type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

#[derive(Clone)]
pub struct Integer {}

impl<T: GcAllocObject<T>> GCTraceable<T> for Integer {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T>> Rootable<T> for Integer {}
impl<T: GcAllocObject<T>> GcAllocObject<T> for Integer {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T>> AsDispatcher<Type<T>, T> for Integer {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::Integer(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::Integer(self)
    }
}

impl<T: GcAllocObject<T>> CoinductiveType<Type<T>, T> for Integer {
    fn is(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Integer(_) => Ok(Some(())),
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
        ctx.arg
            .map_inner(&mut FastCycleDetector::new(), |_, arg| match arg {
                TypeRef::IntegerValue(_) => Ok(arg.clone()),
                TypeRef::CharValue(c) => Ok(IntegerValue::new(c.value() as i64)),
                _ => Err(super::TypeError::TypeMismatch(
                    (ctx.arg.clone(), "IntegerValue or CharValue".into()).into(),
                )),
            })?
    }

    fn is_normal_form(&self) -> bool {
        true
    }
}

impl Representable for Integer {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        "Integer".to_string()
    }
}

impl Integer {
    pub fn new<T: GcAllocObject<T>>() -> Type<T> {
        Self {}.dispatch()
    }
}
