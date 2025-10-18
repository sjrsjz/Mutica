use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        integer_value::IntegerValue, type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

pub struct Integer<T: GcAllocObject<T, Inner = Type<T>>>(std::marker::PhantomData<T>);

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Integer<T> {
    fn clone(&self) -> Self {
        Self(std::marker::PhantomData)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Integer<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Integer<T> {}
impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for Integer<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Integer<T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Integer<T> {
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
                TypeRef::Neg(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Rot(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::Integer(_) => Ok(Some(())),
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
                TypeRef::IntegerValue(_) => Ok(arg.clone_data()),
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Integer<T> {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        "Integer".to_string()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Integer<T> {
    pub fn new() -> Type<T> {
        Self(std::marker::PhantomData).dispatch()
    }
}
