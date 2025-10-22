use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef, float_value::FloatValue, type_bound::TypeBound,
    },
    util::{cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic},
};

pub struct Float<T: GcAllocObject<T, Inner = Type<T>>>(std::marker::PhantomData<T>);

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Float<T> {
    fn clone(&self) -> Self {
        Self(std::marker::PhantomData)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Float<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Float<T> {}
impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for Float<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Float<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::Float(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::Float(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Float<T> {
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
                TypeRef::Float(_) => Ok(Some(())),
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
                TypeRef::FloatValue(_) => Ok(arg.clone_data()),
                TypeRef::IntegerValue(v) => {
                    let float_value = FloatValue::<T>::new(v.value() as f64);
                    Ok(float_value)
                }
                _ => Err(super::TypeError::TypeMismatch(
                    (ctx.arg.clone(), "FloatValue or IntegerValue".into()).into(),
                )),
            })?
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        ThreeValuedLogic::True
    }

    fn recalculate_normal_form(&self, _: &mut FastCycleDetector<TaggedPtr<()>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Float<T> {
    fn represent(&self, _path: &mut FastCycleDetector<TaggedPtr<()>>) -> String {
        "Integer".to_string()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Float<T> {
    pub fn new() -> Type<T> {
        Self(std::marker::PhantomData).dispatch()
    }
}
