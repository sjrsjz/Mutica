use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

pub struct CharacterValue<T: GcAllocObject<T, Inner = Type<T>>> {
    value: char,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for CharacterValue<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for CharacterValue<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for CharacterValue<T> {
    type Inner = Type<T>;
}
impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for CharacterValue<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::CharValue(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::CharValue(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for CharacterValue<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for CharacterValue<T> {
    fn is(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::CharValue(v) => {
                    if self.value == v.value {
                        Ok(Some(()))
                    } else {
                        Ok(None)
                    }
                }
                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::Char(_) => Ok(Some(())),
                TypeRef::Generalize(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Specialize(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
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
        _ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn is_normal_form(&self) -> bool {
        true
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for CharacterValue<T> {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{:?}", self.value)
    }

    fn display(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{}", self.value)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CharacterValue<T> {
    pub fn new(value: char) -> Type<T> {
        Self {
            value,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }

    pub fn value(&self) -> char {
        self.value
    }
}
