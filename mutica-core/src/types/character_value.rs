use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

#[derive(Clone)]
pub struct CharacterValue {
    value: char,
}

impl<T: GcAllocObject<T>> GCTraceable<T> for CharacterValue {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T>> GcAllocObject<T> for CharacterValue {
    type Inner = Type<T>;
}
impl<T: GcAllocObject<T>> AsDispatcher<Type<T>, T> for CharacterValue {
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

impl<T: GcAllocObject<T>> Rootable<T> for CharacterValue {}

impl<T: GcAllocObject<T>> CoinductiveType<Type<T>, T> for CharacterValue {
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
                TypeRef::Generalize(v) => v.has(self, &mut inner_ctx),
                TypeRef::Specialize(v) => v.has(self, &mut inner_ctx),
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
        _ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
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
    pub fn new<T: GcAllocObject<T>>(value: char) -> Type<T> {
        Self { value }.dispatch()
    }

    pub fn value(&self) -> char {
        self.value
    }
}
